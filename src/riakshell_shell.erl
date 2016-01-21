-module(riakshell_shell).

%% main export
-export([
         start/0
        ]).

%% various extensions like history which runs an old command
%% and load which reloads EXT modules need to call back into
%% riakshell_shell
-export([
         register_extensions/1,
         handle_cmd/2
        ]).

-include("riakshell.hrl").

start() -> spawn(fun main/0).

main() ->
    State = startup(),
    process_flag(trap_exit, true),
    io:format("riakshell ~p, write quit();' to exit or " ++
                  "'help();' for help~n", [State#state.version]),
    loop(State).

loop(State) ->
    {Prompt, NewState} = make_prompt(State),
    Cmd = io:get_line(standard_io, Prompt),
    {Result, NewState2} = handle_cmd(Cmd, NewState),
    case Result of
        [] -> ok;
        _  -> io:format(Result ++ "~n")
    end,
    loop(NewState2).

handle_cmd(Cmd, #state{mode = riakshell} = State) ->
    {ok, Toks, _} = cmdline_lexer:string(Cmd),
    case is_complete(Toks, State) of
        {true, Toks2, State2} -> run_cmd(Toks2, State2);
        {false, State2}       -> {"", State2}
    end;
handle_cmd(Cmd, #state{mode = sql} = State) ->
    Cmd2 = string:strip(Cmd, both, $\n),
    try
        Toks = riak_ql_lexer:get_tokens(Cmd2),
        io:format("Toks is ~p~n", [Toks]),
        case riak_ql_parser:parse(Toks) of
            {error, Err} ->
                maybe_switch_mode(Cmd, State, Err);
            SQL ->
                io:format("SQL is ~p~n", [SQL]),
                Result = "SQL not implemented",
                NewState = log(Cmd, Result, State),
                NewState2 = add_cmd_to_history(Cmd, NewState),
                {Result, NewState2}
        end
    catch _:Error ->
            maybe_switch_mode(Cmd, State, Error)
    end;
handle_cmd(Cmd, #state{mode = riak_admin} = State) ->
    Result = "riak-admin not implemented",
    NewState = log(Cmd, Result, State),
    NewState2 = add_cmd_to_history(Cmd, NewState),
    %% there is a double log bug but it is not worth fixing
    %% until the riak_admin lexer/parser is written
    maybe_switch_mode(Cmd, NewState2, Result).

is_complete(Toks, S) ->
    case lists:member({semicolon, ";"}, Toks) of
        true  -> Toks2 = S#state.partial_cmd ++ Toks,
                 NewState = S#state{partial_cmd = []},
                 {true, Toks2, NewState};
        false -> NewP = S#state.partial_cmd ++ Toks,
           {false, S#state{partial_cmd = NewP}}
    end.

maybe_switch_mode(Cmd, State, Err) ->
    {ok, Toks, _} = cmdline_lexer:string(Cmd),
    case cmdline_parser:compile(Toks) of
        {ok, {{Mode, 0}, []}} when Mode =:= sql        orelse
                                   Mode =:= riakshell  orelse
                                   Mode =:= riak_admin ->
            run_cmd(Toks, State);
        Other ->
            io:format("Other is ~p~n", [Other]),
            {io_lib:format("Error: ~p", [Err]), State}
    end.

run_cmd(Toks, State) ->
    case cmdline_parser:compile(Toks) of
        {ok, {{Fn, Arity}, Args}} ->
            Cmd = toks_to_string(Toks),
            {Result, NewState} = run_ext({{Fn, Arity}, Args}, State),
            NewState2 = log(Cmd, Result, NewState),
            NewState3 = add_cmd_to_history(Cmd, NewState2),
            Msg1 = try
                       io_lib:format(Result, [])
                   catch _:_ ->
                           io_lib:format("The extension did not return printable output. " ++
                                             "Please report this bug to the EXT developer.", [])
                   end,
            {Msg1, NewState3};
        Error ->
            Msg2 = io_lib:format("Error: ~p", [Error]),
            {Msg2, State}
        end.

toks_to_string(Toks) ->
    Cmd = [riakshell_util:to_list(TkCh) || {_, TkCh} <- Toks],
    _Cmd2 = riakshell_util:pretty_pr_cmd(lists:flatten(Cmd)).

add_cmd_to_history(Cmd, #state{history = Hs} = State) ->
    N = case Hs of
            []             -> 1;
            [{NH, _} | _T] -> NH + 1
        end,
    State#state{history = [{N, Cmd} | Hs]}.

%% help is a special function
run_ext({{help, 0}, []}, #state{extensions = E} = State) ->
    Msg1 = io_lib:format("The following functions are available~n\r" ++
                             "(the number of arguments is given)~n~n\r", []),
    Msg2 = print_exts(E),
    Msg3 = io_lib:format("~nYou can get more help by calling help with the~n" ++
                             "function name and arguments like 'help(quit, 0);'", []),
   {Msg1 ++ Msg2 ++ Msg3,  State};
%% the help funs are not passed the state and can't change it
run_ext({{help, 2}, [Fn, Arity]}, #state{extensions = E} = State) ->
    Msg = case lists:keysearch({Fn, Arity}, 1, E) of
        {value, {{_, _}, Mod}} ->
            try
                erlang:apply(Mod, help, [Fn, Arity])
            catch _:_ ->
                    io_lib:format("There is no help for ~p",
                                  [{Fn, Arity}])
            end;
        false ->
            io_lib:format("There is no help for ~p", [{Fn, Arity}])
    end,
    {Msg, State};
run_ext({Ext, Args}, #state{extensions = E} = State) ->
    case lists:keysearch(Ext, 1, E) of
        {value, {{Fn, _}, Mod}} ->
            try
                erlang:apply(Mod, Fn, [State] ++ Args)
            catch _:_ ->
                    Msg1 = io_lib:format("Error: invalid function call : ~p:~p(~p)", [Mod, Fn, Args]),
                    {Msg2, NewS} = run_ext({{help, 2}, [Fn, length(Args)]}, State),
                    {Msg1 ++ Msg2, NewS}
            end;
        false ->
            Msg = io_lib:format("Extension ~p not implemented.", [Ext]),
            {Msg, State}
    end.

make_prompt(S = #state{mode        = Mode,
                       count       = SQLN,
                       partial_cmd = []}) ->
    Prompt = atom_to_list(Mode) ++ " (" ++ integer_to_list(SQLN) ++ ")>",
    {Prompt, S#state{count = SQLN + 1}};
make_prompt(S) ->
    Prompt = "->",
    {Prompt, S}.

startup() ->
    State = try
                load_config()
            catch
                _ ->
                    io:format("Invalid_config~n", []),
                    exit(invalid_config)
            end,
    register_extensions(State).

load_config() ->
    try
        {ok, Config} = file:consult(?CONFIGFILE),
        State = #state{config = Config},
        _State2 = set_logging_defaults(State)
    catch _:_ ->
            io:format("Cannot read configfile ~p~n", [?CONFIGFILE]),
            exit('cannot start riakshell')
    end.

set_logging_defaults(#state{config = Config} = State) ->
    Logfile  = read_config(Config, logfile, State#state.logfile),
    Logging  = read_config(Config, logging, State#state.logging),
    Date_Log = read_config(Config, date_log, State#state.date_log),
    State#state{logfile  = Logfile,
                logging  = Logging,
                date_log = Date_Log}.

read_config(Config, Key, Default) when is_list(Config) andalso
                                       is_atom(Key) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, V} -> V;
        false    -> Default
    end.

register_extensions(#state{} = S) ->
    %% the application may already be loaded to don't check
    %% the return value
    _ = application:load(riakshell),
    {ok, Mods} = application:get_key(riakshell, modules),
    %% this might be a call to reload modules so delete
    %% and purge them first
    ReloadFn = fun(X) ->
                       code:delete(X),
                       code:purge(X)
               end,
    [ReloadFn(X) || X <- Mods,
                    is_extension(X),
                    X =/= debug_EXT],
    %% now load the modules
    [{module, X} = code:ensure_loaded(X) || X <- Mods],
    %% now going to register the extensions
    Extensions = [X || X <- Mods, is_extension(X)],
    register_e2(Extensions, S#state{extensions = []}).

register_e2([], #state{extensions = E} = State) ->
    validate_extensions(E),
    State;
register_e2([Mod | T], #state{extensions = E} = State) ->
    %% a fun that appears in the shell like
    %% 'fishpaste(bleh, bloh, blah)'
    %% is implemented like this
    %% 'fishpaste(#state{} = S, Arg1, Arg2, Arg3) ->
    %% so reduce the arity by 1
    Fns = [{{Fn, Arity - 1}, Mod} || {Fn, Arity} <- Mod:module_info(exports),
                                     {Fn, Arity} =/= {help, 2},
                                     {Fn, Arity} =/= {module_info, 0},
                                     {Fn, Arity} =/= {module_info, 1},
                                     Arity =/= 0],
    register_e2(T, State#state{extensions = Fns ++ E}).

is_extension(Module) ->
    case lists:reverse(atom_to_list(Module)) of
        "TXE_" ++ _Rest -> true;
        _               -> false
    end.

validate_extensions(Extensions) ->
    v2(lists:sort(Extensions), []).

v2([], []) -> ok;
v2([], Acc) -> print_errors(Acc);
v2([{{Fn, Arity}, Mod1}, {{Fn, Arity}, Mod2} | T], Acc) ->
    v2(T, [{{Fn, Arity}, [Mod1, Mod2]} | Acc]);
v2([_H | T], Acc) ->
    v2(T, Acc).

print_errors([]) ->
    exit("Shell cannot start because of invalid extensions");
print_errors([{{Fn, Arity}, Mods} | T]) ->
    io:format("function ~p ~p is multiply defined in ~p~n", [Fn, Arity, Mods]),
    print_errors(T).

print_exts(E) ->
    Grouped = group(E, []),
    lists:flatten([begin
                       io_lib:format("~nExtension '~s' provides:~n", [Mod]) ++
                       riakshell_util:printkvs(Fns)
                   end || {Mod, Fns} <- Grouped]).

group([], Acc) ->
    [{Mod, lists:sort(L)} || {Mod, L} <- lists:sort(Acc)];
group([{FnArity, Mod} | T], Acc) ->
    Mod2 = shrink(Mod),
    NewAcc = case lists:keyfind(Mod2, 1, Acc) of
                 false ->
                     [{Mod2, [FnArity]} | Acc];
                 {Mod2, A2} ->
                     lists:keyreplace(Mod2, 1, Acc, {Mod2, [FnArity | A2]})
             end,
    group(T, NewAcc).

shrink(Atom) ->
    re:replace(atom_to_list(Atom), "_EXT", "", [{return, list}]).

log(_Cmd, _Result, #state{logging = off} = State) ->
    State#state{log_this_cmd = true};
log(_Cmd, _Result, #state{log_this_cmd = false} = State) ->
    State#state{log_this_cmd = true};
log(Cmd, Result, #state{mode         = Mode,
                        logging      = on,
                        date_log     = IsDateLog,
                        logfile      = LogFile,
                        current_date = Date} = State) ->
    File = case IsDateLog of
               on  -> LogFile ++ "." ++ Date ++ ".log";
               off -> LogFile ++ ".log"
           end,
    _FileName = filelib:ensure_dir(File),
    Result2 = re:replace(Result, "\\\"", "\\\\\"", [global, {return, list}]),
    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "{{command, ~p, ~p}, {result, \"" ++ Result2 ++ "\"}}.~n",
                      [Mode, Cmd]),
            file:close(Id);
        Err  ->
            exit({'Cannot log', Err})
    end,
    State#state{log_this_cmd = true}.
