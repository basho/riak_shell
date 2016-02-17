%% -------------------------------------------------------------------
%%
%% The main shell runner file for riak_shell
%%
%% Copyright (c) 2007-2016 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(riak_shell).

%% main export
-export([
         start/1,
         start/3
        ]).

%% a test extension is designed to be used with riak_test invocation only
-export([
         init_TEST/1,
         loop_TEST/3
        ]).

%% various extensions like history which runs an old command
%% and load which reloads EXT modules need to call back into
%% riak_shell
-export([
         register_extensions/1,
         handle_cmd/2,
         read_config/3,
         send_to_shell/2
        ]).

-include("riak_shell.hrl").

-define(DONT_INCREMENT, false).
-define(DO_INCREMENT, true).
-define(IN_TEST, false).
-define(IN_PRODUCTION, true).

start(Config) ->
    Fun = fun() ->
                  main(Config, none, none)
          end,
    spawn(Fun).

start(Config, File, RunFileAs) ->
    {Msg, State} = main(Config, File, RunFileAs),
    Msg2 = lists:flatten(Msg),
    case State#state.cmd_error of
        false -> {ok,    Msg2};
        true  -> {error, Msg2}
    end.

main(Config, File, RunFileAs) ->
    State = init(Config),
    case File of
        none -> Msg = io_lib:format("version ~p, use 'quit;' or 'q;' to exit or " ++
                              "'help;' for help", [State#state.version]),
                loop(Msg, State, ?DONT_INCREMENT, ?IN_PRODUCTION);
        File -> run_file(State, File, RunFileAs)
    end.

run_file(State, File, RunFileAs) ->
    %% need to wait for a connection status
    receive
        {connected, {Node, Port}} ->
            NewS = State#state{has_connection = true,
                               connection     = {Node, Port}},
            {_M, _NS} = case RunFileAs of
                            "replay" ->
                                handle_cmd("replay_log \"" ++ File ++ "\";", NewS);
                            "regression" ->
                                handle_cmd("regression_log \"" ++ File ++ "\";", NewS)
                        end
    after
        5000 ->
            Msg = io_lib:format("Unable to connect to riak...", []),
            {Msg, State#state{cmd_error = true}}
    end.

%% this function is spawned to get_input
get_input(ReplyPID, Prompt) ->
    Cmd = io:get_line(standard_io, Prompt),
    send_to_shell(ReplyPID, {command, Cmd}).

send_to_shell(PID, Msg) ->
    PID ! Msg.

loop_TEST(Msg, #state{} = State, ShouldIncrement) 
  when is_list(Msg) andalso
       is_boolean(ShouldIncrement) -> 
    loop(Msg, State, ShouldIncrement, ?IN_TEST).

%% we pass the message around in the loop for printing on entering
%% because then it is available to the test module for introspection
loop(Msg, State, ShouldIncrement, IsProduction) ->
    io:format(Msg ++ "~n", []),
    {Prompt, NewState} = make_prompt(State, ShouldIncrement),
    Self = self(),
    Fun = fun() ->
                  get_input(Self, Prompt)
          end,
    %% in test we simulate standard io
    case IsProduction of
        true  -> spawn(Fun);
        false -> ok
    end,
    receive
        {command, Cmd} ->
            {Result, NewState2} = handle_cmd(Cmd, NewState),
            NewMsg = case Result of
                         [] -> "";
                         _  -> Result
                     end,
            maybe_yield(NewMsg, NewState2#state{cmd_error = false}, 
                        ?DO_INCREMENT, IsProduction);
        {connected, {Node, Port}} ->
            NewMsg = "Connected...",
            maybe_yield(NewMsg, NewState#state{has_connection = true,
                                               connection     = {Node, Port}},
                        ?DONT_INCREMENT, IsProduction);
        disconnected ->
            NewMsg = "Disconnected...",
            maybe_yield(NewMsg, NewState#state{has_connection = false,
                                               connection     = none},
                 ?DONT_INCREMENT, IsProduction);
        Other ->
            NewMsg = io_lib:format("Unhandled message received is ~p~n", [Other]),
            maybe_yield(NewMsg, NewState, ?DONT_INCREMENT, IsProduction)
    end.

maybe_yield(Msg, State, ShouldIncrement, ?IN_TEST) ->
    {Msg, State, ShouldIncrement};
maybe_yield(Msg, State, ShouldIncrment, ?IN_PRODUCTION) ->
    loop(Msg, State, ShouldIncrment, ?IN_PRODUCTION).

handle_cmd(Cmd, #state{} = State) ->
    {ok, Toks, _} = cmdline_lexer:string(Cmd),
    case is_complete(Toks, State) of
        {true, Toks2, State2} -> run_cmd(Toks2, Cmd, State2);
        {false, State2}       -> {"", State2}
    end.

is_complete(Toks, S) ->
    case lists:member({semicolon, ";"}, Toks) of
        true  -> Toks2 = S#state.partial_cmd ++ Toks,
                 NewState = S#state{partial_cmd = []},
                 Toks3 = left_trim(Toks2),
                 {true, Toks3, NewState};
        false -> NewP = S#state.partial_cmd ++ Toks,
                 {false, S#state{partial_cmd = NewP}}
    end.

left_trim([{whitespace, _} | T]) -> left_trim(T);
left_trim(X)                     -> X.

%% TODO add a riak-admin lexer/parser etc, etc
%% run_cmd([{atom, "riak"}, {hyphen, "-"}, {atom, "admin"} | _T] = _Toks, _Cmd, State) ->
%%     {"riak-admin is not supported yet", State};
run_cmd([{atom, Fn} | _T] = Toks, Cmd, State) ->
    case lists:member(normalise(Fn), [atom_to_list(X) || X <- ?IMPLEMENTED_SQL_STATEMENTS]) of
        true  -> run_sql_command(Cmd, State);
        false -> run_riak_shell_cmd(Toks, State)
    end;
run_cmd(_Toks, Cmd, State) ->
    {"Invalid Command: " ++ Cmd, State#state{cmd_error = true}}.

normalise(String) -> string:to_lower(String).

run_sql_command(_Cmd, #state{has_connection = false} = State) ->
    Msg = io_lib:format("Not connected to riak", []),
    {Msg, State#state{cmd_error = true}};
run_sql_command(Cmd, State) ->
    Cmd2 = string:strip(Cmd, both, $\n),
    try
        Toks = riak_ql_lexer:get_tokens(Cmd2),
        case riak_ql_parser:parse(Toks) of
            {error, Err} ->
                Msg1 = io_lib:format("SQL Parser error ~p", [Err]),
                {Msg1, State};
            {ok, _SQL} ->
                %% the server is going to reparse
                Result = connection_srv:run_sql_query(Cmd),
                NewState = log(Cmd, Result, State),
                NewState2 = add_cmd_to_history(Cmd, NewState),
                {Result, NewState2}
        end
    catch _:Error ->
            Msg2 = io_lib:format("SQL Lexer error ~p", [Error]),
            {Msg2, State}
    end.

run_riak_shell_cmd(Toks, State) ->
    case cmdline_parser:parse(Toks) of
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
    Cmd = [riak_shell_util:to_list(TkCh) || {_, TkCh} <- Toks],
    _Cmd2 = riak_shell_util:pretty_pr_cmd(lists:flatten(Cmd)).

add_cmd_to_history(Cmd, #state{history = Hs} = State) ->
    N = case Hs of
            []             -> 1;
            [{NH, _} | _T] -> NH + 1
        end,
    State#state{history = [{N, Cmd} | Hs]}.

%% help is a special function
run_ext({{help, 0}, []}, #state{extensions = E} = State) ->
    Msg1 = io_lib:format("The following functions are available~n", []),
    Msg2 = print_exts(E),
    Msg3 = io_lib:format("~nYou can get more help by calling help with the~n" ++
                             "extension name and function name like 'help shell quit;'", []),
   {Msg1 ++ Msg2 ++ Msg3,  State};
run_ext({{help, 1}, [Mod]}, #state{extensions = E} = State) ->
    ModAtom = list_to_atom(atom_to_list(Mod) ++ "_EXT"),
    Msg =
        case lists:filter(fun({_F, M}) when M =:= ModAtom -> true;
                             ({_F, _Mod}) -> false
                          end, E) of
            [] ->
                io_lib:format("No such extension found: ~ts. See 'help;'", [Mod]);
            List ->
                Msg1 = io_lib:format("The following functions are available~n", []),
                Msg2 = print_exts(List),
                Msg3 = io_lib:format("~nYou can get more help by calling help with the~n" ++
                                         "extension name and function name like 'help ~s ~s;'", [Mod, element(1, hd(List))]),
                Msg1 ++ Msg2 ++ Msg3
        end,
   {Msg,  State};
%% the help funs are not passed the state and can't change it
run_ext({{help, 2}, [Mod, Fn]}, State) ->
    Mod2 = extend_mod(Mod),
    Msg = try
              erlang:apply(Mod2, help, [Fn])
          catch _:_ ->
                  io_lib:format("There is no help for ~p : ~p",
                                [Mod, Fn])
          end,
    {Msg, State};
run_ext({{Ext, _Arity}, Args}, #state{extensions = E} = State) ->
    case lists:keysearch(Ext, 1, E) of
        {value, {Fn, Mod}} ->
            try
                erlang:apply(Mod, Fn, [State] ++ Args)
            catch _A:_B ->
                    Msg1 = io_lib:format("Error: invalid function call : ~p:~p ~p", [Mod, Fn, Args]),
                    Mod2 = cut_mod(Mod),
                    {Msg2, NewS} = run_ext({{help, 2}, [Mod2, Fn]}, State),
                    {Msg1 ++ "\n" ++ Msg2, NewS}
            end;
        false ->
            Msg = io_lib:format("Extension ~p not implemented.", [Ext]),
            {Msg, State}
    end.

extend_mod(Mod) ->
    list_to_atom(atom_to_list(Mod) ++ "_EXT").

cut_mod(Mod) ->
    Mod2 = lists:reverse(atom_to_list(Mod)),
    "TXE_" ++ Rest = Mod2,
    list_to_atom(lists:reverse(Rest)).

make_prompt(S = #state{count       = SQLN,
                       partial_cmd = []}, ShouldIncrement) ->
    Prefix = make_prefix(S),
    NewCount = case ShouldIncrement of
                   true  -> SQLN + 1;
                   false -> SQLN
               end,
    Prompt =  Prefix ++ "riak-shell(" ++ integer_to_list(NewCount) ++ ")>",
    {Prompt, S#state{count = NewCount}};
make_prompt(S, _ShouldIncrement) ->
    Prompt = "->",
    {Prompt, S}.

make_prefix(#state{show_connection_status = false}) ->
    "";
make_prefix(#state{show_connection_status = true,
                   has_connection         = false}) ->
    ?REDCROSS ++ " ";
make_prefix(#state{show_connection_status = true,
                   has_connection         = true}) ->
    ?GREENTICK ++ " ".

init_TEST(Config) -> init(Config).

init(Config) ->
    %% do some housekeeping
    process_flag(trap_exit, true),
    State = State = #state{config = Config},
    State2 = set_logging_defaults(State),
    State3 = set_connection_defaults(State2),
    State4 = set_prompt_defaults(State3),
    _State5 = register_extensions(State4).

set_logging_defaults(#state{config = Config} = State) ->
    Logfile  = read_config(Config, logfile, State#state.logfile),
    Logging  = read_config(Config, logging, State#state.logging),
    Date_Log = read_config(Config, date_log, State#state.date_log),
    State#state{logfile  = Logfile,
                logging  = Logging,
                date_log = Date_Log}.

set_connection_defaults(#state{config = Config,
                               cookie = Cookie} = State) ->
    Cookie2 = read_config(Config, cookie, Cookie),
    true = erlang:set_cookie(node(), Cookie2),
    _State = connect(State#state{cookie = Cookie2}).

connect(#state{config = Config} = State) ->
    Nodes = read_config(Config, nodes, []),
    {ok, _ChildPid} = supervisor:start_child(connection_sup, [self(), Nodes]),
    State.

set_prompt_defaults(#state{config = Config} = State) ->
    Default = State#state.show_connection_status,
    Status = read_config(Config, show_connection_status, Default),
    State#state{show_connection_status = Status}.

read_config(Config, Key, Default) when is_list(Config) andalso
                                       is_atom(Key) ->
    case lists:keyfind(Key, 1, Config) of
        {Key, V} -> V;
        false    -> Default
    end.

register_extensions(#state{} = S) ->
    %% the application may already be loaded to don't check
    %% the return value
    _ = application:load(riak_shell),
    {ok, Mods} = application:get_key(riak_shell, modules),
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
    case validate_extensions(E) of
        ok    -> State;
        error -> shell_EXT:quit(State)
    end;
register_e2([Mod | T], #state{extensions = E} = State) ->
    Fns = [{Fn, Mod} || {Fn, Arity} <- Mod:module_info(exports),
                        {Fn, Arity} =/= {help, 1},
                        {Fn, Arity} =/= {module_info, 0},
                        {Fn, Arity} =/= {module_info, 1},
                        Arity =/= 0,
                        Fn =/= 'riak-admin',
                        not lists:member(Fn, ?IMPLEMENTED_SQL_STATEMENTS)],
    register_e2(T, State#state{extensions = lists:usort(Fns) ++ E}).

is_extension(Module) ->
    case lists:reverse(atom_to_list(Module)) of
        "TXE_" ++ _Rest -> true;
        _               -> false
    end.

validate_extensions(Extensions) ->
    case identify_multiple_definitions(Extensions) of
        [] ->
            ok;
        Problems ->
            print_errors(Problems),
            error
    end.

identify_multiple_definitions(Extensions) ->
    Funs = lists:usort(Extensions),
    CollateFn = fun({Fun, Ext}, Acc) ->
                        NewL = case lists:keyfind(Fun, 1, Acc) of
                                   {Fun, L} -> [Ext | L];
                                   false    -> [Ext]
                               end,
                        lists:keystore(Fun, 1, Acc, {Fun, NewL})
                end,
    AllFnDefs = lists:foldl(CollateFn, [], Funs),
    %% if a fn is exposed in more than one EXT it is an error
    [{Fn, Exts} || {Fn, Exts} <- AllFnDefs,
                   length(Exts) > 1].

print_errors([]) ->
    io:format("Shell cannot start because of invalid extensions~n");
print_errors([{Fn, Mods} | T]) ->
    io:format("Function '~p' is multiply defined:~n", [Fn]),
    [io:format("- ~p~n", [X]) || X <- Mods],
    print_errors(T).

print_exts(E) ->
    Grouped = group(E, []),
    lists:flatten([begin
                       io_lib:format("~nExtension '~s':~n", [Mod]) ++
                       riak_shell_util:print_help(Fns)
                   end || {Mod, Fns} <- Grouped]).

group([], Acc) ->
    [{Mod, lists:sort(L)} || {Mod, L} <- lists:sort(Acc)];
group([{Fn, Mod} | T], Acc) ->
    Mod2 = shrink(Mod),
    NewAcc = case lists:keyfind(Mod2, 1, Acc) of
                 false ->
                     [{Mod2, [Fn]} | Acc];
                 {Mod2, A2} ->
                     lists:keyreplace(Mod2, 1, Acc, {Mod2, [Fn | A2]})
             end,
    group(T, NewAcc).

shrink(Atom) ->
    re:replace(atom_to_list(Atom), "_EXT", "", [{return, list}]).

log(_Cmd, _Result, #state{logging = off} = State) ->
    State#state{log_this_cmd = true};
log(_Cmd, _Result, #state{log_this_cmd = false} = State) ->
    State#state{log_this_cmd = true};
log(Cmd, Result, #state{logging      = on,
                        date_log     = IsDateLog,
                        logfile      = LogFile,
                        current_date = Date} = State) ->
    File = case IsDateLog of
               on  -> LogFile ++ "." ++ Date ++ ".log";
               off -> LogFile ++ ".log"
           end,
    _FileName = filelib:ensure_dir(File),
    R2 = re:replace(Result, "\\\"", "\\\\\"", [
                                               unicode,
                                               global,
                                               {return, list}
                                              ]),
    case file:open(File, [append, {encoding, utf8}]) of
        {ok, Id} ->
            Msg = io_lib:format("{{command, ~p}, {result, \"" ++ R2 ++ "\"}}.~n",
                                [Cmd]),
            io:put_chars(Id, Msg),
            file:close(Id);
        Err  ->
            io:format("Cannot log due to error: ~p~n", [Err]),
            shell_EXT:quit(#state{})
    end,
    State#state{log_this_cmd = true}.
