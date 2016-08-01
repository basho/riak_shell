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
         start/2,
         start/5
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
         handle_cmd/3,
         make_cmd/0, make_cmd/1,
         maybe_print_exception/3,
         read_config/3,
         register_extensions/1,
         send_to_shell/2
        ]).

-include("riak_shell.hrl").

-define(DONT_INCREMENT, false).
-define(DO_INCREMENT, true).
-define(IN_TEST, false).
-define(IN_PRODUCTION, true).

start(Config, DefaultLogFile) ->
    Fun = fun() ->
                  main(Config, DefaultLogFile, none, none, false)
          end,
    spawn(Fun).

start(Config, DefaultLogFile, File, RunFileAs, Debug) ->
    {Response, _State} = main(Config, DefaultLogFile, File, RunFileAs, Debug),
    Msg = lists:flatten(Response#command.response),
    case Response#command.cmd_error of
        false -> {ok,    Msg};
        true  -> {error, Msg}
    end.

main(Config, DefaultLogFile, File, RunFileAs, Debug) ->
    State = init(Config, DefaultLogFile, Debug),
    case File of
        none -> io:format("version ~p, use 'quit;' or 'q;' to exit or " ++
                          "'help;' for help~n", [State#state.version]),
                loop(#command{}, State, ?DONT_INCREMENT, ?IN_PRODUCTION);
        File -> run_file(#command{}, State, File, RunFileAs)
    end.

run_file(Cmd, State, File, RunFileAs) ->
    %% need to wait for a connection status
    receive
        {connected, {Node, Port}} ->
            NewS = State#state{has_connection = true,
                               connection     = {Node, Port}},
            {_Cmd, _NS} = case RunFileAs of
                            "replay" ->
                                handle_cmd("replay_log \"" ++ File ++ "\";", Cmd, NewS);
                            "regression" ->
                                handle_cmd("regression_log \"" ++ File ++ "\";", Cmd, NewS)
                        end
    after
        5000 ->
            Msg = io_lib:format("Unable to connect to riak...", []),
            {Cmd#command{response  = Msg,
                         cmd_error = true}, State}
    end.

%% this function is spawned to get_input
get_input(ReplyPID, Prompt) ->
    Input = io:get_line(standard_io, Prompt),
    send_to_shell(ReplyPID, {command, Input}).

send_to_shell(PID, Msg) ->
    PID ! Msg.

loop_TEST(#command{} = Cmd, #state{} = State, ShouldIncrement)
  when is_boolean(ShouldIncrement) ->
    loop(Cmd, State, ShouldIncrement, ?IN_TEST).

loop(Cmd, State, ShouldIncrement, IsProduction) ->
    {Prompt, NewState} = make_prompt(Cmd, State, ShouldIncrement),
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
        {command, Input} ->
            {Cmd2, NewState2} = handle_cmd(Input, Cmd, NewState),
            maybe_yield(Cmd2#command.response, Cmd2, NewState2,
                        ?DO_INCREMENT, IsProduction);
        {connected, {Node, Port}} ->
            Response = "Connected...",
            maybe_yield(Response, Cmd, NewState#state{has_connection = true,
                                                      connection     = {Node, Port}},
                        ?DONT_INCREMENT, IsProduction);
        disconnected ->
            Response = "Disconnected...",
            maybe_yield(Response, Cmd, NewState#state{has_connection = false,
                                                      connection     = none},
                 ?DONT_INCREMENT, IsProduction);
        Other ->
            Response = io_lib:format("Unhandled message received is ~p~n", [Other]),
            maybe_yield(Response, Cmd, NewState, ?DONT_INCREMENT, IsProduction)
    end.

maybe_yield([], Cmd, State, ShouldIncrement, ?IN_TEST) ->
    {Cmd#command.cmd_error, "", State, ShouldIncrement};
maybe_yield(Result, Cmd, State, ShouldIncrement, ?IN_TEST) ->
    {Cmd#command.cmd_error, lists:flatten(Result), State, ShouldIncrement};
maybe_yield([], Cmd, State, ShouldIncrement, ?IN_PRODUCTION) ->
    loop(Cmd, State, ShouldIncrement, ?IN_PRODUCTION);
maybe_yield(Result, Cmd, State, ShouldIncrement, ?IN_PRODUCTION) ->
    io:format(Result ++ "~n"),
    loop(Cmd, State, ShouldIncrement, ?IN_PRODUCTION).

%% Used by external programmes (riak_test) to avoid including the header
make_cmd() ->
    #command{}.
make_cmd(Input) ->
    #command{cmd = Input}.

handle_cmd(Input, #command{} = Cmd, #state{} = State) ->
    case is_complete(Input, Cmd) of
        {true, Toks2, Cmd2} ->
            run_cmd(full_cmd_name(Toks2), Cmd2, State);
        {false, Cmd2} ->
            {Cmd2#command{response = ""}, State}
    end.

% Convert hyphenated or underscored commands into single command names
% Anything else is not valid
full_cmd_name([{atom, Fn1},{underscore, _},{atom, Fn2} | _T] = Toks) ->
    {normalise(Fn1 ++ "_" ++ Fn2), Toks};
full_cmd_name([{atom, Fn1},{hyphen, _},{atom, Fn2} | _T] = Toks) ->
    {normalise(Fn1 ++ "-" ++ Fn2), Toks};
full_cmd_name([{atom, Fn} | _T] = Toks) ->
    {normalise(Fn), Toks};
full_cmd_name(Toks) ->
    {invalid, Toks}.

normalise(String) -> string:to_lower(String).

%% If the current line is not complete, then cache
%% the input so far into partial_cmd
is_complete(Input, Cmd) ->
    {ok, Toks, _} = cmdline_lexer:string(Input),
    NewCmd = Cmd#command.partial_cmd ++ Input,
    NewToks = Cmd#command.partial_tokens ++ Toks,

    case lists:member({semicolon, ";"}, Toks) of
        true  ->
            Trimmed = left_trim(NewToks),
            {true, Trimmed, #command{cmd = NewCmd}};
        false ->
            {false, Cmd#command{cmd            = [],
                                partial_tokens = NewToks,
                                partial_cmd    = NewCmd}}
    end.

left_trim([{whitespace, _} | T]) -> left_trim(T);
left_trim(X)                     -> X.

%% TODO add a riak-admin lexer/parser etc, etc
run_cmd({invalid, _Toks}, Cmd, State) ->
    {Cmd#command{response  = "Invalid Command: " ++ Cmd#command.cmd,
        cmd_error = true}, State};
run_cmd({Fn, Toks}, Cmd, State) ->
    case lists:member(Fn, [atom_to_list(X) || X <- ?IMPLEMENTED_SQL_STATEMENTS]) of
        true  -> run_sql_command(Cmd, State);
        false -> run_riak_shell_cmd(Toks, Cmd, State)
    end.

run_sql_command(Cmd, #state{has_connection = false} = State) ->
    Msg = io_lib:format("Not connected to riak", []),
    {Cmd#command{response  = Msg,
                 cmd_error = true}, State};
run_sql_command(Cmd, State) ->
    Input = string:strip(Cmd#command.cmd, both, $\n),
    try
        Toks = riak_ql_lexer:get_tokens(Input),
        case riak_ql_parser:parse(Toks) of
            {error, Err} ->
                Msg1 = io_lib:format("SQL Parser error ~p", [Err]),
                {Cmd#command{response = Msg1}, State};
            {ok, _SQL} ->
                %% the server is going to reparse
                Result = connection_srv:run_sql_query(Input),
                Cmd2 = Cmd#command{response = Result},
                {Cmd3, NewState} = log(Cmd2, State),
                NewState2 = add_cmd_to_history(Cmd3, NewState),
                {Cmd3, NewState2}
        end
    catch _:Error ->
            Msg2 = io_lib:format("SQL Lexer error ~p", [Error]),
            {Cmd#command{response = Msg2},  State}
    end.

run_riak_shell_cmd(Toks, Cmd, State) ->
    case cmdline_parser:parse(Toks) of
        {ok, {{Fn, Arity}, Args}} ->
            Input = toks_to_string(Toks),
            {Cmd2, NewState} = run_ext({{Fn, Arity}, Args}, Cmd#command{cmd = Input}, State),
            {Response2, NewState2} = log(Cmd2, NewState),
            NewState3 = add_cmd_to_history(Cmd, NewState2),
            Msg1 = try
                       Response2#command.response
                   catch Type:Err ->
                       maybe_print_exception(State, Type, Err),
                       "The extension did not return printable output. " ++
                            "Please report this bug to the EXT developer."
                   end,
            {Response2#command{response = Msg1}, NewState3};
        Error ->
            Msg2 = io_lib:format("Error: ~p", [Error]),
            {Cmd#command{response  = Msg2,
                         cmd_error = true}, State}
        end.

toks_to_string(Toks) ->
    Input = [riak_shell_util:to_list(TkCh) || {_, TkCh} <- Toks],
    _Input2 = riak_shell_util:pretty_pr_cmd(lists:flatten(Input)).

add_cmd_to_history(#command{cmd = Input}, #state{history = Hs} = State) ->
    N = case Hs of
            []             -> 1;
            [{NH, _} | _T] -> NH + 1
        end,
    State#state{history = [{N, Input} | Hs]}.

%% help is a special function
run_ext({{help, 0}, []}, Cmd, #state{extensions = E} = State) ->
    Msg1 = io_lib:format("The following functions are available~n", []),
    Msg2 = print_exts(E),
    Msg3 = io_lib:format("~nYou can get more help by calling help with the~n" ++
                             "extension name and function name like 'help shell quit;'", []),
   {Cmd#command{response = Msg1 ++ Msg2 ++ Msg3},  State};
run_ext({{help, 1}, [Mod]}, Cmd, #state{extensions = E} = State) ->
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
   {Cmd#command{response = Msg},  State};
%% the help funs are not passed the state and can't change it
run_ext({{help, 2}, [Mod, Fn]}, Cmd, State) ->
    Mod2 = extend_mod(Mod),
    Msg = try
              erlang:apply(Mod2, help, [Fn])
          catch Type:Err ->
              maybe_print_exception(State, Type, Err),
              io_lib:format("There is no help for ~p : ~p",
                                [Mod, Fn])
          end,
    {Cmd#command{response = Msg}, State};
run_ext({{Ext, _Arity}, Args}, Cmd, #state{extensions = E} = State) ->
    case lists:keysearch(Ext, 1, E) of
        {value, {Fn, Mod}} ->
            try
                erlang:apply(Mod, Fn, [Cmd, State] ++ Args)
            catch Type:Err ->
                riak_shell:maybe_print_exception(State, Type, Err),
                Msg1 = io_lib:format("Error: invalid function call : ~p:~p ~p", [Mod, Fn, Args]),
                Mod2 = cut_mod(Mod),
                {Cmd2, NewS} = run_ext({{help, 2}, [Mod2, Fn]}, Cmd, State),
                {Cmd2#command{response  = Msg1 ++ "\n" ++ Cmd2#command.response,
                              cmd_error = true} , NewS}
            end;
        false ->
            Msg = io_lib:format("Extension ~p not implemented.", [Ext]),
            {Cmd#command{response  = Msg,
                         cmd_error = true}, State}
    end.

extend_mod(Mod) ->
    list_to_atom(atom_to_list(Mod) ++ "_EXT").

cut_mod(Mod) ->
    Mod2 = lists:reverse(atom_to_list(Mod)),
    "TXE_" ++ Rest = Mod2,
    list_to_atom(lists:reverse(Rest)).

make_prompt(#command{partial_cmd = []}, S = #state{count = SQLN}, ShouldIncrement) ->
    Prefix = make_prefix(S),
    NewCount = case ShouldIncrement of
                   true  -> SQLN + 1;
                   false -> SQLN
               end,
    Prompt =  Prefix ++ "riak-shell(" ++ integer_to_list(NewCount) ++ ")>",
    {Prompt, S#state{count = NewCount}};
make_prompt(_Cmd, S, _ShouldIncrement) ->
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

init_TEST(Config) -> init(Config, undefined, true).

init(Config, DefaultLogFile, Debug) ->
    %% do some housekeeping
    process_flag(trap_exit, true),
    State = #state{config = Config,
                   debug  = Debug},
    State1 = set_version_string(State),
    State2 = set_logging_defaults(State1, DefaultLogFile),
    State3 = set_connection_defaults(State2),
    connect(State3),
    State4 = set_prompt_defaults(State3),
    _State5 = register_extensions(State4).

set_version_string(State) ->
    Vsn = lists:flatten(io_lib:format("riak_shell ~s/sql compiler ~b",
                                      [?VERSION_NUMBER,
                                      riak_ql_ddl_compiler:get_compiler_version()])),
    State#state{version=Vsn}.

set_logging_defaults(#state{config = Config} = State, DefaultLogFile) ->
    Logfile  = read_config(Config, logfile, DefaultLogFile),
    Logging  = read_config(Config, logging, State#state.logging),
    Date_Log = read_config(Config, date_log, State#state.date_log),
    State#state{logfile  = Logfile,
                logging  = Logging,
                date_log = Date_Log}.

set_connection_defaults(#state{config = Config,
                               cookie = Cookie} = State) ->
    Cookie2 = read_config(Config, cookie, Cookie),
    true = erlang:set_cookie(node(), Cookie2),
    State.

connect(#state{config = Config} = _State) ->
    Nodes = read_config(Config, nodes, []),
    {ok, _ChildPid} = supervisor:start_child(connection_sup, [self(), Nodes]),
    ok.

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

log(Cmd, #state{logging = off} = State) ->
    {Cmd#command{log_this_cmd = true}, State};
log(#command{log_this_cmd = false} = Cmd, State) ->
    {Cmd#command{log_this_cmd = true}, State};
log(Cmd, #state{logging      = on,
                date_log     = IsDateLog,
                logfile      = LogFile,
                current_date = Date} = State) ->
    File = case IsDateLog of
               on  -> LogFile ++ "." ++ Date ++ ".log";
               off -> LogFile ++ ".log"
           end,
    _FileName = filelib:ensure_dir(File),
    R2 = re:replace(Cmd#command.response,
                    "\\\"", "\\\\\"", [
                                       unicode,
                                       global,
                                       {return, list}
                                      ]),
    case file:open(File, [append, {encoding, utf8}]) of
        {ok, Id} ->
            Msg = io_lib:format("{{command, ~p}, {result, \"" ++ R2 ++ "\"}}.~n",
                                [Cmd#command.cmd]),
            io:put_chars(Id, Msg),
            file:close(Id);
        Err  ->
            io:format("Cannot log due to error: ~p~n", [Err]),
            shell_EXT:quit(#state{})
    end,
    {Cmd#command{log_this_cmd = true}, State}.

maybe_print_exception(#state{debug = on}, Type, Err) ->
    io:format("Caught: ~p:~p~n", [Type, Err]);
maybe_print_exception(_State, _type, _Err) ->
    ok.
