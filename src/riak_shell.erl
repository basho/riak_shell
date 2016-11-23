
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
         start/3,
         start/6
        ]).

%% exported to allow the io subsystem to scan tokens one by one
-export([
         scan_riak_shell_cmd/1
        ]).

%% a test extension is designed to be used with riak_test invocation only
-export([
         init_TEST/1,
         loop_TEST/3,
         make_cmd_TEST/0,
         make_cmd_TEST/1
        ]).

%% various extensions like history which runs an old command
%% and load which reloads EXT modules need to call back into
%% riak_shell
-export([
         handle_cmd/2,
         maybe_print_exception/3,
         read_config/3,
         register_extensions/1,
         send_to_shell/2
        ]).

-include("riak_shell.hrl").

-define(DONT_INCREMENT, false).
-define(DO_INCREMENT,   true).
-define(IN_TEST,        false).
-define(IN_PRODUCTION,  true).

-type prompt() :: atom() | unicode:chardata().

%% ErrorDescription is whatever the I/O-server sends.
-type server_no_data() :: {'error', ErrorDescription :: term()} | 'eof'.

start(Config, DefaultLogFile, Format) ->
    Fun = fun() ->
                  main(Config, DefaultLogFile, none, none, false, Format)
          end,
    spawn(Fun).

start(Config, DefaultLogFile, File, RunFileAs, Debug, Format) ->
    {Response, _State} = main(Config, DefaultLogFile, File, RunFileAs, Debug, Format),
    Msg = lists:flatten(Response#command.response),
    case Response#command.cmd_error of
        false -> {ok,    Msg};
        true  -> {error, Msg}
    end.

main(Config, DefaultLogFile, File, RunFileAs, Debug, Format) ->
    State = init(Config, DefaultLogFile, Debug, Format),
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
            Input =  case RunFileAs of
                         "replay"     -> "replay_log \""     ++ File ++ "\";";
                         "regression" -> "regression_log \"" ++ File ++ "\";"
                     end,
            {ok, Toks, _} = cmdline_lexer:lex(Input),
            {_Cmd, _NS} = handle_cmd(Cmd#command{cmd        = Input,
                                                 cmd_tokens = Toks}, NewS)
    after
        5000 ->
            Msg = io_lib:format("Unable to connect to riak...", []),
            {Cmd#command{response  = Msg,
                         cmd_error = true}, State}
    end.

%% this function is spawned to get_input
get_input(ReplyPID, Prompt) ->
    {ok, Tokens, _NoOfLines} = scan_riak_shell_cmd(Prompt),
    send_to_shell(ReplyPID, {command, Tokens}).

send_to_shell(Pid, Msg) ->
    Pid ! Msg.

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
        {command, Tokens} ->
            Input = cmdline_lexer:toks_to_command(Tokens),
            {Cmd2, NewState2} = handle_cmd(Cmd#command{cmd        = Input,
                                                       cmd_tokens = Tokens}, NewState),
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
            Response = io_lib:format("Unhandled message received is ~p~n",
                                     [Other]),
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
make_cmd_TEST() ->
    {[], #command{}}.
make_cmd_TEST(Input) ->
    {ok, Toks, _} = cmdline_lexer:lex(Input),
    Cmd = #command{cmd        = Input,
                   cmd_tokens = Toks},
    {Toks, Cmd}.

handle_cmd(#command{cmd_tokens = Toks} = Cmd, #state{} = State) ->
    CommandName = make_riak_shell_cmd(Toks),
    run_cmd(CommandName, Cmd, State).

%% Convert hyphenated or underscored commands into single command names
%% Anything else is not valid
make_riak_shell_cmd(Toks) ->
    Toks2 = strip(Toks),
    [{_, _, Fn} | Args] = Toks2,
    {normalise(Fn), Args}.

strip([{whitespace, _, _} | T]) -> strip(T);
strip(Toks)                     -> Toks.

normalise(String) when is_list(String) -> string:to_lower(String);
normalise(Int)    when is_integer(Int) -> integer_to_list(Int);
normalise(Float)  when is_float(Float) -> mochinum:digits(Float).

%% TODO add a riak-admin lexer/parser etc, etc
run_cmd({invalid, _Toks}, Cmd, State) ->
    {Cmd#command{response  = "Invalid Command: " ++ Cmd#command.cmd,
                 cmd_error = true}, State};
run_cmd({Fn, _Toks}, Cmd, State) ->
    case lists:member(Fn, [atom_to_list(X) || X <- ?IMPLEMENTED_SQL_STATEMENTS]) of
        true  -> run_sql_command(Cmd, State);
        false -> run_riak_shell_cmd(Cmd, State)
    end.

run_sql_command(Cmd, #state{has_connection = false} = State) ->
    Msg = io_lib:format("Not connected to riak", []),
    {Cmd#command{response  = Msg,
                 cmd_error = true}, State};
run_sql_command(#command{cmd = Input} = Cmd, State) ->
    try
        SQLToks = riak_ql_lexer:get_tokens(Input),
        case riak_ql_parser:parse(SQLToks) of
            {error, Err} ->
                Msg1 = io_lib:format("SQL Parser error ~p", [Err]),
                {Cmd#command{response = Msg1}, State};
            {ok, _SQL} ->
                %% the server is going to reparse
                Result = connection_srv:run_sql_query(Input, State#state.format),
                Cmd2 = Cmd#command{cmd = Input, response = Result},
                {Cmd3, NewState} = log(Cmd2, State),
                NewState2 = add_cmd_to_history(Cmd3, NewState),
                {Cmd3, NewState2}
        end
    catch _:Error ->
            Msg2 = io_lib:format("SQL Lexer error ~p", [Error]),
            {Cmd#command{response = Msg2},  State}
    end.

run_riak_shell_cmd(#command{cmd_tokens = Toks} = Cmd, State) ->
    case cmdline_parser:parse(Toks) of
        {ok, {{Fn, Arity}, Args}} when is_atom(Fn) ->
            {Cmd2, NewState} = run_ext({{Fn, Arity}, Args}, Cmd, State),
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
        {ok, {{Fn, _Arity}, _Args}} ->
            Msg2 = io_lib:format("~p is not a command", [Fn]),
            {Cmd#command{response  = Msg2,
                         cmd_error = true}, State};
        Error ->
            Msg2 = io_lib:format("Error: ~p", [Error]),
            {Cmd#command{response  = Msg2,
                         cmd_error = true}, State}
    end.

add_cmd_to_history(#command{cmd = Input}, #state{history = Hs} = State) ->
    N = case Hs of
            []             -> 1;
            [{NH, _} | _T] -> NH + 1
        end,
    State#state{history = [{N, Input} | Hs]}.

%% help is a special function
run_ext({{help, 0}, []}, Cmd, #state{extensions = E} = State) ->
    Msg = help:help(shell, quit, E),
    {Cmd#command{response = Msg},  State};
run_ext({{help, 1}, [sql]}, Cmd, State) ->
    Msg = help:help(sql),
    {Cmd#command{response = Msg}, State};
run_ext({{help, 1}, [Mod]}, Cmd, #state{extensions = E} = State) ->
    ModAtom = list_to_atom(atom_to_list(Mod) ++ "_EXT"),
    Msg =
        case lists:filter(fun({_F, M}) when M =:= ModAtom -> true;
                             ({_F, _Mod}) -> false
                          end, E) of
            []   -> io_lib:format("No such extension found: ~ts. See 'help;'",
                                  [Mod]);
            List ->
                help:help(Mod, element(1, hd(List)), List)
        end,
    {Cmd#command{response = Msg},  State};
%% the help funs are not passed the state and can't change it
run_ext({{help, 2}, [sql, Fn]}, Cmd, State) ->
    Msg = help:help(Fn),
    {Cmd#command{response = Msg}, State};
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
                    Msg1 = io_lib:format("Error: invalid function call : ~p:~p ~p",
                                         [Mod, Fn, Args]),
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

make_prompt(#command{} = _C, S = #state{count = SQLN}, ShouldIncrement) ->
     Prefix = make_prefix(S),
     NewCount = case ShouldIncrement of
                    true  -> SQLN + 1;
                   false -> SQLN
                end,
    Prompt =  Prefix ++ "riak-shell(" ++ integer_to_list(NewCount) ++ ")>",
     {Prompt, S#state{count = NewCount}}.

make_prefix(#state{show_connection_status = false}) ->
    "";
make_prefix(#state{show_connection_status = true,
                   has_connection         = false}) ->
    ?REDCROSS ++ " ";
make_prefix(#state{show_connection_status = true,
                   has_connection         = true}) ->
    ?GREENTICK ++ " ".

%% By default have the test interface use CSV instead of human-readable
init_TEST(Config) -> init(Config, undefined, true, "csv").

init(Config, DefaultLogFile, Debug, Format) ->
    ok = clique_writer:init(),
    %% do some housekeeping
    process_flag(trap_exit, true),
    State = #state{config = Config,
                   debug  = Debug,
                   format = Format},
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

%%
%% Functions pertaining to the prompt getting tokens in to lex/parse
%%

-spec scan_riak_shell_cmd(Prompt) -> Result when
      Prompt :: prompt(),
      Result :: erl_scan:tokens_result() | server_no_data().
scan_riak_shell_cmd(Prompt) ->
    Io = group_leader(),
    request(Io, {get_until, unicode, Prompt, cmdline_lexer, tokens,[]}).

%% Miscellaneous functions.

request(Pid, Request) when is_pid(Pid) ->
    Req2 = bc_req(Pid, Request, true),
    execute_request(Pid, Req2).

execute_request(Pid, {Convert, Converted}) ->
    Mref = erlang:monitor(process, Pid),
    Pid ! {io_request,self(), Pid, Converted},
    if
        Convert ->
            convert_binaries(wait_io_mon_reply(Pid, Mref));
        true ->
            wait_io_mon_reply(Pid, Mref)
    end.

convert_binaries(Bin) when is_binary(Bin) ->
    unicode:characters_to_binary(Bin, latin1, unicode);
convert_binaries(Else) ->
    Else.

wait_io_mon_reply(From, Mref) ->
    receive
        {io_reply, From, Reply} ->
            erlang:demonitor(Mref, [flush]),
            Reply;
        {'EXIT', From, _What} ->
            receive
                {'DOWN', Mref, _, _, _} -> true
            after 0 -> true
            end,
            {error,terminated};
        {'DOWN', Mref, _, _, _} ->
            receive
                {'EXIT', From, _What} -> true
            after 0 -> true
            end,
            {error,terminated}
    end.

%% there is a reason this test is in here
%% but I am buggered if I know what it is ;-)
bc_req(Pid, {Op, Enc, P, M, F, A}, MaybeConvert) ->
    case net_kernel:dflag_unicode_io(Pid) of
        true ->
            {false, {Op, Enc, P, M, F, A}};
        false ->
            {MaybeConvert, {Op, P, M, F, A}}
    end.
