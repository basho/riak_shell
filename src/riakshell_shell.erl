-module(riakshell_shell).

-export([
         start/0
        ]).

-record(state, {
          version          = "1.0",
          mode             = sql :: sql | riak_admin,
          sql_count        = 1,
          riak_admin_count = 1
         }).

start() -> spawn(fun main/0).

main() ->
    State = startup(),
    io:format("riakshell ~p, write quit;' to exit or 'help;' for help~n", [State#state.version]),
    loop(State).

loop(State) ->
    {Prompt, NewState} = make_prompt(State),
    Input = get_input(Prompt),
    case Input of
        "help"       -> help(),
                        loop(State);
        "quit"       -> quit();
        "riak-admin" -> loop(NewState#state{mode = riak_admin});
        "sql"        -> loop(NewState#state{mode = sql});
        Other        -> io:format("Other is~p~n", [Other]),
                        loop(State)
    end.

help() ->
    Help = "Some help here",
    io:format("~p~n", [Help]).

quit() ->
    io:format("Toodle Ooh!~n"),
    c:q(),
    timer:sleep(5000), % to avoid printing prompt
    ok.

get_input(Prompt) ->
    Cmd = io:get_line(Prompt),
    Cmd2 = string:strip(Cmd, right, $\n),
    {ok, Toks, _} = cmdline_lexer:string(Cmd2),
    _O = case cmdline_parser:parse(Toks) of
             {ok, Output} -> Output;
             {error, Err} -> io:format("Err is ~p~n", [Err]),
                             lists:flatten(io_lib:format("Invalid command: ~p", [Cmd2]))
         end.

make_prompt(S = #state{mode      = sql,
                       sql_count = SQLN}) ->
    Prompt = "sql (" ++ integer_to_list(SQLN) ++ ")>",
    {Prompt, S#state{sql_count = SQLN + 1}};
make_prompt(S = #state{mode             = riak_admin,
                       riak_admin_count = RAN}) ->
    Prompt = "riak-admin (" ++ integer_to_list(RAN) ++ ")>",
    {Prompt, S#state{riak_admin_count = RAN + 1}}.

startup() ->
    try 
        s2()
    catch
        _ ->
            io:format("Invalid_config~n", []),
            exit(invalid_config)
    end.

s2() ->
    {ok, Config} = file:consult("../etc/riakshell.config"),
    Cookie = read_config(Config, cookie),
    true = erlang:set_cookie(node(), Cookie),
    Nodes = read_config(Config, nodes),
    Nodes2 = nodes(),
    io:format("Nodes is ~p~nCookie is ~p~nConfig is ~p~nNodes2 is ~p~n", 
              [Nodes, Cookie, Config, Nodes2]),
    #state{}.

read_config(Config, Key) when is_list(Config) andalso
                              is_atom(Key) ->
    {Key, V} = lists:keyfind(Key, 1, Config),
    V.
