-module(riakshell_shell).

-export([
         start/0
        ]).

-record(state, {
          version = "1.0",
          prompt = "riak_shell>" :: list()
         }).

start() -> spawn(fun main/0).

main() ->
    State = #state{},
    io:format("riakshell ~p, write quit; or q; to exit~n", [State#state.version]),
    loop(State).

loop(#state{prompt = P} = State) ->
    Input = get_input(P),
    case Input of
        "quit"       -> quit();
        "sql"        -> io:format("Got to SQL~n"),
                        loop(State);
        "riak_admin" -> io:format("Go to riak-admin~n"),
                        loop(State);
        Other        -> io:format("Other is ~p~n", [Other]),
                        loop(State)
    end.

quit() ->
    io:format("Toodle Ooh!~n"),
    c:q(),
    timer:sleep(5000), % to avoid printing prompt
    ok.
            
get_input(P) ->
    Cmd = io:get_line(P),
    {ok, Toks, _} = cmdline_lexer:string(Cmd),
    io:format("Toks is ~p~n", [Toks]),
    {ok, Output} = cmdline_parser:parse(Toks),
    io:format("Output is ~p~n", [Output]),
    Output.
