-module(shell_EXT).

%% implements the shell main functions as extensions

-include("riakshell.hrl").

%% explort a help function
-export([
         help/2
        ]).

%% main shell functions
-export([
         quit/1,
         q/1,
         show_config/1,
         show_state/1
         ]).

help(q, 0) -> 
    help(quit, 0);
help(show_state, 0) ->
    io:format("type 'show_state()' to print the state in the shell~n");
help(show_config, 0) ->
    io:format("Type 'show_config()' to print the config in the shell~n");
help(quit, 0) ->
    io:format("Type 'quit();' or the shortcut 'q();' to quit the shell~n").

q(State) -> quit(State).

quit(State) ->
    io:format("Toodle Ooh!~n"),
    c:q(),
    timer:sleep(5000), % to avoid printing prompt
    State.

show_config(#state{config = C} = S) ->
    io:format("The config is ~p~n", [C]),
    S.

show_state(S) ->
    Fields = record_info(fields, state),
    [_H | Vals] = tuple_to_list(S),
    Zip = lists:zip(Fields, Vals),
    io:format("The state consists of~n"),
    riakshell_util:printkvs(Zip),
    S.
