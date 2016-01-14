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
         show_config/1
         ]).

help(show_config, 0) ->
    io:format("Type 'show_config()' to print the config in the shell~n");
help(quit, 0) ->
    io:format("Type 'q();' to quit the shell~n").

quit(State) ->
    io:format("Toodle Ooh!~n"),
    c:q(),
    timer:sleep(5000), % to avoid printing prompt
    State.

show_config(#state{config = C} = S) ->
    io:format("The config is ~p~n", [C]),
    S.
