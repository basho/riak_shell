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
         show_config/1
         ]).

help(q, 0) -> 
    help(quit, 0);
help(show_config, 0) ->
    "Type 'show_config()' to print the config in the shell.";
help(quit, 0) ->
    "Type 'quit();' or the shortcut 'q();' to quit the shell.".

q(State) -> quit(State).

quit(State) ->
    io:format("Toodle Ooh!~n"),
    c:q(),
    timer:sleep(5000), % to avoid printing prompt
    State.

show_config(#state{config = C} = S) ->
    Msg = io_lib:format("The config is ~p~n", [C]),
    {Msg, S}.
