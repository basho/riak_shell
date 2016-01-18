-module(history_EXT).

-include("riakshell.hrl").

-export([
         help/2
        ]).

-export([
         show_history/1,
         clear_history/1
        ]).

help(clear_history, 0) ->
    io:format("type 'clear_history()' to delete all the history from the shell~n");
help(show_history, 0) ->
    io:format("type 'show_history()' to list all the history in the shell~n").

clear_history(S) ->
    S#state{history = [], count = 2}.

show_history(#state{history = H} = S) ->
    io:format(user, "The history contains:~n\r", []),
    riakshell_util:printkvs(lists:reverse(H)),
    S.
