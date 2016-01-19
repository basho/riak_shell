-module(history_EXT).

-include("riakshell.hrl").

-export([
         help/2
        ]).

-export([
         show_history/1,
         clear_history/1,
         history/2,
         h/2
        ]).

-define(SPACE, 32).

help(h, 1) ->
    help(history, 1);
help(history, 1) ->
    "You can rerun a command by typing 'history(3)' or 'h(3)' in the shell.";
help(clear_history, 0) ->
    "Type 'clear_history()' to delete all the history from the shell.";
help(show_history, 0) ->
    "Type 'show_history()' to list all the history in the shell.".

clear_history(S) ->
    Msg = io_lib:format("History has been cleared."),
    {Msg, S#state{history = [], count = 2}}.

show_history(#state{history = H} = S) ->
    Msg1 = io_lib:format("The history contains:~n", []),
    Msg2 = riakshell_util:printkvs(lists:reverse(H)),
    {Msg1 ++ Msg2, S}.

h(S, N) ->
    history(S, N).

history(#state{history = H} = S, N) when is_integer(N) ->
    case lists:keyfind(N, 1, H) of
        false -> 
            Msg = io_lib:format("Error: there is no history for ~p", [N]),
            {Msg, S};
       {N, Cmd} ->
            Cmd2 = riakshell_util:pretty_pr_cmd(Cmd),
            io:format("rerun (~p)-> ~p~n\r", [N, string:strip(Cmd2, both, ?SPACE)]),
            riakshell_shell:handle_cmd(Cmd, S)
    end.
