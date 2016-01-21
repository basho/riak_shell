-module(shell_EXT).

%% implements the shell main functions as extensions

-include("riakshell.hrl").

%% export a help function
-export([
         help/2
        ]).

%% main shell functions
-export([
         quit/1,
         q/1,
         show_config/1,
         riakshell/1,
         sql/1,
         riak_admin/1
         ]).

help(riakshell, 0) ->
    "Type 'riakshell();' to switch to riakshell mode.";
help(sql, 0) ->
    "Type 'sql();' to switch to SQL mode.";
help(riak_admin, 0) ->
    "Type 'riak_admin();' to switch to riak-admin mode.";
help(q, 0) -> 
    help(quit, 0);
help(show_config, 0) ->
    "Type 'show_config()' to print the config in the shell.";
help(quit, 0) ->
    "Type 'quit();' or the shortcut 'q();' to quit the shell.".

riakshell(State) -> 
    {"Switched to riakshell mode.", State#state{mode = riakshell}}.

sql(State) -> 
    {"Switched to sql mode.", State#state{mode = sql}}.

riak_admin(State) -> 
    {"Switched to riak-admin mode.", State#state{mode = riak_admin}}.

q(State) -> quit(State).

quit(State) ->
    io:format("Toodle Ooh!~n"),
    c:q(),
    timer:sleep(5000), % to avoid printing prompt
    State.

show_config(#state{config = C} = S) ->
    Msg = io_lib:format("The config is ~p~n", [C]),
    {Msg, S}.
