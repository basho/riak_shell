%% -*- erlang -*-
%%% @doc       Parser for the RIAK Repl command line
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Nonterminals

Statement                  

.

Terminals

help
quit
riak_admin
semicolon
sql
% stuff 

.

Rootsymbol Statement.
Endsymbol '$end'.

Statement -> help       semicolon : "help".
Statement -> quit       semicolon : "quit".
Statement -> riak_admin semicolon : "riak-admin".
Statement -> sql        semicolon : "sql".

Erlang code.
