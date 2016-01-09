%% -*- erlang -*-
%%% @doc       Parser for the RIAK Repl command line
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Nonterminals

Statement                  

.

Terminals

sql
riak_admin
quit
q
semicolon
% stuff 

.

Rootsymbol Statement.
Endsymbol '$end'.

Statement -> sql        semicolon : "sql".
Statement -> riak_admin semicolon : "riak-admin".
Statement -> quit       semicolon : "quit".
Statement -> q          semicolon : "quit".

Erlang code.
