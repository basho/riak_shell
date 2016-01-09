%%% -*- mode: erlang -*-
%%% @doc       Riak REPL command line lexer
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Definitions.

HELP      = (H|h)(E|e)(L|l)(P|p)
QUIT      = (Q|q)(U|u)(I|i)(T|t)
RIAKADMIN = (R|r)(I|i)(A|a)(K|k)(\-)(A|a)(D|d)(M|m)(I|i)(N|n)
SQL       = (S|s)(Q|q)(L|l)

Rules.

{HELP}      : {token, {help,       TokenChars}}.
{QUIT}      : {token, {quit,       TokenChars}}.
{RIAKADMIN} : {token, {riak_admin, TokenChars}}.
{SQL}       : {token, {sql,        TokenChars}}.
\;          : {token, {semicolon,  TokenChars}}.

%% sook up everything else
. : {token, {stuff, TokenChars}}.

\n : {end_token, {'$end'}}.

Erlang code.
