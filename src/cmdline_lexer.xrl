%%% -*- mode: erlang -*-
%%% @doc       Riak REPL command line lexer
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Definitions.

SQL       = (S|s)(Q|q)(L|l)
RIAKADMIN = (R|r)(I|i)(A|a)(K|k)(\-)(A|a)(D|d)(M|m)(I|i)(N|n)
QUIT      = (Q|q)(U|u)(I|i)(T|t)

Rules.

{SQL}       : {token, {sql,        TokenChars}}.
{RIAKADMIN} : {token, {riak_admin, TokenChars}}.
{QUIT}      : {token, {quit,       TokenChars}}.
Q           : {token, {q,          TokenChars}}.
\;          : {token, {semicolon,  TokenChars}}.

%% sook up everything else
. : {token, {stuff, TokenChars}}.

\n : {end_token, {'$end'}}.

Erlang code.
