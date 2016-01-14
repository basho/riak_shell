%%% -*- mode: erlang -*-
%%% @doc       Riak REPL command line lexer
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Definitions.

ATOM = ([a-zA-Z]+)

STRING = (\"[^"\n]*\")

INT      = (\-*[0-9]+)
FLOATDEC = (\-*([0-9]+)?\.[0-9]+)
FLOATSCI = (\-*([0-9]+)?(\.)?[0-9]+(E|e)(\+|\-)?[0-9]+)

WHITESPACE = ([\000-\s]*)

Rules.

{ATOM}     : {token, {atom,   TokenChars}}.
{STRING}   : {token, {string, TokenChars}}.
{INT}      : {token, {number, list_to_integer(TokenChars)}}.
{FLOATDEC} : {token, {number, list_to_float(TokenChars)}}.
{FLOATSCI} : {token, {number, list_to_float(TokenChars)}}.

\( : {token, {bra,       TokenChars}}.
\) : {token, {ket,       TokenChars}}.
\, : {token, {comma,     TokenChars}}.

\; : {end_token, {semicolon, TokenChars}}.

{WHITESPACE} : {token, {whitespace, TokenChars}}.

%% sook up everything else
. : {token, {invalid_token, TokenChars}}.

Erlang code.
