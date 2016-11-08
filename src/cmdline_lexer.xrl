%%% -*- mode: erlang -*-
%% -------------------------------------------------------------------
%%
%% A lexer for the command line of riak_shell
%%
%% Copyright (c) 2007-2016 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

Definitions.

NODENAME = ([a-zA-Z0-9_]+@[A-Za-z0-9_\.]+)
QUOTEDNODE = ('[a-zA-Z0-9_\.-]+@[A-Za-z0-9_\.-]+')
ATOM = ([a-zA-Z]+)
QUOTEDATOM = ('[^"\n]*')

STRING = (\"[^"\n]*\")

%% " comment to make it colourise proper in yer emacs there

INT      = (\-*[0-9]+)
FLOATDEC = (\-*([0-9]+)?\.[0-9]+)
FLOATSCI = (\-*([0-9]+)?(\.)?[0-9]+(E|e)(\+|\-)?[0-9]+)

WHITESPACE = ([\000-\s]*)

Rules.

{NODENAME}   : {token, {node,   TokenLine, TokenChars}}.
{QUOTEDNODE} : {token, {node,   TokenLine, string:strip(TokenChars, both, $')}}.
{ATOM}       : {token, {atom,   TokenLine, TokenChars}}.
{QUOTEDATOM} : {token, {atom,   TokenLine, TokenChars}}.
{STRING}     : {token, {string, TokenLine, TokenChars}}.
{INT}        : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOATDEC}   : {token, {number, TokenLine, riak_ql_lexer:fpdec_to_float(TokenChars)}}.
{FLOATSCI}   : {token, {number, TokenLine, riak_ql_lexer:fpsci_to_float(TokenChars)}}.

\- : {token, {hyphen,     TokenLine, TokenChars}}.
\_ : {token, {underscore, TokenLine, TokenChars}}.

\; : {end_token, {'$end'}}.

{WHITESPACE} : {token, {whitespace, TokenLine, TokenChars}}.

%% sook up everything else
. : {token, {token, TokenLine, TokenChars}}.

Erlang code.

-export([
         toks_to_command/1
        ]).
         
-define(SPACE, 32).

toks_to_command(Toks) ->
    Input   = [riak_shell_util:to_list(TkCh) || {_, _, TkCh} <- Toks],
    _Input2 = riak_shell_util:pretty_pr_cmd(lists:flatten(Input)) ++ ";".

