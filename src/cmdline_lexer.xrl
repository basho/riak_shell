%%% -*- mode: erlang -*-
%% -------------------------------------------------------------------
%%
%% A lexer for the riakshell mode of riakshell
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

ATOM = ([a-zA-Z]+)

STRING = (\"[^"\n]*\")

%% " comment to make ti colourise proper in yer emacs there

INT      = (\-*[0-9]+)
FLOATDEC = (\-*([0-9]+)?\.[0-9]+)
FLOATSCI = (\-*([0-9]+)?(\.)?[0-9]+(E|e)(\+|\-)?[0-9]+)

WHITESPACE = ([\000-\s]*)

Rules.

{ATOM}     : {token, {atom,   TokenChars}}.
{STRING}   : {token, {string, TokenChars}}.
{INT}      : {token, {number, list_to_integer(TokenChars)}}.
{FLOATDEC} : {token, {number, make_float(TokenChars)}}.
{FLOATSCI} : {token, {number, make_float(TokenChars)}}.

\- : {token, {hyphen,     TokenChars}}.
\_ : {token, {underscore, TokenChars}}.

\; : {end_token, {semicolon, TokenChars}}.

{WHITESPACE} : {token, {whitespace, TokenChars}}.

%% sook up everything else
. : {token, {token, TokenChars}}.

Erlang code.

%% these are not yer fathers floats
make_float("0" ++ _Rest = X) -> list_to_float(X);
make_float(X)                -> list_to_float("0" ++ X). 
