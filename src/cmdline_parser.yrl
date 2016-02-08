%% -*- erlang -*-
%% -------------------------------------------------------------------
%%
%% The parser for the command line of riakshell
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
%% "AS IS" BASIS, WITHOUT WARARANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

Nonterminals

Fun
Atom
Arg
Args

.

Terminals

atom
string
number
hyphen
underscore
semicolon
whitespace
token

.

Rootsymbol Fun.
Endsymbol '$end'.

Fun -> Args            semicolon            : make_fn('$1').
Fun -> Args            semicolon whitespace : make_fn('$1').
Fun -> Args whitespace semicolon            : make_fn('$1').
Fun -> Args whitespace semicolon whitespace : make_fn('$1').

Args -> Arg                 : ['$1'].
Args -> Args whitespace Arg : '$1' ++ ['$3'].

Atom -> Atom hyphen     : append_atom('$1', '$2').
Atom -> Atom underscore : append_atom('$1', '$2').
Atom -> Atom token      : append_atom('$1', '$2').
Atom -> Atom number     : append_atom('$1', '$2').
Atom -> Atom atom       : append_atom('$1', '$2').
Atom -> atom            : '$1'.

Arg -> Atom : make_atom('$1').
Arg -> number : strip('$1').
Arg -> string : strip('$1').
  
Erlang code.

append_atom({_, X}, {number, B}) -> {atom, X ++ riakshell_util:to_list(B)};
append_atom({_, X}, {_,      B}) -> {atom, X ++ B}.

make_atom({atom, A}) -> list_to_atom(A).

strip({number, V}) -> V;
strip({string, V}) -> string:strip(V, both, $").

make_fn([H | T]) when is_atom(H) -> {{H, length(T)}, T}.
