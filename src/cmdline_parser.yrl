%% -*- erlang -*-
%% -------------------------------------------------------------------
%%
%% The parser for the command line of riak_shell
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
Arg
Args

.

Terminals

node
atom
string
number
whitespace

.

Rootsymbol Fun.
Endsymbol '$end'.

Fun -> Args            : make_fn('$1').
Fun -> Args whitespace : make_fn('$1').

Args -> Arg                 : ['$1'].
Args -> whitespace Arg      : ['$2'].
Args -> Args whitespace Arg : '$1' ++ ['$3'].

Arg -> atom   : make_atom('$1').
Arg -> number : strip('$1').
Arg -> string : strip('$1').
Arg -> node   : strip('$1').

Erlang code.

-ignore_xref([format_error/1, parse_and_scan/1]).

make_atom({atom, _, A}) -> list_to_atom(string:to_lower(string:strip(A, both, $'))).

strip({number, _, V}) -> V;
strip({string, _, V}) -> string:strip(V, both, $");
strip({node,   _, V}) -> V.

make_fn([H | T]) -> {{H, length(T)}, T}.
