%% -------------------------------------------------------------------
%%
%% utility functions for riak_shell
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
-module(riak_shell_util).

-export([
         printkvs/1,
         to_list/1,
         pretty_pr_cmd/1,
         datetime/0
        ]).

-define(SPACE, 32).

printkvs([]) ->
    ok;
printkvs(KVs) when is_list(KVs) ->
    Size = lists:max([length(to_list(K)) || {K, _V} <- KVs]),
    Format = if
                 Size < 80 -> "- ~" ++ integer_to_list(Size) ++ "s: ~s~n";
                 el/=se    -> "- ~s: ~sn"
             end,
    lists:flatten([io_lib:format(Format, [trim(K), trim(V)]) || {K, V} <- KVs]).

trim(X) ->
    L = to_list(X),
    L2 = string:strip(L, both, $\n),
    _L3 = string:strip(L2, both, ?SPACE).

to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F)   -> float_to_list(F);
to_list(L) when is_list(L)    -> L.

pretty_pr_cmd(Cmd) ->
    Cmd2  = re:replace(Cmd,  "\n",   " ", [global, {return, list}]),
    _Cmd3 = re:replace(Cmd2, "[ ]+", " ", [global, {return, list}]).

datetime() ->
    {{Y, M, D}, {H, Mn, S}} = calendar:universal_time(),
    lists:flatten(integer_to_list(Y) ++
                      ["_"] ++
                      pad(M) ++
                      ["_"] ++
                      pad(D) ++
                      ["-"] ++
                      pad(H) ++
                      [":"] ++ 
                      pad(Mn) ++
                      [":"] ++
                      pad(S)).

pad(X) when is_integer(X) ->
    io_lib:format("~2.10.0B", [X]).