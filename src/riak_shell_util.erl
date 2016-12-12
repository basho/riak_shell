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
         datetime/0,
         pretty_pr_cmd/1,
         print_help/1,
         print_key_vals/1,
         to_list/1
        ]).

-define(SPACE, 32).

print_key_vals([]) ->
    [];
print_key_vals(KVs) when is_list(KVs) ->
    Size = lists:max([length(to_list(K)) || {K, _V} <- KVs]),
    Format = if
                 Size < 80 -> "- ~" ++ integer_to_list(Size) ++ "s: ~s~n";
                 el/=se    -> "- ~s: ~sn"
             end,
    lists:flatten([io_lib:format(Format, [trim(K), trim(V)]) || {K, V} <- KVs]).

trim(X) ->
    L = to_list(X),
    L2  = string:strip(L,  both, $\n),
    _L3 = string:strip(L2, both, ?SPACE).


print_help([]) ->
    ok;
print_help(Funs) when is_list(Funs) ->
    LoL = split_lists_by_length(
            lists:map(fun erlang:atom_to_list/1, Funs), 2, 70),
    lists:flatten([io_lib:format("    ~s~n", [string:join(FnList, ", ")])
                   || FnList <- LoL]).

%% Divide a list of strings into a list of lists of strings, with the
%% combined length of the strings for each list no greater than the
%% maximum length specified.
%%
%% Each string gets an adjustment to allow for, e.g., ", " added
%% between each element later
split_lists_by_length(Strings, PerString, MaxLen) ->
    {_, W, A} =
        lists:foldl(fun(S, {Len, Working, Acc}) ->
                            Slen = length(S),
                            case Slen + PerString + Len >= MaxLen of
                                true ->
                                    {Slen, [S], Acc ++ [Working]};
                                false ->
                                    {Slen + Len, Working ++ [S], Acc}
                            end
                    end, {0, [], []}, Strings),
    case length(W) > 0 of
        true ->
            A ++ [W];
        false ->
            A
    end.

to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F)   -> mochinum:digits(F);
to_list(L) when is_list(L)    -> L.

pretty_pr_cmd(Cmd) ->
    %% complex list-to-binary unicode dance to make
    %% regexs work with unicode input
    CmdBin = unicode:characters_to_binary(Cmd),
    {ok, Regex1} = re:compile("(\n|[ ]+)", [unicode]),
    CmdBin2 = re:replace(CmdBin,  Regex1,  " ",
                         [global, {return, binary}]),
    {ok, Regex2} = re:compile("^ ", [unicode]),
    _CmdBin3 = re:replace(CmdBin2, Regex2, "", [{return, binary}]).

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pretty_pr_with_unicode_test() ->
    %% 8217 is smart quotes
    Input = [8217, 65, 8217],
    Expected = unicode:characters_to_binary(Input),
    ?assertEqual(Expected, pretty_pr_cmd(Input)).

-endif.
