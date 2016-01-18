-module(riakshell_util).

-export([
         printkvs/1
        ]).

printkvs([]) ->
    ok;
printkvs(KVs) when is_list(KVs) ->
    Size = lists:max([length(to_list(K)) || {K, _V} <- KVs]),
    Format = if
                 Size < 80 -> "- ~" ++ integer_to_list(Size) ++ "s: ~p~n";
                 el/=se    -> "- ~p: ~p~n"
             end,
    [io:format(Format, [to_list(K), V]) || {K, V} <- KVs],
    ok.

to_list(A) when is_atom(A)    -> atom_to_list(A);
to_list(B) when is_binary(B)  -> binary_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(F) when is_float(F)   -> float_to_list(F);
to_list(L) when is_list(L)    -> L. 
