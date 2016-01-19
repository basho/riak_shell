%% -*- erlang -*-
%%% @doc       Parser for the RIAK Repl command line
%%% @author    gguthrie@basho.com
%%% @copyright (C) 2015 Basho

Nonterminals

Fun
Args
Arg
Atom        

.

Terminals

atom
string
number
bra
ket
comma
semicolon
token

.

Rootsymbol Fun.
Endsymbol '$end'.

Fun -> Atom bra      ket semicolon : make_fn('$1', []).
Fun -> Atom bra Args ket semicolon : make_fn('$1', '$3').

Args -> Arg            : ['$1'].
Args -> Args comma Arg : '$1' ++ ['$3'].

Atom -> Atom atom   : append_atom('$1','$2').
Atom -> Atom token  : append_atom('$1','$2').
Atom -> Atom number : append_atom('$1', '$2').
Atom -> atom        : '$1'.

Arg -> Atom   : make_atom('$1').
Arg -> number : strip('$1').
Arg -> string : strip('$1').

           
Erlang code.

-export([
         compile/1
        ]).

compile(Toks) ->
    Toks2 = [{Type, Token} || {Type, Token} <- Toks,
                             Type =/= whitespace],
    parse(Toks2).

append_atom({_, X}, {number, B}) -> {atom, X ++ riakshell_util:to_list(B)};
append_atom({_, X}, {_,      B}) -> {atom, X ++ B}.

make_atom({atom, A}) -> list_to_atom(A).

strip({number, V}) -> V;
strip({string, V}) -> string:strip(V, both, $").

make_fn({atom, Fn}, Args) -> {{list_to_atom(Fn), length(Args)}, Args}.
