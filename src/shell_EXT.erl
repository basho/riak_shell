%% -------------------------------------------------------------------
%%
%% The main shell extention file for riak_shell
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
-module(shell_EXT).

%% implements the shell main functions as extensions

-include("riak_shell.hrl").

%% export a help function
-export([
         help/1
        ]).

%% main shell functions
-export([
         quit/2,
         q/2,
         show_config/2,
         show_version/2,
         about/2
        ]).

help(q) -> 
    help(quit);
help(show_version) ->
    "Type 'show_version;' to see the versions of riak_shell and SQL.";
help(show_config) ->
    "Type 'show_config;' to print the config in the shell.";
help(quit) ->
    "Type 'quit;' or the shortcut 'q;' to quit the shell.".

q(Cmd, State) -> quit(Cmd, State).

quit(_Cmd, _State) ->
    io:format("Toodle Ooh!~n"),
    halt().

show_config(Cmd, #state{config = Config} = S) ->
    Msg = io_lib:format("The config is ~p~n", [Config]),
    {Cmd#command{response = Msg}, S}.

show_version(Cmd, #state{version = Vsn} = S) ->
    Msg = io_lib:format("~s~n", [Vsn]),
    {Cmd#command{response = Msg}, S}.


about(Cmd, State) ->
    {ok, Cols} = io:columns(),
    {ok, Rows} = io:rows(),
    io:format("Cols is ~p Rows is ~p~n", [Cols, Rows]),
    Venus = [
              "                                                                     ;<> .",
              "                                                                   ,C>',<>",
              "                                                                / ,C',<C'.",
              "                            .,,;<<<<<>>>>;,,.             <   .  <> ,C' ,C",
              "                     .;<CCCCCCCCCCCCC>>>'''''.,          .> ,C' <> ,C',<CC",
              "                   .-CCCCCCCCCC><CCC .;<CCCCCCCCCC>;.. -;C ;C,,C' <> ,CCC>",
              "                .<>;<CCCCCCC>'.,CCCCCCCC>>'''.,;CCCCCC>;CCCCC>'.;C> ,CCC  ",
              "          .;CCC CCCC`<CCC>' ,<CCCCCC>>''.;C>>'''`<CCCCCC><CCC;<C>',<CCC   ",
              "        . <CCCC, <CC>;,.,;<CCCC>>''.;<>,,,. -CCCC;.``''.;<C>>''',<CCC>    ",
              "       <C;,CCCCC>.`C>'''''''  --<>;.``''<<<C;.`<<<CCCCCC>,;;<CCCCCCCC>;,  ",
              "     ,CCCCCCCCC>' . `<C>' .,,zc,`<CCCC, -;.``<<>;<CCCCCCCCCC>>''''''<>'   ",
              "    .<CCCCC''' zc$$$ccccc$$$$$$$h,.`<<C> <CCC;,,.``<`'CC>' .;<CCCCCC-     ",
              "     ,CCCC' ,c$$$$$$$$$$$$$$$$$$$$$$cc,.  `'<<CCCCCCCCC,,<C`<CCC>>'    .,.",
              "    ;CCCC' J$$$$$$$$$$$$$$$$$$$$$$$$$$$$$. `<>;,.```<<<<<CCC CC,.,,,;CC>' ",
              " .;C><C> z$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$. .`'<<<>>;,, <CC CCCCCCCCCC,,.",
              "<C' `>   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c CC>>;;, <C> CC,`CC>>'`'CCCCC",
              "' ;C>;> J$$$$$$$$$$$$$$$$$$$$$$$$$????????$$$.`C''''<><>', CC,.,;;C>'`<CCC",
              "  <CCC  $$$$$$$$$$$$$$$$$$$$$P\"  .,ccccccc$$$$. ,CCC>;,;<C,.``'''  ,<> .,'",
              " ,CCC .<$$$$$$$$$$$$$$$$$$$$\"  zJ$$$$$$$$$$$$$$c <CCCC`<CCCCCCCCC>,`CC'<' ",
              ",CCC ; J$$$$$$$$$$$$$$$$$$$$,c$$??C????\"\"???$$$$hc`'>' ;.``<CC>>><C C> C> ",
              "CC>>;C ?$$$$$???????$$$$$$$$$$$FJP\",c===\"   J$$$$$c ;, <CC> <C><\<> C>;CC;",
              ">  >' . $$P\".,,zzcc, \"$$$$$$$$$;\" ,.    $\" J$$$$$$$ <C> CCC <C < > <C `CCC",
              ";<CCCC .`$,J$$$PCCC>?<C$$$$$$$$L -??    ,c$$$$$$$$$c C',CC> <' C,,;>   `CC",
              "CCCC>',C ?$$CCJ?\"\"'_`,`\"$$$$$$$$$c,=cccd$$$$$$$$$$$$ C <C>  ,; C <>     `C",
              "CCCC;,> .`$$$\",==\"\"-.  c<$$$$$$$$$$JJJJC?$$$$$$$$$$$ < CC < CC C,`>      `",
              "CCCCCC;<C,`$F' .    `,JC:<$$$$$$$$$$$$$$$$$$$$$$$$$$>` <>-C CC,`C,`,.     ",
              "`<C>`'CCCC,`h.,..,,,c$3C>:3$$$$$$$$$$$$$$$$$$$$$$$$$$ < <,<><`C;.`, <> .  ",
              "    <C`<CC> $$$$$$$$$$$CC;<$$$$$$$$$$$$$$$$$$$$$$$$$$r`<;CCCCCCC>.' <> C;,",
              "  ;CCC. CC>-`$$$$$$$$$$$CC:$$$$$$$$$?$$$$$$$$$$$$$$$$$.`<'`<CCCC>',C' <CCC",
              "  <CC',<>',;.?$$$$$$$$$$C>;$$$$$9???-<$$$$$$$$$$$$$$$$$c,`-.``---''.,CCC>>",
              ",C'CCCC ;.`C> $$$$$$$$$$$>`$$$$$F<;,c$$$$$$$$$$$$$$$$$$$$L `C>>;;<CCCCCCCC",
              "C> CC>C,`C,`> `?$$$$$$$$$h;` `\"'.<$$$$$$$$$$$$$$$$$$$$$$$P . CCCCC,.;;.`''",
              "'  `C `C;CC,CC; \"$$$$$$$$$$$$$$$$<$$$$$$$$$$$$$$$$$$$$$$P' C,`CCCCCCCCCCCC",
              "    `C,.``<CCCCC, ?$$$$$$$$$$$????\"\"\'\"\"\"\" J$$$$$$$$$$$$\" ..`C,C>`<<<<>>''<",
              "     `CCCC;.``<CC>.`$$$$$$$???     .,c=:c$$$$$$$$$$$$P',$'< CC'C;  <CCCC>;",
              "       <CCCCC>, `CC, \"$$$$$hcc,\"?C????JJ$$$$$$$$$$$$\",J$$ C CC <C . ``'',.",
              "          `C>`CC,`<CC, \"$$$$$$$$$$>;;??$$$$$$$$$$$P',$$$$ C CC> C,`C;. <CC",
              "         .,CC,`<C,,CCC .`?$$$$$$$$JJ$$$$$$$$$$$$P\",J$$$$F.>,CC><CC CCC> <C",
              "       ;C>'CCC>.`<'<CC $c, ?$$$$$$$$$$$$$$$$$$$\" c$$$$$$ <><C> <CC ``<C;.`",
              "      ,CC> `CCCCCCCCC',$$$h.`?$$$$$$$$$$$$$$$\" z$$$$$$$F;CC>',<CC>,C>.`<C;",
              "      <C> <>.`'''<>'  $$$$$$$c \"$$$$$$$$$??\".z$$$$$$$$$ `\".;<CCC> .`<C; CC",
              "      CC <CCC <C'<Cr J$$$$$$$$$c,``''\"\"'.zc$$$$$$$$$$$F;;CCCCC>' ;C, <CCCC",
              "     <CC C><C CC CCC $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ CCCC>' .c;`<C> C <C",
              "     `CC C <C.<C CCC ?$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$F,C>'.;;.`CC,`CCC ;CC",
              ">,.   CC ` CC> <,CCC>`$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\" ' ;<.`CC  `C <C> CC ",
              "`<CCC CCC,.`CC> C, <C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$P  <CCCC <CC  C><C <C'.",
              "C, `< C>;C> CCC,)C, C $$$$$?$$$$$$$$$$$$$$$$$$$$$$$ , CCCCC,<C>.;C>  ,C> C",
              "> <>, C'<CC CCC>;CC ' \".,;;;;, \"$$$$$$$$$$$$$$$$$$' `.`CCCC><C><CC> ,C>  C",
              "C> < ,C CC>CCCC';CC .;CCCCCCCCC,.\"?$$$$$$$$$$$$$$' ' `, <<C><C, >' ;C> < C",
              "CC. >> ;>  CCCC CC> `\"',.,;,``'<<, ??$$$$$$$$$$$F $c'.'--;;;<C> .;C> ,C>,C",
              "C  `.;- ., CCC,,CC ,CCCCCCCCCCC,.`>- \"\"??$$$$$$$,J$$c <>,<>' .;<CC> ;C',<C",
              "`'''.,;C',<CCCCCC',CCCC,<CC''<<CC,`, <Ccd$$$$$$$$$$$$h  .,;CCCCCC' <> ,CCC",
              "CC>>>''.;CCCCCCC  '.,..,.``<C>,.`<; >.\"?$$$$$$$$$$$$$$h.`<CCCC>'.;C> <CC> ",
              "CC>;CCCCCCCCCC> ,;<<<<<<<>>.``CC; <. '  \"\"?$$$$$$$$$$$$$h.`'  --<CC,<CC> <",
              "CCCCCCCCCCCC>  .,;;;;<CC;,.`>, `C,`C <hcccd$$$$$$$$$$$$$$$c -<>>;;,.`<C `C",
              "CCCCCCC>''.,<>>>''''```,'''-<CC,`C <,`??$$$$$$$$$$$$$$???===  .,..``--<C, ",
              ">>''.,,,cccccccc$$$$hcc,``> )CCC, <>.  ,J$$$$???\"\".,,--  _.,c,``<C;  =..`'",
              "cc$$$$$$$$$$$$$$$$$$$$$$$c > CC>C>;.` `??\"\".,;<<>''.,cd$$$$$$$h. .`'>;.`<>",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$,`,`C,`<CCCCC;<CCC>>' ,c$$$$$$$$$$$$$$c,`..`<-<C",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$$ > `C;. ``''''.,zccd$$$$$$?????\"\"\"\"??$$c `C;, `",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$$,`> `<C>;,.```<<<>>,,`\"\".,;;<CCCCCC;, \"$$c `<<C",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ` -.`<CCCC'--;;<<<>>>''.,,;;;,.``<CC,`$$$c  `",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c, `>;.`<<C>>>;;,,;;<CCC>''<<<C> -.`< `$$$$c ",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$.`<CC;,,.``'<>'''''',,_``-;. `.\"$c  ?$$$$h",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$hc,,`''''.,cc$$$$$$$$$$cc `>  -`?hcd$$$$$",
              "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$c`>    `$$$$$$$"],
        Tag = "riak-shell from Basho - Venus Arising From The Waves by Sandro Boticelli",
    Venus2 = clip(Venus, Rows - 1, Cols),
    Response = [io_lib:format("~s~n", [X]) || X <-Venus2] ++ Tag,
    {Cmd#command{response = Response}, State}.

clip(Matrix, Rows, Cols) ->
    Top = shrink(Matrix, Rows),
    [shrink(X, Cols) || X <- Top].
 
shrink(Matrix, No) ->
    Len = length(Matrix),
   if  
        No >= Len -> Matrix;
        No <  Len -> {Keep, _Discard} = lists:split(No, Matrix),
                     Keep
    end.
