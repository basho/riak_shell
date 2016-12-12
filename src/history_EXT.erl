%% -------------------------------------------------------------------
%%
%% history extension for riak_shell
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
-module(history_EXT).

-include("riak_shell.hrl").

-export([
         help/1
        ]).

-export([
         clear_history/2,
         h/3,
         history/3,
         show_history/2
        ]).

-ignore_xref([
              clear_history/2,
              h/3,
              help/1,
              history/3,
              show_history/2
             ]).

help(h) ->
    help(history);
help(history) ->
    "You can rerun a command by finding the command in the history list~n"
    "with `show_history;` and using the number next to it as the argument~n"
    "to `history` or `h`: `history 3;` or `h 3;` for example.";
help(clear_history) ->
    "Type `clear_history;` to delete all the history from the shell.";
help(show_history) ->
    "Type `show_history;` to list all the history in the shell.".

%% Reset prompt and clear history
clear_history(Cmd, S) ->
    {Cmd#command{response = "History has been cleared."},
        S#state{history = [], count = 1}}.

show_history(Cmd, #state{history = Hist} = S) ->
    Msg1 = "The history contains:\n",
    FormatFn = fun({N, Cmd1}) ->
                       Cmd2 = riak_shell_util:pretty_pr_cmd(Cmd1),
                       {N, io_lib:format("~ts", [Cmd2])}
               end,
    Hist2 = [FormatFn(X) || X <- Hist],
    Msg2 = riak_shell_util:print_key_vals(lists:reverse(Hist2)),
    {Cmd#command{response = Msg1 ++ Msg2}, S}.

h(Cmd, S, N) -> history(Cmd, S, N).

history(Cmd, #state{history = H} = S, N) when is_integer(N) andalso
                                         N > 0 ->
    case lists:keyfind(N, 1, H) of
        false ->
            Msg1 = io_lib:format("Error: there is no history for ~p", [N]),
            {Cmd#command{response = Msg1}, S};
        {N, Input} ->
            Msg2 = io_lib:format("rerun (~p)> ~s~n", [N, Input]),
            {ok, Toks, _} = cmdline_lexer:string(Input),
            {Cmd2, NewS} = riak_shell:handle_cmd(Cmd#command{cmd       = Input,
                                                            cmd_tokens = Toks}, S),
            {Cmd2#command{response = Msg2 ++ Cmd2#command.response}, NewS}
    end;
history(Cmd, S, Value) ->
    ErrMsg = io_lib:format("The value `~p` must be a positive integer.", 
                        [Value]),
    {Cmd#command{response  = ErrMsg,
                 cmd_error = true}, S}.
