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
         show_history/1,
         clear_history/1,
         history/2,
         h/2,
         select/1,
         'riak-admin'/1
        ]).

-define(SPACE, 32).

help(h) ->
    help(history);
help(history) ->
    "You can rerun a command by finding the command in the history list~n"
    "with 'show_history;' and using the number next to it as the argument~n"
    "to 'history' or 'h': 'history 3;' or 'h 3;' for example.";
help(clear_history) ->
    "Type 'clear_history;' to delete all the history from the shell.";
help(show_history) ->
    "Type 'show_history;' to list all the history in the shell.".

clear_history(S) ->
    Msg = io_lib:format("History has been cleared."),
    {Msg, S#state{history = [], count = 2}}.

show_history(#state{history = Hist} = S) ->
    Msg1 = io_lib:format("The history contains:~n", []),
    FormatFn = fun({N, Cmd}) ->
                       Cmd2 = riak_shell_util:pretty_pr_cmd(Cmd),
                       {N, io_lib:format("~s", [Cmd2])}
               end,
    Hist2 = [FormatFn(X) || X <- Hist],
    Msg2 = riak_shell_util:printkvs(lists:reverse(Hist2)),
    {Msg1 ++ Msg2, S}.

h(S, N) -> history(S, N).

history(#state{history = H} = S, N) when is_integer(N) andalso
                                         N > 0 ->
    case lists:keyfind(N, 1, H) of
        false -> 
            Msg1 = io_lib:format("Error: there is no history for ~p", [N]),
            {Msg1, S};
        {N, Cmd} ->
            Msg2 = io_lib:format("rerun (~p)> ~s~n", [N, Cmd]),
            {Msg3, NewS} = riak_shell:handle_cmd(Cmd, S),
            {Msg2 ++ Msg3, NewS}
    end;
history(S, Value) ->
    ErrMsg = io_lib:format("The value '~p' must be a positive integer.", 
                        [Value]),
    {ErrMsg, S#state{cmd_error = true}}.

'riak-admin'(State) ->
    {"not good", State}.

select(State) ->
    {"not good", State}.
