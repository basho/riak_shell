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
         show_version/2
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
