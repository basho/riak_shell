%% -------------------------------------------------------------------
%%
%% debug extension for riak_shell
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
-module(debug_EXT).

%% NOTE this is the only extention that is not loaded when you run the load commmand
%% PLEASE DO NOT add additional functions to this module

-export([
         help/1,
         load/2,
         observer/2
        ]).

-include("riak_shell.hrl").

help(observer) ->
    "Typing 'observer;' starts the Erlang observer application.";
help(load) ->
    "This is for developing extensions only.~n~n"
    "Typing 'load;' reloads all the EXT modules after they have been~n"
    "compiled. This only works after a module has been compiled and~n"
    "loaded the first time.~n~n"
    "The first time you create a module you will need to stop and~n"
    "restart the shell.~n~n"
    "If you invoke this command via the history command it will crash~n"
    "the shell.".

observer(Cmd, #state{} = State) ->
    observer:start(),
    {Cmd#command{response    = "Observer started",
                log_this_cmd = false}, State}.

load(Cmd, #state{} = State) ->
    NewState = riak_shell:register_extensions(State),
    {Cmd#command{response     = "Modules reloaded.",
                 log_this_cmd = false}, NewState}.

  
