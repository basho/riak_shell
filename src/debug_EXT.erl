%% -------------------------------------------------------------------
%%
%% debug extension for riakshell
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

-export([
         help/2,
         load/1
        ]).

-include("riakshell.hrl").

help(load, 0) ->
    "typing 'load();' reloads all the EXT modules after they have been compiled." ++
        "This only works after a module has been compiled and loaded the first time. " ++
        "So the first time you create a module you will need to stop and restart the shell." ++
        "This is for developing extensions only. " ++
        "If you try and invoke this command via the history command it will crash the shell " ++
        "because you cannot reload a module while you are running it.".

load(#state{} = State) -> 
    NewState = riakshell_shell:register_extensions(State),
    {"Modules reloaded.", NewState#state{log_this_cmd = false}}.
