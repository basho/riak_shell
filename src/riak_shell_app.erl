%% -------------------------------------------------------------------
%%
%% riak_shell application riak_shell
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
-module(riak_shell_app).

-behaviour(application).

%% Export for the boot process
-export([
         boot/1
        ]).

%% testing export
-export([
         boot_TEST/1
         ]).

%% Application callbacks
-export([
         start/2, 
         stop/1
        ]).

boot_TEST(Config) ->
    ok = application:start(riak_shell),
    _State = riak_shell:init_TEST(Config).

boot([DebugStatus | Rest]) ->
    %% suppress error reporting
    case DebugStatus of
        "debug_off" -> ok = error_logger:tty(false);
        "debug_on"  -> ok
    end,
    case Rest of
        [DefaultLogFile, FileName, RunFileAs] when RunFileAs =:= "replay"     orelse
                                                   RunFileAs =:= "regression" ->
            ok = application:start(riak_shell),
            Config = application:get_all_env(riak_shell),
            {ReturnStatus, Msg} = riak_shell:start(Config, DefaultLogFile, FileName, RunFileAs),
            io:format(lists:flatten(Msg) ++ "~n", []),
            case ReturnStatus of
                ok    -> halt(1);
                error -> halt()
            end;
        [DefaultLogFile, [], []] ->
            ok = application:start(riak_shell),
            Config = application:get_all_env(riak_shell),
            user_drv:start(['tty_sl -c -e', {riak_shell, start, [Config, DefaultLogFile]}]);
        Other -> 
            io:format("Exit invalid args ~p~n", [Other]),
            exit({invalid_args, Other})
    end.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case connection_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
                end.

stop(_State) ->
    ok.
