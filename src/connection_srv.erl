%% -------------------------------------------------------------------
%%
%% supervisor for the protocol buffers connection for riakshell
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
-module(connection_srv).

-behaviour(gen_server).

%% OTP API
-export([
         start_link/2
        ]).

%% user API
-export([
         connect/1,
         reconnect/0,
         run_sql_query/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {shell_pid,
                has_connection = false,
                connection     = none,
                monitor_ref    = none,
                nodes          = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ShellPid, Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ShellPid, Nodes], []).

connect(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {connect, Nodes}).

reconnect() ->
    gen_server:call(?SERVER, reconnect).

run_sql_query(SQL) ->
    gen_server:call(?SERVER, {run_sql_query, SQL}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ShellPid, Nodes]) ->
    process_flag(trap_exit, true),
    State = #state{shell_pid = ShellPid,
                   nodes     = Nodes},
    {Reply, NewState} = pb_connect(State),
    %% let the shell know that you have a connection now
    ShellPid ! Reply,
    {ok, NewState}.

handle_call({run_sql_query, SQL}, _From, #state{connection = Connection} = State) ->
    Ret = riakc_ts:query(Connection, SQL),
    Reply = io_lib:format("~p", [Ret]),
    {reply, Reply, State};
handle_call(reconnect, _From, #state{shell_pid = ShellPid} = State) ->
    NewS = mebbies_kill_connection(State),
    {Reply, NewS2} = pb_connect(NewS),
    ShellPid ! Reply,
    {reply, "Trying to reconnect...", NewS2};
handle_call({connect, Nodes}, _From, #state{shell_pid = ShellPid} = State) ->
    NewS = mebbies_kill_connection(State),
    {Reply, NewS2} = pb_connect(NewS#state{nodes = Nodes}),
    ShellPid ! Reply,
    {reply, "Trying to connect...", NewS2};
handle_call(Request, _From, State) ->
    io:format("not handling request ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, _PID, Reason}, State)
  when Reason =:= killed       orelse
       Reason =:= disconnected ->
    #state{shell_pid = ShellPid} = State,
    {Reply, NewS} = pb_connect(State),
    ShellPid ! Reply,
    {noreply, NewS};
handle_info(Info, State) ->
    io:format("Info is ~p~n\r", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pb_connect(#state{nodes = Nodes} = State) ->
    {Reply, NewS} = case try_and_connect(Nodes) of
                        {ok, Pid, MonRef, R} ->
                            {{connected, R}, State#state{has_connection = true,
                                                         monitor_ref    = MonRef,
                                                         connection     = Pid}};
                        error ->
                            {disconnected, State#state{has_connection = false,
                                                       connection     = none}}
                    end,
    {Reply, NewS}.

try_and_connect([]) ->
    error;
try_and_connect([Node | T]) when is_atom(Node) ->
    case net_adm:ping(Node) of
        pong ->
            case rpc:call(Node, application, get_env, [riak_api, pb]) of
                {ok, [{_, Port}]} ->
                    case start_socket(Node, Port) of
                        {ok, Pid, MonRef, Reply} -> {ok, Pid, MonRef, Reply};
                        err                      -> try_and_connect(T)
                    end;
                _Other ->
                    try_and_connect(T)
            end;
        pang ->
            try_and_connect(T)
    end.

start_socket(Node, Port) ->
    Host = get_host(Node),
    try
        {ok, Pid} = riakc_pb_socket:start(Host, Port),
        %% we need to get down messages
        MonitorRef = erlang:monitor(process, Pid),
        {ok, Pid, MonitorRef, {Node, Port}}
    catch _A:_B ->
            err
    end.

get_host(Node) ->
    N2 = atom_to_list(Node),
    [_, Host] = string:tokens(N2, "@"),
    list_to_atom(Host).

mebbies_kill_connection(#state{has_connection = false} = State) ->
    State;
mebbies_kill_connection(#state{has_connection = true,
                               connection     = C,
                               monitor_ref    = MonitorRef} = State) ->
    true = erlang:demonitor(MonitorRef),
    exit(C, kill),
    State#state{has_connection = false,
                connection     = none,
                monitor_ref    = none}.
