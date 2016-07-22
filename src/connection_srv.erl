%% -------------------------------------------------------------------
%%
%% supervisor for the protocol buffers connection for riak_shell
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
-define(QUERY_TIMEOUT, 30000).

-record(state, {shell_ref,
                has_connection = false,
                connection     = none,
                monitor_ref    = none,
                nodes          = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ShellRef, Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ShellRef, Nodes], []).

connect(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {connect, Nodes}).

reconnect() ->
    gen_server:call(?SERVER, reconnect).

run_sql_query(SQL) ->
    gen_server:call(?SERVER, {run_sql_query, SQL}, ?QUERY_TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ShellRef, Nodes]) ->
    process_flag(trap_exit, true),
    State = #state{shell_ref = ShellRef,
                   nodes     = Nodes},
    {Reply, NewState} = pb_connect(State),
    %% let the shell know that you have a connection now
    riak_shell:send_to_shell(ShellRef, Reply),
    {ok, NewState}.

map_timestamp({Int, timestamp}) ->
    jam_iso8601:to_string(Int, [{precision, 3}]);
map_timestamp({Value, _Type}) ->
    Value.

handle_call({run_sql_query, SQL}, _From, #state{connection = Connection} = State) ->
    Reply = case riakc_ts:query(Connection, SQL, [], undefined, [{datatypes, true}]) of
                {error, {ErrNo, Binary}} ->
                    io_lib:format("Error (~p): ~s", [ErrNo, Binary]);
                {ok, {Header, Rows}} ->
                    Hdr = [binary_to_list(Name) || {Name, _Type} <- Header],
                    Types = [Type || {_Name, Type} <- Header],
                    Rs = [begin
                              Row = lists:zip(tuple_to_list(RowTuple), Types),
                              XlatedRow = lists:map(fun map_timestamp/1, Row),
                              [riak_shell_util:to_list(X) || X <- XlatedRow]
                          end || RowTuple <- Rows],
                    case {Hdr, Rs} of
                        {[], []} ->
                            "";
                        _ ->
                            clique_table:autosize_create_table(Hdr, Rs)
                    end
            end,
    {reply, Reply, State};
handle_call(reconnect, _From, #state{shell_ref = _ShellRef} = State) ->
    NewS = ensure_connection_killed(State),
    {Reply, NewS2} = pb_connect(NewS),
    {reply, Reply, NewS2};
handle_call({connect, Nodes}, _From, #state{shell_ref = _ShellRef} = State) ->
    NewS = ensure_connection_killed(State),
    {Reply, NewS2} = pb_connect(NewS#state{nodes = Nodes}),
    {reply, Reply, NewS2};
handle_call(Request, _From, State) ->
    io:format("not handling request ~p~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, _PID, Reason}, State)
  when Reason =:= killed       orelse
       Reason =:= disconnected ->
    #state{shell_ref = ShellRef} = State,
    {Reply, NewS} = pb_connect(State),
    riak_shell:send_to_shell(ShellRef, Reply),
    {noreply, NewS};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

pb_connect(#state{nodes = Nodes} = State) ->
    {Reply, NewS} = case connect_to_first_available(Nodes) of
                        {ok, Pid, MonRef, R} ->
                            {{connected, R}, State#state{has_connection = true,
                                                         monitor_ref    = MonRef,
                                                         connection     = Pid}};
                        error ->
                            {disconnected, State#state{has_connection = false,
                                                       connection     = none}}
                    end,
    {Reply, NewS}.

connect_to_first_available([]) ->
    error;
connect_to_first_available([Node | T]) when is_atom(Node) ->
    try
        pong = net_adm:ping(Node),
        {ok, [{_, Port}]} = rpc:call(Node, application, get_env, [riak_api, pb]),
        {ok, _Pid, _MonRef, _Reply} = start_socket(Node, Port)
    catch _A:_B ->
            connect_to_first_available(T)
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

ensure_connection_killed(#state{has_connection = false} = State) ->
    State;
ensure_connection_killed(#state{has_connection = true,
                                connection     = C,
                                monitor_ref    = MonitorRef} = State) ->
    true = erlang:demonitor(MonitorRef),
    exit(C, kill),
    State#state{has_connection = false,
                connection     = none,
                monitor_ref    = none}.
