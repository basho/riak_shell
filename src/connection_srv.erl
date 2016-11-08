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
         run_sql_query/2
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

run_sql_query(SQL, Format) ->
    gen_server:call(?SERVER, {run_sql_query, SQL, Format}, ?QUERY_TIMEOUT).

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

map_timestamp({Int, {Name, timestamp}}) ->
    %% We currently only support millisecond accuracy (10^3).
    {Name, jam_iso8601:to_string(jam:from_epoch(Int, 3))};
map_timestamp({Value, {Name, _Type}}) ->
    {Name, Value}.

%% Remove extra new line if present
format_table({"", _}) ->
    "";
format_table({[[]], _}) ->
    "";
format_table({String, _}) ->
    re:replace(lists:flatten(String), "\r", "", [global,{return,list}]).

handle_call({run_sql_query, SQL, Format}, _From,
            #state{connection = Connection} = State) ->
    Reply = case riakc_ts:'query'(Connection, SQL, [], undefined, [{datatypes, true}]) of
                {error, {ErrNo, Binary}} ->
                    io_lib:format("Error (~p): ~s", [ErrNo, Binary]);
                {ok, {Header, Rows}} ->
                    Rs = [begin
                              Row = lists:zip(tuple_to_list(RowTuple), Header),
                              XlatedRow = lists:map(fun map_timestamp/1, Row),
                              [{riak_shell_util:to_list(Name), riak_shell_util:to_list(X)} || {Name, X} <- XlatedRow]
                          end || RowTuple <- Rows],
                    Status = clique_status:table(Rs),
                    format_table(clique_writer:write([Status], Format));
                {error, Err} ->
                    %% a normal Erlang error message in the shell sprays over many lines
                    %% these regexs just strip whitespace to make it more compact
                    %% otherwise the screen just scrolls off and leaves the user bewildered
                    %% if they are not an Erlang dev
                    Err2 = re:replace(Err, "[\r | \n | \t]", " ", [global, {return, list}]),
                    Err3 = re:replace(Err2, "[\" \"]+",    " ", [global, {return, list}]),
                    Msg = "UNEXPECTED ERROR - if you have logging on please send your logfile to Basho: ~s",
                    io_lib:format(Msg, [Err3])
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
