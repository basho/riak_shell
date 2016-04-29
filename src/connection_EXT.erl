%% -------------------------------------------------------------------
%%
%% connection management extension for riak_shell
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
-module(connection_EXT).

-include("riak_shell.hrl").

-export([
         help/1
        ]).

-export([
         connect/3,
         connection_prompt/3,
         ping/2,
         ping/3,
         reconnect/2,
         show_connection/2,
         show_cookie/2,
         show_nodes/2
        ]).

help(show_nodes) ->
    "Type 'show_nodes;' to see which nodes riak-shell is connected to.";
help(show_cookie) ->
    "Type 'show_cookie;' to see what the Erlang cookie is for riak-shell.~n"
    "The riak-shell needs to have the same cookie as the riak nodes you~n"
    "are connecting to.";
help(ping) ->
    "Typing 'ping;' will ping all the nodes specified in the config file~n"
    "and print the results. Typing 'ping \"dev1@127.0.0.1\"; will ping~n"
    "a particular node. You need to replace dev1 etc with your actual~n"
    "node name";
help(reconnect) ->
    "Typing 'reconnect;' will try to connect you to one of the nodes~n"
    "listed in your riak_shell.config. It will try each node until it~n"
    "succeeds (or doesn't).~n~n"
    "To connect to a specific node (or one not in your riak_shell.config)~n"
    "please use the connect command.";
help(connect) ->
    "You can connect to a specific node (whether in your riak_shell.config~n"
    "or not) by typing 'connect \"dev1@127.0.0.1\";' substituting your~n"
    "node name for dev1.~n~n"
    "You may need to change the Erlang cookie to do this.~n~n"
    "See also the 'reconnect' command.";
help(connection_prompt) ->
    "Type 'connection_prompt on;' to display the connection status in~n"
    "the prompt, or 'connection_prompt off; to disable it.~n~n"
    "Unicode support in your terminal is highly recommended.";
help(show_connection) ->
    "This shows which riak node riak-shell is connected to".

show_nodes(Cmd, State) ->
    Msg = io_lib:format("The connected nodes are: ~p", [lists:sort(nodes())]),
    {Cmd#command{response = Msg}, State}.

show_cookie(Cmd, #state{cookie = Cookie} = State) ->
    Msg = io_lib:format("Cookie is ~p [actual ~p]", [Cookie, erlang:get_cookie()]),
    {Cmd#command{response = Msg}, State}.

ping(Cmd, #state{config = Config} = State) ->
    Nodes = riak_shell:read_config(Config, nodes, []),
    FoldFn = fun(Node, Msg) ->
                     Msg2 = ping_single_node(State, Node),
                     [Msg2] ++ Msg
             end,
    Msgs = lists:foldl(FoldFn, [], Nodes),
    {Cmd#command{response     = string:join(Msgs, "\n"),
                 log_this_cmd = false}, State}.

ping(Cmd, State, Node) when is_atom(Node) ->
    {Cmd#command{response     = ping_single_node(State, Node),
                 log_this_cmd = false}, State};
ping(Cmd, State, Node) when is_list(Node) ->
    ping(Cmd, State, list_to_atom(Node));
ping(Cmd, State, Node) ->
    Msg = io_lib:format("Error: node has to be a valid node name ~p", [Node]),
    {Cmd#command{response     = Msg,
                 cmd_error    = true,
                 log_this_cmd = false}, State}.

ping_single_node(State, Node) ->
    {Prefix1, Prefix2} = case State#state.show_connection_status of
                             true  -> {?GREENTICK, ?REDCROSS};
                             false -> {"", ""}
                         end,
    case net_adm:ping(Node) of
        pong -> io_lib:format("~p: " ++ Prefix1 ++ " (connected)",    [Node]);
        pang -> io_lib:format("~p: " ++ Prefix2 ++ " (disconnected)", [Node])
    end.

show_connection(Cmd, #state{has_connection = false} = State) ->
    {Cmd#command{response = "riak-shell is not connected to riak"}, State};
show_connection(Cmd, #state{has_connection = true,
                            connection     = {Node, Port}} = State) ->
    Msg = io_lib:format("riak-shell is connected to: ~p on port ~p",
                        [Node, Port]), 
    {Cmd#command{response = Msg}, State}.

reconnect(Cmd, S) ->
    case connection_srv:reconnect() of
        {connected, {Node, Port}} ->
            Msg = io_lib:format("Reconnected to ~p on port ~p", [Node, Port]),
            {Cmd#command{response = Msg}, S#state{has_connection = true,
                                                  connection     = {Node, Port}}};
        disconnected ->
            Msg = "Reconnection failed",
            {Cmd#command{response = Msg}, S#state{has_connection = false,
                                                  connection     = none}}
    end.

connect(Cmd, S, Node) when is_atom(Node) ->
    case connection_srv:connect([Node]) of
        {connected, {N, Port}} ->
            Msg = io_lib:format("Connected to ~p on port ~p", [N, Port]),
            {Cmd#command{response = Msg}, S#state{has_connection = true,
                                                  connection     = {Node, Port}}};
        disconnected ->
            Msg = io_lib:format("Connection to ~p failed", [Node]),
            {Cmd#command{response = Msg}, S#state{has_connection = false,
                                                  connection     = none}}
    end;
connect(Cmd, S, Node) when is_list(Node) ->
    connect(Cmd, S, list_to_atom(Node));
connect(Cmd, State, Node) ->
    Msg = io_lib:format("Error: node has to be a valid node name ~p", [Node]),
    {Cmd#command{response  = Msg,
                 cmd_error = true}, State}.

connection_prompt(Cmd, State, on) ->
    Msg = io_lib:format("Connection Prompt turned on", []),
    {Cmd#command{response = Msg}, State#state{show_connection_status = true}};
connection_prompt(Cmd, State, off) ->
    Msg = io_lib:format("Connection Prompt turned off", []),
    {Cmd#command{response = Msg}, State#state{show_connection_status = false}};
connection_prompt(Cmd, State, Toggle) ->
    ErrMsg = io_lib:format("Invalid parameter passed to connection_prompt ~p. Should be 'off' or 'on'.", [Toggle]),
    {Cmd#command{response  = ErrMsg,
                 cmd_error = true}, State}.
                              
