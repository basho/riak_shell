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
         show_cookie/1,
         show_nodes/1,
         reconnect/1,
         connect/2,
         ping/1,
         ping/2,
         connection_prompt/2,
         show_connection/1
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

show_nodes(State) ->
    Msg = io_lib:format("The connected nodes are: ~p", [nodes()]),
    {Msg, State}.

show_cookie(#state{cookie = Cookie} = State) ->
    Msg = io_lib:format("Cookie is ~p [actual ~p]", [Cookie, erlang:get_cookie()]),
    {Msg, State}.

ping(#state{config = Config} = State) ->
    Nodes = riak_shell:read_config(Config, nodes, []),
    FoldFn = fun(Node, {Msg, S}) ->
                     {Msg2, S2} = ping2(S, Node),
                     {[Msg2] ++ Msg, S2}
             end,
    {Msgs2, S2} = lists:foldl(FoldFn, {[], State}, Nodes),
    {string:join(Msgs2, "\n"), S2#state{log_this_cmd = false}}.

ping(State, Node) when is_atom(Node) ->
    ping2(State#state{log_this_cmd = false}, Node);
ping(State, Node) ->
    Msg = io_lib:format("Error: node has to be an atom ~p", [Node]),
    {Msg, State#state{cmd_error = true}}.

ping2(State, Node) ->
    {Prefix1, Prefix2} = case State#state.show_connection_status of
                             true  -> {?GREENTICK, ?REDCROSS};
                             false -> {"", ""}
                         end,
    Msg = case net_adm:ping(Node) of
              pong -> io_lib:format("~p: " ++ Prefix1 ++ " (connected)",    [Node]);
              pang -> io_lib:format("~p: " ++ Prefix2 ++ " (disconnected)", [Node])
          end,
    {Msg, State}.
    
show_connection(#state{has_connection = false} = State) ->
    {"riak-shell is not connected to riak", State};
show_connection(#state{has_connection = true,
                       connection     = {Node, Port}} = State) ->
    Msg = io_lib:format("riak-shell is connected to: ~p on port ~p",
                        [Node, Port]), 
    {Msg, State}. 

reconnect(S) ->
    Reply = connection_srv:reconnect(),
    Msg = io_lib:format("~p", [Reply]),
    {Msg, S}.

connect(S, Node) when is_atom(Node) ->
    Reply = connection_srv:connect([Node]),
    Msg = io_lib:format("~p", [Reply]),
    {Msg, S};
connect(S, Node) ->
    Msg = io_lib:format("Error: node has to be an atom ~p", [Node]),
    {Msg, S#state{cmd_error = true}}.

connection_prompt(State, on) ->
    Msg = io_lib:format("Connection Prompt turned on", []),
    {Msg, State#state{show_connection_status = true}};
connection_prompt(State, off) ->
    Msg = io_lib:format("Connection Prompt turned off", []),
    {Msg, State#state{show_connection_status = false}};
connection_prompt(State, Toggle) ->
    ErrMsg = io_lib:format("Invalid parameter passed to connection_prompt ~p. Should be 'off' or 'on'.", [Toggle]),
    {ErrMsg, State#state{cmd_error = true}}.
                              
