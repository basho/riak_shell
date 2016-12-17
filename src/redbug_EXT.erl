%% -------------------------------------------------------------------
%%
%% redbug extension for riak_shell
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
-module(redbug_EXT).

%% NOTE this is the only extention that is not loaded when you run the load commmand
%% PLEASE DO NOT add additional functions to this module

-export([
         help/1,
         trace/3,
         trace/4
        ]).

-ignore_xref([
              help/1,
              trace/3,
              trace/4
             ]).

-include("riak_shell.hrl").

-record(trace_node, {
          status  = off            :: on | off,
          traces  = []             :: [list()],
          options = [{msgs, 1000}]
         }).

help(trace) ->
    "Typing `trace add \"riak_ql_ddl:current_version -> return\";` will set "
        "a redbug trace on the currently connected node.~n~n"
        "Typing `trace remove \"riak_ql_ddl:current_version -> return\";` "
        "will remove a redbug trace on the currently connected node.~n~n"
        "Typing `trace show;` whill show the status of redbug traceing "
        "and the current traces set.~n~n"
        "Typing `trace pause;` will pause the current redbug traces.~n~n"
        "Typing `trace restart;` will restart a paused redbug trace, "
        "or a redbug trace that has terminated because it has exceeded "
        "the msg limit.~n~n"
        "Typing `trace clear;` will stop redbug tracing and clear "
        "the set of traces.".

trace(Cmd, #state{has_connection = false} = State, _Action) ->
    ErrMsg = "riak-shell must be connected to a node to trace",
    {Cmd#command{response  = ErrMsg,
                 cmd_error = true}, State};
trace(Cmd, #state{'EXT_state' = EXTState} = State,
      Action) when Action =:= show    ->
    RedbugState = get_state(EXTState),
    Resp = print_redbug_state(RedbugState),
    {Cmd#command{response = Resp}, State};
trace(Cmd, #state{'EXT_state' = _EXTState} = State,
      Action) when Action =:= pause   orelse
                   Action =:= restart orelse
                   Action =:= clear   ->
    Resp = io_lib:format("in ~p", [Action]),
    {Cmd#command{response = Resp}, State}.

trace(Cmd, #state{has_connection = false} = State, _Action, _Arg) ->
    ErrMsg = "riak-shell must be connected to a node to trace",
    {Cmd#command{response = ErrMsg,
                 cmd_error = true}, State};
trace(Cmd, #state{connection  = {Node, _},
                  'EXT_state' = EXTState} = State,
      Action, TraceString) when Action =:= add       andalso
                                is_list(TraceString) ->
    io:format("in trace add with ~p~n", [TraceString]),
    RedbugNodeState = get_node_state(Node, EXTState),
    #trace_node{traces  = Traces,
                options = Options} = RedbugNodeState,
    NewTraces = lists:usort([TraceString | Traces]),
    NewRedbugNodeState = RedbugNodeState#trace_node{traces = NewTraces},
    NewEXTState = store_node_state(Node, NewRedbugNodeState, EXTState),
    Ret = rpc:call(Node, redbug, start, [TraceString, Options]),
    io:format("Ret from rpc call is ~p~n", [Ret]),
    Resp = io_lib:format("in ~p with ~p", [Action, TraceString]),
    io:format("EXTState is ~p~nRedbugNodeState is ~p~n"
              "NewRedbugNodeState is ~p~nNewEXTState is ~p~n", 
              [EXTState, RedbugNodeState, NewRedbugNodeState, NewEXTState]),
    {Cmd#command{response = Resp}, State#state{'EXT_state' = NewEXTState}};
trace(Cmd, State, Action, Arg) when Action =:= remove andalso
                                    is_list(Arg)      ->
    Resp = io_lib:format("in ~p with ~p", [Action, Arg]),
    {Cmd#command{response = Resp}, State}.

%%
%% Internal functions
%%

get_state(EXTState) ->
    case lists:keyfind(redbug, 1, EXTState) of
        false          -> [];
        {redbug, List} -> List
    end.

get_node_state(Node, EXTState) ->
    EXT = case lists:keyfind(redbug, 1, EXTState) of
              false -> [];
              List  -> List
          end,
    case lists:keyfind(Node, 1, EXT) of
        false       -> #trace_node{};
        {Node, Rec} -> Rec
    end.

store_node_state(Node, NewNodeState, EXTState) ->
    lists:keystore(Node, 1, EXTState, {Node, NewNodeState}).

print_redbug_state(Traces) ->
    case Traces of
        [] -> "There are no traces on any nodes";
        _ -> lists:flatten([print_node_state(Node, Rec) 
                            || {Node, Rec} <- lists:sort(Traces)])
    end.

print_node_state(Node, #trace_node{status = St,
                                   traces = Tr}) ->
    Msg1 = io_lib:format("Tracing on ~p is ~p~n", [Node, St]),
    Msg2 = case Tr of
               [] -> "No traces set";
               _  -> "Tracing:\n" ++
                         string:join([io_lib:format(" ~p", [X]) || X <- Tr], "\n")
           end,
    Msg1 ++ Msg2.
