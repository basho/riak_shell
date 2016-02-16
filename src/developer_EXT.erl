%% -------------------------------------------------------------------
%%
%% developer extension for riak_shell
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
-module(developer_EXT).

-export([
         help/1,
         observer/1,
         show_path/1,
         show_EXT_load_paths/1,
         add_EXT_load_path/2
        ]).

-include("riak_shell.hrl").

help(show_path) ->
    "Typeing 'show_path;' will list the Erlang code path";
help(show_EXT_load_paths) ->
    "Typing 'show_EXT_load_path' will show all the locations where additional EXT files are being loaded from.";
help(add_EXT_load_path) ->
    "Typing 'add_EXT_load_path \"../some/path/\"' will add a path to the EXT load path~n"
        "To load the beams from the new directory you must invoke 'load;'";
help(observer) ->
    "Typing 'observer;' starts the Erlang observer application.".

show_path(State) ->
    Msg1 = "The Erlang path is:~n* ",
    Msg2 = string:join(code:get_path(), "\n* "),
    {Msg1 ++ Msg2, State}.

observer(#state{} = State) ->
    observer:start(),
    {"Observer started", State#state{log_this_cmd = false}}. 
  
show_EXT_load_paths(#state{load_paths_for_EXTs = []} = State) ->
    {"Not loading additional EXTs", State};
show_EXT_load_paths(#state{load_paths_for_EXTs = Paths} = State) ->
    Msg1 = io_lib:format("Loading EXTs from:~n", []),
    Msg2 = lists:flatten([io_lib:format("* ~s~n", [X]) || X <- Paths]),
    Msg = Msg1 ++ string:strip(Msg2, right, $\n),
    {Msg, State}.

add_EXT_load_path(#state{load_paths_for_EXTs = Paths} = State, Path) 
  when is_list(Path) ->
    case filelib:is_dir(Path) of
        true -> 
            Msg = io_lib:format("~p added to extension load paths", [Path]),
            State2 = State#state{load_paths_for_EXTs = [Path | Paths]},
            {Msg, State2};
        false -> 
            ErrMsg = io_lib:format("~p is not a valid directory. "
                                   "Extension load paths unchanged",
                                   [Path]),
            {ErrMsg, State#state{cmd_error = true}}
    end;
add_EXT_load_path(State, Path) ->
    ErrMsg = io_lib:format("Path ~p must be a string.", [Path]),
    {ErrMsg, State#state{cmd_error = true}}.
