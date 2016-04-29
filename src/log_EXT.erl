%% -------------------------------------------------------------------
%%
%% The logging extention for riak_shell
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
-module(log_EXT).

-export([
         help/1
         ]).

-export([
         date_log/3,
         log/3,
         logfile/3,
         regression_log/3,
         replay_log/2,
         replay_log/3,
         show_log_status/2
        ]).

-include("riak_shell.hrl").

help(regression_log)  ->
    "Type 'regression_log \"myregression.log\" ;' to run a regression.~n~n"
    "This will replay the log and check the results are the same~n"
    "as the last time you ran it. Useful for testing.~n"
    "You can run this in batch mode with the -r flag, see the README for details.";
help(replay_log)  ->
    "Type 'replay_log;' to replay the current logfile, or~n"
    "'replay_log \"myfilename.log\";' to replay a different log file.~n"
    "You can run this in batch mode with the -f flag, see the README for details.";
help(show_log_status) ->
    "Type 'show_log_status;' to see the logging status.";
help(logfile) ->
    "Type 'logfile \"mylogname\"'; to set the name of the logfile~n"
    "or 'logfile default ;' to reset to the default log file which~n"
    "can be set in the config.";
help(date_log) ->
    "Toggle adding a timestamp to the name of the log file with 'date_log on ;'~n"
    "and off with 'date_log off ;'~n"
    "The filename will be something like 'riak_shell.2016_02_15-16:42:22.log'~n"
    "You will get a new log file for each session of riak-shell.~n~n"
    "The default can be set in the config file.";
help(log) ->
    "Switch logging on with 'log on ;' and off with 'log off ;'~n~n"
    "The default can be set in the config file.".

regression_log(Cmd, #state{} = State, LogFile) ->
    io:format("~nRegression Testing ~p~n", [LogFile]),
    case replay(Cmd, State, LogFile, regression_fold_fn()) of
        {#command{response = []}, NewS} ->
            {Cmd#command{response = "No Regression Errors."}, NewS};
        {Cmd2, NewS} ->
            {Cmd2#command{cmd_error = true}, NewS}
    end.

replay_log(Cmd, #state{logfile = LogFile} = State) ->
    replay_log(Cmd, State, LogFile ++ ".log").

replay_log(Cmd, State, LogFile) when is_list(LogFile) ->
    io:format("~nReplaying ~p~n", [LogFile]),
    replay(Cmd, State, LogFile, replay_fold_fn()).

replay(Cmd, State, LogFile, FoldFn) ->
    {Cmd3, S} = try
                    true = filelib:is_file(LogFile),
                    try
                        {ok, Inputs} = file:consult(LogFile),
                        {Msgs, _N, Cmd2, NewState} = lists:foldl(FoldFn, {[], 1, Cmd, State}, Inputs),
                        {Cmd2#command{response = lists:reverse(Msgs)}, NewState}
                    catch Type:Err ->
                        riak_shell:maybe_print_exception(State, Type, Err),
                        Msg2 = io_lib:format("File ~p is corrupted.",
                                             [LogFile]),
                        {Cmd#command{response  = Msg2,
                                     cmd_error = true}, State}
                    end
                catch Type2:Err2 ->
                    riak_shell:maybe_print_exception(State, Type2, Err2),
                    Msg3 = io_lib:format("File ~p does not exist.", [LogFile]),
                    {Cmd#command{response  = Msg3,
                                 cmd_error = true}, State}
                end,
    {Cmd3#command{log_this_cmd = false}, S}.

replay_fold_fn() ->
    fun({{command, Input}, {result, _}}, {Msgs, N, Cmd, S}) ->
            case should_replay(Input) of
                false ->
                    {Msgs, N, Cmd, S};
                true ->
                    Msg1 = io_lib:format("replay (~p)> ~s\n", [N, Input]),
                    {Cmd2, NewS} = riak_shell:handle_cmd(Input, Cmd, S),
                    {[Msg1 ++ Cmd2#command.response ++ "\n" | Msgs], N + 1, Cmd2, NewS}
            end
    end.

regression_fold_fn() ->
    fun({{command, Input}, {result, Res}}, {Msgs, N, Cmd, S}) ->
            case should_replay(Input) of
                false ->
                    {Msgs, N + 1, Cmd, S};
                true ->
                    {Cmd2, NewS} = riak_shell:handle_cmd(Input, Cmd, S),
                    Msg1 = lists:flatten(Cmd2#command.response),
                    Msgs2 = case Msg1 of
                                Res ->
                                    Msgs;
                                _Diff  -> 
                                    Msg = io_lib:format("Cmd ~p (~p) failed\n" ++
                                                            "Got:\n- ~p\nExpected:\n- ~p\n",
                                                        [Cmd2#command.cmd, N, Msg1, Res]),
                                    [lists:flatten(Msg) | Msgs]
                            end,
                    {Msgs2, N + 1, Cmd2, NewS}
            end
    end.

should_replay("load"           ++ _Rest) -> false;
should_replay("regression_log" ++ _Rest) -> false; 
should_replay("replay_log"     ++ _Rest) -> false; 
should_replay(_)                         -> true.

show_log_status(Cmd, #state{logging      = Logging,
                            date_log     = Date_Log,
                            logfile      = Logfile,
                            current_date = Date} = State) ->
    Msg = io_lib:format("Logging     : ~p~n" ++
                        "Date Log    : ~p~n" ++
                        "Logfile     : ~p~n" ++
                        "Current Date: ~p",
                        [Logging, Date_Log, Logfile, Date]),
    {Cmd#command{response = Msg}, State}.

log(Cmd, State, on) ->
    {Cmd#command{response     = "Logging turned on.",
                 log_this_cmd = false},
        State#state{logging = on}};
log(Cmd, State, off) ->
    {Cmd#command{response     = "Logging turned off.",
                 log_this_cmd = false}, State#state{logging = off}};
log(Cmd, State, Toggle) ->
    ErrMsg = io_lib:format("Invalid parameter passed to log ~p. Should be 'off' or 'on'.", [Toggle]),
    {Cmd#command{response  = ErrMsg,
                 cmd_error = true}, State}.

date_log(Cmd, State, on) ->
    {Cmd#command{response = "Log files will contain a date/time stamp."},
        State#state{date_log = on}};
date_log(Cmd, State, off) ->
    {Cmd#command{response = "Log files will not contain a date/time stamp."},
        State#state{date_log = off}};
date_log(Cmd, State, Toggle) ->
    ErrMsg = io_lib:format("Invalid parameter passed to log ~p. Should be 'off' or 'on'.", [Toggle]),
    {Cmd#command{response  = ErrMsg,
                 cmd_error = true}, State}.

logfile(Cmd, State, FileName) when is_list(FileName) ->
    Msg = io_lib:format("Log file changed to ~p~n", [FileName]),
    {Cmd#command{response = Msg}, State#state{logfile = FileName}};
logfile(Cmd, #state{logfile = LogFile} = State, default) ->
    Msg = io_lib:format("Log file changed to ~p (default)~n", [LogFile]),
    {Cmd#command{response = Msg}, State};
logfile(Cmd, State, FileName) ->
    Msg = io_lib:format("Filename ~p must be a string.", [FileName]),
    {Cmd#command{response  = Msg,
                 cmd_error = true}, State}.
