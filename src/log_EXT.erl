-module(log_EXT).

-export([
         help/2
         ]).

-export([
         log/2,
         replay_log/1,
         replay_log/2,
         regression_log/2,
         date_log/2,
         logfile/2,
         show_log_status/1
        ]).

-include("riakshell.hrl").

help(regression_log, 1)  ->
    "Type 'regression_log(myregresion.log);' to run a regression. This will replay the log and check the results are the same " ++
        "as the last time you ran it. Useful for smoke testing and stuff.";
help(replay_log, N) when N =:= 0 orelse
                         N =:= 1 ->
    "Type 'replay_log();' to replay the current logfile. This will work if logging is on or off.~n" ++
        "Type 'replay_log(myfilename.log);' to replay a different log file.";
help(show_log_status, 0) ->
    "Type 'show_log_status();' to see the logging status.~n" ++
        "Is logging on? are the log files datestamped? what is the logfile?.";
help(logfile, 1) ->
    "Type 'logfile(\"mylogname\")'; to set the name of the logfile~n" ++
        "or 'logfile(default);' to reset to the default log file " ++
        "which can be set in the config.";
help(date_log, 1) ->
    "Toggle adding a Time/Datestamp to the log files with 'date_log(on);' " ++
        "and off with 'date_log(off);'" ++
        "The default can be set in the config file. ";
help(log, 1) ->
    "Switch logging on with 'log(on);' and off with 'log(off);'" ++
        "The default can be set in the config file.".

regression_log(#state{} = State, LogFile) ->
    io:format("~nRegression Testing ~p~n", [LogFile]),
    case replay(State, LogFile, regression_fold_fn()) of
        {[], NewS}   -> {io_lib:format("No Regression Errors.", []), NewS};
        {Msgs, NewS} -> {Msgs, NewS}
    end.

replay_log(#state{logfile = LogFile} = State) ->
    replay_log(State, LogFile ++ ".log").

replay_log(State, LogFile) when is_list(LogFile) ->
    io:format("~nReplaying ~p~n", [LogFile]),
    replay(State, LogFile, replay_fold_fn()).

replay(State, LogFile, FoldFn) ->
    try
        true = filelib:is_file(LogFile),
        try
            {ok, Cmds} = file:consult(LogFile),
            {Messages, _N, NewState} = lists:foldl(FoldFn, {[], 1, State}, Cmds),
            {lists:reverse(Messages), NewState}
        catch _:_ ->
                Msg2 = io_lib:format("File ~p is corrupted.", 
                                     [LogFile]),
                {Msg2, State}
        end
    catch _:_ ->
            Msg3 = io_lib:format("File ~p does not exist.", [LogFile]),
            {Msg3, State}
    end.            

replay_fold_fn() ->
    fun({{command, Cmd}, {result, _}}, {Msgs, N, S}) ->
            case should_replay(Cmd) of
                false ->
                    {Msgs, N, S};
                true ->
                    Msg1 = io_lib:format("replay (~p)> ~s\n", [N, Cmd]),
                    {Msg2, NewS} = riakshell_shell:handle_cmd(Cmd, S),
                    {[Msg1 ++ Msg2 ++ "\n" | Msgs], N + 1, NewS}
            end
    end.

regression_fold_fn() ->
    fun({{command, Cmd}, {result, Result}}, {Msgs, N, S}) ->
            case should_replay(Cmd) of
                false ->
                    {Msgs, N, S};
                true ->
                    {Msg2, NewS} = riakshell_shell:handle_cmd(Cmd, S),
                    Msgs2 = case lists:flatten(Msg2)  of
                                Result -> Msgs;
                                _Diff  -> Msg = io_lib:format("Cmd ~p (~p) failed\n" ++
                                                                  "Got:\n- ~p\nExpected:\n- ~p\n",
                                                              [Cmd, N, lists:flatten(Msg2), Result]),
                                          [lists:flatten(Msg) | Msgs]
                            end,
                    {Msgs2, N + 1, NewS}
            end
    end.

should_replay("load"           ++ _Rest) -> false;
should_replay("regression_log" ++ _Rest) -> false; 
should_replay("replay_log"     ++ _Rest) -> false; 
should_replay(_)                         -> true.

show_log_status(#state{logging     = Logging,
                       date_log     = Date_Log,
                       logfile      = Logfile,
                       current_date = Date} = State) ->
    Msg = io_lib:format("Logging    : ~p~nDate Log   : ~p~nLogfile    : ~p~n" ++
                            "Current Date: ~p", 
                        [Logging, Date_Log, Logfile, Date]),
    {Msg, State}.

log(State, on) ->
    {"Logging turned on.", State#state{logging = on}};
log(State, off) ->
    {"Logging turned off.", State#state{logging = off}}.

date_log(State, on) ->
    {"Log files will contain a date/time stamp.", State#state{date_log = on}};
date_log(State, off) ->
    {"Log files will not contain a date/time stamp.", State#state{date_log = off}}.

logfile(State, FileName) when is_list(FileName) ->
    Msg = io_lib:format("Log file changed to ~p~n", [FileName]),
    {Msg, State#state{logfile = FileName}};
logfile(State, default) ->
    {"Flrp.", State}.
