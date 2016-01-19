-module(log_EXT).

-export([
         help/2
         ]).

-export([
         log/2,
         date_log/2,
         logfile/2,
         show_log_status/1
        ]).

-include("riakshell.hrl").

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
