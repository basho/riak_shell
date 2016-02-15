-define(CONFIGFILE, "../etc/riak_shell.config").
-define(GREENTICK, [9989]).
-define(REDCROSS,  [10060]).

-define(VERSION_NUMBER, "0.9").

-record(state, {
          version                = undefined,
          count                  = 1   :: integer(),
          extensions             = [],
          history                = [],
          config                 = [],
          has_connection         = false :: boolean,
          connection             = none,
          cookie                 = riak,
          logfile                = undefined,
          logging                = off :: on | off,
          date_log               = off :: on | off,
          current_date           = riak_shell_util:datetime(),
          show_connection_status = false,
          %% these fields are used to convey information about the
          %% the current execution loop, is the command complete
          %% should this command be written to the log and
          %% does this command return an error
          partial_cmd            = []  :: [char()],
          cmd_error              = false,
          log_this_cmd           = true
         }).

%% commands beginning with the implemented SQL keywords listed
%% cannot be used as command names in extensions
%% these keywords are used to select which lexer-parser will be used as well
-define(IMPLEMENTED_SQL_STATEMENTS, [
                                     select,
                                     create,
                                     describe
                                    ]).
