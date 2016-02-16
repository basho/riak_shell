-define(CONFIGFILE, "../etc/riak_shell.config").
-define(GREENTICK, [9989]).
-define(REDCROSS,  [10060]).

-record(state, {
          version                = "riak_shell 0.9",
          count                  = 0   :: integer(),
          extensions             = [],
          history                = [],
          config                 = [],
          has_connection         = false :: boolean,
          connection             = none,
          cookie                 = riak,
          logfile                = "../log/riak_shell",
          logging                = off :: on | off,
          date_log               = off :: on | off,
          current_date           = riak_shell_util:datetime(),
          show_connection_status = false,
          load_paths_for_EXTs    = [],
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
