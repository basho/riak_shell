-define(CONFIGFILE, "../etc/riakshell.config").

-record(state, {
          version      = "1.0",
          count        = 1   :: integer(),
          partial_cmd  = []  :: [char()],
          extensions   = [],
          history      = [],
          config       = [],
          log_this_cmd = true,
          logfile      = "../log/riakshell",
          logging      = off :: on | off,
          date_log     = off :: on | off,
          current_date = riakshell_util:datetime()
         }).

%% commands beginning with the implemented SQL keywords listed
%% cannot be used as command names in extensions
%% these keywords are used to select which lexer-parser will be used as well
-define(IMPLEMENTED_SQL_STATEMENTS, [
                                     select,
                                     create,
                                     describe
                                    ]).
