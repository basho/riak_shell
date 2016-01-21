-define(CONFIGFILE, "../etc/riakshell.config").

-record(state, {
          version      = "1.0",
          mode         = riakshell :: riakshell | sql | 'riak-admin',
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
