-define(CONFIGFILE, "../etc/riakshell.config").

-record(state, {
          version     = "1.0",
          count       = 1   :: integer(),
          partial_cmd = []  :: [char()],
          extensions  = [],
          history     = [],
          config      = [],
          logfile     = "../log/riakshell",
          logging     = off :: on | off,
          date_log    = off :: on | off,
          current_date = riakshell_util:datetime()
         }).
