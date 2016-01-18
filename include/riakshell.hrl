-record(state, {
          version     = "1.0",
          count       = 1   :: integer(),
          partial_cmd = []  :: [char()],
          extensions  = [],
          history     = [],
          config
         }).
