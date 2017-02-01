-define(CONFIGFILE, "../etc/riak_shell.config").
-define(GREENTICK, [9989]).
-define(REDCROSS,  [10060]).

-define(VERSION_NUMBER, "1.5").

-record(command, {
    %% these fields are used to convey information about the
    %% the current execution loop, is the command complete
    %% should this command be written to the log and
    %% does this command return an error
    cmd_error              = false :: boolean(),
    cmd                    = []    :: [char()],
    cmd_tokens             = []    :: riak_shell:tokens(),
    log_this_cmd           = true  :: boolean(),
    response               = []    :: [char()],
    increment              = false :: boolean(),
    in_production          = true  :: boolean()
}).

-record(state, {
    config                 = []  :: riak_shell:proplist(),
    connection             = none,
    cookie                 = riak,
    count                  = 1   :: integer(),
    current_date           = riak_shell_util:datetime(),
    date_log               = off :: riak_shell:on_off(),
    debug                  = off :: riak_shell:on_off(),
    extensions             = [],
    format                 = "human" :: riak_shell:format(),
    has_connection         = false :: boolean(),
    history                = [],
    logfile                = undefined,
    logging                = off :: riak_shell:on_off(),
    show_connection_status = false,
    version                = undefined
}).


%% commands beginning with the implemented SQL keywords listed
%% cannot be used as command names in extensions
%% these keywords are used to select which lexer-parser will be used as well
-define(IMPLEMENTED_SQL_STATEMENTS, [
                                     sql, % reserved for SQL help
                                     alter,
                                     create,
                                     delete,
                                     describe,
                                     explain,
                                     insert,
                                     select,
                                     show
                                    ]).
