Riakshell
---------

A configurable, scriptable and extendable shell for riak.

Goals
-----

The goals of riakshell are to have a single shell that can:
* run sql commands
* run riak-admin commands
* be used a developer/devops tool for managing riak clusters

Current Capabilities
--------------------

To that end the shell has integral:
* logging
* log replay
* log regression replay
* config
* you can run replay and regression logs in batch mode
  - by specifying a file of commands to replay
* specification of alternative configuration files at run time

The shell is also trivially extendable for developer use.

Lacunae
-------

The shell is in the early stages. The following are well supported:
* extensible architecture
* logging
* replay/regression
* history
* configuration
* sql mode
* batch mode
* management of connections to remote riak nodes
* shell management (including cookies)

The following is not yet supported:
* riak-admin mode
* integration with riak_test so that replay logs can be run as simple regression tests

Dependencies
------------

riak_ql needs to be fixed in a couple of ways before proper integration with riakshell:
* stop \n being an end token so that SQL commands can span multiple lines
* make the lexer parser expect a ";"
* decide on/understand the many-statements per line possibilities

Running/Getting Started
-----------------------

```
cd ~/riakshell/bin
./riakshell
```

You get help on what is implemented in the riakshell with the help command:
```
riakshell (1)> help;
```

The current state is:
```
Erlang R16B02-basho5 (erts-5.10.3) [source] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]

version "riakshell 0.9/sql 1.2", use 'quit;' or 'q;' to exit or 'help;' for help
riakshell(1)>help;
The following functions are available
(the number of arguments is given)
riakshell(2)>select * from mytable;
{error,{1019,
<<"Table mytable has not been activated (is in state 'undefined')">>}}
 riakshell(3)>help;
The following functions are available
(the number of arguments is given)

Extension 'connection' provides:
-           connect: 1
- connection_prompt: 1
-            msg_me: 1
-              ping: 0
-              ping: 1
-         reconnect: 0
-   show_connection: 0
-       show_cookie: 0
-        show_nodes: 0

Extension 'debug' provides:
-     load: 0
- observer: 0

Extension 'history' provides:
- clear_history: 0
-             h: 1
-       history: 1
-  show_history: 0

Extension 'log' provides:
-        date_log: 1
-             log: 1
-         logfile: 1
-  regression_log: 1
-      replay_log: 0
-      replay_log: 1
- show_log_status: 0

Extension 'shell' provides:
-           q: 0
-        quit: 0
- show_config: 0

You can get more help by calling help with the
function name and arguments like 'help shell quit;'
```

Configuration
-------------

Configuration is in the file
```
~/riakshell/etc/riakshell.config
```

The following things can be configured:
```
logging  = on | off
date_log = on | off
logfile  = "../some/dir/mylogfile.log"
```

Configuration will be exended to:
* shell mode on startup
* riak_nodes to connect to
* erlang cookie to set

Command Line Flags
------------------

There are 4 different configurations, two of which trigger batch mode.

By default riakshell swallows error messages, this makes it hard to develop new extentions. You can run it in debug mode as shown below:
``` 
./riakshell -d
```

You can pass in a different config file than `../etc/riakshell.config`:
```
./riakshell -c ../path/to/my.config
```

You can run a riakshell replay log for batch/scripting:
```
./riakshell -f ../path/to/my.log
```

You can run a riakshell regression log for batch/scripting:
```
./riakshell -r ../path/to/my.log
```

Extending The Riakshell
-----------------------

Riakshell uses a 'magic' architecture with convention.

Riak modules with names like:
```
mymodule_EXT.erl
```
are considered to be riakshell extension modules.

All exported functions with an arity >= 1 are automaticaly exposed in riakshell mode, with come exceptions.

Exported functions with the following names will be silently ignored:
* `module_info/0`
* `module_info/1`
* `help/1`
* `riak-admin'/N`

Functions that share a name with the first keyword of supported SQL statements will likewise be ignored:
* `create/N`
* `describe/N`
* `select/N`

As additional SQL statements are supported adding them to the macro `IMPLEMENTED_SQL_STATEMENTS` in `riakshell.hrl` will automatically make them available to riakshell and exclude them from extensions.

To add a function which appears to the user like:
```
riakshell (12)> frobulator bish bash bosh;
```

You implement a function with the following signature:
```
frobulator(#state{} = State, _Arg1, _Arg2, N) when is_integer(N) ->
    Result = "some string that is the result of the fn",
    {Result, State};
frobulator(S, _Arg1, Arg2, N) ->
    ErrMsg = io_lib:format("The third parameter '~p' should be an integer",
        [N]),
   {ErrMsg, S#state{cmd_error = true}}.
```

Your function may modify the state record if appropriate. All the shell functions are implemented as extensions. Have a look at the different EXT files for examples.

This example shows you how to handle errors - return an error message and a state record with the cmd_error flag set to 'true'.

To be a good citizen you should add a clause to the help function like:
```
-help(frobulator) ->
    "This is how you use my function";
```
The second param is the arity *as it appears in the riakshell*.

As a convenience to the developer there is a module called:
```
debug_EXT.erl
```

This implements a function which reloads and reregisters all extensions:
```
riakshell (11)>load;
```
and can hot-load changes into the shell (it won't work on first-creation of a new EXT module, only on reloading). The only EXT that debug doesn't load is `debug_EXT` so please do not add functions to it.

The riakshell suppresses error messages that would otherwise be written to the console (for instance if the remote riak node goes down the protocol buffer connection is torn down). This makes debugging painful. You can stop this behaviour by starting riakshell in the debug mode by starting it from the shell with the `-d` flag:
```
cd ~/riakshell/bin
./riakshell -d
```

Architecture Notes
------------------

This shell has a much simpler architecture than conventional Erlang/LFE/Elixir REPLS.

Those shells go through some complex process spawning stuff to ensure that the stacktrace on evaluation is clean. That is not relevant for riakshell.

The reason for implementing the shell as 3-mode is because it keeps the different command syntaxes seperate, but thoughts/comments on that are welcome.

Although there are no -spec() annotations this is actually an example of spec-first development.
