Riakshell
---------

A configurable, scriptable and extendable shell for riak

Goals
-----

The goals of riakshell are to have a single shell that can:
* run sql commands
* run riak-admin commands
* be used a developer/devops tool for managing riak clusters

To that end the shell has integral:
* logging
* log replay
* log regression replay
* config

It has three modes:
* riakshell - runs single erlang functions via an extensible architecture
* sql - executes sql commands against a remote riak node
* riak-admin - executes riak-admin commands against a remote riak node

It is intended that it will support:
* replay and regression in batch mode
  - by specifying a file of commands to replay
  - by piping in a command set
* specification of alternative configuration files at run time

The shell is also trivially extendable for developer use.

Lacunae
-------

The shell is in early stages. The following are well supported:
* extensible architecture
* logging
* replay/regression
* history
* configuration

The following are only partially supported the moment:
* sql mode
  - lexing/parsing is supported but nothing else
* riak-admin mode

The following are not yet implemented:
* riak-admin lexer/parser
* connection to remote riak nodes
* shell management (including cookies)
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
riakshell (1)> help();
```

The current state is:
```
riakshell (1)>help();
The following functions are available
(the number of arguments is given)

Extension 'debug' provides:
- load: 0

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
-  riak_admin: 0
-   riakshell: 0
- show_config: 0
-         sql: 0

You can get more help by calling help with the
function name and arguments like 'help(quit, 0);'
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

Extending The Riakshell
-----------------------

Riakshell using a 'magic' architecture with convention.

Riak modules with names like:
```
mymodule_EXT.erl
```
are considered to be riakshell extension modules.

All exported functions with an arity >= 1 are automaticaly exposed in riakshell mode.

To add a function which appears to the user like:
```
riakshell (12)> frobulator(bish, bash, bosh);
```

You implement a function with the following signature:
```
frobulator(#state{} = State, _Arg1, _Arg2, _Arg3) ->
    Result = "some string that is the result of the fn",
    {Result, State}.
```

Your function may modify the state record if appropriate. All the shell functions are implemented as extensions. Have a look at the different EXT files for examples.

To be a good citizen you should add a clause to the help function like:
```
-help(frobulator, 3) ->
    "This is how you use my function";
```
The second param is the arity *as it appears in the riakshell*

As a convenience to the developer there is a module called:
```
debug_EXT.erl
```

This implements a function which reloads and reregisters all extensions:
```
riakshell (11)>load();
```
and can hot-load changes into the shell (it won't work on first-creation of a new EXT module, only on reloading).

Architecture Notes
------------------

This shell has a much simpler architecture than conventional Erlang/LFE/Elixir REPLS.

Those shells go through some complex process spawning stuff to ensure that the stacktrace on evaluation is clean. That is not relevant for riakshell.

The reason for implementing the shell as 3-mode is because it keeps the different command syntaxes seperate, but thoughts/comments on that are welcome.

Although there are no -spec() annotations this is actually an example of spec-first development.
