riak_shell
---------

A configurable, scriptable and extendable shell for riak.

Goals
-----

The goals of riak_shell are to have a single shell that can:
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

Running/Getting Started
-----------------------

```
cd ~/riak_shell/bin
./riak_shell
```

You get help on what is implemented in the riak_shell with the help command:
```
riak_shell (1)> help;
```

The current state is:
```
riak_shell(1)>help;
The following functions are available
(the number of arguments is given)

Extension 'connection' provides:
-           connect: 1
- connection_prompt: 1
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
extension name and function name like 'help shell quit;'
```

Configuration
-------------

Configuration is in the file:
```
~/riak_shell/etc/riak_shell.config
```

The following things can be configured:
```
logging                = on | off
date_log               = on | off
logfile                = "../some/dir/mylogfile.log" defaults to "../log/riak_shell.log"
cookie                 = any erlang atom - the underlying Erlang cookie riak_shell uses to connect
show_connection_status = true | false show the green tick or red cross in the command line
nodes                  = [ nodenames] a list of nodes to try and connect to on startup or 'reconnect;'
```

Command Line Flags
------------------

There are 4 different configurations, two of which trigger batch mode.

By default riak_shell swallows error messages, this makes it hard to develop new extentions. You can run it in debug mode as shown below:
``` 
./riak_shell -d
```

You can pass in a different config file than `../etc/riak_shell.config`:
```
./riak_shell -c ../path/to/my.config
```

You can run a riak_shell replay log for batch/scripting:
```
./riak_shell -f ../path/to/my.log
```

You can run a riak_shell regression log for batch/scripting:
```
./riak_shell -r ../path/to/my.log
```

Extending The riak_shell
-----------------------

riak_shell uses a 'magic' architecture with convention.

Riak modules with names like:
```
mymodule_EXT.erl
```
are considered to be riak_shell extension modules.

All exported functions with an arity >= 1 are automaticaly exposed in riak_shell mode, with come exceptions.

Exported functions with the following names will be silently ignored:
* `module_info/0`
* `module_info/1`
* `help/1`
* `'riak-admin'/N`

Functions that share a name with the first keyword of supported SQL statements will likewise be ignored:
* `create/N`
* `describe/N`
* `select/N`

As additional SQL statements are supported adding them to the macro `IMPLEMENTED_SQL_STATEMENTS` in `riak_shell.hrl` will automatically make them available to riak_shell and exclude them from extensions.

To add a function which appears to the user like:
```
riak_shell (12)> frobulator bish bash bosh;
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

If you have a function with the same name that appears in 2 EXT modules riak_shell will not start. It will not check if the arities match. You may have the same function with different arities in the same module - but there is only one help call.

As a convenience to the developer there is a module called:
```
debug_EXT.erl
```

This implements a function which reloads and reregisters all extensions:
```
riak_shell (11)>load;
```
and can hot-load changes into the shell (it won't work on first-creation of a new EXT module, only on reloading). The only EXT that debug doesn't load is `debug_EXT` so please do not add functions to it.

The riak_shell suppresses error messages that would otherwise be written to the console (for instance if the remote riak node goes down the protocol buffer connection is torn down). This makes debugging painful. You can stop this behaviour by starting riak_shell in the debug mode by starting it from the shell with the `-d` flag:
```
cd ~/riak_shell/bin
./riak_shell -d
```

Architecture Notes
------------------

This shell has a simpler architecture than conventional Erlang/LFE/Elixir REPLS.

Although there are no `-spec()` annotations this is actually an example of spec-first development.
