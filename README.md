riak shell
---------

A configurable, scriptable, and extendable shell for Riak.

It is designed to be built and deployed with Riak, and cannot
trivially be built independently of a Riak installation.

Goals
-----

The goals of riak shell are to have a single shell that can:
* run SQL commands
* run `riak-admin` commands (**not implemented yet**)
* be used as a developer/devops tool for managing Riak clusters

Current Capabilities
--------------------

To that end the shell has integral:
* logging
* log replay
* log regression replay
* config
* you can run replay and regression logs in batch mode
  - by specifying a file of commands to replay
  - see the README section: Command Line Flags
* specification of alternative configuration files at run time
* integration with riak

The shell is also trivially extendable for developer use.

Lacunae
-------

The shell is in the early stages. The following are well supported:
* extensible architecture
* logging
* replay/regression
* history
* configuration
* SQL mode
* batch mode
* management of connections to remote riak nodes
* shell management (including cookies)
* integration with riak_test so that replay logs can be run as simple regression tests

The following is not yet supported:
* riak-admin mode

Usage
-----------------------

You can read all about running, using, and configuring riak shell on our [docs site](http://docs.basho.com/riak/ts/latest/using/riakshell/)

`riak-shell` is installed in the same directory as `riak-admin`:
```
./riak-shell
```


Command Line Flags
------------------

There are 4 different configurations, two of which trigger batch mode.

By default riak shell swallows error messages, this makes it hard to develop new extensions. You can run it in debug mode as shown below:
```
./riak-shell -d
```

You can pass in a different config file than `../etc/riak_shell.config`:
```
./riak-shell -c ../path/to/my.config
```

You can run a riak shell replay log in batch mode for scripting:
```
./riak-shell -f ../path/to/my.log
```

You can run a riak shell regression log in batch mode for scripting:
```
./riak-shell -r ../path/to/my.log
```


Extending riak shell
-----------------------

riak shell uses a 'magic' architecture with convention.

Riak modules with names like:
```
mymodule_EXT.erl
```
are considered to be riak shell extension modules.

NOTE: the part before `_EXT` must be lower-case only.

All exported functions with an arity >= 1 are automaticaly exposed in riak shell, with some exceptions.

Exported functions with the following names will be silently ignored:
* `module_info/0`
* `module_info/1`
* `help/1`
* `'riak-admin'/N`

Functions that share a name with the first keyword of supported SQL statements will likewise be ignored:
* `sql/N`
* `create/N`
* `delete/N`
* `describe/N`
* `explain/N`
* `insert/N`
* `select/N`
* `show/N`

As additional SQL statements are supported adding them to the macro `IMPLEMENTED_SQL_STATEMENTS` in `riak_shell.hrl` will automatically make them available to riak-shell and exclude them from extensions.

To add a function which appears to the user like:
```
riak-shell (12)> frobulator bish bash bosh;
```

You implement a function with the following signature:
```
frobulator(#command{} = Cmd, #state{} = State, _Arg1, _Arg2, N) when is_integer(N) ->
    Result = "some string that is the result of the fn",
    {Cmd#command{response = Result}, State};
frobulator(Cmd, S, _Arg1, Arg2, N) ->
    ErrMsg = io_lib:format("The third parameter '~p' should be an integer",
        [N]),
   {Cmd#command{response = ErrMsg, cmd_error = true}, S}.
```

Your function may modify the state and command records if appropriate. All the shell functions are implemented as extensions. Have a look at the different EXT files for examples.

This example shows you how to handle errors - return an error message and a state record with the cmd_error flag set to 'true'.

To be a good citizen you should add a clause to the help function like:
```
-help(frobulator) ->
    "This is how you use my function";
```

If you have a function with the same name that appears in 2 EXT modules riak shell will not start. It will not check if the arities match. You may have the same function with different arities in the same module - but there is only one help call.

As a convenience to the developer there is a module called:
```
debug_EXT.erl
```

This implements a function which reloads and reregisters all extensions:
```
riak-shell (11)>load;
```
and can hot-load changes into the shell (it won't work on first-creation of a new EXT module, only on reloading). The only EXT that debug doesn't load is `debug_EXT` so please do not add functions to it.

The riak shell suppresses error messages that would otherwise be written to the console (for instance if the remote riak node goes down the protocol buffer connection is torn down). This makes debugging painful. You can stop this behaviour by starting riak shell in the debug mode by starting it from the shell with the `-d` flag:
```
cd ~/riak_shell/bin
./riak-shell -d
```

Architecture Notes
------------------

This shell has a simpler architecture than conventional Erlang/LFE/Elixir REPLS.

Although there are no `-spec()` annotations this is actually an example of spec-first development.
