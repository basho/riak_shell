riak-shell
---------

A configurable, scriptable and extendable shell for riak.

It is designed to be built and deployed with Riak, and cannot
trivially be built independently of a Riak installation.

Goals
-----

The goals of riak-shell are to have a single shell that can:
* run SQL commands
* run `riak-admin` commands
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

Running/Getting Started
-----------------------

`riak-shell` is installed in the same directory as `riak-admin`:
```
./riak-shell
```

You get help on what is implemented in the riak-shell with the help command:
```
riak-shell (1)> help;
```

The current state is:
```
The following functions are available

Extension 'connection':
    connect, connection_prompt, ping, reconnect, show_connection, show_cookie
    show_nodes

Extension 'debug':
    load, observer

Extension 'history':
    clear_history, h, history, show_history

Extension 'log':
    date_log, log, logfile, regression_log, replay_log, show_log_status

Extension 'shell':
    q, quit, show_config

You can get more help by calling help with the
extension name and function name like 'help shell quit;'
```

Configuration
-------------

Configuration is in the file:
```
riak_shell.config
```

The following configuration items are available (`*` indicates the
default value):

```
logging                = on | off*
date_log               = on | off*
logfile                = defaults to "riak_shell/riak_shell.log" under the Riak log folder
cookie                 = the Erlang cookie used by the Riak cluster
show_connection_status = true | false* show the green tick or red cross in the command line
nodes                  = [ nodenames] a list of nodes to try and connect to on startup or 'reconnect;'
```

Command Line Flags
------------------

There are 4 different configurations, two of which trigger batch mode.

By default riak-shell swallows error messages, this makes it hard to develop new extensions. You can run it in debug mode as shown below:
```
./riak-shell -d
```

You can pass in a different config file than `../etc/riak_shell.config`:
```
./riak-shell -c ../path/to/my.config
```

You can run a riak-shell replay log in batch mode for scripting:
```
./riak-shell -f ../path/to/my.log
```

You can run a riak-shell regression log in batch mode for scripting:
```
./riak-shell -r ../path/to/my.log
```

Basic Usage
-----------

Show which node to which you are currently connected:
```
✅ riak-shell(5)>show_connection;
riak-shell is connected to: 'dev1@127.0.0.1' on port 10017
```

To connect to a node:
```
✅ riak-shell(2)>connect 'dev2@127.0.0.1';
"Trying to connect..."
✅ riak-shell(4)>show_connection;
riak-shell is connected to: 'dev2@127.0.0.1' on port 10027
```
**Note:** The node name must be an Erlang atom.

To show the current configuration tuple:
```
✅ riak-shell(23)>show_config;
The config is [{nodes,['dev1@127.0.0.1','dev2@127.0.0.1','dev3@127.0.0.1',
                       'dev4@127.0.0.1','dev5@127.0.0.1','dev6@127.0.0.1',
                       'dev7@127.0.0.1','dev8@127.0.0.1']},
               {show_connection_status,true},
               {cookie,riak},
               {logging,on},
               {logfile,"riak_shell.log"},
               {included_applications,[]}]
```

Toggle the connection status flag.  Unicode support in the terminal is
recommended when the flag is on.  It is **off** by default.
```
riak-shell(3)>connection_prompt on;
Connection Prompt turned on
✅ riak-shell(4)>connection_prompt off;
Connection Prompt turned off
```

To show the current status of each node in your config file:
```
✅ riak-shell(11)>ping;
'dev8@127.0.0.1': ❌ (disconnected)
'dev7@127.0.0.1': ❌ (disconnected)
'dev6@127.0.0.1': ❌ (disconnected)
'dev5@127.0.0.1': ❌ (disconnected)
'dev4@127.0.0.1': ❌ (disconnected)
'dev3@127.0.0.1': ❌ (disconnected)
'dev2@127.0.0.1': ✅ (connected)
'dev1@127.0.0.1': ✅ (connected)
```

**Note:** The `ping` command is not logged.
**Note:** The unicode connected/disconnected icons only appear if the connection_prompt is set to on - it is off by default.

To retry connecting to a node in your config file:
```
✅ riak-shell(12)>reconnect;
"Trying to reconnect..."
✅ riak-shell(15)>show_connection;
riak-shell is connected to: 'dev1@127.0.0.1' on port 10017
```

To find the current Erlang cookie:
```
✅ riak-shell(16)>show_cookie;
Cookie is riak [actual riak]
```

To show the connected nodes:
```
✅ riak-shell(15)>show_connection;
riak-shell is connected to: 'dev1@127.0.0.1' on port 10017
```

To start the Erlang `observer` debugger:
```
✅ riak-shell(25)>observer;
Observer started
```

To show the history and replay a command:
```
✅ riak-shell(6)>show_history;
The history contains:
- 1: show_connection;
- 2: ping;
- 3: reconnect;

✅ riak-shell(7)>h 1;
rerun (1)> show_connection;
riak-shell is connected to: 'dev1@127.0.0.1' on port 10017
```

To change the logfile and turn on logging:
```
✅ riak-shell(2)>logfile "mylogfile";
Log file changed to "mylogfile"

✅ riak-shell(3)>log on;
Logging turned on.
✅ riak-shell(4)>show_connection;
riak-shell is connected to: 'dev1@127.0.0.1' on port 10017
✅ riak-shell(5)>show_nodes;
The connected nodes are: ['dev1@127.0.0.1','dev2@127.0.0.1']
✅ riak-shell(6)>log off;
Logging turned off.
```
**Note:** The logfile name must be a string.

To replay a log:
```
✅ riak-shell(4)>replay_log "mylogfile.log";

Replaying "mylogfile.log"
replay (1)> show_connection;
riak-shell is connected to: 'dev1@127.0.0.1' on port 10017
replay (2)> show_nodes;
The connected nodes are: ['dev1@127.0.0.1','dev2@127.0.0.1']
```
**Note:** If no logfile name is supplied, the default logfile is
replayed.

To verify the results of commands in a log file:
```
File "mylogfile" does not exist.
✅ riak-shell(7)>regression_log "mylogfile.log";

Regression Testing "mylogfile.log"
No Regression Errors.
```

Or a failure:
```
✅ riak-shell(2)>regression_log "mylogfile.log";

Regression Testing "mylogfile.log"
Cmd "show_nodes; " (2) failed
Got:
- "The connected nodes are: ['dev1@127.0.0.1','dev2@127.0.0.1']"
Expected:
- "The connected nodes are: ['dev1@127.0.0.1','dev3@127.0.0.1']"
```

Regression logs can be integrated into riak_test trivially. Please see this test for an example:
https://github.com/basho/riak_test/blob/riak_ts-develop/tests/ts_cluster_riak_shell_regression_log.erl

To create a table and see its schema:
```
riak-shell(25)>CREATE TABLE GeoCheckin (myfamily varchar not null, myseries varchar not null, time  timestamp not null, weather  varchar not null, temperature double, PRIMARY KEY ((myfamily, myseries, quantum(time, 15, 'm')), myfamily, myseries, time));
✅ riak-shell(26)>describe GeoCheckin;
+-----------+---------+-------+-----------+---------+--------+----+
|  Column   |  Type   |Is Null|Primary Key|Local Key|Interval|Unit|
+-----------+---------+-------+-----------+---------+--------+----+
| myfamily  | varchar | false |     1     |    1    |        |    |
| myseries  | varchar | false |     2     |    2    |        |    |
|   time    |timestamp| false |     3     |    3    |   15   | m  |
|  weather  | varchar | false |           |         |        |    |
|temperature| double  | true  |           |         |        |    |
+-----------+---------+-------+-----------+---------+--------+----+
```

To query a table:
```
✅ riak-shell(27)>select time, weather, temperature from GeoCheckin where myfamily='family1' and myseries='seriesX' and time > 0 and time < 1000;
+----+----------------+---------------------------+
|time|    weather     |        temperature        |
+----+----------------+---------------------------+
| 1  |    z«êPò¹      |4.19111744258298777600e+18 |
| 2  |  ^OOgz^Blu7)   |6.07861409217513676800e+18 |
| 3  |      ÔÖã       |6.84034338181623808000e+17 |
| 4  |       ^G       |-5.55785206740398080000e+16|
| 5  |   ¸LËäà«d      |-3.62555783091625574400e+18|
| 6  |    ^AE^S¥      |1.11236574770119680000e+18 |
| 7  |    ïö?ï^Fv     |5.51455556936744140800e+18 |
| 8  | ^FtFVÅë=+#^Y5  |2.44525777392835584000e+17 |
| 9  |ðÁÖ·©Ü^GV^^^DkU |6.90864738609726668800e+18 |
| 10 | QÝZa^QËfQ      |5.08590022245487001600e+18 |
+----+----------------+---------------------------+
```

To quit:
```
✅ riak-shell(29)>q;
Toodle Ooh!
```

**N.B. As of Riak TS 1.2, there is no way to write data via `riak-shell`**.

Extending The riak-shell
-----------------------

riak-shell uses a 'magic' architecture with convention.

Riak modules with names like:
```
mymodule_EXT.erl
```
are considered to be riak-shell extension modules.

All exported functions with an arity >= 1 are automaticaly exposed in riak-shell, with some exceptions.

Exported functions with the following names will be silently ignored:
* `module_info/0`
* `module_info/1`
* `help/1`
* `'riak-admin'/N`

Functions that share a name with the first keyword of supported SQL statements will likewise be ignored:
* `create/N`
* `describe/N`
* `insert/N`
* `select/N`

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

If you have a function with the same name that appears in 2 EXT modules riak-shell will not start. It will not check if the arities match. You may have the same function with different arities in the same module - but there is only one help call.

As a convenience to the developer there is a module called:
```
debug_EXT.erl
```

This implements a function which reloads and reregisters all extensions:
```
riak-shell (11)>load;
```
and can hot-load changes into the shell (it won't work on first-creation of a new EXT module, only on reloading). The only EXT that debug doesn't load is `debug_EXT` so please do not add functions to it.

The riak-shell suppresses error messages that would otherwise be written to the console (for instance if the remote riak node goes down the protocol buffer connection is torn down). This makes debugging painful. You can stop this behaviour by starting riak-shell in the debug mode by starting it from the shell with the `-d` flag:
```
cd ~/riak_shell/bin
./riak-shell -d
```

Architecture Notes
------------------

This shell has a simpler architecture than conventional Erlang/LFE/Elixir REPLS.

Although there are no `-spec()` annotations this is actually an example of spec-first development.
