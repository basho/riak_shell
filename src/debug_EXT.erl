-module(debug_EXT).

-export([
         help/2,
         load/1
        ]).

help(load, 0) ->
    "typing 'load();' reloads all the EXT modules after they have been compiled." ++
        "This only works after a module has been compiled and loaded the first time. " ++
        "So the first time you create a module you will need to stop and restart the shell." ++
        "This is for developing extensions only. " ++
        "If you try and invoke this command via the history command it will crash the shell " ++
        "because you cannot reload a module while you are running it.".

load(State) -> {"Modules reloaded.", riakshell_shell:register_extensions(State)}.
