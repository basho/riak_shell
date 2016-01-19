-module(debug_EXT).

-export([
         help/2,
         load/1
        ]).

help(load, 0) ->
    "typing 'load();' reloads all the EXT modules after they have been compiled." ++
        "This only works after a module has been compiled and loaded the first time. " ++
        "So the first time you create a module you will need to stop and restart the shell.".

load(State) -> {"Modules relaoded.", riakshell_shell:register_extensions(State)}.
