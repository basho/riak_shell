-module(riakshell_boot).

-export([
         start/0
        ]).

start() -> user_drv:start(['tty_sl -c -e', {riakshell_shell, start, []}]).
