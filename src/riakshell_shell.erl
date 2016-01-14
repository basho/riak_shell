-module(riakshell_shell).

-export([
         start/0
        ]).

-include("riakshell.hrl").

start() -> spawn(fun main/0).

main() ->
    State = startup(),
    io:format("riakshell ~p, write quit();' to exit or 'help();' for help~n", [State#state.version]),
    loop(State).

loop(State) ->
    {Prompt, NewState} = make_prompt(State),
    case io:request({get_until, Prompt, cmdline_lexer, tokens, [1]}) of
        {ok, Toks, _} -> io:format("Other is ~p~n", [Toks]);
        Other         -> io:format("Error ~p~n", [Other])
    end,
    loop(NewState).


%% get_input(Prompt) ->
%%     Cmd = io:get_line(Prompt),
%%     Cmd2 = string:strip(Cmd, right, $\n),
%%     {ok, Toks, _} = cmdline_lexer:string(Cmd2),
%%     _O = case cmdline_parser:parse(Toks) of
%%              {ok, Output} -> Output;
%%              {error, Err} -> io:format("Err is ~p~n", [Err]),
%%                              lists:flatten(io_lib:format("Invalid command: ~p", [Cmd2]))
%%          end.

make_prompt(S = #state{count = SQLN}) ->
    Prompt = "riak (" ++ integer_to_list(SQLN) ++ ")>",
    {Prompt, S#state{count = SQLN + 1}}.

startup() ->
    State = try 
                load_config()
            catch
                _ ->
                    io:format("Invalid_config~n", []),
                    exit(invalid_config)
            end,
    register_extensions(State).

load_config() ->
    {ok, Config} = file:consult("../etc/riakshell.config"),
    #state{config = Config}.

%% read_config(Config, Key) when is_list(Config) andalso
%%                               is_atom(Key) ->
%%     {Key, V} = lists:keyfind(Key, 1, Config),
%%     V.

register_extensions(#state{} = S) ->
    ok = application:load(riakshell),
    {ok, Mods} = application:get_key(riakshell, modules),
    %% now load the modules
    [{module, X} = code:ensure_loaded(X) || X <- Mods],
    %% now going to register the extensions
    Extensions = [X || X <- Mods, is_extension(X)],
    register_e2(Extensions, S).

register_e2([], #state{extensions = E} = State) -> 
    validate_extensions(E),
    State;
register_e2([Mod | T], #state{extensions = E} = State) -> 
    %% a fun that appears in the shell like
    %% 'fishpaste(bleh, bloh, blah)'
    %% is implemented like this
    %% 'fishpaste(#state{} = S, Arg1, Arg2, Arg3) ->
    %% so reduce the arity by 1
    Fns = [{{Fn, Arity - 1}, Mod} || {Fn, Arity} <- Mod:module_info(exports),
                                     {Fn, Arity} =/= {help, 1},
                                     {Fn, Arity} =/= {module_info, 0},
                                     {Fn, Arity} =/= {module_info, 1},
                                     Arity =/= 0],
    register_e2(T, State#state{extensions = Fns ++ E}).

is_extension(Module) ->
    case lists:reverse(atom_to_list(Module)) of
        "TXE_" ++ _Rest -> true;
        _               -> false
    end.

validate_extensions(Extensions) ->
    v2(lists:sort(Extensions), []).

v2([], []) -> ok;
v2([], Acc) -> print_errors(Acc);
v2([{{Fn, Arity}, Mod1}, {{Fn, Arity}, Mod2} | T], Acc) ->
    v2(T, [{{Fn, Arity}, [Mod1, Mod2]} | Acc]);
v2([_H | T], Acc) ->
    v2(T, Acc).

print_errors([]) ->
    exit("Shell cannot start because of invalid extensions");
print_errors([{{Fn, Arity}, Mods} | T]) ->
    io:format("function ~p ~p is multiply defined in ~p~n", [Fn, Arity, Mods]),
    print_errors(T).
