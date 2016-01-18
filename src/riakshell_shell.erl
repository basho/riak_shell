-module(riakshell_shell).

-export([
         start/0
        ]).

-include("riakshell.hrl").

start() -> spawn(fun main/0).

main() ->
    State = startup(),
    process_flag(trap_exit, true),
    io:format("riakshell ~p, write quit();' to exit or 'help();' for help~n", [State#state.version]),
    loop(State).

loop(State) ->
    {Prompt, NewState} = make_prompt(State),
    Cmd = io:get_line(standard_io, Prompt),
    NewState2 = handle_cmd(Cmd, NewState),
    loop(NewState2).

handle_cmd(Cmd, State) ->
    {ok, Toks, _} = cmdline_lexer:string(Cmd),
    case is_complete(Toks, State) of
        {true, Toks2, State2} -> run_cmd(Toks2, State2);
        {false, State2}       -> State2
    end.        

is_complete(Toks, S) ->
    case lists:member({semicolon, ";"}, Toks) of
        true  -> Toks2 = S#state.partial_cmd ++ Toks,
                 NewState = S#state{partial_cmd = []},
                 {true, Toks2, NewState};
        false -> NewP = S#state.partial_cmd ++ Toks, 
                 {false, S#state{partial_cmd = NewP}}
    end.

run_cmd(Toks, State) ->
    case cmdline_parser:compile(Toks) of
        {ok, {{Fn, Arity}, Args}} -> 
            NewState = run_ext({{Fn, Arity}, Args}, State),
            add_cmd_to_history(Toks, NewState);        
        Other -> 
            io:format(user, "Other is ~p~n\r", [Other]),
            State
        end.

add_cmd_to_history(Toks, #state{history = Hs} = State) ->
    Cmd = lists:flatten([TokenChars || {_, TokenChars} <- Toks]),
    N = case Hs of
            []             -> 1;
            [{NH, _} | _T] -> NH + 1
        end,
    State#state{history = [{N, Cmd} | Hs]}.

%% help is a special function
run_ext({{help, 0}, []}, #state{extensions = E} = State) ->
    {Fns, _} = lists:unzip(E),
    io:format(user, "The following functions are available~n\r" ++
                  "(the number of arguments is given)~nr", []),
    riakshell_util:printkvs(Fns),
    io:format(user, "You can get more help by calling help with the~n\r" ++
                  "function name and arguments like 'help(quit, 0);'~n\r", []),
    State;
%% the help funs are not passed the state and can't change it
run_ext({{help, 2}, [Fn, Arity]}, #state{extensions = E} = State) ->
    case lists:keysearch({Fn, Arity}, 1, E) of
        {value, {{_, _}, Mod}} ->
            try 
                erlang:apply(Mod, help, [Fn, Arity])
            catch _:_ ->
                    io:format(user, "There is no help for ~p~n\r", 
                              [{Fn, Arity}])
            end;
        false ->
            io:format(user, "There is no help for ~p~n\r", [{Fn, Arity}])
    end,
    State;    
run_ext({Ext, Args}, #state{extensions = E} = State) ->
    case lists:keysearch(Ext, 1, E) of
        {value, {{Fn, _}, Mod}} ->
            erlang:apply(Mod, Fn, [State] ++ Args);
        false ->
            io:format(user, "Extension ~p not implemented~n\r", [Ext]),
            State
    end.

make_prompt(S = #state{count       = SQLN,
                       partial_cmd = []}) ->
    Prompt = "riak (" ++ integer_to_list(SQLN) ++ ")>",
    {Prompt, S#state{count = SQLN + 1}};
make_prompt(S) ->
    Prompt = "->",
    {Prompt, S}.

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
                                     {Fn, Arity} =/= {help, 2},
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
