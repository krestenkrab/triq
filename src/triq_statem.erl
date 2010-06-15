-module(triq_statem).

-define(TRIES,100).
-define(FORALL(X,Gen,Property),
	{'prop:forall', Gen, ??X, fun(X)-> begin Property end end, ??Property}).

-import(triq_dom, [eval/2, pick/2, domain/3]).
-export([commands/1, run_commands/2, run_commands/3, state_after/2, prop_statem/1]).


commands(Module) ->
    domain(commands, 
	   fun(_,Size) ->
		   gen_commands(Module,
				Module:initial_state(),
				[],[],[],
				Size, Size, ?TRIES)
	   end,
	   undefined).

gen_commands(Module,_,SymbolicStates,CallDoms,Commands,_,0,_) ->
    {shrinkable_commands(Module,
			 lists:reverse(SymbolicStates),
			 lists:reverse(CallDoms)), 
     lists:reverse(Commands)};

gen_commands(Module,State,_,_,_,_,_,0) ->
    erlang:error({cannot_generate,Module,State});

gen_commands(Module,State,States,Domains,Commands,Size,Count,Tries) ->
    
    CmdDom = Module:command(State), 
    {CD,C} = pick(CmdDom,Size),
    
    case Module:precondition(State, C) of
	true ->
	    Var = {var, Size-Count},
	    NextState = Module:next_state(State, Var, C),
	    Command = {set, Var, C},
	    gen_commands(Module,
			 NextState, 
			 [State|States],
			 [CD|Domains],
			 [Command|Commands],
			 Size, 
			 Count-1, ?TRIES);
	_ ->
	    %% try again, up to Tries times...
	    gen_commands(Module,State,States,Domains,Commands,Size,Count,Tries-1)
    end.

shrinkable_commands(Module,SymbolicStates,Domains) ->
    domain
      (shrinking_commands,
       undefined,
       fun(Dom,Commands) ->
	       commands_shrink(Module,SymbolicStates,Domains,Dom,Commands,?TRIES)
       end).

-define(MIN(A,B), (if (A<B) -> A; (B<A) -> B; (A==B) -> A end)).
			   
commands_shrink(_,_,_,_,[],_) -> {[],[]};
commands_shrink(_,_,_,Dom,Commands,0) -> {Dom,Commands};
commands_shrink(Module,SymbolicStates,Domains, Dom, Commands,Tries) ->
    Len = length(Commands),
    true = (Len > 0),

    %% choose a segment of commands to delete...
    RemIdx = random:uniform(Len),
    RemLen = if RemIdx==Len -> 
		     0;
		true -> 
		     random:uniform(?MIN(5, Len-RemIdx)) 
	     end,
    
    NewCommands = without(RemIdx,RemLen,Commands),
    NewStates = without(RemIdx,RemLen,SymbolicStates),
    NewDomains = without(RemIdx,RemLen,Domains),

    StartState =
	if RemIdx == 1 -> hd(SymbolicStates);
	   true -> Module:initial_state()
	end,

    case validate(Module,
		  StartState, 
		  NewStates,
		  NewCommands) of

	%% yay! removing that transition left us with a valid set of states
	true -> 
	    {shrinkable_commands(Module,NewStates,NewDomains), 
	     NewCommands};

	%% oops, removing transition at RemIdx didn't validate...
	_ ->
	    commands_shrink(Module,SymbolicStates,Domains,Dom,Commands,Tries-1)
    end.


%%
%% validate a shrunken command sequence
%%
validate(_Mod,_State,_States,[]) -> true;

validate(Module,_State,[_|States],[{init,S}|Commands]) ->
    validate(Module,S,States,Commands);

validate(Module,State,[_|States],[{set,Var,Call}|Commands]) ->
    case Module:precondition(State,Call) of
	true ->
	    case Module:next_state(State, Var, Call) of
		NextState -> 
		    validate(Module,NextState, States, Commands)
	    end;
	_ -> false
    end.

run_commands(Module,Commands) ->
    run_commands(Module,Commands,[]).

run_commands(Module,Commands,Env) ->

    do_run_command(Commands,
		   Env,
		   Module,
		   [],
		   eval(Env,Module:initial_state())).

do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] -> 
	    {History, State, ok};

	[{init,S}|Rest] ->
	    State2 = eval(Env, S),
	    do_run_command(Rest, Env, Module, History, State2);
	
	[{set, {var,V}=Var, {call,M,F,A}}|Rest] ->
	    M2=eval(Env,M), 
	    F2=eval(Env,F), 
	    A2=eval(Env,A),
	    
	    Res = apply(M2,F2,A2),

	    Call = {call, M2,F2,A2},
	    History2 = [{Call,Res}|History],
    
	    case Module:postcondition(State,Call,Res) of
		true ->
		    Env2 = [{V,Res}|proplists:delete(V,Env)],
		    State2 = Module:next_state(State,Var,Call),
		    do_run_command(Rest, Env2, Module, History2, State2);
			   
		Other ->
		    {History, State, {postcondition, Other}}
	    end
    end.


%%-----------------------------------------------------------------
%% @doc Evaluate command list, and return final state.
%%
%% Given a `Module' and `Commands', a value picked from the domain
%% `triq_statem:commands(Module)' 
%% @end
%%-----------------------------------------------------------------
state_after(Module,Commands) ->
    NextState = fun(S,V,C) -> Module:next_state(S,V,C) end,
    lists:foldl(fun({init,S}, _) ->
			S;
		   ({set,Var,Call},S) ->
			NextState(S,Var,Call)
		end,
		Module:initial_state(),
		Commands).

%%-----------------------------------------------------------------
%% @doc Boiler-plate property for testing state machines.
%% <pre>
%% prop_statem(Module) ->
%%     ?FORALL(Commands, commands(Module),
%%        begin
%% 	    {_,_,ok} = run_commands(Module, Commands),
%%          true
%%        end).
%% </pre>
%% @end
%%-----------------------------------------------------------------
prop_statem(Module) ->
    ?FORALL(Commands, commands(Module),
	    begin
		{_,_,ok} = run_commands(Module, Commands),
		true
	    end).


%%
%% utility to delete Count elements at RemIndex of List
%%
without(_, 0, List) -> List;
without(RemIdx, Count, List) ->
    without(RemIdx, Count-1, without(RemIdx, List)).
    

without(RemIdx,List) when is_list(List) ->
    {First,Rest} = lists:split(RemIdx-1,List),
    First ++ tl(Rest);

without(RemIdx,Tup) when is_tuple(Tup) ->
    list_to_tuple(without(RemIdx, tuple_to_list(Tup))).


    
    




