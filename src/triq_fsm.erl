%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2010 by Trifork
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(triq_fsm).

-export([commands/1,
	 commands/2,
	 run_commands/2,
	 run_commands/3,
	 state_names/1]).
-import(triq_expr,
	[eval/2,
	 free_vars/1]).
-import(triq_dom,
	[oneof/1]).


%% FSM API

commands(Module) when is_atom(Module) ->
    StubModule = triq_fsm_stub:new(Module),
    triq_statem:commands(StubModule).

commands(Module, {InitialName, State}=Init) when is_atom(Module) ->
    StubModule = triq_fsm_stub:new(Module),
    triq_statem:commands(StubModule, {InitialName, State}).

run_commands(Module, Commands) ->
    run_commands(Module, Commands, []).

run_commands(Module,Commands,Env) ->
    StubModule = triq_fsm_stub:new(Module),
    do_run_command(Commands,
		   Env,
		   StubModule,
		   [],
		   StubModule:initial_state()).

do_run_command(Commands, Env, Module, History, State) ->
    case Commands of
	[] ->
	    {History, eval(Env,State), ok};

	[{init,S}|Rest] ->
	    do_run_command(Rest, Env, Module, History, S);

	[{set, {var,V}=Var, {call,M,F,A}=SymCall}|Rest] ->
	    M2=eval(Env,M),
	    F2=eval(Env,F),
	    A2=eval(Env,A),

	    Res = apply(M2,F2,A2), % Same as eval(Env, SymCall), but we need to log in History.

	    SubstCall = {call, M2,F2,A2},
            {Name, _} = State,
	    History2 = [{{Name,SubstCall},Res}|History],

	    case Module:postcondition(State,SubstCall,Res) of
		true ->
		    Env2 = [{V,Res}|proplists:delete(V,Env)],
		    State2 = Module:next_state(State,Var,SymCall),
		    do_run_command(Rest, Env2, Module, History2, State2);

		Other ->
		    {History, eval(Env,State), {postcondition, Other}}
	    end
    end.

state_names(H) ->
    [N || {{N,_},_} <- H].
