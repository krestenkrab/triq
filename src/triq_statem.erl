%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2011-2013 by Trifork
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

%% TODO: Share code with triq_statem_fsm. Until then make sure you
%% apply any changes to triq_statem_fsm as well.

-module(triq_statem).

-define(TRIES,100).
-define(FORALL(X,Gen,Property),
        {'prop:forall', Gen, ??X, fun(X)-> begin Property end end, ??Property}).

-import(triq_dom,
        [pick/2,
         domain/3]).
-import(triq_expr,
        [eval/2,
         free_vars/1]).
-export([commands/1,
         commands/2,
         run_commands/2,
         run_commands/3,
         state_after/2,
         prop_statem/1,
         command_names/1,
         zip/2]).


commands(Module) ->
    domain(commands,
           fun(_,Size) ->
                   gen_commands(Module,
                                Module:initial_state(),
                                [],[],[],
                                Size, Size, ?TRIES)
           end,
           undefined).

commands(Module, InitialState) ->
    domain(commands,
           fun(_,Size) ->
                   gen_commands(Module,
                                InitialState,
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
gen_commands(Module,State,States,Domains,[],Size,Count,Tries) ->
    gen_commands(Module,State,States,Domains,[{init,State}],Size,Count,Tries);
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
            gen_commands(Module,State,States,Domains,Commands,Size,Count,
                         Tries-1)
    end.

shrinkable_commands(Module,SymbolicStates,Domains) ->
    domain
      (shrinking_commands,
       undefined,
       fun(Dom,Commands) ->
               commands_shrink(Module,SymbolicStates,Domains,Dom,Commands,
                               ?TRIES)
       end).

-define(MIN(A,B), (if (A<B) -> A; (B<A) -> B; (A==B) -> A end)).

commands_shrink(_,_,_,_,[],_) -> {[],[]};
commands_shrink(_,_,_,Dom,Commands,0) -> {Dom,Commands};
commands_shrink(Module,SymbolicStates,Domains, Dom, Commands,Tries) ->
    Len = length(Commands),
    true = (Len > 0),

    %% choose a segment of commands to delete...
    RemIdx = rand:uniform(Len),
    RemLen = if RemIdx==Len ->
                     0;
                true ->
                     rand:uniform(?MIN(5, Len-RemIdx))
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
validate(Mod, State, _States, Commands) ->
    validate2(Mod, State, Commands, []).

validate2(_Mod,_State,[], _KnownVars) ->
    true;
validate2(Module,_State,[{init,S}|Commands], _KnownVars) ->
    validate2(Module,S,Commands, []);
validate2(Module,State,[{set,Var,Call}|Commands], KnownVars) ->
    FreeVars = free_vars(Call),
    UnknownVars = FreeVars -- KnownVars,
    (UnknownVars == [])
        andalso (Module:precondition(State,Call)==true)
        andalso begin
                    NextState = Module:next_state(State, Var, Call),
                    validate2(Module,NextState, Commands, [Var|KnownVars])
                end.

run_commands(Module,Commands) ->
    run_commands(Module,Commands,[]).

run_commands(Module,Commands,Env) ->
    do_run_command(Commands,
                   Env,
                   Module,
                   [],
                   Module:initial_state()).

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

            %% Same as eval(Env, SymCall), but we need to log in History.
            Res = apply(M2,F2,A2),

            SubstCall = {call, M2,F2,A2},
            History2 = [{SubstCall,Res}|History],

            case Module:postcondition(State,SubstCall,Res) of
                true ->
                    Env2 = [{V,Res}|proplists:delete(V,Env)],
                    State2 = Module:next_state(State,Var,SymCall),
                    do_run_command(Rest, Env2, Module, History2, State2);

                Other ->
                    {History, eval(Env,State), {postcondition, Other}}
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
%%          {_,_,ok} = run_commands(Module, Commands),
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

%%
%% simplify command names
%%
command_names(Calls) ->
    [{M,F,length(Args)} || {call, M, F, Args} <- Calls].

zip(X, Y) ->
    zip(X, Y, []).

zip([], _, Acc) -> lists:reverse(Acc);
zip(_, [], Acc) -> lists:reverse(Acc);
zip([A|T1], [B|T2], Acc) ->
    zip(T1, T2, [{A,B}|Acc]).
