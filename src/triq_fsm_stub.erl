%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
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
-module(triq_fsm_stub, [Module]).

-export([command/1,
         initial_state/0,
         next_state/3,
         postcondition/3,
         precondition/2]).
-import(triq_dom,
        [oneof/1]).

%%
%% An instance of this module implements the "statem" API, but
%% delegates calls to an underlying implementation of the "fsm" API.
%%

precondition({FromName, StateData}, Call) ->
    case find_next_state(Module, FromName, StateData, Call) of
        [ToName] ->
            Module:precondition(FromName, ToName, StateData, Call);
        _ ->
            false
    end.

postcondition({ToName, StateData}, Call, Result) ->
    Module:postcondition(undefined, ToName, StateData, Call, Result).

next_state({FromName, StateData}, Result, Call) ->
    case find_next_state(Module, FromName, StateData, Call) of
        [ToName] ->
            ToStateData =
                Module:next_state_data(FromName,
                                       ToName,
                                       StateData,
                                       Result,
                                       Call),
            {ToName, ToStateData};
        [_|_] ->
            exit({multiple_choise, FromName, Call});
        [] ->
            exit({no_transition, FromName, Call})
    end.

command({FromName, StateData}) ->
    Calls = [ Call || {_ToName, Call} <- Module:FromName( StateData ) ],
    oneof( Calls ).

initial_state() ->
    {Name, _} = Module:initial_state(),
    { Name, Module:initial_state_data() }.

%% INTERNAL

find_next_state(Module, FromName, StateData, {call, M,F,Arg}=_Call) ->
    Candidates1 = Module:FromName(StateData),
    Candidates2 =
        lists:foldl(fun ({ToName, {call, M2, F2, ArgDom}}, Acc)
                          when M2 == M, F2 == F
                               ->
                            case length(ArgDom) == length(Arg) of
                                true when ToName == history ->
                                    ordsets:add_element(FromName, Acc);
                                true ->
                                    ordsets:add_element(ToName, Acc);
                                false ->
                                    Acc
                            end;
                        (_, Acc) ->
                            Acc
                    end,
                    ordsets:new(),
                    Candidates1),
    ordsets:to_list(Candidates2).
