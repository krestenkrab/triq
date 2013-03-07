%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2013 Tuncer Ayaz
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
%%
%% Trivial gen_fsm based on example from gen_fsm Users' Guide
%% http://www.erlang.org/doc/design_principles/fsm.html
%%

-module(lock_fsm).
-behaviour(gen_fsm).

-export([start/1,
         stop/0,
         init/1,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         code_change/4,
         terminate/3]).

-export([button/1,
         locked/2,
         unlocked/2]).

-export([initial_state/0,
         initial_state_data/0,
         precondition/4,
         postcondition/5,
         next_state_data/5]).
-export([locked/1,
         unlocked/1]).

-include("triq.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 1000).
-define(CODE, [1, 2, 3, 4]).

-type digit() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9.
-type code()  :: [digit()].

-record(state_data, {sofar :: code(),
                     code  :: code()}).

%%=====================================================================
%% API
%%=====================================================================

start(Code) ->
    gen_fsm:start({local, ?MODULE}, ?MODULE, lists:reverse(Code), []).

stop() ->
    gen_fsm:send_all_state_event(?MODULE, stop).

button(Digit) ->
    gen_fsm:send_event(?MODULE, {button, Digit}).

%%=====================================================================
%% gen_fsm callbacks
%%=====================================================================

init(Code) ->
    {ok, locked, #state_data{sofar=[], code=Code}}.

locked({button, Digit}, #state_data{sofar=SoFar, code=Code}=StateData) ->
    case [Digit|SoFar] of
        Code ->
            {next_state, unlocked, StateData#state_data{sofar=[]}, ?TIMEOUT};
        Incomplete when length(Incomplete) < length(Code) ->
            {next_state, locked, StateData#state_data{sofar=Incomplete}};
        _Wrong ->
            {next_state, locked, StateData#state_data{sofar=[]}}
    end.

unlocked(timeout, State) ->
    {next_state, locked, State};
unlocked({button, _}, State) ->
    {next_state, locked, State, ?TIMEOUT}.

handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info(_Info, _StateName, _StateData) ->
    ok.

terminate(normal, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%=====================================================================
%% Triq fsm callbacks/specification
%%=====================================================================

locked(_State) ->
    [{unlocked, {call, ?MODULE, button, [digit()]}}].

unlocked(_State) ->
    [{locked, {call, ?MODULE, button, [digit()]}}].

digit() ->
    choose(0,9).

initial_state() ->
    locked.

initial_state_data() ->
    #state_data{}.

precondition(_From, _Target, _StateData, {call,_,_,_}) ->
    true.

postcondition(_From, _Target, _StateData, {call,_,_,_}, Result) ->
    Result =:= ok.

next_state_data(_From, _Target, StateData, _Result, {call,_,_,_}) ->
    StateData.

%%=====================================================================
%% Properties and unit tests
%%=====================================================================

prop_lock_fsm() ->
    ?FORALL(
       Cmds, triq_fsm:commands(?MODULE),
       begin
           {ok, _} = start(?CODE),
           {History, State, Result} = triq_fsm:run_commands(?MODULE, Cmds),
           ok = stop(),
           ?WHENFAIL(io:format(user,
                               "History: ~p~n"
                               "State: ~p~n"
                               "Result: ~p~n",
                               [History, State, Result]),
                     Result =:= ok)
       end).

lock_fsm_test_() ->
    {timeout, 10,
     fun() ->
             ?assert(triq:module(?MODULE))
     end}.
