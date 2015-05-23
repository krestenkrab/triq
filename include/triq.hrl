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

-define(LAZY(X), DELAY(X)).
-define(DELAY(X), fun()->X end).
-define(FORCE(X), (X)() ).
-define(DOMAIN_MODULE, triq_dom).

%% properties
-define(FORALL(X,Gen,Property),
        {'prop:forall', Gen, ??X, fun(X)-> begin Property end end, ??Property}).
-define(IMPLIES(Pre, Property),
        {'prop:implies', Pre, ??Pre, ?DELAY( Property ), ??Property}).
-define(WHENFAIL(Action, Property),
        {'prop:whenfail', ?DELAY(Action), ?DELAY(Property), ??Property}).
-define(TRAPEXIT(Property),
        {'prop:trapexit', ?DELAY(Property), ??Property}).
-define(TIMEOUT(Limit,Property),
        {'prop:timeout', Limit, ?DELAY(Property), ??Property}).
-define(SETUP(SetupFun,Property),
        {'prop:setup', SetupFun, ?DELAY(Property), ??Property}).

%%
%% import property functions
%%
-import(triq,
        [fails/1,
         check/1]).


%% value domains

%% LET is also defined by eunit; what to do?
-ifndef(LET).
-define(LET(X,Gen1,Gen2),
        ?DOMAIN_MODULE:bind(Gen1, fun(X)->Gen2 end)).
-endif.
-define(LETSHRINK(X,Gen1,Gen2),
        ?DOMAIN_MODULE:bindshrink(Gen1, fun(X)->Gen2 end)).

-define(SIZED(Size,Gen),
        ?DOMAIN_MODULE:sized(fun(Size) -> Gen end)).

-define(SUCHTHAT(X,G,P),
        ?DOMAIN_MODULE:suchthat(G, fun(X) -> P end)).


%%
%% import domain functions (a.k.a. generators)
%%
-include("triq_dom.hrl").
-import(?DOMAIN_MODULE, ?TRIQ_DOM_EXPORTS).
-import(?DOMAIN_MODULE, ?TRIQ_DOM_UNICODE_EXPORTS).
-import(?DOMAIN_MODULE, ?TRIQ_DOM_GENERATOR_EXPORTS).

%%
%% Enabling this (the default) does two things (similar to eunit).
%%
%% - Make all prop_* function be exported, and
%%
%% - Define this exported function:
%%
%%     ?MODULE:check() -> triq:module(?MODULE).
%%
-ifndef(TRIQ_NOAUTO).
-compile({parse_transform, triq_autoexport}).
-endif.
