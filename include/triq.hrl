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

-include("triq_syntax.hrl").

%%
%% import property functions
%%
-import(triq, [fails/1, check/1]).

%%
%% import domain functions (a.k.a. generators)
%%
-import(?DOMAIN_MODULE, [list/1, tuple/1, int/0, real/0, elements/1, any/0, atom/0,
                         choose/2, bool/0, char/0, oneof/1, return/1, vector/2, binary/0, binary/1,
			 unicode_binary/0, unicode_binary/1, unicode_char/0,
			 non_empty/1,
                         shrink_without_duplicates/1, resize/2, frequency/1]).


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
