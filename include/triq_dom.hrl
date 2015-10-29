%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright the Triq Contributors (c.f. AUTHORS)
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

-define(TRIQ_DOM_EXPORTS,
        [list/1,
         tuple/1,
         int/0,
         int/1,
         int/2,
         largeint/0,
         byte/0,
         real/0,
         sized/1,
         elements/1,
         any/0,
         atom/0,
         atom/1,
         choose/2,
         oneof/1,
         frequency/1,
         bool/0,
         char/0,
         return/1,
         vector/2,
         binary/1,
         binary/0,
         bitstring/0,
         bitstring/1,
         non_empty/1,
         resize/2,
         non_neg_integer/0,
         pos_integer/0]).

-define(TRIQ_DOM_UNICODE_EXPORTS,
        [unicode_char/0,
         unicode_string/0,
         unicode_string/1,
         unicode_binary/0,
         unicode_binary/1,
         unicode_binary/2,
         unicode_characters/0,
         unicode_characters/1]).

-define(TRIQ_DOM_GENERATOR_EXPORTS,
        [bind/2,
         bindshrink/2,
         suchthat/2,
         pick/2,
         shrink/2,
         sample/1,
         sampleshrink/1,
         seal/1,
         open/1,
         peek/1,
         domain/3,
         shrink_without_duplicates/1]).
