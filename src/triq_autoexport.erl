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
%% @private
%% @see triq
%% @doc Parse transform for automatic exporting of prop_ functions.

-module(triq_autoexport).

-define(DEFAULT_PROP_PREFIX, "prop_").

-export([parse_transform/2]).

-define(CHECK,check).

parse_transform(Forms, Options) ->
    PropPrefix = proplists:get_value(triq_prop_prefix, Options,
                                     ?DEFAULT_PROP_PREFIX),
    F = fun (Form, Set) ->
                t_form(Form, Set, PropPrefix)
        end,
    Exports = sets:to_list(lists:foldl(F, sets:new(), Forms)),
    t_rewrite(Forms, Exports).

t_form({function, _L, Name, 0, _Cs}, S, PropPrefix) ->
    N = atom_to_list(Name),
    case lists:prefix(PropPrefix, N) of
        true ->
            sets:add_element({Name, 0}, S);
        false ->
            S
    end;
t_form(_, S, _) ->
    S.

t_rewrite([{attribute,_,module,{Name,_Ps}}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
t_rewrite([{attribute,_,module,Name}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
t_rewrite([F | Fs], Exports) ->
    [F | t_rewrite(Fs, Exports)];
t_rewrite([], _Exports) ->
    [].    %% fail-safe, in case there is no module declaration

rewrite([{function,_,?CHECK,0,_}=F | Fs], As, Module, _GenQC) ->
    rewrite(Fs, [F | As], Module, false);
rewrite([F | Fs], As, Module, GenQC) ->
    rewrite(Fs, [F | As], Module, GenQC);
rewrite([], As, Module, GenQC) ->
    {if GenQC ->
             [{function,0,?CHECK,0,
               [{clause,0,[],[],
                 [{call,0,{remote,0,{atom,0,triq},{atom,0,check}},
                   [{atom,0,Module}]}]}]}
              | As];
        true ->
             As
     end,
     GenQC}.

module_decl(Name, M, Fs, Exports) ->
    Module = Name,
    {Fs1, GenQC} = rewrite(Fs, [], Module, true),
    Es = if GenQC -> [{?CHECK,0} | Exports];
            true -> Exports
         end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)].
