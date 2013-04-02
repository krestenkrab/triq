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

-module(triq_expr).

-export([eval/1,
         eval/2,
         free_vars/1]).

%%-----------------------------------------------------------------------
%% @doc Evaluate `Body'.  Occurrences of `{call,M,F,A}'
%% is replaced by the result of calling `erlang:apply(M,F,A)', and
%% occurrences of `{var,Name}' in `Body' are not substituted.
%%
%% This is a plain function, not a compile_transform or anything like that,
%% so nested functions are not traversed in the substitution.  However, nested
%% occurrences of `{call,M,F,A}' are substituted as one would think:
%% depth first, left-to-right.
%%
%% @spec eval(Body::any()) -> any()
%% @equiv eval([],Body)
%% @end
%% -----------------------------------------------------------------------
eval(Term) ->
    eval([], Term).

%%-----------------------------------------------------------------------
%% @doc Evaluate `Body', replacing occurrences of `{call,M,F,A}' and `{var,N}'.
%% Occurrences of `{call,M,F,A}' is replaced by `erlang:apply(M,F,A)', and
%% `{var,Name}' is replaced by the value with key `Name' in `PropList'.
%%
%% Exceptions happening when calling `erlang:apply/3' are not caught.
%% If `Name' is unbound i.e., `Name' does not appear in `PropList' or if
%% `Name' is not an atom, `{var,Name}' is unchanged.
%%
%% This is a plain function, not a compile_transform or anything like that,
%% so nested functions are not traversed in the substitution.  However, nested
%% occurrences of `{call,M,F,A}' are substituted as one would think:
%% depth first, left-to-right.
%%
%% @spec eval(PropList::[{atom(),any()}], Body::any()) -> any()
%% @end
%%-----------------------------------------------------------------------
eval(PropList, [H|T]) ->
    [eval(PropList,H) | eval(PropList,T)];
eval(PropList, Tuple) when is_tuple(Tuple) ->
    case tuple_to_list(Tuple) of
        [call, Mod, Fun, Args] ->
            M = eval(PropList, Mod),
            F = eval(PropList, Fun),
            A = eval(PropList, Args),
            erlang:apply(M,F,A);

        [var, Name] when is_integer(Name) ->
            case proplists:lookup(Name, PropList) of
                none -> {var, Name};
                {Name, Value} -> Value
            end;

        List ->
            list_to_tuple(eval(PropList,List))
    end;

eval(_, Term) ->
    Term.

free_vars([H|T]) -> free_vars(H) ++ free_vars(T);
free_vars({var, Name}=Var) when is_integer(Name) -> [Var];
free_vars(Tuple) when is_tuple(Tuple) ->
    lists:concat(lists:map(fun free_vars/1, tuple_to_list(Tuple)));
free_vars(_Term) -> [].
