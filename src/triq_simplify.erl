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


-module(triq_simplify).

-include("triq_internal.hrl").

-export([simplify_value/2, simplify_internal/2]).
-import(triq_domain, [generate/2, component_domain/2]).

%%
%% public API looks like this
%%
simplify_value(Dom=#?DOM{simplify=SFun}, Val) ->
    SFun(Dom,Val);

simplify_value(Dom,Val) ->
    simplify_internal(Dom,Val).


%%
%% when the domain is a tuple...
%%
simplify_tuple({},{},_) ->
    {};

simplify_tuple(TupDom,Tup,0) when is_tuple(TupDom), is_tuple(Tup) ->
    Tup;

simplify_tuple(TupDom,Tup,NAttempts) when is_tuple(TupDom), is_tuple(Tup) ->
    case simplify_member(Tup, TupDom, random:uniform(len(Tup)+1)) of
	Tup -> simplify_tuple(TupDom, Tup, NAttempts-1);
	NewTup -> NewTup
    end.

%%
%% Simplify a list by simplifying one of the elements
%%
simplify_list(_ListDom,[],_) ->
    [];

simplify_list(_ListDom,List,0) ->
    List;

simplify_list(ListDom,List,NAttempts) when is_list(List) ->
    case simplify_member(List, ListDom, random:uniform(len(List)+1)) of
	List -> simplify_list(ListDom, List, NAttempts-1);
	NewList -> NewList
    end.


%%
%% the meat of the shrinking function
%%
simplify_internal(_, []) -> 
    [];

simplify_internal(_, {}) -> 
    {};

simplify_internal(_, '') -> 
    '';

simplify_internal(_, 0) -> 
    0;

simplify_internal(_, 0.0) -> 
    0.0;

simplify_internal(#?DOM{kind=any}=Dom, Val) when is_tuple(Val) ->
    list_to_tuple(simplify_internal(Dom,tuple_to_list(Val)));

simplify_internal(#?DOM{kind=any}=Dom, Val) when is_atom(Val) ->
    list_to_atom(simplify_internal(Dom,atom_to_list(Val)));

simplify_internal(#?DOM{kind=atom}=Dom, Val) when is_atom(Val) ->
    list_to_atom(simplify_internal(Dom,atom_to_list(Val)));

simplify_internal(#?DOM{kind={vector,_,_}}=Dom, Val) ->
    simplify_list(Dom,Val,100);

simplify_internal(TupDom,Tup) when is_tuple(TupDom), is_tuple(Tup) ->
    %% try to simplify it 10 times...
    simplify_tuple(TupDom, Tup, 10);

simplify_internal(ListDom,List) when is_list(ListDom), is_list(List) ->
    %% try to simplify it 10 times...
    simplify_list(ListDom, List, 10);


%%
%% aggregates (tuple or list) are simplified either by 
%% simplifying one of the elements, or ... by removing
%% one of the elements
%%
simplify_internal(AggrDom,Aggr) when is_list(Aggr); is_tuple(Aggr)->
    simplify_dom_list(AggrDom,Aggr,100);

simplify_internal(_,0.0) -> 
    0.0;
simplify_internal(_NumDom,Num) when is_float(Num) ->
    Num/2;


simplify_internal(_,0) -> 
    0;
simplify_internal(_NumDom,Num) when is_integer(Num), Num > 0 ->
    Num-1;
simplify_internal(_NumDom,Num) when is_integer(Num), Num < 0 ->
    Num+1;


simplify_internal(Any,Any) -> Any;


simplify_internal(_,Any) ->
    if 
	is_integer(Any) ->
	    erlang:throw({int, Any});
	is_float(Any) ->
	    erlang:throw({float, Any});
	is_atom(Any) ->
	    erlang:throw({atom, Any})
    end.


simplify_dom_list(_ListDom,List,0) ->
    List;

simplify_dom_list(AggrDom,Aggr,N) ->
    NewAggr = 
	case random:uniform(2) of
	    1 -> remove_any(Aggr);
	    2 -> simplify_member(Aggr, AggrDom, random:uniform(len(Aggr)+1))
	end,

    % io:format("simplified ~p: ~p -> ~p~n", [N,Aggr,NewAggr]),

    %% if the above did not change anything; 
    case NewAggr of 
	Aggr -> simplify_dom_list(AggrDom,Aggr,N-1);
	NewAggr -> NewAggr
    end    .



%% remove any element of a list
remove_any(List) when is_list(List) ->
    Len = length(List),

    %%
    %% remove element at idx RemIdx
    %%
    RemIdx = random:uniform(Len),    
    case RemIdx of
	1 -> 
	    lists:sublist(List,2,Len-1);
	Len -> 
	    lists:sublist(List,1,Len-1);
	_ -> 
	    lists:sublist(List,RemIdx-1) ++ lists:sublist(List,RemIdx+1,Len)
    end;
remove_any(Tup) when is_tuple(Tup) ->
    list_to_tuple(remove_any(tuple_to_list(Tup))).

    
%% simplify HowMany element(s) of the list
simplify_member(Any, _Dom, 0) -> Any;
simplify_member(List, ListDom, HowMany) when is_list(List), not is_atom(List) ->
    Len = length(List),

    %%
    %% replace element at RemIdx with simplified one
    %%
    RemIdx = random:uniform(Len),    
    Elm = lists:nth(RemIdx, List),
    ElmDom = component_domain(RemIdx, ListDom),
    NextList = case RemIdx of
	1 -> 
	    [simplify_value(ElmDom,Elm)] ++ lists:sublist(List,2,Len-1);
	Len -> 
	    lists:sublist(List,1,Len-1) ++ [simplify_value(ElmDom,Elm)];
	_ -> 
	    lists:sublist(List,RemIdx-1) ++ [simplify_value(ElmDom,Elm)] ++ lists:sublist(List,RemIdx+1,Len)
    end,

    simplify_member(NextList, ListDom,HowMany-1);

simplify_member(Tuple, TupleDom, HowMany) when is_tuple(Tuple) ->
    erlang:list_to_tuple(simplify_member(erlang:tuple_to_list(Tuple), TupleDom, HowMany));
    
simplify_member(Atom, AtomDom, HowMany) when is_atom(Atom) ->
    erlang:list_to_atom(simplify_member(erlang:atom_to_list(Atom), AtomDom, HowMany)).
    

len(T) when is_atom(T) -> length(atom_to_list(T));
len(T) when is_tuple(T) -> erlang:tuple_size(T);
len(T) when is_list(T) -> length(T)
.

