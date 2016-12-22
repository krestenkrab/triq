%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2010-2013 by Trifork
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

-module(triq_dom).

-include_lib("eunit/include/eunit.hrl").

%% the name of te special record for domains
-define(DOM,'@').

-define(DEFAULT_PROP_PREFIX, "prop_").

%% must correspond to the definitions in triq.hrl, but we don't want to
%% include that file.
-define(DELAY(X), fun()->X end).
-define(SIZED(Size,Gen),
        sized(fun(Size) -> Gen end)).

%% How many times we try pick a value in order to satisfy a ?SUCHTHAT property.
-define(SUCHTHAT_LOOPS,100).

%% how many times we try to shrink a value before we bail out
-define(SHRINK_LOOPS,100).

%% @type pick_fun(T). Picks members of the `domain(T)'.
%% Return pair of `{domain(T),T}'; the "output domain" is what will
%% be used for shrinking the value.
-type pick_fun(T) :: fun( (domain(T),integer()) ->
                                {domain(T),T} | no_return() ).

%% @type shrink_fun(T). Shrinks members of the `domain(T)'.
%% Return pair of `{domain(T),T}'; the "output domain" is what will
%% be used for further shrinking the value.
-type shrink_fun(T) :: fun( (domain(T),T) -> {domain(T),T} | no_return() ).
-type domrec(T) :: {?DOM,
                    atom() | tuple(),
                    pick_fun(T),
                    shrink_fun(T),
                    boolean()}.

-define(BOX,'@box').
-record(?BOX, {dom :: domain(T), value :: T}).

%%--------------------------------------
%% @doc A box(T) contains a value of type T, along with
%% information needed to shrink T.
%%
%% For example, not all integers shrink the same.  If the
%% integer was generated to produce only even numbers for
%% instance, then that knowledge needs to be kept with the
%% value.
%%
%% @end
%%--------------------------------------
-type box(T) :: #?BOX{dom:: domain(T), value :: T}.

%% @type domain(T). Domain of values of type T.
%%
-type domain(T) :: domrec(T) |  T.

%% @type uchar(). Valid unicode code point.
-type uchar() :: 0..16#D7FF | 16#E000..16#10FFFF.
-type uchars() :: unicode:unicode_characters().

-record(?DOM,
        {kind :: atom() | tuple(),
         pick = fun error_pick/2 :: pick_fun(T),
         shrink = fun error_shrink/2 :: shrink_fun(T),
         empty_ok = true :: boolean()
        }).


-record(list,  {elem}).
-record(tuple, {elem}).
-record(vector,{size, elem}).
-record(binary,{size}).
-record(atom,  {size}).
-record(oneof, {size, elems=[]}).
-record(resize,{size, dom}).
-record(bind,  {dom, body}).
-record(sized, {body}).
-record(suchthat,{dom,pred}).
-record(bound_domain,{dom1,val1,dom2,fun2,size}).
-record(choose,{min,max}).
-record(elements,{elems,size,picked=none}).
-record(seal,{dom,seed}).
-record(unicode_binary, {size, encoding = utf8}).


%% generators
-export([list/1,
         tuple/1,
         int/0,
         int/1,
         int/2,
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
         non_empty/1,
         resize/2,
         non_neg_integer/0,
         pos_integer/0]).

%% Unicode
-export([unicode_char/0,
         unicode_string/0,
         unicode_string/1,
         unicode_binary/0,
         unicode_binary/1,
         unicode_binary/2,
         unicode_characters/0,
         unicode_characters/1]).

%% using a generator
-export([bind/2,
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


%%
%% Default values for pic/shrink in ?DOM records
%%
error_pick(#?DOM{kind=Kind},_) -> erlang:error({pick,Kind}).
error_shrink(#?DOM{kind=Kind},_) -> erlang:error({shrink,Kind}).

%%
%% @doc The heart of the random structure generator; pick a value from the
%% domain.
%%   Returns a pair of `{domain(T), T}' where the first component describes
%%   the structure of the picked value.
%% @spec pick(domain(T), pos_integer()) -> {domain(T), T}
%%
-spec pick(domain(T), pos_integer()) -> {domain(T), T}.
pick(Dom=#?DOM{pick=PickFun}, SampleSize)
  when SampleSize > 0, is_integer(SampleSize) ->
    PickFun(Dom,SampleSize);
%%
%% A tuple is generated by generating each element
%%
pick({}=Empty, _) -> {Empty,Empty};
pick(T,SampleSize) when is_tuple(T), SampleSize > 0, is_integer(SampleSize) ->
    {DomList,List} = pick(tuple_to_list(T), SampleSize),
    {list_to_tuple(DomList), list_to_tuple(List)};
%%
%% For lists, we traverse down the list and generate each head
%%
pick([], _) -> {[],[]};
pick([H|T], SampleSize)
  when SampleSize > 0, is_integer(SampleSize) ->
    {HDom,HVal} = pick(H,SampleSize),
    {TDom,TVal} = pick(T,SampleSize),
    {[HDom|TDom], [HVal|TVal]};
%%
%% Lazy elements...
%%
pick(F,SampleSize) when erlang:is_function(F,0) ->
    pick(F(),SampleSize);
%%
%% Simple values that generate themselves
%%
pick(V,_) ->
    {V,V}.


pick_test() ->
    case pick(int(), 10) of
        {Dom, Val} when Val >= -5, Val =< 5 ->
            Dom = int()
    end.

pick_tuple_test() ->
    case pick({int(),int()}, 10) of
        {Dom, {Val1,Val2}} when is_integer(Val1), is_integer(Val2) ->
            Dom = {int(), int()}
    end.

pick_lazy_test() ->
    case pick(?DELAY(int()), 10) of
        {_, Val} when Val >= -5, Val =< 5 ->
            ok
    end.

pick_list_test() ->
    case pick([int(),int()], 10) of
        {Dom, [Val1,Val2]} when is_integer(Val1), is_integer(Val2) ->
            Dom = [int(), int()]
    end.

pick_pair_test() ->
    repeat( fun() ->
                    case pick([choose(0,10) | {choose(0,10)}], 10) of
                        {Dom, [Int1| {Int2}]=Val} when is_integer(Int1),
                                                       is_integer(Int2) ->
                            case shrink(Dom, Val) of
                                {_, [SInt1 | {SInt2}]}
                                  when SInt1<Int1; SInt2<Int2 -> ok;
                                {_, [_ | {_}]} when Int1==0;Int2==0 -> ok
                            end

                    end
            end,
            20).


shrink({Domain,Value}) ->
    shrink(Domain,Value).

%% @doc The shrinking step function used internally in Triq.
%%
%% Performs one single step of shrinking.  If unsuccessful,
%% i.e. value cound not be shrunk, the output is equal to the input.
%%
%% Takes a `Domain' and a `Value' from said domain, and shrinks
%% the value within the constraints of the domain.  The result is
%% a tuple of a (possibly smaller) output domain, and the
%% shrunken value.
%%
%% @spec shrink(Domain::domain(T),Value::T) -> {domain(T), T}
-spec shrink(domain(T),T) -> {domain(T), T}.
shrink(Domain=#?DOM{shrink=SFun}, Value) ->
    SFun(Domain,Value);
shrink(TupDom,Tup) when is_tuple(TupDom),
                        is_tuple(Tup),
                        tuple_size(TupDom) =:= tuple_size(Tup) ->
    shrink_tuple_samesize(TupDom, Tup, 10);
%%
%% well-formed lists are shrunk using this case.
%% the "length(X)>=0 tests if it is well-formed list"
%%
shrink(ListDom, List) when is_list(ListDom), is_list(List), length(List) >= 0 ->
    ?assert(length(ListDom) == length(List)),
    shrink_list_samesize(ListDom, List, length(List), 10);
%%
%% other non-well-formed lists [cons pairs] use this clause
%%
shrink([_|_]=ListDom, [_|_]=List) ->
    shrink_pair(ListDom,List, 10);
%% finally, if the generator is the value itself, it simplifies to itself
shrink(Any,Any) -> {Any,Any}.

%% @doc
%% Support functions for the generic shrinking
%% @private
%% @end
-spec shrink_pair([domain(H)|domain(T)], [H|T], non_neg_integer()) ->
                         {[domain(H)|domain(T)],[H|T]}.
shrink_pair(ListDom,List,0) ->
    {ListDom,List};
shrink_pair([HDom|TDom]=ListDom, [H|T]=List, NAttempts) ->
    %% choose if we shrink the head or the tail
    ShrinkHead = (rand:uniform(2) =:= 1),
    ShrinkTail = (rand:uniform(2) =:= 1),

    %% then do it
    case

        %% shrink head and/or tail
        {
      case ShrinkHead of
          true -> shrink(HDom,H);
          false -> {HDom,H}
      end
      ,
      case ShrinkTail of
          true -> shrink(TDom,T);
          false -> {TDom,T}
      end
     }

    of

        %% it did not shrink
        {{_,H}, {_,T}} ->
            shrink_pair(ListDom,List,NAttempts-1);

        %% either H or T changed
        {{HSDom,HS}, {TSDom,TS}} ->
            {[HSDom|TSDom], [HS|TS]}
    end.

%%
%% We shrink tuples by turning it into a list and shrinking that...
%%
shrink_tuple_samesize(TupDom, Tup, NAttempts) ->
    ?assert(tuple_size(TupDom) =:= tuple_size(Tup)),
    ListDom = tuple_to_list(TupDom),
    List = tuple_to_list(Tup),
    {SDom,SList} = shrink_list_samesize(ListDom, List, tuple_size(Tup),
                                        NAttempts),
    { list_to_tuple(SDom), list_to_tuple(SList) }.


%%
%% Shrink a list by simplifying one or more of the elements.
%%
-spec shrink_list_samesize([domain(T)],[T],non_neg_integer(),
                           non_neg_integer()) -> {[domain(T)],[T]}.
shrink_list_samesize([],[],_,_) ->
    {[],[]};
shrink_list_samesize(ListDom,List,_,0) ->
    {ListDom,List};
shrink_list_samesize(ListDom,List,Length,NAttempts) when is_list(List) ->
    ?assert(length(ListDom) == length(List)),

    HowManyToShrink = shrink_members(Length),
    case shrink_list_members(ListDom, List, Length, HowManyToShrink) of

        %% it did not shrink, try again
        {_,List} ->
            shrink_list_samesize(ListDom, List, Length, NAttempts-1);

        %% else, we got a simpler list
        {_,_}=Result ->
            Result
    end.

%%
%% Given a list, shrink HowMany of it's elements,
%% but don't reduce the list length.
%%
-spec shrink_list_members([domain(T)],[T],non_neg_integer(),
                          non_neg_integer()) -> {[domain(T)],[T]}.
shrink_list_members(ListDom, List, _, 0) -> {ListDom,List};
shrink_list_members(ListDom, List, Len, HowMany)
  when is_list(List), is_list(ListDom) ->
    ?assert(Len == length(List)),

    %%
    %% replace element at RemIdx with simplified one
    %%
    RemIdx = rand:uniform(Len),
    Elm = lists:nth(RemIdx, List),
    ElmDom = lists:nth(RemIdx, ListDom),

    {NextDom,NextList} =
        case shrink(ElmDom,Elm) of
            {_,Elm} -> {ListDom,List};
            {SElmDom,SElm} ->
                Dom2  = lists:sublist(ListDom,RemIdx-1) ++ [SElmDom]
                    ++ lists:sublist(ListDom,RemIdx+1,Len),
                List2 = lists:sublist(List,RemIdx-1) ++ [SElm]
                    ++ lists:sublist(List,RemIdx+1,Len),
                {Dom2, List2}
        end,

    shrink_list_members(NextDom, NextList, Len, HowMany-1).


%%-------------------------------------------------------------------
%%
%% Now, the specific domains
%%
%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% Returns the domain of lists of the argument.
%% For example, `list(int())' yields the domain of lists of integers.
%%
%% @spec list( domain(T) ) -> domain([T])
%% @end
%%--------------------------------------------------------------------
-spec list(domain(T)) -> domrec([T]).
list(ElemDom) ->
    #?DOM{kind=#list{elem=ElemDom}, pick=fun list_pick/2 }.

list_pick(#?DOM{kind=#list{elem=ElemDom},empty_ok=EmptyOK},
          SampleSize) ->
    OutLen = if EmptyOK =:= false ->
                     rand:uniform(SampleSize);
                EmptyOK =:= true ->
                     rand:uniform(SampleSize)-1
             end,

    %%
    %% TODO: if ElemDom is "simple" no need to build template
    %%

    %% generate template Domain and corresponding List
    {ListDom,List} =
        foldn(fun({Dom,T}) ->
                      {EDom,E} = pick(ElemDom,SampleSize),
                      {[EDom|Dom], [E|T]}
              end,
              {[],[]},
              OutLen),

    shrinkable_list(ListDom, List, OutLen, EmptyOK).


%% oops, if length==1 and EmptyOK=false; just return the fixed list
shrinkable_list(ListDom, List, 1, false) ->
    {ListDom, List};
shrinkable_list(_, [], 0, _) ->
    {[], []};
shrinkable_list(ListDom, List, Len, EmptyOK) ->
    ?assert(length(List) == length(ListDom)),
    ?assert(length(List) == Len),

    SDom = #?DOM{kind={shrinkable_list, ListDom, Len},
                 shrink=fun list_shrink/2, empty_ok=EmptyOK},
    {SDom,List}.

list_shrink(#?DOM{kind={shrinkable_list, ListDom, Len}, empty_ok=EmptyOK},
            List) ->
    ?assert(length(List) == Len),
    SmallerOK = ((EmptyOK and (Len>0)) or (Len>1)),

    case SmallerOK and (rand:uniform(5) == 1) of
        true ->
            shorter_list(ListDom,List,Len,EmptyOK);

        false ->
            case shrink(ListDom,List) of
                {_, List} when SmallerOK ->
                    shorter_list(ListDom,List,Len,EmptyOK);

                {ShrunkenListDom,ShrunkenList} ->
                    shrinkable_list(ShrunkenListDom, ShrunkenList, Len, EmptyOK)
            end
    end.

shorter_list(ListDom,List,Len,EmptyOK) ->
    case rand:uniform(3) of
        1 -> %% Remove one element.
            RemIdx = rand:uniform(Len),
            shrinkable_list(without(RemIdx, ListDom), without(RemIdx, List),
                            Len-1, EmptyOK);
        2 -> %% Remove or keep a random sublist.
            Idx1 = rand:uniform(Len),
            Idx2 = rand:uniform(Len),
            if Idx1 < Idx2 ->         % Remove the sublist [Idx1;Idx2]
                    shrinkable_list( without(Idx1,Idx2, ListDom),
                                     without(Idx1,Idx2, List),
                                     Len-(Idx2-Idx1), EmptyOK);
               true ->                % Remove all but the sublist [Idx2;Idx1]
                    NewLen = Idx1-Idx2+1,
                    shrinkable_list( lists:sublist(ListDom, Idx2, NewLen),
                                     lists:sublist(List   , Idx2, NewLen),
                                     NewLen, EmptyOK)
            end;
        3 -> %% Remove a random sublist.
            Zipped = lists:zip(ListDom, List),
            TrueTreshold  = rand:uniform(),
            FalseTreshold = rand:uniform(),
            %% This may happen to be the original list again.
            Pruned = markov_prune_list(Zipped, TrueTreshold, FalseTreshold,
                                       false),
            NewLen = length(Pruned),
            {ListDom2,List2} = lists:unzip(Pruned),
            shrinkable_list( ListDom2, List2, NewLen, EmptyOK)
    end.

markov_prune_list([], _,_,_) -> [];
markov_prune_list([H|T], TrueTreshold, FalseTreshold, Prev) ->
    Rnd = rand:uniform(),
    Threshold = if Prev -> TrueTreshold;
                   true -> FalseTreshold
                end,
    Include = Rnd > Threshold,
    NewTail = markov_prune_list(T, TrueTreshold, FalseTreshold, Include),
    if Include -> [H|NewTail];
       true    -> NewTail
    end.

%%
%% Generator for tuples
%%
%% @spec tuple(domain(ElemType::any())) -> domain(tuple(ElemType))
-spec tuple(domain(any())) -> domrec(tuple()).
tuple(ElemDom) ->
    #?DOM{kind=#tuple{elem=ElemDom}, pick=fun tuple_pick/2 }.

tuple_pick(#?DOM{kind=#tuple{elem=ElemDom},empty_ok=EmptyOK},
           SampleSize) ->

    OutLen = if EmptyOK =:= false ->
                     rand:uniform(SampleSize);
                EmptyOK =:= true ->
                     rand:uniform(SampleSize)-1
             end,

    %%
    %% TODO: if ElemDom is "simple" no need to build template
    %%

    %% generate template Domain and corresponding Tuple
    {ListDom,List} =
        foldn(fun({Dom,T}) ->
                      {EDom,E} = pick(ElemDom,SampleSize),
                      {[EDom|Dom], [E|T]}
              end,
              {[],[]},
              OutLen),

    shrinkable_tuple(list_to_tuple(ListDom), list_to_tuple(List), EmptyOK).


%% If length==1 and EmptyOK=false; just return the fixed tuple
shrinkable_tuple(TupleDom, Tuple, false) when tuple_size(Tuple) =:= 1 ->
    {TupleDom, Tuple};
shrinkable_tuple(TupleDom, Tuple, EmptyOK) ->
    ?assert(tuple_size(Tuple) == tuple_size(TupleDom)),

    SDom = #?DOM{kind={shrinkable_tuple, TupleDom},
                 shrink=fun tuple_shrink/2, empty_ok=EmptyOK},
    {SDom,Tuple}.

tuple_shrink(#?DOM{kind={shrinkable_tuple, TupleDom}, empty_ok=EmptyOK},
             Tuple) ->
    AllowSmaller = allow_smaller(tuple_size(Tuple), any, EmptyOK),
    case shrink(TupleDom,Tuple) of
        {_, Tuple} when AllowSmaller ->

            RemIdx = rand:uniform(tuple_size(Tuple)),
            shrinkable_tuple(without(RemIdx, TupleDom),
                             without(RemIdx, Tuple),
                             EmptyOK);

        Result -> Result
    end.


%% @doc The domain of integers.
%% @spec int() -> domain(integer())
-spec int() -> domrec(integer()).
int() ->
    #?DOM{kind=int,
          shrink=fun(Dom,Val) when Val>0 -> {Dom,Val-1};
                    (Dom,Val) when Val<0 -> {Dom,Val+1};
                    (Dom,0) -> {Dom,0}
                 end,
          pick=fun(Dom,SampleSize) ->
                       {Dom, rand:uniform(SampleSize) - (SampleSize div 2)}
               end
         }.

int(Max) ->
    int(0, Max).

int(Min, Max) ->
    Diff = Max - Min,
    #?DOM{kind=int,
          shrink=fun(Dom,Val) when Val>0 -> {Dom,Val-1};
                    (Dom,Val) when Val<0 -> {Dom,Val+1};
                    (Dom,0) -> {Dom,0}
                 end,
          pick=fun(Dom,_SampleSize) ->
                       {Dom, rand:uniform(Diff) + Min}
               end
         }.


-spec byte() -> domrec(integer()).
byte() ->
    int(0, 255).


%% @doc The domain of non-negative integers.
%% @spec non_neg_integer() -> domain(non_neg_integer())
-spec non_neg_integer() -> domrec(non_neg_integer()).
non_neg_integer() ->
    #?DOM{
        kind=int,
        shrink=fun(Dom,Val) when Val>0  -> {Dom,Val-1};
                  (Dom,0) -> {Dom,0}
               end,
        pick=fun(Dom,SampleSize) ->
                     {Dom, abs(rand:uniform(SampleSize)) + 0}
             end
       }.

%% @doc The domain of positive integers.
%% @spec pos_integer() -> domain(pos_integer())
-spec pos_integer() -> domrec(pos_integer()).
pos_integer() ->
    #?DOM{
        kind=int,
        shrink=fun(Dom,Val) when Val>1 -> {Dom,Val-1};
                  (Dom,1) -> {Dom,1}
               end,
        pick=fun(Dom,SampleSize) ->
                     {Dom, abs(rand:uniform(SampleSize)) + 1}
             end
       }.


%% @doc The domain of floats.
%% @spec real() -> domain(float())
-spec real() -> domrec(float()).
real() ->
    #?DOM{
        kind=real,
        pick=fun(Dom,SampleSize) ->
                     {Dom, (rand:uniform()*SampleSize) - (SampleSize / 2)}
             end,
        shrink=fun(Dom,Val) -> {Dom, Val/2.0} end
       }.


%% @doc The domain of booleans.  Shrinks to false.
%% @spec bool() -> domain( true | false )
bool() ->
    #?DOM{
        kind=boolean,
        pick=fun(Dom,_) -> {Dom, rand:uniform(2)==1} end,
        shrink=fun(_,_) -> {false, false} end
       }.


-spec char() -> domrec(32..126).
char() ->
    #?DOM{
        kind=char,
        pick=fun(Dom,_) ->
                     {Dom, $a + rand:uniform($z - $a + 1)-1}
             end,
        shrink=fun(Dom,V) when V =< $c ->
                       {Dom,V};
                  (Dom,N) when N > $c, N =< $z ->
                       {Dom,N - rand:uniform(3)}
               end
       }.


-spec binary() -> domrec(binary()).
binary() ->
    #?DOM{kind=#binary{size=any},
          pick=fun binary_pick/2,
          shrink=fun binary_shrink/2}.

-spec binary(Size::non_neg_integer()) -> domrec(binary()).
binary(Size) ->
    #?DOM{kind=#binary{size=Size},
          pick=fun binary_pick/2,
          shrink=fun binary_shrink/2}.


binary_pick(#?DOM{kind=#binary{size=Size}, empty_ok=EmptyOK}=BinDom,
            SampleSize) ->
    Sz = case Size of
             any ->
                 case EmptyOK of
                     true ->
                         rand:uniform(SampleSize)-1;
                     false ->
                         rand:uniform(SampleSize)
                 end;
             Size ->
                 Size
         end,
    BinValue = list_to_binary(foldn(fun(T) -> [rand:uniform(256)-1 | T] end,
                                    [],
                                    Sz)),
    {BinDom, BinValue}.


allow_smaller(Len,any,true) when Len>0 ->
    true;
allow_smaller(Len,any,false) when Len>1 ->
    true;
allow_smaller(_,_,_) ->
    false.


binary_shrink(#?DOM{kind=#binary{size=Size}, empty_ok=EmptyOK}=BinDom,
              BinValue) ->
    List = binary_to_list(BinValue),
    Length = byte_size(BinValue),
    AllowSmaller = allow_smaller(Length,Size,EmptyOK) ,
    case shrink_list_with_elemdom(int(), List, Length, AllowSmaller) of
        List -> {BinDom, BinValue};
        NewList -> {BinDom, list_to_binary(NewList)}
    end.


-spec atom() -> domrec(atom()).
atom() ->
    #?DOM{kind=#atom{size=any},
          pick=fun atom_pick/2,
          shrink=fun atom_shrink/2}.

-spec atom(non_neg_integer()) -> domrec(atom()).
atom(Size) ->
    #?DOM{kind=#atom{size=Size},
          pick=fun atom_pick/2,
          shrink=fun atom_shrink/2}.


atom_pick(#?DOM{kind=#atom{size=Size}, empty_ok=EmptyOK}=AtomDom, SampleSize) ->
    Sz = case Size of
             any ->
                 case EmptyOK of
                     true ->
                         rand:uniform(xmin(SampleSize,256))-1;
                     false ->
                         rand:uniform(xmin(SampleSize,256))
                 end;
             Size ->
                 Size
         end,

    CharDom = char(),
    Fun=fun(T) -> {_,Char} = pick(CharDom,SampleSize), [Char | T] end,
    AtomValue = list_to_atom(foldn(Fun, [], Sz)),
    {AtomDom, AtomValue}.

xmin(A,B) when A<B -> A;
xmin(A,B) when B<A -> B;
xmin(A,B) when A==B -> A.

atom_shrink(#?DOM{kind=#atom{size=Size}, empty_ok=EmptyOK}=AtomDom,
            AtomValue) ->
    List = atom_to_list(AtomValue),
    Length = length(List),
    AllowSmaller = allow_smaller(Length,Size,EmptyOK) ,
    case shrink_list_with_elemdom(char(), List, Length, AllowSmaller) of
        List -> {AtomDom, AtomValue};
        NewList -> {AtomDom, list_to_atom(NewList)}
    end.


vector(Size,ElemDom) ->
    #?DOM{kind=#vector{size=Size,elem=ElemDom},
          pick=fun vector_pick/2}.

vector_pick(#?DOM{kind=#vector{size=Size,elem=ElemDom}}, SampleSize) ->
    foldn(fun({TDom,T}) ->
                  {HDom,H} = pick(ElemDom, SampleSize),
                  {[HDom|TDom], [H|T]}
          end,
          {[], []},
          Size).


%%
%% @doc Shrink `List' where all elements have the same domain `ElemDom'.
%% If parameter `AllowSmaller' is true, then we may also make the list
%% shorter.
%% @end
shrink_list_with_elemdom(_,List,0,_) -> List;
shrink_list_with_elemdom(ElemDom,List,Length,AllowSmaller) ->
    %% 1/5 of the time, try shrinking by removing an elemet
    case AllowSmaller andalso shrink_smaller(Length) of
        true ->
            RemoveIdx = rand:uniform(Length),
            without(RemoveIdx, List);
        false ->
            HowManyToShrink = shrink_members(Length),
            case shrink_list_members_generic(ElemDom, List, Length,
                                             HowManyToShrink) of

                %% can we remove an element?
                List when AllowSmaller ->
                    RemIdx = rand:uniform(Length),
                    without(RemIdx, List);

                %% it changed!
                ShrunkenList ->
                    ShrunkenList
            end
    end.

%% decide if something of size `Length' should be shrunk by removing an element
shrink_smaller(0) -> false;
shrink_smaller(_Length) ->
    rand:uniform(5)==1.

%% decide how many of
shrink_members(0) -> 0;
shrink_members(Length) when Length>0 ->
    case rand:uniform(5) of
        1 -> rand:uniform(5);
        _ -> 1
    end.


%%
%% Same, but when component element is fixed
%% (also returns List, not {Dom,List})!
%%
-spec shrink_list_members_generic(domain(T), [T], non_neg_integer(),
                                  integer()) -> [T].
shrink_list_members_generic(_, List, _, 0) -> List;
shrink_list_members_generic(#?DOM{}=ElemDom, List, Len, HowMany) ->
    NextList = shrink_list_N(ElemDom,List,Len, ?SHRINK_LOOPS),
    shrink_list_members_generic(ElemDom, NextList, Len, HowMany-1).

shrink_list_N(_, List, _, 0) -> List;
shrink_list_N(#?DOM{}=ElemDom, List, Len, N) ->
    ?assert(Len == length(List)),

    %%
    %% replace element at RemIdx with simplified one
    %%
    RemIdx = rand:uniform(Len),
    Elm = lists:nth(RemIdx, List),
    %% io:format("shrinking elem ~p (~p) of ~p~n", [RemIdx,Elm,List]),

    case shrink(ElemDom,Elm) of
        {_,Elm} ->
            shrink_list_N(ElemDom, List, Len, N-1);
        {_,SElm} ->
            lists:sublist(List,RemIdx-1) ++ [SElm]
                ++ lists:sublist(List,RemIdx+1,Len)
    end.


non_empty(#?DOM{}=Dom) ->
    Dom#?DOM{empty_ok=false}.


%% @doc Support function for the `?LET(Vars,Dom1,Dom2)' macro.
%% @private
%% @spec bind(domain(T), fun( (T) -> domain(D) )) -> domain(D)
-spec bind(domain(T::any()),
           fun( (T::any()) -> domain(D) )) -> domain(D).
bind(Gen1,FG2) ->
    #?DOM{kind=#bind{dom=Gen1,body=FG2},
          pick = fun bind_pick/2
         }.

-spec bind_pick(domain(T),pos_integer()) -> T.
bind_pick(#?DOM{kind=#bind{dom=Dom,body=Fun}}, SampleSize) ->
    {Dom1,Val1} = pick(Dom, SampleSize),
    {Dom2,Val2} = pick( Fun(Val1), SampleSize),
    { bound_domain(Dom1,Val1,Dom2,Fun, SampleSize), Val2 }.

bound_domain(Dom1,Val1,Dom2,Fun,SampleSize) ->
    #?DOM{kind=#bound_domain{dom1=Dom1,val1=Val1,dom2=Dom2,fun2=Fun,
                             size=SampleSize},
          shrink= fun bound_shrink/2
         }.

bound_shrink(#?DOM{kind=#bound_domain{dom1=Dom1,val1=Val1,dom2=Dom2,fun2=Fun,
                                      size=SampleSize}}, Val2) ->
    case shrink(Dom1,Val1) of
        %% it did not shrink val1
        {_,Val1} ->
            %% try to shrink the secondary domain
            shrink(Dom2,Val2);

        %% Val1 did shrink!
        {SDom1,SVal1} ->
            %% pick a new value from the secondary domain
            {SDom2,SVal2} = pick( Fun(SVal1), SampleSize),

            %% and return the new bound domain
            { bound_domain(SDom1,SVal1,SDom2,Fun, SampleSize), SVal2 }
    end.

%% @doc support function for `?LETSHRINK([X,...],[domain(),...],domain())'
%% @private
bindshrink(Dom,_Fun) when not is_list(Dom) ->
    error(argument_to_LETSHRINK_macro_must_be_lists);
bindshrink(Dom,Fun) when is_function(Fun,1) ->
    domain(letshrink,
           fun(_,SampleSize) ->
                   Box1 = {_,List1} = pick(Dom, SampleSize),
                   ?assert(is_list(List1) and (length(List1)>0) ),
                   Box2 = {_,List2} = pick(Fun(List1), SampleSize),

                   { bindshrink2(Box1,Box1,Box2,Fun,SampleSize), List2 }
           end,
           undefined).

bindshrink2(OrigBox1,Box1,Box2,Fun,SampleSize) ->
    domain(letshrink2,
           undefined,
           fun(_,_) ->
                   {Dom2,Val2}=Box2,
                   case shrink(Dom2,Val2) of
                       {_,Val2} ->
                           case shrink({_,List1}=Box1) of
                               {_, List1} ->
                                   {OrigDom1,OrigList1}=OrigBox1,
                                   Index = rand:uniform(length(OrigList1)),
                                   { lists:nth(Index,OrigDom1),
                                     lists:nth(Index,OrigList1) };

                               {_,NewList1}=NewBox1 ->
                                   NewBox2 = pick(Fun(NewList1), SampleSize),
                                   {_,NewVal2} = NewBox2,
                                   { bindshrink2(OrigBox1,
                                                 NewBox1,
                                                 NewBox2,
                                                 Fun,
                                                 SampleSize),
                                     NewVal2 }
                           end;

                       {_, NewVal2}=NewBox2 ->
                           { bindshrink2(OrigBox1,
                                         Box1,
                                         NewBox2,
                                         Fun,
                                         SampleSize),
                             NewVal2 }
                   end
           end).

%% @doc Support function for the ?SUCHTHAT macro.
%% @private
%% @spec suchthat(domain(T),fun((T) -> boolean())) -> domain(T)
-spec suchthat(domain(T),fun((T) -> boolean())) -> domain(T).
suchthat(Dom,Predicate) ->
    #?DOM{kind=#suchthat{dom=Dom,pred=Predicate},
          pick=fun(#?DOM{kind=#suchthat{dom=Dom1,pred=Fun1}},SampleSize) ->
                       suchthat_loop(?SUCHTHAT_LOOPS,Dom1,Fun1,SampleSize)
               end
         }.

suchthat_loop(0,_,_,_) ->
    erlang:exit(suchthat_failed);
suchthat_loop(N,Dom,Fun,SampleSize) ->
    {ValDom,Val} = pick(Dom,SampleSize),
    case Fun(Val) of
        true -> {ValDom,Val};
        _ -> suchthat_loop(N-1, Dom, Fun, SampleSize)
    end.


smaller(Domain) ->
    ?SIZED(SZ, resize(rand:uniform((SZ div 2)+1), Domain)).


-spec any() -> domain(any()).
any()  ->
    oneof([int(), real(), bool(), atom(),

           [smaller(?DELAY(any())) | smaller(?DELAY(any()))],

           %% list(any()), but with a size in the range 1..GenSize
           list(smaller(?DELAY(any()))),

           tuple(smaller(?DELAY(any())))

          ]).

-spec oneof([domain(T)]) -> domain(T).
oneof(DomList) when is_list(DomList) ->
    #?DOM{kind=#oneof{elems=DomList, size=length(DomList)},
          pick=fun oneof_pick/2
         }.

oneof_pick(#?DOM{kind=#oneof{elems=DomList, size=Length}}, SampleSize) ->
    Dom = lists:nth(rand:uniform(Length), DomList),
    pick(Dom, SampleSize).

%% --------------------------------------------------------------
%% @doc Choose domain from list [{Weight, Domain}, ...]
%% @end
%% --------------------------------------------------------------
frequency(GenList) when is_list(GenList) ->
    Sum = lists:foldl(fun({Freq, _}, Acc) ->
                              Freq + Acc
                      end,
                      0,
                      GenList),

    domain(frequency,
           fun(_,SampleSize) ->
                   Limit = rand:uniform(Sum),
                   {ok,Gen} = lists:foldl(
                                fun (_, {ok, _}=Acc) ->
                                        Acc;
                                    ({Freq,Generator}, {AccSum, none}) ->
                                        NextAcc = AccSum+Freq,
                                        if NextAcc >= Limit ->
                                                {ok, Generator};
                                           true ->
                                                {NextAcc, none}
                                        end
                                end,
                                {0, none},
                                GenList),
                   pick(Gen, SampleSize)
           end,
           undefined).


%% @doc Returns the domain containing exactly `Value'.
%% Triq uses internally records of type `@'; and so to avoid
%% interpretation of such values you can wrap it with this.  This would
%% be the case if you have constants in your domains contain the atom `@'.
%% I.e., the following would break because Triq tries to interpret the `@':
%%<pre>?FORALL(X, [int(), {'@', 4}],
%%   [IntVal, {'@', 4}] = X
%%)</pre>
%%To fix it, do like this:
%%<pre>?FORALL(X, [int(), return({'@', 4})],
%%   [IntVal, {'@', 4}] = X
%%)</pre>
%% @spec return(Value::Type) -> domain(Type)
-spec return(Value::Type) -> domain(Type).
return(Val) ->
    domain(return,
           fun(Self,_) -> {Self,Val} end,
           fun(Self,_) -> {Self,Val} end).

%% @doc Support function for the ?SIZED macro.
%% @spec sized( fun((integer()) -> domain(T)) ) -> domain(T)
-spec sized( fun((integer()) -> domain(T)) ) -> domain(T).
sized(Fun) ->
    #?DOM{kind=#sized{body=Fun},
          pick=fun(#?DOM{kind=#sized{body=F}},GS) -> pick(F(GS),GS) end
         }.

resize(Sz,Dom) ->
    #?DOM{kind=#resize{dom=Dom,size=Sz},
          pick=fun(#?DOM{kind=#resize{dom=D,size=SampleSize}},_) ->
                       pick(D,SampleSize)
               end
         }.

-spec choose(M::integer(), N::integer()) -> domrec(integer()).
choose(M,N) when is_integer(M), is_integer(N), M<N ->
    #?DOM{kind={choose,M,N},
          pick = fun choose_pick/2,
          shrink = fun choose_shrink/2
         }.

choose_pick(#?DOM{kind=#choose{min=M,max=N}}=Dom, _) ->
    Value = rand:uniform(N-M+1) - 1 + M,
    {Dom,Value}.

choose_shrink(#?DOM{kind=#choose{min=M}}=Dom, Value) ->
    Mid = (Value - M) div 2,
    {Dom, M + Mid}.

%% @doc Generates a member of the list `L'.  Shrinks towards the first element
%% of the list.
%% @spec elements([any()]) -> domain(any())
%% @end
-spec elements([T,...]) -> domain(T).
elements(L) when is_list(L), length(L)>0 ->
    #?DOM{kind=#elements{elems=L,size=length(L)},
          pick = fun elements_pick/2,
          shrink = fun elements_shrink/2
         }.

-spec elements_pick(domain(T), pos_integer()) -> {domain(T), T}.
elements_pick(#?DOM{kind=#elements{elems=Elems,size=Length}=Kind}=Dom, _) ->
    Picked = rand:uniform(Length),
    Value = lists:nth(Picked,Elems),
    { Dom#?DOM{kind=Kind#elements{picked=Picked}},
      Value }.

elements_shrink(#?DOM{kind=#elements{elems=Elems,picked=Picked}=Kind}=Dom, _)
  when Picked > 1 ->
    Value = lists:nth(Picked-1,Elems),
    { Dom#?DOM{kind=Kind#elements{picked=Picked-1}},
      Value };
elements_shrink(Dom,Value) ->
    { Dom, Value }.


shrink_without_duplicates(Dom) ->
    domain(shrink_without_duplicates1,
           fun(_,GS) ->
                   {Dom,Val} = pick(Dom,GS),
                   Tested = gb_sets:add(Val, gb_sets:new()),
                   {shrink_without_duplicates(Dom,Tested), Val}
           end,
           undefined).

shrink_without_duplicates(Dom,Tested) ->
    domain(shrink_without_duplicates2,
           undefined,
           fun(_,Val) ->
                   shrink_without_duplicates_loop(Dom,Val,Tested,?SHRINK_LOOPS)
           end).

shrink_without_duplicates_loop(_,Val,_,0) ->
    Val;
shrink_without_duplicates_loop(Dom,Val,Tested,Tries) ->
    {Dom2,Val2} = shrink(Dom,Val),
    case gb_sets:is_member(Val2, Tested) of
        true ->
            shrink_without_duplicates_loop(Dom,Val,Tested,Tries-1);
        false ->
            {shrink_without_duplicates(Dom2, gb_sets:add(Val, Tested)),
             Val2}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Generate a sample of output values from a generator.
%%
%% @spec sample( domain(T) ) -> [T]
%% @end
%%--------------------------------------------------------------------
sample(Dom) ->
    foldn(fun(T) ->
                  {_,Val} = pick(Dom, 20 + rand:uniform(10) ),
                  [Val|T]
          end,
          [],
          11).

%%-------------------------------------------------------------------
%% @doc Get the domain of boxes of T
%% @spec seal(domain(T)) -> domain(box(T))
%% @end
%%-------------------------------------------------------------------
-spec seal(Dom::domain(T)) -> domrec(box(T)).
seal(Dom) ->
    Seed = rand:seed(exsplus),
    rand:seed(exsplus, Seed),
    #?DOM{kind=#seal{dom=Dom,seed=Seed}, pick=fun seal_pick/2}.

seal_pick(#?DOM{kind=#seal{dom=Dom,seed=Seed}}, SampleSize) ->
    OldSeed = rand:seed(exsplus, Seed),
    {BoxDom,BoxValue} = pick(Dom,SampleSize),
    rand:seed(exsplus, OldSeed),
    #?BOX{dom=BoxDom,value=BoxValue}.

%%-------------------------------------------------------------------
%% @doc Open a box, yielding a domain which always generates the same value.
%% @spec open(box(T)) -> domain(T)
%% @end
%%-------------------------------------------------------------------
-spec open(Box::box(T)) -> domain(T).
open(#?BOX{}=Box) ->
    #?DOM{kind=Box, pick=fun box_pick/2}.

box_pick(#?DOM{kind=#?BOX{dom=Dom,value=Value}}, _) ->
    {Dom,Value}.

-spec peek(box(T)) -> T.
peek(#?BOX{value=Value}) ->
    Value.

%%------------------------------------------------------------------
%% @doc Print a value generated by `Domain', followed by a sample of shrinkings.
%% For each line of successive output, it prints up to five samples of
%% shrinking.  The first value on each like is used as the target for the next
%% round of shrinking.
%%
%% <pre> 1> sampleshrink(list(int())).
%%[-2,-8,2]
%%[[-1,-8,2],[0,-8,2],[-1,-7,2],[-2,-8,1],[-1,-8,1]]
%%[[0,-8,2],[0,-6,1],[-1,-7,2],[0,-7,2]]
%%[[0,-8,0],[0,-7,0],[0,-7,2],[0,-8,1],[0,-5,2],[0,-7,1]]
%%[[0,-7,0],[0,-5,0]]
%%[[0,-5,0],[0,-6,0]]
%%[[0,-4,0],[0,-3,0]]
%%[[0,-2,0],[0,-3,0],[0,-1,0]]
%%[[0,-1,0]]
%%[[0,0,0]]
%%[[0,0]]
%%[[0]]
%%[[]]
%%ok</pre>
%% @spec sampleshrink(domain(any())) -> ok
%% @end
%%------------------------------------------------------------------
-spec sampleshrink(domain(any())) -> ok.
sampleshrink(Domain) ->
    {Dom2,Value} = pick(Domain, 20),
    io:format("~p~n", [Value]),
    sampleshrink_loop(Dom2,Value).

sampleshrink_loop(Dom,Val) ->
    case shrink(Dom,Val) of
        {_,Val} ->
            ok;
        {Dom2,Val2} ->
            Samples = foldn(fun(T) -> {_,V} = shrink(Dom,Val),
                                      case lists:member(V,T) of
                                          true -> T;
                                          false -> [V|T]
                                      end
                            end,
                            [Val2],
                            5),

            io:format("~p~n", [lists:reverse(Samples)]),
            sampleshrink_loop(Dom2,Val2)
    end.

%%------------------------------------------------------------------------
%% @doc Create custom domain.
%% This function allows you to create a custom domain with it's own
%% shrinking logic.  For instance, the even numbers can be specified thus:
%%
%% <pre>even() ->
%%    domain(even,
%%      fun(Self,Size) ->
%%            Value = (rand:uniform(Size) * 2) div 2,
%%            {Self, Value}
%%      end,
%%      fun(Self,Value) when Value>0 ->
%%            {Self, Value-2};
%%         (Self,_,0) ->
%%            {0, 0}
%%      end).</pre>
%%
%% The domain itself (`Self' in the above code) is passed as the first argument
%% to each invocation of both the picking and the shrinking functions.
%%
%% Both the picking and the shrinking function must return a 2-tuple of
%% the domain of the resulting value, and the value itself.
%%
%% @spec domain(Name::any(),
%%           PickFun :: pick_fun(T),
%%           ShrinkFun :: shrink_fun(T)) -> domain(T)
%% @end
%%------------------------------------------------------------------------
-spec domain(Name::atom(),
             PickFun::pick_fun(T),
             ShrinkFun::shrink_fun(T)) -> domain(T).
domain(Name,PickFun,ShrinkFun) ->
    #?DOM{kind=Name, pick=PickFun, shrink=ShrinkFun}.


%%
%% Utility functions
%%

foldn(_,Acc,0) -> Acc;
foldn(Fun,Acc,Count) when Count > 0 ->
    foldn(Fun, Fun(Acc), Count-1).

%% remove the RemIdx'th element of List [1-indexed]
without(RemIdx,List) when is_list(List) ->
    {First,Rest} = lists:split(RemIdx-1,List),
    First ++ tl(Rest);
without(RemIdx,Tup) when is_tuple(Tup) ->
    list_to_tuple(without(RemIdx, tuple_to_list(Tup))).

%% remove the RemIdx1 through RemIdx2-1'th element of List [1-indexed]
without(RemIdx1, RemIdx2, List) when is_list(List) ->
    {First,Tail}   = lists:split(RemIdx1-1,List),
    {_Middle,Rest} = lists:split(RemIdx2-RemIdx1,Tail),
    First ++ Rest.

repeat(_,0) ->
    ok;
repeat(Fun,N) ->
    Fun(),
    repeat(Fun,N-1).


%% Code points in the range U+D800..U+DBFF (1,024 code points) are known as
%% high-surrogate code points, and code points in the range U+DC00..U+DFFF
%% (1,024 code points) are known as low-surrogate code points.
%% A high-surrogate code point (also known as a leading surrogate) followed
%% by a low-surrogate code point (also known as a trailing surrogate)
%% together form a surrogate pair used in UTF-16 to represent 1,048,576
%% code points outside BMP.
%% High and low surrogate code points are not valid by themselves. Thus the
%% range of code points that are available for use as characters is
%% U+0000..U+D7FF and U+E000..U+10FFFF (1,112,064 code points).
%% The value of these code points (i.e. excluding surrogates) is sometimes
%% referred to as the character's scalar value.
-define(UNICODE_CHAR_SHRINK_STEP, 3).
-spec unicode_char() -> domrec(uchar()).
unicode_char() ->
    P = fun(Dom,_) ->
                {Dom, random_unicode_char()}
        end,
    S = fun(Dom,V) ->
                NewV = case (V - rand:uniform(?UNICODE_CHAR_SHRINK_STEP)) of
                           X when X < 0 -> V;
                           X when X >= 16#D800, X =< 16#DFFF ->
                               %% skip surrogates.
                               16#D799;
                           X when X =:= 16#FFFF; X =:= 16#FFFE ->
                               16#FFFD;
                           X -> X
                       end,

                {Dom, NewV}
        end,

    #?DOM{
        kind=unicode_char,
        pick=P,
        shrink=S}.

-spec random_unicode_char() -> uchar().
random_unicode_char() ->
    case (rand:uniform(16#10FFFF + 1) - 1) of
        C when C >= 16#D800 andalso C =< 16#DFFF ->
            %% surrogates
            random_unicode_char();
        16#FFFF ->
            random_unicode_char();
        16#FFFE ->
            random_unicode_char();
        C -> C
    end.


%% @doc Generate a list of unicode code points.
-spec unicode_string() -> domrec([uchar()]).
unicode_string() ->
    list(unicode_char()).


%% @doc Generate a list of unicode code points of length `Size'.
-spec unicode_string(non_neg_integer()) -> domrec([uchar()]).
unicode_string(Size) ->
    vector(Size, unicode_char()).


-spec unicode_binary() -> domrec(binary()).
unicode_binary() ->
    unicode_binary(any, utf8).


%% @doc Generate an unicode binary binary.
-spec unicode_binary(Size | Encoding) -> domrec(binary()) when
      Size :: non_neg_integer(),
      Encoding :: unicode:encoding().

unicode_binary(Size) when is_integer(Size) ->
    unicode_binary(Size, utf8);
unicode_binary(Encoding) ->
    unicode_binary(any, Encoding).


%% @doc Generate an unicode binary.
-spec unicode_binary(Size, Encoding) -> domrec(binary()) when
      Size :: non_neg_integer() | undefined,
      Encoding :: unicode:encoding().

unicode_binary(undefined, Encoding) ->
    unicode_binary(any, Encoding);
unicode_binary(Size, Encoding) ->
    #?DOM{kind=#unicode_binary{size=Size, encoding=Encoding},
          pick=fun unicode_binary_pick/2,
          shrink=fun unicode_binary_shrink/2}.


unicode_binary_pick(#?DOM{kind=#unicode_binary{size=Size, encoding=Encoding},
                          empty_ok=EmptyOK}=BinDom, SampleSize)
  when SampleSize > 1 ->
    Sz = case Size of
             any ->
                 case EmptyOK of
                     true ->
                         rand:uniform(SampleSize)-1;
                     false ->
                         rand:uniform(SampleSize)
                 end;
             Size ->
                 Size
         end,
    CharList = foldn(fun(T) -> [random_unicode_char() | T] end, [], Sz),
    BinValue = unicode:characters_to_binary(CharList, unicode, Encoding),
    {BinDom, BinValue}.

unicode_binary_shrink(#?DOM{kind=#unicode_binary{size=Size, encoding=Encoding},
                            empty_ok=EmptyOK}=BinDom, BinValue) ->
    List = unicode:characters_to_list_int(BinValue, utf8),
    Length = string:len(List),
    AllowSmaller = allow_smaller(Length,Size,EmptyOK),
    case shrink_list_with_elemdom(unicode_char(), List, Length, AllowSmaller) of
        List -> {BinDom, BinValue};
        NewList ->
            NewBin = unicode:characters_to_binary(NewList, unicode, Encoding),
            {BinDom, NewBin}
    end.


-spec unicode_characters() -> domrec(uchars()).
unicode_characters() ->
    unicode_characters(unicode).


%% `unicode_characters()' should not return a single `unicode_char()'.
-spec unicode_characters(Encoding) -> domrec(uchars()) when
      Encoding :: unicode:encoding().
unicode_characters(Encoding) ->
    ?SIZED(Size,
           frequency([{1, unicode_string()},
                      {1, unicode_binary(Encoding)},
                      {5, ?DELAY(resize(Size div 2,
                                        unicode_characters1(Encoding)))}
                     ])).
unicode_characters1(Encoding) ->
    ?SIZED(Size, unicode_characters1(Size, Encoding)).


unicode_characters1(0, _Encoding) ->
    list(unicode_char());
unicode_characters1(Size, Encoding) ->
    Chars = ?DELAY(resize(Size, unicode_characters(Encoding))),
    %% TODO: Unicode characters can be of type `maybe_improper_list()'.
    list(frequency([{10,unicode_char()}, {1, Chars}])).
