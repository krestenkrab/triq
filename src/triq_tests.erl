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

%%
%% This file contains some sample properties, which also
%% function as a very simple test suite for Triq itself.
%%

-module(triq_tests).

% including this also auto-exports all properties
-include("triq.hrl").

% use eunit
-include_lib("eunit/include/eunit.hrl").

%% eunit test; we need a longer timeout because some of it is rather slow...
triq_test_() ->
    {timeout, 60, 
     fun() -> 
	     true = triq:module(?MODULE)
     end}.

boolean_test() ->
    Unique = fun ordsets:from_list/1,
    ?assertEqual([false, true], Unique(triq_dom:sample(bool()))).

prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
       ?TRAPEXIT(lists:reverse(Xs++Ys)
		 ==
		 lists:reverse(Ys) ++ lists:reverse(Xs))).

xprop_delete() ->
     ?FORALL(L,list(int()), 
	?IMPLIES(L /= [],
	    ?FORALL(I,elements(L), 
		?WHENFAIL(io:format("L=~p, I=~p~n", [L,I]),
		    not lists:member(I,lists:delete(I,L)))))).
					 
delete_test() ->
    false = triq:check(xprop_delete()).


inverse('<') -> '>=';
inverse('>') -> '=<';
inverse('==') -> '/=';
inverse('=:=') -> '=/=';
inverse('=/=') -> '=:=';
inverse('/=') -> '=='.

prop_binop() ->
    ?FORALL({A,B,OP}, {any(),any(),elements(['>','<','==','=:=','=/=','/='])},
	    erlang:OP(A,B) 
	    ==
	    begin 
		ROP = inverse(OP),
		not  ( erlang:ROP(A,B) )
	    end
	   ).


prop_timeout() ->
 fails(
   ?FORALL(N,shrink_without_duplicates(choose(50,150)),
     ?TIMEOUT(100,
       timer:sleep(N) == ok))
)
.

prop_sized() ->
    ?FORALL(T, ?SIZED(S, {true, choose(0,S)}),
	    (erlang:tuple_size(T) == 2)
	    and
	    begin {true, Int} = T, Int >= 0 end
	   ).

prop_simple1() ->
    ?FORALL(V, [1,int(),3|4], 
	    begin [1,X,3|4]=V, is_integer(X) end ).

prop_simple2() ->
    ?FORALL(V, {}, V == {}).

prop_simple3() ->
    ?FORALL(V, atom(), 
	    ?IMPLIES(V /= '',
		     begin
			 [CH|_] = erlang:atom_to_list(V),
			 (CH >= $a) and (CH =< $z)
		     end)).


%%
%% This should be able to succeed
%%
prop_suchthat() ->
    ?FORALL({X,Y}, 
	    ?SUCHTHAT({XX,YY}, 
		      {int(),int()}, 
		      XX < YY), 
	    X < Y).

suchthat_test() ->
    true = triq:counterexample(prop_suchthat()).

tuple_failure_test() ->
    false = check(?FORALL(T, {int()},
			  begin
			      {V} = T,
			      V > 0
			  end)).

list_shrink_test() ->
    %% test that a list shrinks to the empty list
    true = lists:all(fun(_)->
			     [[]] == triq:counterexample(
				       ?FORALL(_, list(int()), false)
				      )
		     end, lists:seq(1,100)).

list_shrink2_test() ->
    %% test that a list doesn't easily end in a local 'smallest counterexample'
    true = lists:all(fun(_)->
			     [[]] == triq:counterexample(
				       ?FORALL(L, list(oneof([a,b])),
					       not is_pairs_list(L))
				      )
		     end, lists:seq(1,100)).

is_pairs_list([])      -> true;
is_pairs_list([X,X|T]) -> is_pairs_list(T);
is_pairs_list(_)       -> false.

oneof_test() ->
    [{X,Y}] = triq:counterexample(
	      ?FORALL({X,Y}, 
		      ?SUCHTHAT({A,B},
				{oneof([int(),real()]),
				 oneof([int(),real()])},
				A < B),
		      begin
%			  io:format("{X,Y} = ~p~n", [{X,Y}]),
			  is_integer(X) == is_integer(Y)
		      end
			 )),
    %% Note: 0 == 0.0
    ?assert((X == 0) and (Y == 0)).

%%
%% This test makes sure that X shrinks only to 3. 
%%
oneof2_test() ->
    [X] = triq:counterexample
	    (?FORALL(_, 
		     oneof([choose(3,7)]),
		     false)),
    3 = X.

%%
%% Test that vector doesn't shrink the length
%%
vector_test() ->
    [L] = triq:counterexample
            (?FORALL(_, vector(4, choose(3,7)),
		     false)),
    [3,3,3,3] = L.

    
%%
%% Test binary shrinking
%%
binary_test() ->
    [X] = triq:counterexample
	    (?FORALL(_, binary(2), false)),
    <<0,0>> = X.

not_reach_rsn() ->
       ?LET(Rsn,choose(2,5),<<Rsn>>).

binary2_test() ->
    [<<2>>] = triq:counterexample
            (?FORALL(_, not_reach_rsn(), false)).



%%
%% Test shrinking of elements
%%
elements_test() ->
    [X] = triq:counterexample
	    (?FORALL(_, 
		     elements([one,two,three]),
		     false)),
    one = X.

%%
%% Test passing counterexamples to properties
%%
recheck_test_() ->
    Good = [[1,2], 1],
    Bad = [[1,1], 1],
    [?_assertEqual(true, triq:check(xprop_delete(), Good)),
     ?_assertEqual(false, triq:check(xprop_delete(), Bad))].

counterexample_test() ->
    Counterexample = triq:counterexample(xprop_delete()),
    ?assertEqual(false, triq:check(xprop_delete(), Counterexample)).
