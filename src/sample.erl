-module(sample).

-include("triq.hrl").

-export([prop_append/0, prop_delete_2/0, prop_binop/0, prop_timeout/0]).

prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
       ?TRAPEXIT(lists:reverse(Xs++Ys)
		 ==
		 lists:reverse(Ys) ++ lists:reverse(Xs))).

					 
prop_delete_2() ->
  fails(
    ?FORALL(L,list(int()), 
	?IMPLIES(L /= [],
	    ?FORALL(I,elements(L), 
		?WHENFAIL(io:format("L=~p, I=~p~n", [L,I]),
		    not lists:member(I,lists:delete(I,L))))))).


inverse('<') -> '>=';
inverse('>') -> '=<';
inverse('==') -> '/=';
inverse('=:=') -> '=/=';
inverse('=/=') -> '=:=';
inverse('/=') -> '=='.
    

prop_binop() ->
    ?FORALL({A,B,OP}, {any(),any(),elements(['>','<'])},
	    erlang:OP(A,B) 
	    ==
	    begin 
		ROP = inverse(OP),
		not  ( erlang:ROP(A,B) )
	    end
	   ).


prop_timeout() ->
   ?FORALL(N,choose(50,150),
     ?TIMEOUT(100,
       timer:sleep(N) == ok)).
