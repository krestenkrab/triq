-module(sample).

-include("triq.hrl").

-export([prop_append/0, prop_echo/0, prop_delete_2/0, main/0]).

prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
       ?TRAPEXIT(lists:reverse(Xs++Ys)
		 ==
		 lists:reverse(Ys) ++ lists:reverse(Xs))).

					 
prop_delete_2() ->
    ?FORALL(L,list(int()), 
	?IMPLIES(L /= [],
	    ?FORALL(I,elements(L), 
		?WHENFAIL(io:format("L=~p, I=~p~n", [L,I]),
		    not lists:member(I,lists:delete(I,L)))))).


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

prop_echo() ->
    ?FORALL(X, any(), 
	    X == X).

main() ->
    triq:check(?MODULE)
.
