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

-module(triq_domain).

-include("triq_internal.hrl").

%% @type domain(). A domain of values.
-type domain() :: any().

%% generators
-export([list/1, tuple/1, int/0, real/0, sized/1, elements/1, any/0, atom/0, choose/2,
	 oneof/1, boolean/0, char/0, return/1, vector/2]).

%% using a generator
-export([generate/2, component_domain/2, dom_let/2, bind/2, suchthat/2]).


%%--------------------------------------------------------------------
%% @doc
%% Returns the domain of lists of the argument.  
%% For example, `list(int())' yields the domain of lists of integers.
%%
%% @spec list( domain() ) -> domain()
%% @end
%%--------------------------------------------------------------------
list(ElemDom) ->
    #?DOM{kind={list,ElemDom},
	generate   = fun(#?DOM{kind={list,EG}},GS) -> 
				Len = random:uniform(GS)-1,
				generate_list(Len, EG, GS)
		     end
	 }.


%% GenSize for elements inside a list (or tuple)
list_element_gensize(N) when N<3 ->
    1;
list_element_gensize(N) ->
    N div 2.


%% support function for generate({gen_list, ...})
generate_list(Len,_,_) when Len =< 0 ->
    [];
generate_list(Len,EG,GS) ->
    [generate(EG,list_element_gensize(GS)) | generate_list(Len-1, EG, GS)].



%%--------------------------------------------------------------------
%% @doc
%% Returns the domain of tuples of the argument.  
%% For example, `tuple(int())' yields the domain of tuple of integers.
%% Notice that `{int(),int()}' also describes a domain og tuples of integer,
%% but in this case only two-tuples.
%%
%% @spec tuple( domain() ) -> domain()
%% @end
%%--------------------------------------------------------------------
tuple(ElemDom) ->
    #?DOM{kind={tuple,ElemDom},
	generate   = fun(#?DOM{kind={tuple,EG}},GS) -> 
				Len = random:uniform(GS)-1,
				list_to_tuple(generate_list(Len, EG, GS))
		     end}.

%% @doc Return the Nth component domain of AggrDom.
%% AggrDom must be a list domain, a tuple domain, or the any domain.
%% For example `component_domain(2, {int(),boolean()})' is `boolean()'.
%% @spec component_domain(Nth::int(),AggrDom::domain()) -> domain()
%%    
component_domain(_, #?DOM{kind=any}=ElemDom) ->
    ElemDom;
component_domain(_, #?DOM{kind={list,ElemDom}}) ->
    ElemDom;
component_domain(_, #?DOM{kind={tuple,ElemDom}}) ->
    ElemDom;
component_domain(N, Gen) when is_tuple(Gen), N > 0, tuple_size(Gen) >= N ->
    element(N,Gen);
component_domain(N, Gen) when is_list(Gen), N > 0, length(Gen) >= N ->
    lists:nth(N,Gen).

generate_int(_,GS) ->
    random:uniform(GS) - (GS div 2).

%% @doc The domain of integers.
%% @spec(int() -> domain()).
	     
int() -> 
    #?DOM{kind=int,
	 generate  = fun generate_int/2
	}.

%% @doc The domain of (64 bit) floats.
%% @spec real() -> domain()
real() -> 
    #?DOM{kind=real,
	 generate  = fun(_,GS) -> (random:uniform()*GS) - (GS / 2) end
	}.

%%
%% @doc The domain of booleans.
%% Generate a boolean true or false with equal probability.
%% @spec boolean() -> domain()
%%
boolean() -> 
    #?DOM{kind=int,
	  generate  = fun(_,_) -> random:uniform(2) == 1 end
	 }.

rand(Min,Max,GS) ->
    Val = random:uniform(GS)-1+Min,
    if Val =< Max -> Val;
       true -> Max
    end.
	

%% @doc Returns the domain of atoms.
%% @spec atom() -> domain()
-spec(atom() -> domain()).
atom() -> 
    #?DOM{kind=atom,
	  generate = fun(_,GS) ->
	      erlang:list_to_atom(generate_list(rand(0,255,GS), char(), GS)) 
	  end
	 }.

%% @doc Returns the domain of characters i.e., integers in the range `$a..$z'.
%% @spec char() -> domain()
char() -> 
    #?DOM{kind=char,
	  generate  = fun(_,_GS) -> $a + random:uniform($z - $a + 1)-1 end,
	  simplify = fun(_,V) when V=:=$a;V=:=$b;V=:=$c -> V; (_,N) -> N-1 end
	 }.

%% @doc Support function for the `LET(Vars,Dom1,Dom2)' macro.
-spec(dom_let(domain(), fun( ( any() ) -> domain() )) -> domain()).
dom_let(Gen1,FG2) -> 
    #?DOM{kind={dom_let,Gen1,FG2},
	 generate  = fun(#?DOM{kind={dom_let,G1,G2}},GS) -> 
			     Va = generate(G1, GS),
			     G = G2(Va),
			     generate(G,GS)
		     end
	}.

%%
%% This is the heart of the random structure generator
%%
generate(Gen=#?DOM{generate=GenFun}, GS) ->
    GenFun(Gen,GS);

generate(Dom, GS) ->
    generate_internal(Dom,GS).


generate_internal({call, Mod, Fun, Args},GS) 
  when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    generate (apply(Mod,Fun,Args), GS);


%%
%% A tuple is generated by generating each element
%%
generate_internal({}, _) -> {};
generate_internal(T,GS) when is_tuple(T) ->
    TList = erlang:tuple_to_list(T),
    GList = lists:map(fun(TE) -> generate(TE,GS) end, TList),
    erlang:list_to_tuple(GList);

%%
%% for Lists, we traverse down the list and generate 
%% each head
%%
generate_internal([], _) -> [];
generate_internal([H|T], GS) -> [generate(H,GS)|generate(T,GS)];

%%
%% simple values that generate themselves
%%
generate_internal(V,_) when is_atom(V);
		   is_number(V);
		   is_list(V);
		   is_function(V)
		   ->
    V.


bind(#?DOM{kind={sized,Fun}}=_Dom,GenSize) ->
    bind(Fun(GenSize),GenSize);

bind(#?DOM{kind={dom_let,Gen1,FG2}}=_Dom,GenSize) ->
    Va = generate(bind(Gen1,GenSize),GenSize),
    bind(FG2(Va),GenSize);

bind(#?DOM{kind={oneof,Gs1,Len}}, GenSize) ->
    bind (lists:nth(random:uniform(Len), Gs1), GenSize);

bind(Dom,_GenSize) ->
    Dom.


-spec(sized( fun((integer()) -> domain()) ) -> domain()).	     
sized(Fun) ->
    #?DOM{kind={sized,Fun},
	  generate=fun(_,GS) -> generate(Fun(GS),GS) end
	 }. 

suchthat_loop(0,_,_,_) ->
    erlang:exit(suchthat_failed);

suchthat_loop(N,Dom,Fun,GS) ->
    Val = generate(Dom,GS),
    case Fun(Val) of
	true -> Val;
	_ -> suchthat_loop(N-1, Dom, Fun,GS)
    end.
	     
    

suchthat(Dom,Fun) ->
    #?DOM{kind=suchthat,
	  generate=fun(_,GS) -> suchthat_loop(100,bind(Dom,GS),Fun,GS) end
	 }.			   

%% @doc Domain specified by a list of members.
%% Generating values from this domain yields a random
%% element from the given list.
%% @spec elements(Members::list(Member)) -> domain()
%% where 
%%    Member = any()
-spec(elements(list(any())) -> any()).	      
elements([]) -> undefined;
elements(L) when is_list(L) ->
    #?DOM{kind={elements,L,length(L)}, 
	 generate=fun(#?DOM{kind={elements,L2,Len}},_GS) ->			  
			  lists:nth(random:uniform(Len), L2)
		  end,
	 simplify=fun(_Dom,Val) -> Val end}.


%% @spec any()  -> domain()
any()  ->
    #?DOM{kind=any,
	  generate=fun(#?DOM{kind=any}=Dom,GS) ->
			   case random:uniform(6) of
			       1 -> generate(int(),GS);
			       2 -> generate(real(),GS);
			       3 -> generate(list(Dom),random:uniform(GS));
			       4 -> generate(tuple(Dom),random:uniform(GS));
			       5 -> generate(boolean(),GS);
			       6 -> generate(atom(),GS)
			   end
		   end
	 }.

%% @doc Returns the domain of the union of the domains in Gs.
%% @spec oneof(Gs::list(domain()))  -> domain()
oneof(Gs) when is_list(Gs) ->
    #?DOM{kind={oneof, Gs, length(Gs)},
	  generate=fun(#?DOM{kind={oneof,Gs1,Len}},GS) ->			  
			   generate(lists:nth(random:uniform(Len), Gs1), GS)
		   end}.

%% @doc Returns the domain of integers in the range M =&lt; X =&lt; N
%% @spec choose(M,N) -> domain()
choose(M,N) when is_integer(M), is_integer(N), M<N ->
    #?DOM{kind={choose,M,N},
	  generate=fun(_,_) ->
			   random:uniform(N-M+1) - 1 + M
		   end,
	  simplify=fun(_Dom,Val) when Val>M -> 
			   Val-1;
		      (_Dom,_) -> 
			   M
		   end}.

%% @doc Returns the doamin of Val.
%% @spec return(Val) -> domain()
return(Val) -> 
    #?DOM{kind={return, Val},
	  generate  = fun(_,_) -> Val end
	 }.

%% @doc Returns a list of length Len of values generated by G.
%% @spec vector(Len::integer(), G::domain()) -> domain()
vector(Len, G) when Len > 0 ->
    #?DOM{kind={vector, G, Len},
	  generate=fun(#?DOM{kind={vector,G1,Len1}},GS) ->			  
			   [generate(G1, GS) || _<-lists:seq(1,Len1)]
		   end}.
