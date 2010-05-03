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

-module(triq).

-export([check/1,fails/1]).

-import(triq_domain, [generate/2]).

-record(triq, {count=0,
	      context=[],
	      size=100,
	      report= fun(pass,_)->ok;
			 (fail,_)->ok;
			 (skip,_)->ok end,
	      shrinking= false,
	      result=undefined,
	      body}).


report(pass,_) ->
    io:format(".");
report(skip,_) ->
    io:format("x");
report(fail,false) ->
    io:format("Failed!~n");
report(fail,Value) ->
    io:format("Failed with: ~p~n", [Value]).


check(Fun,Input,IDom,#triq{count=Count,report=DoReport}=QCT) ->

    

    try Fun(Input) of	
	true -> 
	    DoReport(pass,true),
	    {success, Count+1};
	
	{success, NewCount} -> 
	    {success, NewCount};
	
	{failure, _, _, _, _}=Fail -> 
	    Fail;

	{'prop:fails', Property} ->
	    case check(fun(none)->Property end,none,none,QCT#triq{}) of
		{success, _} ->
		    {failure, Fun, Input, IDom, QCT#triq{result=unexpected_success}};
		_ -> {success, Count+1}
	    end;
	
	{'prop:implies', false, _, _, _} ->
	    DoReport(skip,true),
	    {success, Count};
	
	{'prop:implies', true, _Syntax, Fun2, Body2} ->
	    check(fun(none)->Fun2()end,none,none,QCT#triq{body=Body2});
	
	{'prop:whenfail', Action, Fun2, Body2} ->
	    case check(fun(none)->Fun2()end,none,none,QCT#triq{body=Body2}) of
		{success, _}=Success ->
		    Success;
		Any when not QCT#triq.shrinking ->
		    Action(),
		    Any;
		Any ->
		    Any
	    end;
	
	{'prop:trapexit', Fun2, Body2} ->
	    WasTrap = process_flag(trap_exit, true),
	    Owner = self(),
	    PID = spawn_link
		    (fun() ->
			     Result = check(fun(none)->Fun2()end,none,none,QCT#triq{body=Body2}),
			     Owner ! {self(), Result}
		     end),
	    receive 
		{PID, Result} -> 

		    unlink(PID),
		    receive
			{'EXIT', PID, _} ->
			    true
		    after 0 ->
			    true
		    end,
		    process_flag(trap_exit, WasTrap),
		    Result;

		{'EXIT', PID, Reason} ->
		    process_flag(trap_exit, WasTrap),
		    DoReport(fail, Reason),
		    {failure, Fun, Input, IDom, QCT#triq{count=Count+1,result={'EXIT', Reason}}}
		    
	    end;
	
	{'prop:timeout', Limit, Fun2, Body2} ->
	    WasTrap = process_flag(trap_exit, true),
	    Owner = self(),
	    PID = spawn_link
		    (fun() ->
			     Result = check(fun(none)->Fun2()end,none,none,QCT#triq{body=Body2}),
			     Owner ! {self(), Result}
		     end),
	    receive 
		{PID, Result} -> 

		    unlink(PID),
		    receive
			{'EXIT', PID, _} ->
			    true
		    after 0 ->
			    true
		    end,
		    process_flag(trap_exit, WasTrap),
		    Result;

		{'EXIT', PID, Reason} ->
		    process_flag(trap_exit, WasTrap),
		    DoReport(fail, Reason),
		    {failure, Fun, Input, IDom, QCT#triq{count=Count+1,result={'EXIT', Reason}}}

	    after Limit ->
		    process_flag(trap_exit, WasTrap),
		    Reason = {timeout, Limit},
		    DoReport(fail, Reason),
		    {failure, Fun, Input, IDom, QCT#triq{count=Count+1,result={'EXIT', Reason}}}
		    
	    end;
	
	{'prop:forall', Dom2, Syntax2, Fun2, Body2} ->
	    check_forall(0, Dom2, Fun2, Syntax2, QCT#triq{body=Body2});

	Any ->
	    DoReport(fail,Any),
	    {failure, Fun, Input, IDom, QCT#triq{count=Count+1,result=Any}}

    catch
	Class : Exception ->
	    DoReport(fail, {Class, Exception, erlang:get_stacktrace()}),
	    {failure, Fun, Input, IDom, QCT#triq{count=Count+1,result={'EXIT',Exception}}}
	
    end.
    

check_forall(GS,_,_,_,#triq{size=GS,count=Count}) ->
    {success, Count};

check_forall(N,Dom,Fun,Syntax,#triq{size=GS,context=Context}=QCT) ->
    Input = generate(Dom,GS),

    case check(Fun,Input,Dom,QCT#triq{context=[{Syntax,Fun,Input,Dom}|Context]}) of

	{success,NewCount} -> 
	    check_forall(N+1, Dom, Fun, Syntax, QCT#triq{count=NewCount});

	{failure, _, _, _, Ctx} ->
	    {failure, Fun, Input, Dom, Ctx}

    end.




check(Module) when is_atom(Module) ->
    Info = Module:module_info(exports),
    lists:all(fun({Fun,0}) ->
		      case atom_to_list(Fun) of
			  "prop_" ++ _ ->
			      io:format("Testing ~p:~p/0~n", [Module, Fun]),
			      check(Module:Fun());
			  _ -> true
		      end;
		 ({_,_}) -> true
	      end,
	     Info);


check(Property) ->

    case check(fun(nil)->Property end, 
	       nil,
	       nil,
	       #triq{report=fun report/2}) of

	{failure, Fun, Input, InputDom, #triq{count=Count,context=Ctx,body=_Body,result=Error}} ->

	    io:format("~nFailed after ~p tests with ~p~n", [Count,Error]),

	    %%
	    %% Context is a [{Syntax,Fun,Input,Domain}...] list
	    %% one element for each ?FORALL level in the property.
	    %% the check/5 function constructs in backwards, so...
	    %%
	    Context = lists:reverse(Ctx),
	    
	    %% Run the shrinking function
	    %%
	    Simp = simplify(Fun,Input,InputDom,1000,tl(Context)),

	    io:format("Simplified:~n"),

	    lists:foreach(fun({{Syntax,_,_,_},Val}) ->
			     io:format("\t~s = ~w~n", [Syntax,Val])
			  end,
			  lists:zip(Context,Simp)),

	    Error;

	{success, Count} ->
	    io:format("~nRan ~p tests~n", [Count]),
	    true

    end
.


%%
%% when the property has nested ?FORALL statements,
%% this is the function that tries to make the inner 
%% ?FORALL smaller; after trying the outer.
%%

simplify_deeper(Input,[{_,F1,I1,G1}|T]) -> 
    [Input | simplify(F1,I1,G1,100,T)];
simplify_deeper(Input,[]) -> [Input].


%% this is the main logic for the simplify function

simplify(_,Input,_,0,Context) ->
    simplify_deeper(Input,Context);

simplify(Fun,Input,InputDom,GS,Context) ->

    %%
    %% simplify_value will attempt to shrink the
    %% value of Input (beloging to the InputDom domain).
    %% There is randomness involved, so it may just
    %% return it's Input argument...
    %%
    case triq_simplify:simplify_value(InputDom,Input) of

	%% value was unchanged, try to simplify again
	Input -> 
	    simplify(Fun,Input,InputDom,GS-1,Context);

	%% value was changed!
	NewInput ->
	    %io:format("s2 ~p -> ~p~n", [Input,NewInput]),
	    case check (Fun,NewInput,InputDom,#triq{size=GS,shrinking=true}) of
		
		%% still failed, try to simplify some more
		{failure, _, _, _, #triq{context=C2}} -> 
		    simplify(Fun,NewInput,InputDom,GS,C2);

		%% oops, we simplified too much; try again
		%% with the same inputs
		{success, _} -> 
		    simplify(Fun,Input,InputDom,GS-1,Context)
	    end
    end
.

%%
%% A Property which succeeds when its argument fails.
%%
fails(Prop) ->
    {'prop:fails', Prop}.

