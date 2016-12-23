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

-module(triq).

%%
%% For each ?FORALL, we try to shrink the value
%% this many iterations.
%%
-define(SHRINK_COUNT, 1000).

%%
%% The default number of tests to run
%%
-define(TEST_COUNT, 100).

-export([check/1,
         check/2,
         check/3,
         fails/1,
         module/1,
         module/2,
         counterexample/0,
         counterexample/1,
         numtests/2]).

-import(triq_dom,
        [pick/2,
         shrink/2]).

-record(triq, {count=0,
               context=[],
               size=?TEST_COUNT,  %% todo: remove this
               run_iter=?TEST_COUNT,
               report= fun report_none/2,
               shrinking= false,
               result=undefined,
               body,
               values=[]}).

%%
%% Default reporting function, ... is silent
%%
report_none(pass, _) ->
    ok;
report_none(fail, _) ->
    ok;
report_none(skip, _) ->
    ok.

%%
%% Reporting function used while testing, prints "..xxxx Failed!"
%%
report(pass,_) ->
    io:format(".");
report(skip,_) ->
    io:format("x");
report(fail,false) ->
    io:format("Failed!~n");
report(fail,Value) ->
    io:format("Failed with: ~p~n", [Value]).

%%
%%
%%
check_input(Fun,Input,IDom,#triq{count=Count,report=DoReport}=QCT) ->
    try Fun(Input) of
        true ->
            DoReport(pass,true),
            {success, Count+1};

        {success, NewCount} ->
            {success, NewCount};

        {failure, _, _, _, _}=Fail ->
            Fail;

        {'prop:timeout', Limit, Fun2, Body2} ->
            Yield = check_timeout(Fun,Input,IDom,Limit,Fun2,
                                  QCT#triq{body=Body2}),
            Yield;

        {'prop:fails', Property} ->
            case check_input(fun(none)->Property end,none,none,QCT#triq{}) of
                {success, _} ->
                    {failure, Fun, Input, IDom,
                     QCT#triq{result=unexpected_success,
                              context=[{"?",Fun,Input,IDom}
                                       |QCT#triq.context]}};
                _ -> {success, Count+1}
            end;

        {'prop:implies', false, _, _, _} ->
            DoReport(skip,true),
            {success, Count};

        {'prop:implies', true, _Syntax, Fun2, Body2} ->
            check_input(fun(none)->Fun2()end,none,none,QCT#triq{body=Body2});

        {'prop:numtests', Iters, Property} ->
            check_input(fun(none)->Property end,none,none,
                        QCT#triq{ run_iter=Iters });

        {'prop:whenfail', Action, Fun2, Body2} ->
            case check_input(fun(none)->Fun2()end,none,none,
                             QCT#triq{body=Body2}) of
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
            Main = self(),
            PID = spawn_link
                    (fun() ->
                             Result = check_input(fun(none)->Fun2()end,none,
                                                  none,QCT#triq{body=Body2}),
                             Main ! {self(), Result}
                     end),
            receive
                {PID, Result} ->

                    %% unlink and flush any EXITs
                    unlink(PID),
                    process_flag(trap_exit, WasTrap),
                    receive {'EXIT', PID, _} -> true
                    after 0 -> true end,

                    Result;

                {'EXIT', PID, Reason} ->
                    process_flag(trap_exit, WasTrap),
                    DoReport(fail, Reason),
                    {failure, Fun, Input, IDom,
                     QCT#triq{count=Count+1,result={'EXIT', Reason}}}

            end;

        {'prop:forall', Dom2, Syntax2, Fun2, Body2} ->
            check_forall(0, QCT#triq.run_iter, Dom2, Fun2, Syntax2,
                         QCT#triq{body=Body2});

        Any ->
            DoReport(fail,Any),
            {failure, Fun, Input, IDom, QCT#triq{count=Count+1,result=Any}}

    catch
        Class : Exception ->
            DoReport(fail, {Class, Exception, erlang:get_stacktrace()}),
            {failure, Fun, Input, IDom, QCT#triq{count=Count+1,
                                                 result={'EXIT',Exception}}}

    end.


check_timeout(Fun,Input,IDom,Limit,Fun2,
              #triq{count=Count,report=DoReport}=QCT) ->
    Main = self(),
    Controller =
        spawn
          (fun() ->
                   process_flag(trap_exit, true),
                   Controller = self(),

                   Slave = spawn_link
                             (fun() ->
                                      Slave = self(),
                                      Result = check_input(fun(none)->Fun2()end,
                                                           none,
                                                           none,
                                                           QCT),
                                      Controller ! {Slave, Result}
                              end),

                   receive
                       {Slave, Result} ->
                           %% from Slave
                           Main ! {Controller, Result };

                       {'EXIT', Slave, Reason} ->
                           %% from Slave
                           DoReport(fail, Reason),
                           Main ! {Controller,
                                   {failure, Fun, Input, IDom,
                                    QCT#triq{count=Count+1,
                                             result={'EXIT', Reason}}}};

                       {'EXIT', _, timeout} ->
                           %% from Main
                           erlang:exit(Slave,kill)
                   end
           end),

    Yield = receive
                {Controller, Result} ->
                    Result

            after Limit ->

                    %% Yank the controller (and the slave)
                    erlang:exit(Controller, timeout),

                    %% flush any reply from our queue
                    receive {Controller, _} -> ignore
                    after 5 -> ignore end,

                    Reason = {timeout, Limit},
                    DoReport(fail, Reason),
                    {failure, Fun, Input, IDom,
                     QCT#triq{count=Count+1,result={'EXIT', Reason}}}
            end,

    Yield.

check_forall(N,N,_,_,_,#triq{count=Count}) ->
    {success, Count};
check_forall(N,NMax,Dom,Fun,Syntax,#triq{context=Context,values=Values}=QCT) ->

    DomSize = 2 + 2*N,

    {{InputDom,Input},NewValues} =
        case Values of
            [V|Vs] ->
                {{V, V}, Vs};
            [] ->
                {pick(Dom, DomSize), []}
        end,

    case check_input(Fun,Input,InputDom,
                     QCT#triq{size=DomSize,
                              context=[{Syntax,Fun,Input,InputDom}|Context],
                              values=NewValues})
    of

        %% it did not fail, try again with N := N+1
        {success,NewCount} ->
            check_forall(N+1, NMax, Dom, Fun, Syntax, QCT#triq{count=NewCount});

        %% it failed, report it!
        {failure, _, _, _, Ctx} ->
            {failure, Fun, Input, InputDom, Ctx}
    end.


all(_Fun,[]) ->
    true;
all(Fun,[H|T]) ->
    case Fun(H) of
        true -> all(Fun,T);
        NonTrue ->
            NonTrue
    end.


%%--------------------------------------------------------------------
%% @doc
%% Run QuickCheck on all properties in a module.
%% If all checks succeed, true is returned; otherwise return the
%% result of the first check that fails.
%%
%% @spec module( atom() ) -> true | any()
%% @end
%%--------------------------------------------------------------------
module(Module) when is_atom(Module) ->
    module(Module, ?TEST_COUNT).
    
module(Module, RunIters) when is_integer(RunIters), RunIters>0 ->
    Info = Module:module_info(exports),
    all(fun({Fun,0}) ->
                case atom_to_list(Fun) of
                    "prop_" ++ _ ->
                        io:format("Testing ~p:~p/0~n", [Module, Fun]),
                        check(Module:Fun(), RunIters);
                    _ -> true
                end;
           ({_,_}) -> true
        end,
        Info).


%%--------------------------------------------------------------------
%% @doc
%% Run QuickCheck.  If argument is an atom, it runs triq:module/1
%% checking all the properties in said module; otherwise if the
%% argument is a property, it runs QuickCheck on said property.
%%
%% @spec check( atom() | property() ) -> any()
%% @end
%%--------------------------------------------------------------------
check(Module) when is_atom(Module)->
    module(Module);
check(Property) ->
    check(Property, [], ?TEST_COUNT).

check(Module, RunIters) when is_atom(Module), is_integer(RunIters), RunIters>0 ->
    module(Module, RunIters);
check(Property, RunIters) when is_integer(RunIters), RunIters>0 ->
    check(Property, [], RunIters);
check(Property, CounterExample) when is_list(CounterExample) ->
    check(Property, CounterExample, ?TEST_COUNT).


%%--------------------------------------------------------------------
%% @doc
%% Run QuickCheck on a property, specifying a specific example to test.
%% The example can be obtained by calling {@link counterexample/0}.
%%
%% @spec check( property(), [any()], integer() ) -> any()
%% @end
%%--------------------------------------------------------------------
check(Property, Counterexample, RunIters) ->
    generate_randomness(),
    case check_input(fun(nil)->Property end,
                     nil,
                     nil,
                     #triq{report=fun report/2, run_iter=RunIters,
                           values=Counterexample}) of

        {failure, Fun, Input, InputDom, #triq{count=Count,context=Ctx,
                                              body=_Body,result=Error}} ->

            io:format("~nFailed after ~p tests with ~p~n", [Count,Error]),

            %%
            %% Context is a [{Syntax,Fun,Input,Domain}...] list
            %% one element for each ?FORALL level in the property.
            %% the check/5 function constructs in backwards, so...
            %%
            Context = lists:reverse(Ctx),

            %% Run the shrinking function
            %%
            Simp = shrink_loop(Fun,Input,InputDom,?SHRINK_COUNT,tl(Context)),

            %%
            %% Compute the counter example
            %%
            CounterExample = [{Syntax,Fun2,SimplifiedInput,Dom2} ||
                                 {{Syntax,Fun2,_Input,Dom2}, SimplifiedInput}
                                     <- lists:zip(Context,Simp)],

            %% save the counter example
            put('triq:counterexample', CounterExample),

            io:format("Simplified:~n"),
            print_counter_example(CounterExample),

            Error;

        {success, Count} ->
            io:format("~nRan ~p tests~n", [Count]),
            true
    end.

print_counter_example(CounterExample) ->
    lists:foreach(fun({Syntax,_Fun,Val,_Dom}) ->
                          io:format("\t~s = ~w~n", [Syntax,Val])
                  end,
                  CounterExample).

counterexample(Prop) ->
    case check(Prop) of
        true -> true;
        _ -> counterexample()
    end.

counterexample() ->
    [ Val || {_,_,Val,_} <- get('triq:counterexample') ].

%%
%% when the property has nested ?FORALL statements,
%% this is the function that tries to make the inner
%% ?FORALL smaller; after trying the outer.
%%
shrink_deeper(Input,[{_,F1,I1,G1}|T]) ->
    [Input | shrink_loop(F1,I1,G1,?SHRINK_COUNT,T)];
shrink_deeper(Input,[]) -> [Input].


%% this is the main logic for the simplify function
shrink_loop(Fun,Input,InputDom,GS,Context) ->
    InitialTested = gb_sets:add(Input,gb_sets:new()),
    shrink_loop(Fun,Input,InputDom,GS,Context, InitialTested).

shrink_loop(_,Input,_,0,Context,_) ->
    shrink_deeper(Input,Context);

shrink_loop(Fun,Input,InputDom,GS,Context,Tested) ->
    %%
    %% simplify_value will attempt to shrink the
    %% value of Input (beloging to the InputDom domain).
    %% There is randomness involved, so it may just
    %% return it's Input argument...
    %%
    {NewDom,NewInput} = shrink(InputDom,Input),

    %%io:format("simp ~p -> ~p (~p)~n", [Input, NewInput, InputDom]),

    IsTested = gb_sets:is_member(NewInput,Tested),

    if
        IsTested ->
            %% aparently, there was some randomness in the
            %% shrinking that made us shrink again to a value
            %% we shrunk to before.
            shrink_loop(Fun,Input,InputDom,GS-1,Context,Tested);

        Input =:= NewInput ->
            shrink_deeper(Input, Context);

        true ->
            NewTested = gb_sets:add(NewInput,Tested),

            case check_input(Fun,NewInput,NewDom,
                             #triq{size=GS,shrinking=true}) of

                %% still failed, try to simplify some more
                {failure, _, _, _, #triq{context=C2}} ->
                    shrink_loop(Fun,NewInput,NewDom,GS,C2,NewTested);

                %% oops, we simplified too much; try again
                %% with the same inputs
                {success, _} ->
                    shrink_loop(Fun,Input,InputDom,GS-1,Context,NewTested)
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% A Property which succeeds when its argument fails, and fails
%% if the argument succeeds.  This is very handy for properties
%% that <em>should fail</em>.
%%
%% @spec fails( property() ) -> property()
%% @end
%%--------------------------------------------------------------------
fails(Prop) ->
    {'prop:fails', Prop}.
numtests(Num,Prop) ->
    {'prop:numtests', Num, Prop}.

%%
%% 12 crypto-safe random bytes to seed erlang random number generator
%%
generate_randomness() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {A, B, C}).
