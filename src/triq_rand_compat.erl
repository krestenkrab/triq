%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2016 Tuncer Ayaz
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

-module(triq_rand_compat).

%% API
-export([ init/0
        , init/1
        ]).

-define(DEFAULT_MODNAME, "rnd").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generate, compile and load rnd module.
%% @end
%%--------------------------------------------------------------------
-spec init() -> {'ok', module()}.
init() ->
    init(?DEFAULT_MODNAME).

%%--------------------------------------------------------------------
%% @doc
%% Generate, compile and load ModName module.
%% @end
%%--------------------------------------------------------------------
-spec init(string()) -> {'ok', module()}.
init(ModName) ->
    %% First, select the right module, then generate the appropriate code as a
    %% string, and finally compile and load it as ModName.
    Src = select(ModName),
    {ok, Mod, Bin, []} = compile(Src),
    {module, Mod} = code:load_binary(Mod, ModName++".erl", Bin),
    {ok, Mod}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Select right rand module and return wrapper module's source as string
-spec select(string()) -> string().
select(ModName) ->
    case code:which(rand) of
        non_existing ->
            src(ModName, fun funs_random/0);
        _ ->
            src(ModName, fun funs_rand/0)
    end.

%% Return module's implementation as a string.
-spec src(string(), fun(() -> binary())) -> string().
src(ModName, GenFuns) ->
    lists:flatten(
      io_lib:format(
        <<"
-module(~s).
-export([ seed/0
        , seed/1
        , uniform/0
        , uniform/1
        , uniform_s/1
        , uniform_s/2
        ]).

%% Functions
~s
">>
, [ModName, GenFuns()])).

%% random.beam wrapper
funs_random() ->
    <<"
seed()           -> random:seed().
seed(Exp)        -> random:seed(Exp).
uniform()        -> random:uniform().
uniform(N)       -> random:uniform(N).
uniform_s(St)    -> random:uniform_s(St).
uniform_s(N, St) -> random:uniform_s(N, St).
">>.

%% rand.beam wrapper
funs_rand() ->
    <<"
seed()           -> rand:seed(exsplus).
seed(Exp)        -> rand:seed(exsplus, Exp).
uniform()        -> rand:uniform().
uniform(N)       -> rand:uniform(N).
uniform_s(St)    -> rand:uniform_s(St).
uniform_s(N, St) -> rand:uniform_s(N, St).
">>.

compile(String) ->
    Forms = convert(String ++ eof, []),
    compile:forms(Forms, [return]).

%% Parse string into forms for compiler.
convert({done, {eof, _EndLocation}, _LeftOverChars}, Acc)->
    %% Finished
    lists:reverse(Acc);
convert({done, {error, ErrorInfo, _EndLocation}, _LeftOverChars}, _Acc)->
    ErrorInfo;
convert({done, {ok, Tokens, _EndLocation}, LeftOverChars}, Acc)->
    case erl_parse:parse_form(Tokens) of
        {ok, AbsForm} ->
            convert(LeftOverChars, [AbsForm|Acc]);
        {error, AbsForm} ->
            convert(LeftOverChars, AbsForm)
    end;
convert({more, Continuation}, Acc)->
    convert(erl_scan:tokens(Continuation, [], 1), Acc);
convert(String, Acc) ->
    convert(erl_scan:tokens([], String, 1), Acc).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    DefMod = list_to_atom(?DEFAULT_MODNAME),
    ok = unload(DefMod),
    ?assertMatch(false, code:is_loaded(DefMod)),
    ?assertMatch({ok, DefMod}, init()),
    ?assertMatch({file, _}, code:is_loaded(DefMod)),
    check_api(DefMod),
    CustomMod = foornd,
    CustomName = "foornd",
    ok = unload(CustomMod),
    ?assertMatch(false, code:is_loaded(CustomMod)),
    ?assertMatch({ok, CustomMod}, init(CustomName)),
    ?assertMatch({file, _}, code:is_loaded(CustomMod)),
    check_api(CustomMod).

unload(Mod) ->
    case code:is_loaded(Mod) of
        false ->
            ok;
        {file, _} ->
            code:delete(Mod),
            code:purge(Mod),
            ok
    end.

check_api(Mod) ->
    Exports = [ {seed, 0}
              , {seed, 1}
              , {uniform, 0}
              , {uniform, 1}
              , {uniform_s, 1}
              , {uniform_s, 2}
              , {module_info, 0}
              , {module_info, 1}
              ],
    ?assertMatch(Exports, Mod:module_info(exports)).

-endif.
