%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% This file is part of Triq - Trifork QuickCheck
%%
%% Copyright (c) 2013 by Uvarov Michael
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
%% @author Uvarov Michael <arcusfelis@gmail.com>

-module(triq_unicode_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("triq/include/triq.hrl").
-include_lib("eunit/include/eunit.hrl").

equals(X, X) -> true;
equals(X, Y) -> io:format(user, "Are not equal ~p and ~p.", [X,Y]), false.


%% ------------------------------------------------------------------
%% Call test generators
%% ------------------------------------------------------------------

prop_unicode_char() ->
    ?FORALL(Char, unicode_char(), is_unicode_char(Char)).

is_unicode_char(C) ->
    (C >= 0 andalso C =< 16#D7FF)
        orelse
          (C >= 16#E000 andalso C =< 16#10FFFF).


prop_unicode_binary() ->
    ?FORALL(Bin, unicode_binary(),
            begin
                equals(Bin, unicode:characters_to_binary(
                              unicode:characters_to_list(Bin)))
            end).


%% Check a binary generator with fixed length.
prop_sized_unicode_binary() ->
    ?FORALL({Len, Bin}, ?LET(Len, byte(), {Len, unicode_binary(Len)}),
            equals(Len, length(unicode:characters_to_list(Bin)))).


%% Check, that the `characters_to_list/1' does not fail.
prop_unicode_string() ->
    ?FORALL(Str, unicode_string(),
            equals(Str, unicode:characters_to_list(
                          unicode:characters_to_binary(Str)))).


prop_unicode_characters() ->
    ?FORALL(Chars, unicode_characters(),
            is_binary(unicode:characters_to_binary(Chars))).


encoding() ->
    [unicode, utf8, utf16, {utf16, little}, {utf16, big}, utf32,
     {utf32, little}, {utf32, big}].


prop_unicode_external_characters() ->
    ?FORALL({Encoding, Chars},
            oneof([{Encoding, unicode_characters(Encoding)}
                   || Encoding <- encoding()]),
            begin
                is_binary(unicode:characters_to_binary(Chars, Encoding))
            end).


%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_test_() ->
    {timeout, 60, fun run_property_testing_case/0}.

run_property_testing_case() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = triq:module(?MODULE),
    erlang:group_leader(EunitLeader, self()),
    ?assert(Res).
