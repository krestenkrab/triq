
# Welcome to Triq -- Trifork QuickCheck for Erlang

Triq (pronounced trick) is a free alternative to [QuviQ eqc](http://www.quviq.com/). QuviQ `eqc` has many features not found in `triq`, but it is open source licensed under the Apache license.

## Installation

To use `triq`, you download the latest version from [here](http://github.com/krestenkrab/triq/downloads), and untar it into your erlang lib directory (typically `/usr/local/lib/erlang/lib`):

<pre>prompt$ cd /usr/local/lib/erlang/lib
propmt$ tar xvzf triq-0.1.0.tgz
...</pre>

And you're all set.  Next, to use `triq`, include the header file:

<pre>-include_lib("triq/include/triq.hrl").</pre>

And you're ready to write property tests.  An example property could be:

<pre>prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
            lists:reverse(Xs++Ys)
            == 
            lists:reverse(Ys) ++ lists:reverse(Xs)).</pre>

To test this property, run `triq:check/1`, thus:

<pre>1> triq:check(prop_append()).
......................................................................
..............................
Ran 100 tests
true
2> </pre>

If the test fails, it will try to shrink the result; here is an example:

<pre>prop_delete() ->
    ?FORALL(L,list(int()), 
        ?IMPLIES(L /= [],
            ?FORALL(I,elements(L), 
                ?WHENFAIL(io:format("L=~p, I=~p~n", [L,I]),
                          not lists:member(I,lists:delete(I,L)))))).
</pre>

Which runs like this:
<pre>1> triq:check(triq_tests:prop_delete()).
x....Failed!
L=[4,5,5], I=5

Failed after 5 tests with false
Simplified:
        L = [0,0]
        I = 0
false
2> </pre>

Modules compiled with the `triq.hrl` header, auto-export all functions named `prop_*`, 
and have a function added called `check/0` which runs `triq:check/1` on all the properties in the module.

    1> mymodule:check().

A handy addition that I use it to also add an `eunit` test, which tests it:

    property_test() -> true == check().

Which can then automatically be run using your favourite `enuit` runner. 

Good luck!


