# Welcome to Triq -- Trifork QuickCheck for Erlang


[![Build Status](https://travis-ci.org/open-triq/triq.svg?branch=master)](https://travis-ci.org/open-triq/triq) 

This is a fork of Triq that is being run under the ZeroMQ Collaberation rules, http://rfc.zeromq.org/spec:22 with the one exception being that it is under the apache licence 



Triq (pronounced "Trick Check") is a free alternative to [QuviQ
eqc](http://www.quviq.com/). Triq's API is modelled closely after
`eqc`, so I recommend their tutorials and slides for an introduction
to QuickCheck.  Notice that QuviQ `eqc` has many features not found in
`triq`, but it is open source licensed under the Apache license.  For
instance, `eqc` has features for reporting, management, probably a
much better shrinking mechanism, cool C integration, and
professional support.


## Installation

To use `triq`, you download the latest version from
[here](http://github.com/open-triq/triq/), and untar it
into your erlang lib directory (typically
`/usr/local/lib/erlang/lib`):

```sh
prompt$ cd /usr/local/lib/erlang/lib
propmt$ tar xvzf triq-0.1.0.tgz
...
```

And you're all set.

Or, checkout the triq source code and soft link / copy into your Erlang lib directory:

```sh
prompt$ git clone git://github.com/open-triq/triq.git
prompt$ cd triq
prompt$ ln -s . /usr/local/lib/erlang/lib/triq-0.1.0
```

Next, to use `triq`, include the header file:

```erlang
-include_lib("triq/include/triq.hrl").
```

And you're ready to write property tests.  An example property could be:

```erlang
prop_append() ->
    ?FORALL({Xs,Ys},{list(int()),list(int())},
            lists:reverse(Xs++Ys)
            ==
            lists:reverse(Ys) ++ lists:reverse(Xs)).
```

To test this property, run `triq:check/1`, thus:

```erlang
1> triq:check(prop_append()).
......................................................................
..............................
Ran 100 tests
true
2>
```

If the test fails, it will try to shrink the result; here is an example:

```erlang
prop_delete() ->
    ?FORALL(L,list(int()),
        ?IMPLIES(L /= [],
            ?FORALL(I,elements(L),
                ?WHENFAIL(io:format("L=~p, I=~p~n", [L,I]),
                          not lists:member(I,lists:delete(I,L)))))).
```

Which runs like this:
```erlang
1> triq:check(triq_tests:prop_delete()).
x....Failed!
L=[4,5,5], I=5

Failed after 5 tests with false
Simplified:
        L = [0,0]
        I = 0
false
2>
```

You can get the values used for the failing test with `counterexample`,
and reuse the same test values with `check/2`:
```erlang
3> A = triq:counterexample(triq_tests:xprop_delete()).
x.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxFailed!
L=[3,2,1,1,1], I=1

Failed after 101 tests with false
Simplified:
	L = [0,0]
	I = 0
[[0,0],0]
4> A.
[[0,0],0]
5> triq:check(triq_tests:xprop_delete(), A).
Failed!
L=[0,0], I=0

Failed after 1 tests with false
Simplified:
	L = [0,0]
	I = 0
false
6>
```

Modules compiled with the `triq.hrl` header, auto-export all functions named `prop_*`,
and have a function added called `check/0` which runs `triq:check/1` on all the properties in the module.

```erlang
1> mymodule:check().
```

A handy addition that I use is to also add an `eunit` test, which tests it:

```erlang
property_test() -> true = check().
```
Which can then automatically be run using your favourite `eunit` runner.

Good luck!

This repository is copyright by Triq Contributors (as enumerated in
 AUTHORS).  It is licensed under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance with the
 License.  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0
       or in the file LICENSE

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

This repository also includes build scripts from Erlang Solutions,
Ltd. made available under the Apache License, Version 2.0.
