:- module(_,_,[]).

:- use_module(library(compiler),[use_module/1]).
:- use_module(library('make/system_extra')).

test(X) :- 
        use_module(X),
        call_unknown(_:mygoal),
        fail.
test(X).

cu :- call_unknown(_:mygoal).
