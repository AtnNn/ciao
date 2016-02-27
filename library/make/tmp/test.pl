:- module(_,_,[]).

:- use_module(library(compiler),[use_module/1]).
:- use_module(library('make/system_extra')).
:- use_module('/home/herme/lpmake/make/tmp/aux').

test1(X) :- 
        use_module(X),
	display('** hello\n'),
	call_unknown(_:mygoal),
        fail.

test2(X) :- 
        use_module(X),
	display('** hello\n'),
        inaux,
        fail.

test3(X) :- 
	loadaux(X),
 	display('** hello\n'),
        inaux,
        fail.

cu :- call_unknown(_:mygoal).
