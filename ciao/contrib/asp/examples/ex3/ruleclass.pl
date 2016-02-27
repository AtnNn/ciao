:- class(ruleclass).

:- use_package([objects]).

:- export(rule/3).
:- export(addRule/3).
:- export(incRef/0).
:- export(decRef/0).

:- data rule/3.
:- data ref/1.

ref(0).
incRef :- ref(C),!,retractall_fact(ref(_)),
	C1 is C+1,
	assertz_fact(ref(C1)).
incRef :- assertz_fact(ref(0)).
decRef :- ref(C), retractall_fact(ref(_)),
	C1 is C-1,
	assertz_fact(ref(C1)),
	(C1=0 -> self(SELF),destroy(SELF);true).
addRule(Index,Head, Body) :-
	assertz_fact(rule(Index,Head,Body)).

