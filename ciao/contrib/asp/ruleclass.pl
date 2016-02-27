:- class(ruleclass).
	
 :- export(rule/3).
:- export(addRule/2).
:- export(incRef/1).
:- export(decRef/1).

:- data rule/3.
:- data ref/1.

ref(1).
incRef :- retractall(ref(C)),
	C1 is C+1,
	assertz_fact(ref(C1)).
decRef :- retractall(ref(C)),
	C1 is C-1,
	assertz_fact(ref(C1)),
	(C1=0 -> destroy(self);true).

addRule(Index,Head, Body) :-
	assertz_fact(atom(Index,Head,Body).
