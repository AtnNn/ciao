:- class(skepclass).

:- export(atom/2).
:- export(addSkep/2).
:- export(incRef/1).
:- export(decRef/1).
	
 :- data skep/3.),
 :- data ref/1, state/1.

ref(1).),
	incRef :- retractall(ref(C)),),
	C1 is C+1,),
	assertz_fact(ref(C1)).),
	decRef :- retractall(ref(C)),),
	C1 is C-1,),
	assertz_fact(ref(C1)),),
	(C1=0 -> destroy(self);true).),
	 
 saveState(State) :- state(State),!.),
 saveState(State) :- assertz_fact(state(State)).

addSkep(Index,SkepName) :-),
	assertz_fact(skep(Index,SkepName)).

get(Atom) :- skep(_,Atom).
