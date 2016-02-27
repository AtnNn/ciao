:- module(codecoverage_tr, [
		put_coverpoints/3,
		damm_assertions/3
	    ], [assertions]).
:- use_module(library(codecoverage(codecoverage_rt)), [
		reset_coverage/1
	    ]).
:- use_module(library(compiler(c_itf_internal)),      [
		location/1
	    ]).

:- doc(title, "Code Coverage Transformation").

:- doc(author, "Santiago D@'{i}ez P@'{e}rez").

:- doc(summary, "This module does de transformation
	needed to do the coverage analysis.").

:- data pos/4.

next_clause(Module, PredKey) :-
	( retract_fact(pos(Module, PredKey, Cls0, _)) ->
	    Cls is Cls0 + 1;
	    Cls = 1 ),
	asserta_fact(pos(Module, PredKey, Cls, 1)).

current_pos(pos(Module, PredKey, Cls, Lit)) :-
	retract_fact(pos(Module, PredKey, Cls, Lit)),
	Lit1 is Lit + 1,
	asserta_fact(pos(Module, PredKey, Cls, Lit1)).

:- data coverpoints/1.

% put_coverpoints(Old,New,Module) : does the transformation
%	  
put_coverpoints(0, _, Module) :- !,
	reset_coverage(pos(Module, _, _, _)),
	retractall_fact(pos(_, _, _, _)),
	retractall_fact(coverpoints(_)),
	asserta_fact(coverpoints([])),
	fail.

put_coverpoints((:- _), _, _) :- !, fail.

put_coverpoints(end_of_file, NewEOF, _Module) :-
	!,
	retractall_fact(pos(_, _, _, _)),
	retract_fact(coverpoints(L)),
	NewEOF = [(:- initialization(init_coverage(L))),end_of_file].


put_coverpoints((Head :- Body), (Head :- NewBody), Module)
:- !,
	functor(Head, PredName, PredArity),
	PredKey = PredName/PredArity,
	next_clause(Module, PredKey), % position handling
	process_one_body(Body, NewBody).

put_coverpoints(Head, (Head :- NewBody), Module) :- !,
	functor(Head, PredName, PredArity),
	PredKey = PredName/PredArity,
	next_clause(Module, PredKey),
	current_pos(Position),
	location(Location),
	retract_fact(coverpoints(Cps)),
	assertz_fact(coverpoints([coverpoint(Position, Location)|Cps])),
	NewBody = '$COVER_TERM'(Position).

damm_assertions(clause(0,0),_,_) :-
	!, fail.

%damm_assertions(Clause,[clause('$COVERPOINTS'(L),_),Clause],_) :-
%	retract_fact(coverpoints(L)).

% process_one_body(OldBody,NewBody)
%
process_one_body(OldBody, (NewLit, NewLits)) :-
	nonvar(OldBody),
	OldBody = (OldLit, OldLits),
	process_lit(OldLit, NewLit),
	process_one_body(OldLits, NewLits).
process_one_body(OldLit, NewLit) :-
	process_lit(OldLit, NewLit).

% process_lit(OldLit, NewLit)
%
process_lit(Lit, (NewA -> NewB)) :-
	nonvar(Lit),
	Lit = (A -> B),
	process_one_body((A), NewA),
	process_one_body((B), NewB).
process_lit(Lit, (NewA;NewB)) :-
	nonvar(Lit),
	Lit = (A;B),
	process_one_body((A), NewA),
	process_one_body((B), NewB).
process_lit(Lit, if(NewA, NewB, NewC)) :-
	nonvar(Lit),
	Lit = if(A, B, C),
	process_one_body((A), NewA),
	process_one_body((B), NewB),
	process_one_body((C), NewC).
process_lit(Lit, NewLit) :-
	current_pos(Position),
	location(Location),
	retract_fact(coverpoints(Cps)),
	assertz_fact(coverpoints([coverpoint(Position, Location)|Cps])),
	NewLit = ('$COVER_TERM'(Position), Lit).
