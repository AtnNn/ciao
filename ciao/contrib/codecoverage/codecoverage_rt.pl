:- module(codecoverage_rt,
	    [
		'$COVER_TERM'/1,
		get_coverage_info/3,
		get_clauses/3,
		reset_coverage/1,
		init_coverage/1,
		spec/1
	    ],
	    [assertions,
		regtypes]).

:- use_module(library(aggregates),
	    [
		setof/3
	    ]).

:- use_module(library(compiler(c_itf_internal)), [
		location_t/1
	    ]).

:- doc(title,  "Code Coverage Rt").
:- doc(author, "Santiago D@'{i}ez P@'{e}rez").
:- doc(module, "This module implements all the predicates
	that handle the code coverage database.").

% coverpoint(p(Module,PredName,ClauNum,LitNum),loc(File,Line1,Line2))

% SDP: Not used
% :- pred '$COVERPOINT'(Position, Location)
% 	: pos_t * location_t.

% :- multifile '$COVERPOINT'/2.

:- data coverpoint/2.

:- data covered/1.

:- pred '$COVER_TERM'(Position) : pos_t #
 	"Marks the literal in @var{Position} as covered.".

'$COVER_TERM'(Position) :-
	mark_as_covered(Position).

:- regtype pos_t(Position) # "@var{Position} identifies a position in
 	the code, @includedef{pos_t/1}".

pos_t(pos(Module, Predicate, Clause, Literal)) :-
	compat(Module, atm),
	compat(Predicate, term),
	compat(Clause, nnegint),
	compat(Literal, nnegint).

mark_as_covered(Position) :-
	current_fact(covered(Position)) -> true;
	assertz_fact(covered(Position)).


:- pred get_coverage_info(Position, Location, Status)
 	: ( compat(Position,pos_t) )
        => pos_t * location_t * atm
        # "Retrieves the coverage information for the literal
 	  in @var{Position}.".

get_coverage_info(Position, Location, Status) :-
	current_fact(coverpoint(Position, Location)),
	( current_fact(covered(Position)) ->
	    Status = true;
	    Status = false ).

:- pred reset_coverage(Position)
	: ( compat(Position,pos_t) )
        # "Resets the coverage information for all
	  the literals in @var{Position}.".

reset_coverage(Position) :-
	retractall_fact(covered(Position)).

:- pred get_clauses(Module, Predicate, Clauses)
 	: ( compat(Module,atm), compat(Predicate, term) )
        => ( list(Clauses,nnegint) )
        # "@var{Clauses} is the lists of clauses of the
 	  @var{Predicate} in @var{Module}.".

get_clauses(Mod, Pred, Clauses) :-
	setof(C, get_clauses_(Mod, Pred, C), Clauses).
get_clauses_(Mod, Pred, C) :-
	current_fact(coverpoint(pos(Mod, Pred, C, _), _)).

:- pred init_coverage(List) 
	: list
	# "Initializes the coverage database for @var{Module}.".

init_coverage(List) :-
	List = [coverpoint(pos(Module,_,_,_),_)|_],
	retractall_fact(covered(pos(Module, _, _, _))),
	retractall_fact(coverpoint(pos(Module, _, _, _), _)),
	assert_coverpoints(List).

assert_coverpoints([]).
assert_coverpoints([L|Ls]) :-
	asserta_fact(L),
	assert_coverpoints(Ls).

:- regtype spec(PredKey) 
	# "@var{PredKey} identifies a predicate and its arity.".

spec(PredName/Arity) :-
	compat(PredName,atm),
	compat(Arity,nnegint).
