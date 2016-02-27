:- module(codecoverage_utils,
	    [
		program_point/4,
		is_covered/4,
		show_coverage_statistics/0,
		show_coverage_statistics/1,
		show_coverage_status/0,
		show_coverage_status/1,
		reset_coverage/0,
		reset_coverage/4
	    ],
	    [
		assertions
	    ]
	).

:- use_module(codecoverage_rt, [
		get_coverage_info/3,
		reset_coverage/1,
		get_clauses/3,
		spec/1
	    ]).
:- use_module(library(aggregates),
	    [
		findall/3
	    ]).
:- use_module(library(lists),
	    [
		append/3
	    ]).
:- use_module(library(format),
	    [
		sformat/3
	    ]).

:- doc(title,  "Code Coverage Utils").
:- doc(author, "Santiago D@'{i}ez P@'{e}rez").
:- doc(module, "This module implements the
	high-level predicates that let the user interact 
	with the codecoverage module").


:- pred program_point(Module, Predicate, Clause, Literal)
	: ( compat(Module, atm), compat(Predicate, term),
	    compat(Clause, nnegint), compat(Literal, nnegint) )
        => atm * spec * nnegint * nnegint
        # "Gives all the program points on backtracking".

program_point(Module,Predicate,Clause,Literal) :-
	get_coverage_info(pos(Module,Predicate,Clause,Literal),_,_).


:- pred is_covered(Module, Predicate, Clause, Literal)
	: ( compat(Module, atm), compat(Predicate, term),
	    compat(Clause, nnegint), compat(Literal, nnegint) )
        => atm * spec * nnegint * nnegint
        # "Success if
	@var{Module}/@var{Predicate}/@var{Clause}/@var{Literal} is
	covered. @var{Predicate} is a term with a 'PredName/Arity' form.".

is_covered(Module, Pred, Clause, Literal) :-
	get_coverage_info(pos(Module, Pred, Clause, Literal), _L, St),
	( St \== true -> !),
	fail.

is_covered(Module, Pred, Clause, Literal) :-
	get_coverage_info(pos(Module, Pred, Clause, Literal), _L, true).



:- pred show_coverage_statistics # "Shows some basic coverage 
 	statistics for all the modules.".

show_coverage_statistics :-
	show_coverage_statistics(_Module),
	fail.
show_coverage_statistics.

:- pred show_coverage_statistics(Module)
	: ( compat(Module,atm) )
	# "Shows some basic coverage 
 	statistics for @var{Module}.".

show_coverage_statistics(Module) :-
	get_coverage_stats(Module, CovCls, TotCls, CovLits, TotLits),
	PerctgCls is (CovCls/TotCls) * 100,
	sformat(ClsS, "~2f", PerctgCls),
	PerctgLits is (CovLits/TotLits) * 100,
	sformat(LitsS, "~2f", PerctgLits),
	messages([
		note(['Code coverage statistics (', Module, ')']),
		note(['Clauses: ', $$(ClsS),
			'% covered (', CovCls, '/', TotCls, ')']),
		note(['Literals : ', $$(LitsS),
			'% covered (', CovLits, '/', TotLits, ')']
		)
	    ]).
show_coverage_statistics(_Module).

:- pred show_coverage_status #
 	"Shows the current status of the coverage for all the modules.".

show_coverage_status :-
	findall(info(M, P, Clauses),
	    get_clauses(M, P, Clauses), Info),
	info2messages(Info, Covered, Uncovered),
	messages(Covered),
	messages(Uncovered).

:- pred show_coverage_status(Module)
	: ( compat(Module,atm) ) 
        # "Shows the current status of the coverage for @var{Module}.".

show_coverage_status(Module) :-
	findall(info(Module, P, Clauses),
	    get_clauses(Module, P, Clauses), Info),
	info2messages(Info, Covered, Uncovered),
	messages(Covered),
	messages(Uncovered).

:- pred reset_coverage 
	# "Resets the coverage information 
	  for all the modules.".

reset_coverage :-
	reset_coverage(pos(_M, _P, _C, _L)).

:- pred reset_coverage(Module, Predicate, Clause, Literal) 
	: ( compat(Module, atm), compat(Predicate, term),
	    compat(Clause, nnegint), compat(Literal, nnegint) )
 	# "Resets the coverage information for the specified position
	  @var{Module}/@var{Predicate}/@var{Clause}/@var{Literal}.".

reset_coverage(Module, Pred, Clause, Literal) :-
	reset_coverage(pos(Module, Pred, Clause, Literal)).

info2messages([],       [],      []).
info2messages([I|Info], Covered, Uncovered) :-
	I = info(Mod, Pred, Clauses),
	clause_message(Mod, Pred, Clauses, Cover, Uncover),
	append(Cover,   Cover1,   Covered),
	append(Uncover, Uncover1, Uncovered),
	info2messages(Info, Cover1, Uncover1).

clause_message(_Mod, _Pred, [],     [],      []).
clause_message(Mod,  Pred,  [C|Cs], Covered, Uncovered) :-
	get_coverage_info(pos(Mod, Pred, C, 1), Loc, _),
	findall(Lit, get_coverage_info(pos(Mod, Pred, C, Lit), _, false), Lits
	),
	Loc = loc(S, L0, L1),
	( Lits == [] ->
	    M = message_lns(S, L0, L1, note, [Mod, ':', ''(Pred),
		    ' clause ', C, ' covered']),
	    Covered = [M|Cov1],
	    Uncovered = Uncov1;
	    M = message_lns(S, L0, L1, warning, [Mod, ':', ''(Pred),
		    ' clause ', C, ' has uncovered literals ', Lits]),
	    Covered = Cov1,
	    Uncovered = [M|Uncov1] ),
	clause_message(Mod, Pred, Cs, Cov1, Uncov1).

:- data coverage_stats/5.

get_coverage_stats(Mod, _CovCls, _TotalCls, _CovLits, _TotalLits)
:- get_coverage_info(pos(Mod, Pred, Cl, 1), _, _), % all the clauses
	( retract_fact(coverage_stats(Mod, CovC, TotC, CovL, TotL)) -> true;
	    CovC =0, TotC = 0, CovL = 0, TotL = 0 ),
	get_coverage_clause_stats(Mod, Pred, Cl, S, CCL, CTL),
	(S == true -> CovC1 is CovC + 1; CovC1 = CovC),
	TotC1 is TotC + 1,
	CovL1 is CovL + CCL,
	TotL1 is TotL + CTL,
	asserta_fact(coverage_stats(Mod, CovC1, TotC1, CovL1, TotL1)),
	fail.
get_coverage_stats(Mod, CovCls, TotalCls, CovLits, TotalLits) :-
	retract_fact(coverage_stats(Mod, CovCls, TotalCls, CovLits,
		TotalLits)).


:- data coverage_cl_stats/5.

get_coverage_clause_stats(Mod, Pred, Cl, _Status, _CovLits,
	    _TotalLits) :-
	asserta_fact(coverage_cl_stats(Mod, Pred, Cl, 0, 0)),
	get_coverage_info(pos(Mod, Pred, Cl, _L), _, S),
	retract_fact(coverage_cl_stats(Mod, Pred, Cl, CovLits, TotalLits)),
	( S == false ->
	    CovLits1 = CovLits;
	    CovLits1 is CovLits + 1 ),
	TotalLits1 is TotalLits + 1,
	asserta_fact(coverage_cl_stats(Mod, Pred, Cl, CovLits1, TotalLits1)),
	fail.
get_coverage_clause_stats(Mod, Pred, Cl, Status, CovLits, TotalLits)
:- retract_fact(coverage_cl_stats(Mod, Pred, Cl, CovLits, TotalLits)
	),
	( CovLits == TotalLits -> Status = true; % clause covered
	    Status = false ).

