:- module(database, [asserta/1, assertz/1, clause/2, retract/1,
		     abolish/1, current_predicate/1, copy_term/2]).

:- use_module(err, [err_check/2]).
:- use_module(execution, [convert_to_goal/2]).
:- use_module(lists, [member/2]).

% ----------------
asserta((Head :- Body)) :- !,
	asserta_conv(Head, Body).
asserta(Head):-
	asserta2(Head, true).

asserta_conv(Head, Body) :-
	convert_to_goal(Body, Body1), !,
	asserta2(Head, Body1).
asserta_conv(Head, Body) :-
	err_check(asserta((Head :- Body)),
		   [conv_to_goal(Body)]).

asserta2(Head, Body) :-
	'$asserta'(Head, [Body], _), !.
asserta2(Head, _Body) :-
	err_check(asserta(_),
		   [inst(Head),
		    callable(Head),
		    static_procedure(Head)]).

% ----------------
assertz((Head :- Body)) :- !,
	assertz_conv(Head, Body).
assertz(Head):-
	assertz2(Head, true).

assertz_conv(Head, Body) :-
	convert_to_goal(Body, Body1), !,
	assertz2(Head, Body1).
assertz_conv(Head, Body) :-
	err_check(assertz((Head :- Body)),
		   [conv_to_goal(Body)]).

assertz2(Head, Body) :-
	'$assertz'(Head, [Body], _), !.
assertz2(Head, _Body) :-
	err_check(assertz(_),
		   [inst(Head),
		    callable(Head),
		    static_procedure(Head)]).

% ----------------
nth_clause_try(Ref, Clause, Clause, Ref).
nth_clause_try(Ref, Clause, _, OutRef):-
	Ref1 is Ref+1,
	nth_clause(Ref1, Clause, OutRef).

nth_clause(Ref, Clause, OutRef) :-
	'$clause'(Ref, Head, Body),
	nth_clause_try(Ref, Clause, Head-Body, OutRef).

clause(Head, Body) :-
	( var(Body) ; callable(Body) ),
	'$clauselist'(Head, Ref), !,
	nth_clause(Ref, Head-Body, _).
clause(Head, Body) :-
	err_check(clause(Head, Body),
		   [inst(Head),
		    callable(Head),
		    callable(Body),
		    private_procedure(Head)
		   ]).

% ----------------
retract((Head:-Body)) :-
	!,
	retract2(Head, Body).
retract(Head) :-
	retract2(Head, true).

retract2(Head, Body) :-
	'$clauselist'(Head, Ref), !,	
	nth_clause(Ref, Head-Body, RefOut),
	'$retract'(RefOut).
retract2(Head, _Body) :-
	err_check(retract(_),
		   [inst(Head),
		    callable(Head),
		    static_procedure(Head)
		   ]).
	
% ----------------
retractall(Clause) :-
        retract(Clause), fail.
retractall(_).

% ----------------
abolish(Name/Arity) :-
	functor(Funct, Name, Arity),
	'$pred_info'(Funct, Type, _),
	( Type = pred_dynamic ; Type = []),
	'$abolish'(Funct), !.
abolish(X) :-
	err_check(abolish(X),
		   [inst(X),
		    pred_ind(X),
		    do(X = Name/Arity),
		    inst(Name),
		    inst(Arity),
		    integer(Arity),
		    nonneg(Arity),
		    max_arity(Arity),
		    do(functor(Funct, Name, Arity)),
		    static_procedure(Funct)]).

% ----------------
current_predicate(PredInd) :-
	check_pred_ind(PredInd), !,
	list_predicates(0, Preds),
	member(PredInd, Preds).
current_predicate(X) :-
	err_check(current_predicate(X), 
	          [pred_ind_strict(X)]).
	
list_predicates(N, Preds) :-
	'$nth_functor'(N, Funct), !,
	'$pred_info'(Funct, Type, Access),
	is_predicate(Access, Type, Funct, Preds, Preds1),
	N1 is N + 1,
	list_predicates(N1, Preds1).
list_predicates(_, []).

is_predicate(access_user, Type, Funct, Preds, Preds1) :-
	( Type = pred_static ; Type = pred_dynamic ), !,
	functor(Funct, Name, Arity),
	Preds = [Name/Arity|Preds1].
is_predicate(_, _, _, Preds, Preds).

check_pred_ind(X) :-
	var(X), !.
check_pred_ind(N/A) :-
	( var(N) ; atom(N) ),
	( var(A) ; integer(A), A >= 0, A < 256 ).

% ----------------
copy_term(Term1, Term2) :-
	   asserta('$copied'(Term1)),
	   retract('$copied'(Term)),
	   Term = Term2.
