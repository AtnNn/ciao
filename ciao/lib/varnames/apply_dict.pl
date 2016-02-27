:- module(apply_dict, [apply_dict/3, select_applicable/3, select_applicable/4],
	    [assertions, basicmodes, dcg, hiord, fsyntax]).

:- use_module(library(varnames(dict_types))).

sel_member(E, Applicable) -->
	{member(E0, Applicable)},
	{E0 == E},
	!,
	[E].
sel_member(_, _) --> [].

sel_members([],    _) --> [].
sel_members([E|L], R) -->
	sel_member(E, R),
	sel_members(L, R).

:- pred select_applicable(?term, +varnamesl, -varnamesl).

select_applicable(Term, Dict, Applicable) :-
	select_applicable(Term, Dict, Applicable0, []),
	sel_members(Dict, Applicable0, Applicable, []). % To remove duplicates

select_applicable(Term, Dict) -->
	( {var(Term)} -> select_applicable_var(Term, Dict)
	; {functor(Term, _, N)},
	    select_applicable_args(N, Term, Dict) ).

select_applicable_var(Var, Dict) -->
	{member(Name = Value, Dict)},
	{Var == Value} ->
	[Name = Value]
    ;
	[].

select_applicable_args(0, _,    _) --> [], !.
select_applicable_args(N, Term, Dict) -->
	{arg(N, Term, Arg)},
	select_applicable(Arg, Dict),
	!,
	{N1 is N - 1},
	select_applicable_args(N1, Term, Dict).

:- pred apply_dict(?term, +varnamesl, ?term).

apply_dict(Var0, Dict, Var) :-
	var(Var0),
	!,
	(
	    member(Name = Value, Dict),
	    Var0 == Value ->
	    ( var(Name) -> Var = '$VAR'('_')
	    ; Var = '$VAR'(Name)
	    )
	;
	    Var = Var0
	).
apply_dict('$VAR'(Term0), Dict, '$VAR'(Term)) :-
	!,
	( atom(Term0) ->
	    atom_concat('\'',  Term0, Term1),
	    atom_concat(Term1, '\'',  Term2),
	    Term = '$VAR'(Term2)
	;
	    apply_dict(Term0, Dict, Term)
	).
apply_dict(Term0, Dict, Term) :-
	functor(Term0, F, A),
	functor(Term,  F, A),
	apply_dict_args(A, Term0, Dict, Term).

apply_dict_args(0, _,     _,    _) :- !.
apply_dict_args(N, Term0, Dict, Term) :-
	arg(N, Term0, Arg0),
	arg(N, Term,  Arg),
	apply_dict(Arg0, Dict, Arg),
	!,
	N1 is N - 1,
	apply_dict_args(N1, Term0, Dict, Term).
