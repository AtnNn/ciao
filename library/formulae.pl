
:- module(formulae,
	[ list_to_conj/3,
	  list_to_conj/2,
	  conj_to_list/2,
	  list_to_disj/2,
	  disj_to_list/2,
	  conj_to_llist/2,
	  llist_to_conj/2,
	  disj_to_llist/2,
	  llist_to_disj/2
	],
	[ assertions
	] ).

:- use_module(library(lists),[append/3]).
:- use_module(library(idlists),[memberchk/2,union_idlists/3]).

:- use_module(library(sort)).
:- use_module(library(write),[numbervars/3]).

:- comment(title,"Lists and conjunctions and disjunctions").

:- comment(list_to_conj(List,Conj,End),"
	@var{Conj} is the conjunction made up of the elements of @var{List}
        plus a final element @var{End}.").

list_to_conj([X|More],(X,Next),End):-
        list_to_conj(More,Next,End).
list_to_conj([],End,End).

:- push_prolog_flag(multi_arity_warnings,off).

:- comment(list_to_conj(List,Conj),"
	@var{Conj} is the conjunction made up of the elements of
	@var{List} (@tt{[]} is @tt{true}).").

list_to_conj([X],X):- !.
list_to_conj([X|More],(X,Next)):-
	list_to_conj(More,Next).
list_to_conj([],true).

:- pop_prolog_flag(multi_arity_warnings).

:- comment(conj_to_list(Conj,List),"
	@var{List} is the list made up of the elements of conjunction 
	@var{Conj} (@tt{true} is @tt{[]}).").

conj_to_list((A,B),[A|List]):- !,
	conj_to_list(B,List).
conj_to_list(true,[]):- !.
conj_to_list(A,[A]).

:- comment(list_to_disj(List,Disj),"
	@var{Disj} is the disjunction made up of the elements of
	@var{List} (@tt{[]} is @tt{true}).").

list_to_disj([],true):- !.
list_to_disj([X],X):- !.
list_to_disj([X|Xs],(X;Ys)):-
	list_to_disj(Xs,Ys).

:- comment(disj_to_list(Disj,List),"
	@var{List} is the list made up of the elements of disjunction
	@var{Disj} (@tt{true} is @tt{[]}).").

disj_to_list((A;B),[A|List]):- !,
	disj_to_list(B,List).
disj_to_list(true,[]):- !.
disj_to_list(A,[A]).

:- comment(conj_to_llist/2,"Turns a conjunctive (normal form) formula
	into a list (of lists of ...). As a side-effect, inner 
	conjunctions get flattened. No special care for @tt{true}.").

conj_to_llist(D,L):-
	conj_to_llist_diff(D,L,[]).

conj_to_llist_diff((A,B),LL,LT):- !,
	conj_to_llist_diff(A,LL,LA),
	conj_to_llist_diff(B,LA,LT).
conj_to_llist_diff((A;B),[LL|LT],LT):- !,
	disj_to_llist_diff(A,LL,LA),
	disj_to_llist_diff(B,LA,[]).
conj_to_llist_diff(A,[A|LT],LT).

:- comment(llist_to_conj/2,"Inverse of @tt{conj_to_llist/2}. No provisions
	for anything else than a non-empty list on input (i.e., they will
	go `as are' in the output.").

llist_to_conj([LL],C):- !,
	llist_to_disj(LL,C).
llist_to_conj([LL|LLs],(C,Cs)):- !,
	llist_to_disj(LL,C),
	llist_to_conj(LLs,Cs).
llist_to_conj(C,C).

:- comment(disj_to_llist/2,"Turns a disjunctive (normal form) formula 
	into a list (of lists of ...). As a side-effect, inner 
	disjunctions get flattened. No special care for @tt{true}.").

disj_to_llist(D,L):-
	disj_to_llist_diff(D,L,[]).

disj_to_llist_diff((A;B),LL,LT):- !,
	disj_to_llist_diff(A,LL,LA),
	disj_to_llist_diff(B,LA,LT).
disj_to_llist_diff((A,B),[LL|LT],LT):- !,
	conj_to_llist_diff(A,LL,LA),
	conj_to_llist_diff(B,LA,[]).
disj_to_llist_diff(A,[A|LT],LT).

:- comment(llist_to_disj/2,"Inverse of @tt{disj_to_llist/2}. No provisions
	for anything else than a non-empty list on input (i.e., they will
	go `as are' in the output.").

llist_to_disj([LL],D):- !,
	llist_to_conj(LL,D).
llist_to_disj([LL|LLs],(D;Ds)):- !,
	llist_to_conj(LL,D),
	llist_to_disj(LLs,Ds).
llist_to_disj(D,D).

%-------------------------------------------------------------------------

:- comment(version_maintenance,dir('../version')).

:- comment(version(0*7+14,1998/09/30,19:04*31+'MET DST'), "Separated
   module formulae from module metaterms.  (Francisco Bueno
   Carrillo)").

%-------------------------------------------------------------------------
