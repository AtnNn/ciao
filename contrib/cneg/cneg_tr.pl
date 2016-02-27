%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by cneg to add store_clauses/2 facts
% to flatten the structure of facts and clauses of a program
% that contains cneg/1 calls to be able to aply the try/2 
% technique for the constructive negation of the goals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(cneg_tr,[flat_dpred/3,flat_cpred/3,conj_to_list/2],[]).

:- use_module(library(aggregates),[findall/4]).
%:- use_module(library(term_basic),[functor/3]).

% dynamic predicates to store clauses that are going to be
% expanded. It is used to expand them in a continous way
:- data pre_stored/1.
:- data pre_clause/1.
:- data pre_pred/1.

% flat_dpred(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are discontinous.
% It is not expanded the predicate stored_pred/2 to obtain
% the non meta_term of the name of a predicate.

% +++ DTM this is not necesary
%flat_dpred(end_of_file,[end_of_file],_):- 
%	!.

flat_dpred((Head :- Body), [(Head :- Body),(stored_clause(Head,ListBody))],_):-
	!,
	conj_to_list(Body,ListBody).
flat_dpred(Fact, [Fact,(stored_clause(Fact,[]))],_).

% free_list(N,L) returns a list L of N free variables
free_list(0,[]).
free_list(N,[_|L]):-
	N1 is N-1,
	free_list(N1,L).

% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]):-
	!,
	conj_to_list(B,ListB).
conj_to_list(A,[A]).	

% flat_cpred(Sentence,SentList,Module) sustitutes Sentence in 
% the program Module that is being compilated by the list of 
% sentences SentList. The result clauses are continous

% 0 indicates that expansion starts...
flat_cpred(0, _, _) :- !.
flat_cpred(end_of_file,Cls,_):- 
	!,
	findall(CL,(retract_fact(pre_clause(CL))),Cls,Cls1),
	findall(CL,(retract_fact(pre_stored(CL))),Cls1,Clauses),
	findall(CL,(retract_fact(pre_pred(CL))),Clauses,[stored_pred( dist:dist(X,Y) ,dist(X,Y)),end_of_file]).
flat_cpred((Head :- Body), [],_):-
	!,
	assert_pred(Head),
	assertz_fact(pre_clause((Head :- Body))),
	conj_to_list(Body,ListBody),
	assertz_fact(pre_stored(stored_clause(Head,ListBody))).
flat_cpred( (':-'(_)), [], _) :- 
	!,
	fail.

flat_cpred(Fact, [], _):-
	assert_pred(Fact),
	assertz_fact(pre_clause(Fact)),
	assertz_fact(pre_stored(stored_clause(Fact,[]))).

% assert_pred(Head) asserts the fact pre_pred(Pred,Pred) if it is not
% asserted yet where Pred is Head with free vars in the arguments
assert_pred(Head):-
	functor(Head,Name,Arity),
	free_list(Arity,Args),
	Pred=..[Name|Args],
	\+ pre_pred(Pred),!,
	assertz_fact(pre_pred(stored_pred(Pred,Pred))).
assert_pred(_Head).

% !!!!!!!!!!!!!!!!!!!!
% Con esto del assert_pred/1 intentaba que no se insertara mas que una
% vez por predicado pero no lo consigo, lo inserta tantas veces como
% clausulas y hechos haya y es porque no se puede consultar normalmente
% el predicado

