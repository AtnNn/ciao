
:- module(llists, [
	append/2,
	flatten/2,
	collect_singletons/2
		  ],
		  [assertions]).

:- use_module(library(lists),[append/3]).

append([],[]).
append([L],L):- !.
append([L0,L1|Ls],L):-
	append(L0,L1,L2),
	append([L2|Ls],L).

flatten(Xs,Ys) :- flatten_dif(Xs,Ys,[]).

flatten_dif([], Xs, Xs).
flatten_dif([X|Xs],Ys,Zs) :-
        flatten_dif(X,Ys,Ys1),
        flatten_dif(Xs,Ys1,Zs).
flatten_dif(X, [X|Xs], Xs) :-
	\+ ( X = [],
	     X = [_|_] ).

%-------------------------------------------------------------------------
% collect_singletons(+,-)                                                |
% collect_singletons(Xss,Ys)                                             |
% Collects in a list Ys the singletons lists appearing in Xss.           |
%-------------------------------------------------------------------------

collect_singletons([],[]).
collect_singletons([[X]|Xss],[X|Ys]):- !,
	collect_singletons(Xss,Ys).
collect_singletons([_|Xss],Ys):-
	collect_singletons(Xss,Ys).

/*
%-------------------------------------------------------------------------
% combinations(+,-)                                                      |
% combinations(Xss,Css)                                                  |
% Css = {Xs | forall Xsi in Xss, exists X in Xsi, X in Xs}               |
%-------------------------------------------------------------------------

combinations([],[]).
combinations([Ls|Lss],Cs):-
	list_to_list_of_lists(Ls,Init),
	combinations(Lss,Init,Cs).

combinations([],Cs,Cs).
combinations([Ls|Lss],Cs0,Cs):-
	combinations2(Ls,Cs0,Cs1),
	sort(Cs1,Cs2),
	combinations(Lss,Cs2,Cs).

combinations2([],_Cs,[]).
combinations2([E|Es],Cs0,Cs1):-
	add_to_each(Cs0,E,Cs1,Tail),
	combinations2(Es,Cs0,Tail).

add_to_each([],_,L,L).
add_to_each([Ls|Lss],E,[NewLs|NewLss],Tail):-
	insert(Ls,E,NewLs),
	add_to_each(Lss,E,NewLss,Tail).
*/


:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global Ciao version.  (Manuel Hermenegildo)").

%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: Ciao
%% update-version-comments: "../version"
%% End:

