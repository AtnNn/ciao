
:- module(metaterms,
	[ varset/2, varsbag/3, varset0/2,
	  varset_in_args/2,
	  ask/2,
	  instance/2,
	  variant/2,
	  make_atom/2,
	  copy_Nterm/3
	],
	[ assertions
	] ).

%% This should be called 'terms' or something else ...

:- use_module(library(lists),[append/3]).
:- use_module(library(idlists),[memberchk/2,union_idlists/3]).

:- use_module(library(sort)).
:- use_module(library(write),[numbervars/3]).

:- comment(title,"Term manipulation").

:- comment(hide,varset0/2).
:- comment(hide,varset_in_args/2).
:- comment(hide,make_atom/2).
:- comment(hide,copy_Nterm/3).

%-------------------------------------------------------------------------
:- comment(varset(Term,Xs),"@var{Xs} is the sorted list of all the 
	variables in @var{Term}.").

:- comment(varsbag(Term,Vs,Xs),"@var{Vs} is the list of all the variables
	in @var{Term} ordered as they appear in @var{Term} left-to-right
	depth-first (including duplicates) plus @var{Xs}.").

varset(X,Xs) :- 
	varsbag(X,Xs_uns,[]),
	sort(Xs_uns,Xs).

varsbag(X,Vars,Tail) :- 
	var(X),!,
	Vars = [X|Tail].
varsbag([H|T],Vars,Tail) :- !,
	varsbag(H,Vars,Tail0),
	varsbag(T,Tail0,Tail).
varsbag(Term,Vars,Tail) :-
	functor(Term,_,A),
	go_inside(A,Term,Vars,Tail).

go_inside(0,_,Tail,Tail) :- !.
go_inside(N,T,Bag,Tail) :-
	Nth is N-1,
	arg(N,T,ARG),
	varsbag(ARG,Bag,Tail0),
	go_inside(Nth,T,Tail0,Tail).

varset0(X,[X]) :- var(X), !.
varset0(Term,Vars) :-	
	functor(Term,_,N),	
	extract_vartype(N,Term,Vars), !.	

extract_vartype(0,_,[]) :- !.
extract_vartype(N,Term,Bag) :-
	Nth is N-1,
	extract_vartype(Nth,Term,Ts),
	arg(N,Term,T),
	( var(T) ->
	  ( memberchk(T,Ts) ->
	    Bag = Ts
	  ; Bag = [T|Ts]
	  )
	; atomic(T) ->
	  Bag = Ts
	; varset0(T,TVs),
	  union_idlists(TVs,Ts,Bag)
	).

%-------------------------------------------------------------------------
% varset_in_args(+,-)                                              |
% varset_in_args(Term,Xss)                                         |
% Collects variables in each arg of Term into a list of variables Xss    |
%-------------------------------------------------------------------------

varset_in_args(Term,Xss) :-
	Term =.. [_|Args],
	vars_in_args(Args,Xss).

vars_in_args([],[]).
vars_in_args([Arg|Rest],[Arg_list|Rest_list]) :-
	varset(Arg,Arg_list),
	vars_in_args(Rest,Rest_list).

%-------------------------------------------------------------------------
:- comment(variant(Term1,Term2),"@var{Term1} and @var{Term2} are identical
	up to renaming.").

variant(Term1,Term2) :-
	\+ \+
        (  numbervars(Term1,0,N),
	   numbervars(Term2,0,N),
	   Term1 = Term2
        ).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

make_atom(X,Y):-
	make_name(X,Z),
	name(Y,Z).

make_name([X],X1) :-
	name(X,X1).
make_name([X|Y],Z) :-
	[Slash] = "/",
	name(X,X1),
	make_name(Y,Y1),
	append(X1,[Slash|Y1],Z).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

copy_Nterm(0,_,_):-!.
copy_Nterm(N,Term,Copy):-
        arg(N,Term,ArgN),
        arg(N,Copy,ArgN),
        N1 is N - 1,
        copy_Nterm(N1,Term,Copy).

%-------------------------------------------------------------------------
:- comment(ask(Term1,Term2),"@var{Term1} and @var{Term2} unify without
	producing bindings for the variables of @var{Term1}. I.e.,
	@tt{instance(@var{Term1},@var{Term2})} holds.").

:- comment(instance(Term1,Term2),"@var{Term1} is an instance of @var{Term2}.").

ask(Goal1,Goal2) :- 
        \+ \+ (mynumbervars(Goal1,Goal2,0,_N)), !.

instance(Goal1,Goal2):-
	\+ \+ (mynumbervars(Goal1,Goal2,0,_N)), !.

mynumbervars(X,Y,N,N1) :- var(X), !, var(Y), N1 is N+1, X=N,Y=N.
mynumbervars(X,Y,N,N) :- var(Y), !, X = Y.
mynumbervars(A,B,N,N) :- atomic(A),!, A=B.
mynumbervars(F1,F2,N,N1) :-
        functor(F1,F,A),
        functor(F2,F,A),
        mynumbervars6(0,A,F1,F2,N,N1).

mynumbervars6(A,A,_,_,N,N):- !.
mynumbervars6(I,A,F1,F2,N,N1) :-
         I1 is I+1, 
         arg(I1,F1,X), 
         arg(I1,F2,Y), 
         mynumbervars(X,Y,N,N0), 
         mynumbervars6(I1,A,F1,F2,N0,N1).

%-------------------------------------------------------------------------

% change this to make_atom([F,A],Name)
fullname(F/A,Name) :-
	name(F,Functor),
	name(A,Arity),
	append(Functor,[47|Arity],AName),
	name(Name,AName).

%-------------------------------------------------------------------------
%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
%-------------------------------------------------------------------------

:- comment(version(0*8+30,1998/12/17,09:55*33+'MET'), "Eliminated
   multiple arities.  (Manuel Hermenegildo)").

:- comment(version(0*7+13,1998/09/30,18:14*26+'MET DST'), "Separated
   module formulae from module metaterms.
   (Francisco Bueno Carrillo)").

:- comment(version(0*4+5,1998/2/24), "Synchronized file versions with
   global CIAO version.  (Manuel Hermenegildo)").

%-------------------------------------------------------------------------

