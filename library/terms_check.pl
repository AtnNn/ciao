:- module(terms_check,
	[ ask/2, instance/2, variant/2,
	  most_general_instance/3,
	  most_specific_generalization/3
	],
	[ assertions ]).

:- comment(title,"Term checking utilities").

:- comment(author,"The CLIP Group").

:- comment(module,"This module implements the term checking
   utilities.").

:- comment(appendix,"Currently, @tt{ask/2} and @tt{instance/2} are
   exactly the same. However, @tt{ask/2} is more general, since it is
   also applicable to constraint domains (although not yet
   implemented): for the particular case of Herbrand terms, it is just
   @tt{instance/2} (which is the only ask check currently
   implemented).").

%-------------------------------------------------------------------------

:- comment(variant(Term1,Term2),"@var{Term1} and @var{Term2} are
   identical up to renaming.").

/* Not safe! E.g.: variant(p('$VAR'(0),X),p(Y,'$VAR'(0)))
variant(Term1,Term2) :-
	\+ \+
        (  numbervars(Term1,0,N),
	   numbervars(Term2,0,N),
	   Term1 == Term2
        ).
*/

variant(Term1,Term2) :-
	samevarpositions(Term1,Term2,VarDic,[]),
	\+ \+ numbervarpairs(VarDic,0).

samevarpositions(X,Y,Dic,Dic0):- var(X), !, var(Y), Dic=[X=Y|Dic0].
samevarpositions(X,Y,Dic,Dic0):- atomic(X), !, X==Y, Dic=Dic0.
samevarpositions(X,Y,Dic,Dic0):-
	nonvar(Y),
        functor(X,F,A),
        functor(Y,F,A),
	samevarpositions_(0,A,X,Y,Dic,Dic0).

samevarpositions_(A,A,_,_,Dic,Dic):- !.
samevarpositions_(I,A,X,Y,Dic,Dic1):-
	I1 is I+1, 
	arg(I1,X,X1), 
	arg(I1,Y,Y1), 
	samevarpositions(X1,Y1,Dic,Dic0), 
        samevarpositions_(I1,A,X,Y,Dic0,Dic1).

numbervarpairs([X=Y|VarDic],N):-
	varpair(X,Y,N,N1),
	numbervarpairs(VarDic,N1).
numbervarpairs([],_).

varpair(X,Y,N,N1):- var(X), !, X=N, X=Y, N1 is N+1.
varpair(X,Y,N,N1):- var(Y), !, Y=N, X=Y, N1 is N+1.
varpair(X,X,N,N).

%-------------------------------------------------------------------------
:- comment(ask(Term1,Term2),"@var{Term1} and @var{Term2} unify without
	producing bindings for the variables of @var{Term1}. I.e.,
	@tt{instance(@var{Term1},@var{Term2})} holds.").

:- true prop instance(A,B) + native.
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
:- comment(most_specific_generalization(Term1,Term2,Term),"@var{Term} 
	satisfies @tt{instance(@var{Term1},@var{Term})} and
	@tt{instance(@var{Term2},@var{Term})} and there is no term less
	general than @var{Term} (modulo variants) that satisfies it.").

most_specific_generalization(T1,_T2,T):-
	var(T1), !,
	var(T).
most_specific_generalization(_T1,T2,T):-
	var(T2), !,
	var(T).
most_specific_generalization(T1,T2,T):-
	functor(T1,F,A),
	functor(T2,F,A), !,
	functor(T,F,A),
	msg_each_arg(A,T1,T2,T).
most_specific_generalization(_T1,_T2,T):-
	var(T).

msg_each_arg(0,_T1,_T2,_T):- !.
msg_each_arg(N,T1,T2,T):-
	arg(N,T1,A1),
	arg(N,T2,A2),
	arg(N,T,A),
	N1 is N-1,
	most_specific_generalization(A1,A2,A),
	msg_each_arg(N1,T1,T2,T).

:- comment(most_general_instance(Term1,Term2,Term),"@var{Term} 
	satisfies @tt{instance(@var{Term},@var{Term1})} and
	@tt{instance(@var{Term},@var{Term2})} and there is no term more
	general than @var{Term} (modulo variants) that satisfies it.").

most_general_instance(T1,T2,T):-
	copy_term(T1,T),
	copy_term(T2,T).

/*
most_general_instance(T1,T2,T):-
	var(T1), !,
	copy_term(T2,T).
most_general_instance(T1,T2,T):-
	var(T2), !,
	copy_term(T1,T).
most_general_instance(T1,T2,T):-
	functor(T1,F,A),
	functor(T2,F,A), !,
	functor(T,F,A),
	mgi_each_arg(A,T1,T2,T).

mgi_each_arg(0,_T1,_T2,_T):- !.
mgi_each_arg(N,T1,T2,T):-
	arg(N,T1,A1),
	arg(N,T2,A2),
	arg(N,T,A),
	N1 is N-1,
	most_general_instance(A1,A2,A),
	mgi_each_arg(N1,T1,T2,T).
*/

%-------------------------------------------------------------------------
%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
%-------------------------------------------------------------------------

:- comment(version(1*9+219,2003/12/21,18:51*46+'CET'), "Added comment
   author and module.  (Edison Mera)").

:- comment(version(1*9+103,2003/09/09,18:29*10+'CEST'), "Added
   most_specific_generalization/3 and most_general_instance/3.
   (Francisco Bueno Carrillo)").

:- comment(version(1*7+143,2001/11/12,17:51*00+'CET'), "Removed
   metaterms:copy_Nterm which is identical to terms:copy_args.
   (Francisco Bueno Carrillo)").

:- comment(version(1*5+43,2000/02/04,19:53*51+'CET'), "Changed
   @pred{variant/2} to a safe (more efficient?) version.  (Francisco
   Bueno Carrillo)").

%-------------------------------------------------------------------------
