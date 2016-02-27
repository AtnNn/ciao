
:- module(terms_check,[ ask/2, instance/2, variant/2 ],[ assertions ]).

:- comment(title,"Term checking utilities").
:- comment(appendix,"Currently, @tt{ask/2} and @tt{instance/2} are
   exactly the same. However, @tt{ask/2} is more general, since it
   is also applicable to constraint domains (although not yet 
   implemented): for the particular case of Herbrand terms, it is 
   just @tt{instance/2} (which is the only ask check currently 
   implemented).").

%-------------------------------------------------------------------------
:- comment(variant(Term1,Term2),"@var{Term1} and @var{Term2} are identical
	up to renaming.").

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
%% Version comment prompting control for this file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "../version"
%% End:
%-------------------------------------------------------------------------

:- comment(version(1*7+143,2001/11/12,17:51*00+'CET'), "Removed
   metaterms:copy_Nterm which is identical to terms:copy_args.
   (Francisco Bueno Carrillo)").

:- comment(version(1*5+43,2000/02/04,19:53*51+'CET'), "Changed
   @pred{variant/2} to a safe (more efficient?) version.  (Francisco
   Bueno Carrillo)").

%-------------------------------------------------------------------------
