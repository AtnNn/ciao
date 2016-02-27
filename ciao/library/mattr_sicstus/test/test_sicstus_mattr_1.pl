:- module( _ , _ , [ mattr_global , mattr_sicstus ] ).

:- attribute locked/0, at1.

stest1( X, Y ) :- 
	put_atts( X , locked ),
	put_atts( X , at1(1,2) ),
	get_atts( X , Y ).


stest2( X , Y ) :-
	put_atts( X ,  at1(a) ),
	get_atts( X , +at1(Y) ).


stest3( X , Y ) :-
	put_atts( X ,  at1(a) ),
	get_atts( X , -at1(Y) ).

stest4( X ) :-
	put_atts( X ,  locked ),
	get_atts( X , -at1(a) ).

stest5( X ) :-
	put_atts( X , +locked ),
	put_atts( X , -at1(a) ),
	put_atts( X , -locked ),
	put_atts( X , +at1(b) ).


stest6 :-
	put_atts( X , locked ),
	X = 1.

stest7(X) :-
	put_atts( X , at1(x) ),
	put_atts( Y , at1(y) ),
	X = Y.

verify_attributes( X , Y , [display( 'End Of Verify' ),nl] ) :-
	display( 'Verify attributed called\n' ),
	( var(Y) ->
	      get_atts( Y , -locked ),
	      get_atts( Y , at1(A) ),
	      get_atts( X , at1(B) ),
	      put_atts( X , at1(A,B) ),
	      put_atts( Y , -at1(A) )
	;
	    get_atts( X , -locked )
	).
	      



% OUTPUT:
%
% ?- stest1(X,Y).
%
% Y = [locked,at1(1)],
% X attributed sicstus_attr(_,locked,at1(1)) ? ;
%
% no
% ?- stest2(X,Y).
%
% Y = a,
% X attributed sicstus_attr(_,_,at1(a)) ? ;
%
% no
% ?- stest3(X,Y).
%
% no
% ?- stest4(X).
%
% X attributed sicstus_attr(_,locked,_) ? ;
%
% no
% ?- stest5(X).
%
% X attributed sicstus_attr(_,_,at1(b)) ? ;
%
% no
% ?- stest6.
% Verify attributed called
%
% no
% ?- stest7(X).
% Verify attributed called
% End Of Verify
%
% X attributed simple_attr(['500test_sicstus_mattr_1'(sicstus_attr(_,_,at1(x,y)))]) ? ;
%
% no
% ?- 
