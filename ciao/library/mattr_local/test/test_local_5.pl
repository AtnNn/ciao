:- module( test_local_5 , _ , [ mattr_global,  mattr_local] ).


:- attribute at1,at2/2,at3.

main(X,Y) :-
	set_attr( X , at1 ),
	set_attr( Y , at2(21,22) ),
	set_attr( X , at3 ),
	set_attr( Y , at3 ),
	X=Y.


lget_attr( X , Y ) :- get_attr( X , Y ).
lset_attr( X , Y ) :- set_attr( X , Y ).

main2:-
	set_attr( X , at1 ),
	X=1.


combine_local_attributes( 
	test_local1( at1 , _ ,_ ) , 
	test_local1( at1 , _ , _ ) , _ ) :-
         	display( 'both have at1' ), nl.

combine_local_attributes(
	test_local1( _ , at2(X,Y) ,_ ) , 
	test_local1( _ , at2(X,Y) , _ ) , _ ) :-
             	display( 'both have at2' ), nl.




combine_attr( At1 , At2 , _ ) :-
	At1 @ at3 == At2 @ at3,
	display( 'both have at3' ), nl.


check_attr( test_local_5( _ , _ , no ) , _ ) :-
	display( 'you cannot unify with a term if var has no at3\n' ),
	fail.

check_attr( test_local_5( _ , _ , at3 ) , _ ).

