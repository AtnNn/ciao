:- module( test_local1 , _ , _ ).


:- use_package( '/home/dtm/mattr_local' ).
%:- use_package( show_trans ).



%:- multifile combine_attributes/3.

:- attribute at1,at2/2,at3.
main :-
	set_attr( X , at1 ),
	set_attr( Y , at2(21,22) ),
	set_attr( X , at3 ),
%	set_attr( Y , at3 ),
	X=Y.

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


combine_local_attributes( 
	test_local1( _ , _ , at3 ) ,
	test_local1( _ , _ , at3 ) , _ ) :-
	        display( 'both have at3' ), nl.


verify_local_attribute( test_local1( _ , _ , no ) , _ ) :-
	display( 'you cannot unify with a term if var has no at3\n' ),
	fail.

verify_local_attribute( test_local1( _ , _ , at3 ) , _ ).

