:- module(  _ , _ , [ mattr_global ] ).



test :-
	set_attr( A , attr( A , whatever ) ),
	set_attr( B , attr( B , whatever ) ),
	A = B.


combine_attr( attr( X , _ ) , attr( Y , _ ) , Z ) :-
	display( e( X , Y , Z ) ) , nl.
