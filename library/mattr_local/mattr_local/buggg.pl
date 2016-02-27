:- module( buggg , _ , _ ).

a(  X ) :-
	(\+ X == 1 -> display( 'X is not 1\n')).
