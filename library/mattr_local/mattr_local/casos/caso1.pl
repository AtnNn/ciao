

% OPCIONAL:
:- attributes patata,manzana.


main :-
	set_attr( X , patata ),
	set_attr( X , manzana ),
	imprimir_attr( X ).


imprimir_attr( X ) :-
	get_attr( X , Y ),
	display(Y),
	fail.

imprimir_attr( _ ).


