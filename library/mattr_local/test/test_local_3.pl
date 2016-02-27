% compile time checking
% OUTPUT:
% ERROR: Atribute 'at2/1' defined in module 'test_local' with different arity, check it out: 'at2/2'
% ERROR: Atribute 'not_exist/0' not defined in module 'test_local'

:- module( test_local_3 , _ , [mattr_global,
	                       mattr_local,
			       show_trans] ).

:- attribute at1,at2/2,at3.

main :-
	set_attr( X , not_exist ),
	set_attr( X , at1(1,2,3) ),
	set_attr( X , at1(1) ),
	set_attr( X , at2(a,b) ),
	set_attr( X , at2(a) ).

