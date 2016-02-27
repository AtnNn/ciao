% this is for manaual testing... this test mare in atest_local
:- module( test_local_6 , _ , [mattr_global , mattr_local ,show_trans
	                      ] ).



:- attribute at1,at2/2,at3.
main :-
	set_attr( X , not_exist ),
	set_attr( X , at1(1,2,3) ),
	set_attr( X , at1(1) ),
	set_attr( X , at2(a,b) ),
	set_attr( X , at2(a) ).

% OUPUT:
% compilations errors should appear:
% ERROR: Atribute 'at2/1' defined in module 'test_local' with different arity, check it out: 'at2/2'
% ERROR: Atribute 'not_exist/0' not defined in module 'test_local'


main2(A,B) :- 
	set_attr( X , at1(1,2,3) ),
	set_attr( X , at1(1) ),
	get_attr( X , A ),
	get_attr( X , at1(B) ).

% OUTPUT:
% ?- main2(A,B).
%
% A = at1(1),
% B = 1 ? ;
%
% no


main3(A) :-
	set_attr( X , at3(1) ),
	set_attr( X , at2(21,22) ),	
	get_attr( X , A ).

% OUTPUT:
% ?- main3(X).
%
% X = at2(21,22) ? ;
%
% X = at3(1) ? ;
%
% no
% ?- main3(at1(X)).
%
% no
% ?- main3(at3(X)).
%
% X = 1 ? ;
%
% no
% ?- main3( at2(A,B) ).
%
% A = 21,
% B = 22 ? ;
%
% no


main4(A1,A2,Y) :-
	set_attr( X , A1 ),
	set_attr( X , A2 ),
	get_attr( X , Y ).

% ?- main4( at2(1), at3(2) , Y).
% ERROR: Call to set_attr(_432,at2(1)), does not match with attribute declaration:at2/2
%
% Y = at2(1) ? ;
%
% Y = at3(2) ? ;
%
% no
% ?- main4( at1(5), at2(1,2) , Y).
%
% Y = at1(5) ? ;
%
% Y = at2(1,2) ? ;
%
% no


