:- module( atest_local2 , _ , [mattr_global,mattr_local]).

%- use_module( library( case_tester ) , [case_tester/2] ).
:- use_package( case_tester ).

:- use_module( library( lists       ) , [length/2     ] ).

:- attribute at1,at2/2,at3.


test( Y ) :-
	set_attr( X , at1 ),
	set_attr( X , at3 ),
	detach_attr( X , at1 ),
	get_attr( X , Y ).

test2( X ) :-
	set_attr( X , at1 ),
	set_attr( X , at3 ),
	detach_attr( X , at1 ),
	detach_attr( X , at3 ),
	\+ get_attr( X , _ ).


test3( Y ) :-
	set_attr( X , at2( a , b ) ),
	set_attr( X , at1 ),
	set_attr( X , at3 ),
	detach_attr( X , at1 ),
	set_attr( X , at2( a , c ) ),
	get_attr( X , Y ).


test4( Y ) :-
	set_attr( X , at1( a , b ) ),
	set_attr( X , at1 ),
	set_attr( X , at3 ),
	detach_attr( X , at3 ),
	set_attr( X , at2( a , c ) ),
	get_attr( X , Y ).


%%%%%%%%%%%%%%%%%
%% COMBINATION %%
%%%%%%%%%%%%%%%%%

test5( X ) :-
	set_attr( A , at1 ),
	set_attr( X , at3 ),
	set_attr( A , at3 ),
	A = X.

main :-
	L = [
		( A     , test(  A ) , [at3] ),

		( X     , test2( A ) , [_] ),


		( X     , test3( X ) , [at2(a,c),at3]),
		( X     , test4( X ) , [at1, at2(a,c)])
	      ],

	 case_tester( L , Res , 'Multi local-attributes test II' ),
	 length( L , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '% (', Res , ' of ' , LL , ')' ] ).



check_attr( _X , _V ).

combine_attributes( X , Y , V ) :-
	at3@X == at3@Y ,
	
