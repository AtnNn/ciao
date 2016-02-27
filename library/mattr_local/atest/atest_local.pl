:- module( atest_local , _ , [mattr_global,mattr_local]).

%- use_module( library( case_tester ) , [case_tester/2] ).
:- use_package( case_tester ).

:- use_module( library( lists       ) , [length/2     ] ).

:- attribute at1,at2/2,at3.


test( Y ) :-
	set_attr( X , at1(2) ),
	set_attr( X , at1(1) ),
	set_attr( X , at2(a,b) ),
	set_attr( X , at2(a,c) ),
	get_attr( X , Y ).


test2( A , B ) :- 
	set_attr( X , at1(1,2,3) ),
	set_attr( X , at1(1) ),
	get_attr( X , A ),
	get_attr( X , at1(B) ).



test3( A ) :-
	set_attr( X , at3(1) ),
	set_attr( X , at2(21,22) ),	
	get_attr( X , A ).



test4( A1 , A2 , Y ) :-
	set_attr( X , A1 ),
	set_attr( X , A2 ),
	get_attr( X , Y ).





main :-
	L = [
		( A     , test(A)    , [at1(1),at2(a,c)] ),

		( (A,B) , test2(A,B) , [(at1(1),1)] ),


		( X     , test3( X        ) , [at2(21,22),at3(1)]),
		( X     , test3( at1(X )  ) , []),
		( X     , test3( at3(X )  ) , [1]),
		( (A,B) , test3( at2(A,B) ) , [ (21,22) ]),

                ( Y     , test4( at2(1,3), at3(2) , Y ) , [at2(1,3),at3(2)] ),
		( Y1    , test4( at1(5), at2(1,2) , Y1) , [at1(5),at2(1,2)] )
	      ],

	 case_tester( L , Res , 'Multi local-attributes test' ),
	 length( L , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '% (', Res , ' of ' , LL , ')' ] ).
