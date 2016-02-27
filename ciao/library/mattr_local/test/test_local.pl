:- module( test_local , _ , [mattr_global,mattr_local] ).

:- use_module(library(tester), [run_tester/10]).


:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(library(terms_check), [variant/2]).
:- use_module(library(conc_aggregates), [findall/3]).


init_func.
	

tester_func( (PatArg,PArg,SOL) ) :-
	copy_term( (PatArg , PArg ) , (Pat,P ) ),
	write( 'me pasan ' ), write( (Pat , P) ) , nl,
	findall( Pat , P , L ),
	write( 'compare ' ), write( (L,SOL) ) , nl,
	compare( L , SOL ).


compare( [A|RA] , [B|RB] ) :-
	variant(A,B),
	!,
	compare( RA , RB ).

compare( [] , [] ).

checker_func( _ ).

end_func.


:- attribute at1,at2/2,at3.


main2(A,B) :- 
	set_attr( X , at1(1,2,3) ),
	set_attr( X , at1(1) ),
	get_attr( X , A ),
	get_attr( X , at1(B) ).



main3(A) :-
	set_attr( X , at3(1) ),
	set_attr( X , at2(21,22) ),	
	get_attr( X , A ).



main4(A1,A2,Y) :-
	set_attr( X , A1 ),
	set_attr( X , A2 ),
	get_attr( X , Y ).





main :-
	L = [
		( (A,B) , main2(A,B) , [(at1(1),1)] ),

		( X     , main3( X        ) , [at2(21,22),at3(1)]),
		( X     , main3( at1(X )  ) , []),
		( X     , main3( at3(X )  ) , [1]),
		( (A,B) , main3( at2(A,B) ) , [ (21,22) ]),

                ( Y     , main4( at2(1,2), at3(2)   , Y ) , [at2(1,2),at3(2)] ),
		( Y1    , main4( at1(5), at2(1,2) , Y1) , [at1(5),at2(1,2)] )
	      ],

	run_tester(
		      'test.log',
		      'result.log',
		      init_func ,
		      tester_func ,
		      L,
		      checker_func,
		      L,
		      end_func,
		      Res,
		      slider( 'Local Multiattributes: ' )
		  ),
	 length( L , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '%' ] ).
