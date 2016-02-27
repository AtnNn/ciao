:- module( modtester , [tester_func/1,modules_tester/2,pred_tester/2] , [assertions] ).


:- comment(title,"Automatic modules caller tester").  
:- comment(author, "David Trallero Mena").

:- comment(module, "This module is intended to agilizy the work of calling
  several modules as prove cases of some program. Usually when you are
  developing a program you have several auto-test program cases that you
  would like to execute whenever you do some modification in your
  program/system. The predicate @pred{mod_tester/2} was created with the
  propouse of execute this test an report to you which of them were
  correctly executed and which one were not." ).

:- use_module( library(tester) , [run_tester/9] ).
:- use_module( library(lists) ).
:- use_module( library(write) ).

:- use_module(library(filenames)).

:- use_module( library( compiler ) ).

:- use_module(engine(internals), [term_to_meta/2,module_concat/3]).

:- use_module( library( terms_check ) , [variant/2] ).
:- use_module( library(conc_aggregates) , [findall/3] ).
:- use_module( library(system) , [cd/1,working_directory/2,file_exists/1] ).

init_func :-
	write( 'Starting the test\n' ).


tester_func( X ) :-
	write( 'Running test ' ) , write( X ) , nl ,
	(unload( X ) -> true ; true ),
	get_module( X , Y , Path ),
	(
	    file_exists( Path )
	->
	    working_directory( CWD , Path ),
%	    display( 'CWD : ' ) , display( Path ) , nl ,
	    ( use_module( Y ),
	      Y:main,
	      unload( Y ),
	      cd( CWD )
	    -> true ; cd( CWD ) , fail
	    )
	;
	    message( error , [ 'Module: ' , X , 
	                       ' in path ' , Path , ' does not exists'] ),
	    fail
	).
		 

get_module( Path , Module , FWPath ) :-
	no_path_file_name( Path , File ),
	(
	    atom_concat( Module , '.pl' , File )
	-> 
	    true
	; 
	    Module = File 
	),
	( 
	    atom_concat( WPath , File , Path )
	->
	    true
	;
	    WPath = './'
	),
	working_directory( CWD , CWD ),
	(
	    atom_concat( '/' , _ , Path ) 
	->
	    FWPath = WPath
	;
	    atom_concat( CWD , '/' , SafeCWD ),
	    atom_concat( SafeCWD , WPath , FWPath )
	).




checker_func( _ ).


end_func.


:- pred modules_tester( BaseName , ModulesList ) 

     : atom * list

# "modules_tester accepts an atom as basename of the two generated files. For each module in @var{ModulesList} an output and report is saved in 'basename_test_output.log' and 'basename_test_summary.log' respectevely". 


modules_tester( BaseName , ModulesList ) :-
	atom_concat( BaseName , '_test_output.log' , Test ) ,
	atom_concat( BaseName , '_test_summary.log' , Summary ) ,
 	run_tester(
		      Test,
		      Summary,
		      init_func ,
		      tester_func ,
		      ModulesList,
		      checker_func,
		      ModulesList,
		      end_func,
		      Res
		  ),

	 length( ModulesList , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '%' ] ).
	

modules_tester( _ , _ ) :- 
	message( note , [ 'ModTester: Something was wrong' ] ).



pred_tester_func( (PatArg,PArg,SOL) ) :-
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


:- pred modules_tester( BaseName , PredList ) 

     : atom * list

# "pred_tester accepts an atom as basename of the two generated files. For each element with the pattern (FindPatter, precidate, [results]), module in @var{PredList} an output and report is saved in 'basename_test_output.log' and 'basename_test_summary.log' respectevely. For example, you can call this predicate as: pred_tester( test , [(X,mypred(X),[1,2,3]),( (X,Y),mypred2( X, aa , Y ), [(1,2),(2,3)])] ). ". 


pred_tester( BaseName , L ) :-
	atom_concat( BaseName , '_ptest_output.log' , Test ) ,
	atom_concat( BaseName , '_ptest_summary.log' , Summary ) ,

	run_tester(
		      Test,
		      Summary,
		      init_func ,
		      tester_func ,
		      L,
		      checker_func,
		      L,
		      end_func,
		      Res
		  ),
	 length( L , LL ),
	 Op is (Res / LL) * 100,
	 message( note , [ 'Analysis result: ' , Op , '%' ] ).

pred_tester( _ , _ ) :- 
	message( note , [ 'PredTeser: Something was wrong' ] ).


:- comment(version_maintenance,dir('../../version')).  :-
comment(version(1*0+3,2003/12/03,11:57*15+'CET'), "now modules are executed
in their executing path (David Trallero Mena)").

:- comment(version(1*0+0,2003/10/16,11:52*43+'CEST'), "Version 1.0 (David
Trallero Mena)").


