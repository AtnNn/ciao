:- module( tester , [run_tester/9] , [assertions,hiord]).

:- use_module( library(lists) , [length/2] ).
:- use_module( library(write) , [write/2] ).
:- use_module( library(io_alias_redirection) ).


:- comment(title,"Automatic tester").  
:- comment(author, "David Trallero Mena").

:- comment(module, "This module have been created to automatizate the test
 that a predicate should pass hopefully. With that intention we have to 
 provide a set of test and its correct answers. The predicate
 @pred{run_tester/9} will execute every test and compare it with its answer,
 generating two traces, one with detailed information, and another with the
 summary of executions of the tests.").

:- meta_predicate safe_call( pred(1) , ? , pred(1) , ? , ? ).

safe_call( P , [A|B] , C , [LC1 | LCR] , ResStream , Good ) :-
	safe_call( P , B , C , LCR , ResStream , RGood ),
	(
	    P(A)
	->
	    % Run the checker,
	    (
		C( LC1 )
	    ->
		% Write in file => TRUE
	        write( ResStream , '   OK   ' ), 
		write( ResStream , A ) , 
		write( ResStream , ' <==> ' ) , 
		write( ResStream , LC1 ),
		Good is RGood + 1
	    ;
		% Write in file => FALSE
	        write( ResStream , ' FAIL   ' ), 
		write( ResStream , A ) , 
		write( ResStream , ' <==> ' ) , 
		write( ResStream , LC1 ),
		Good = RGood
	    ),
	    write( ResStream , '\n' )
	;	
          % Write in file => FALSE
	  write( ResStream , ' FAIL   EXECUTING TEST OF ' ), 
	  write( ResStream , A ) , 
	  write( ResStream , '\n' ),
	  Good = RGood
	).

safe_call( _P , [] , _C , _CL , _Stream , 0 ).

:- pred run_tester( LogFile , ResultFile , Begin , Test , TestList , Check
	, CheckList , End , GoorExamples ) 

     : string * string * callable * callable * list * callable * list * callable * var

# "run_tester is a predicate for automatizate testers. It get 2 file names
	as entry (@var{LogFile} and @var{ResultFile}) for saving the trace
	and the short result scheme respectevely. @var{Being} and @var{End}
	are called at the beginning and at the end of the test. @var{Test}
	is called which each element of @var{TestList} and after,
	@var{Check} is called with the corresponding element in
	@var{CheckList} for checking the results of @var{Test}
	predicate. @var{GoodExample} is ground(int) at the exit and tells
	the number of examples that passed the test correctly".

:- meta_predicate run_tester( ? , ? , pred(0), pred(1), ? , pred(1) , ? ,
                        pred(0), ? ).

run_tester( LogFile , ResultFile , I , T , L , C , L2 , E , GoodExamples ) :-
	open( LogFile , write , Stream ),
	open( ResultFile , write , ResStream ),
	set_output( user_error ),
        current_output(O),
        set_output(Stream),
	set_stream( user_error , Stream , OldUserError ),

	( call( I ),
	  safe_call( T , L , C , L2 , ResStream , GoodExamples ),
	  call( E ) -> true ; GoodExamples=0 ),
	
	set_stream( user_error , OldUserError , _ ),
	set_output(O),
	close(Stream),
	close( ResStream ).


:- comment(appendix,"

   Two simple examples of the use of the run_tester are provided.

    @subsection{Understanding run_test predicate}

@noindent
@begin{verbatim}
@includeverbatim{test/tester_test2.pl}
@end{verbatim}

    @subsection{More complex example}

In this example we just want to test if the output of Ciaopp is readable by CIAO.

Tester function succeds if it is able to write the output file. 

Checker function succeds if it is able to load the written file.

@noindent
@begin{verbatim}
@includeverbatim{test/tester_test1.pl}
@end{verbatim}

   ").


:- comment(version_maintenance,dir('../../version')).
:- comment(version(1*0+0,2003/10/16,11:52*43+'CEST'), "Version 1.0 (David
Trallero Mena)").

