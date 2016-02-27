:- module(codecoverage_doc, [], [assertions, regtypes]).

:- doc(title, "Code Coverage").

:- doc(author, "Santiago D@'{i}ez P@'{e}rez").

:- doc(summary, "This package provides basic functionality to 
   perform coverage analysis in user programs. The idea is to use this 
   package with the @concept{unit tests} package to measure how well 
   the program is exercised by a test suite.").

:- doc(bug, "There is a problem with the code transformation and
the assertions package; end_of_file is not transformed correctly.").

:- doc(usage, "The recommended procedure in order to enable 
   the code coverage analysis in user programs is to include the 
   @lib{codecoverage} library, using one of the following declarations, 
   as appropriate:

@begin{verbatim}
   :- module(...,...,[codecoverage]).
   :- include(library(codecoverage)).
   :- use_package([codecoverage]).
@end{verbatim}
   
   It is also possible to use the new @concept{compile packages} module
   wich provides preliminary support for the application to a given 
   module or modules of selected packages at compile-time without the
   need for declaring it in the source code, using the predicate:

@begin{verbatim}
   add_compile_packages([...],[codecoverage]).
@end{verbatim}
   ").

:- doc(module, "This library provides the functionality needed to
   perform coverage analysis in user programs.
 
   The coverage criteria used is @index{clause coverage}. A clause is covered 
   iff all the goals of clause's body have been exercised. Please note 
   that no failure analysis is done so the fact that a literal is covered 
   does not imply that its execution has succeed.

  The package performs the following code transformation:

@begin{verbatim}
% Original predicate

predicate(Arg1, ... , Argn) :- 
	lit1(Arg1Lit1,...),
	lit2(Arg1Lit2,...),
	lit3(Arg1Lit3,...).  

% Predicate after transformation

predicate(Arg1, ... , Argn) :- 
	'$COVER_TERM'(position_lit1),
	lit1(Arg1Lit1,...),
	'$COVER_TERM'(position_lit2),
	lit2(Arg1Lit2,...),
	'$COVER_TERM'(position_lit3),
	lit3(Arg1Lit3,...).  


@end{verbatim}

   Where '$COVER_TERM'/1 is defined in the module @em{codecoverage_rt} and
   @em{position_litX} is an structure which identifies a point in a given 
   program. '$COVER_TERM'/1 always succeed so the code transformation does not 
   change the semantics of modules.

   During execution '$COVER_TERM'/1 stores information about covered positions in
   the module. After execution it is possible to know wich clauses are covered, and
   the literals that are not covered in the remaining clauses. Also, some statistical
   analysis is done to know the percentage of code covered.

   @section{Additional notes} 

   @begin{enumerate}

   @item The @concept{code coverage} package uses the predicate
         @em{'$COVER_TERM'/1} so could not be used nor redefined 
         in user programs.
   @item High level predicates are available in the @em{codecoverage utils} 
         module wich can be loaded as usual: @begin{verbatim}:- use_module(library(codecoverage(codecoverage_utils))).
         @end{verbatim}
 @end{enumerate}

").
