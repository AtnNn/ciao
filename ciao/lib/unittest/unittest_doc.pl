:- module(unittest_doc, [], [assertions, regtypes]).

:- doc(title, "Unit Testing Library").

:- doc(author, "Edison Mera").

:- doc(summary, "This package provides basic unit testing
	functionality.  The central idea is to use the assertion
	language to provide specifications of test cases for a given
	predicate.").

:- doc(module, "This library provides an extension of the Ciao
   assertion language which allows writing @index{unit tests}. The
   central idea is to use the assertion language to provide
   specifications of test cases for a given predicate. The package
   also provides some special properties that are convenient when
   specifying unit tests and the required run-time libraries.

   In general, a @index{test assertion} is written as follows:

@begin{verbatim}
:- test predicate(A1, A2, ..., An) 
   :  <Precondition>
   => <Postcondition>
   +  <Global properties>
   #  <Comment>.
@end{verbatim}

   Where the fields of the test assertion have the usual meaning in
   Ciao assertions, i.e., they contain conjunctions of properties
   which must hold at certain points in the execution. Here we give a
   somewhat more operational (``test oriented''), reading to these
   fields: @pred{predicate/n} is the predicate to be tested.
   @var{Precondition} is a goal that is called before the predicate
   being tested, and can be used to generate values of the input
   parameters.  @var{Postcondition} is a goal that should succeed
   after @pred{predicate/n} has been called.  The idea appears to be
   simple, but note that due to the non-determinism of logic programs,
   the test engine needs to test all the solutions that can be tested
   up to given limits (for example, a maximum number of solutions, or
   a given time-out).  @var{Properties} specifies some global
   properties that the predicate should meet, for example,
   @code{not_fails} means that the program does not fail,
   @code{exception(error(a,b))} means that the program should throw
   the exception @code{error(a,b)}, and so on.  But there are some
   specific properties that only applies to testing specified in the
   module unittest_props.pl, for example @code{times(N)} specifies
   that the given test should be executed N times, @code{try_sols(N)}
   specifies that the first N solutions of the predicate
   @code{predicate/n} are tested.  @var{Comment} is a string that
   document the test.

   A convenient way to run these tests is by selecting options in the
   CiaoDbg menu within the development environment:
   @cindex{running unit tests}

   @begin{enumerate}

   @item @tt{Run tests in current module}: execute only the tests
         specified in the current module.

   @item @tt{Run tests in all related modules}: execute the tests
         specified in the module and in all the modules being used by
         this.

   @item @tt{Show untested predicates}: show the @em{exported}
         predicates that do not have any test assertion.

   @end{enumerate}

   @section{Additional notes} 

   @begin{enumerate}

   @item The test assertions allow performing @em{unit} testing, i.e.,
         in Ciao, performing tests @em{at the predicate level}.

   @item The tests currently can only be applied to exported predicates.

       @comment{, this restriction encourages modularity and makes
         modules less coupled, this is because if you want to write a
         test for a predicate that is not exported, then you can move
         such predicate (or predicates) to a new module that export
         them and use it in the original module}

   @item If you need to write tests for predicates that are spread
         over several modules, but work together, then it is best to
         create a separate module, and reexport to the predicates
         required to build the test.  This allows performing
         @em{integration testing}, using the same syntax of the unit
         tests.

   @item The Ciao system includes a good (and growing) number of unit
         tests. To run all the tests among the other standard tests
         within the CiaoDE run the following (at the top level of the
         source tree):

@begin{verbatim}
./ciaosetup runtests
@end{verbatim}

   @end{enumerate}

").

%    @item @em{Additional issue}: Each time somebody modifies CiaoDE,
%          the tests are executed, the error log is preserved to allow
%          regression testing (in cliptest1), and if a change is
%          detected, a message is sent to the authors.

:- doc(bug, "load_compilation_module, load_test_module and
	load_resource_module directives have similar behavior").

:- decl load_test_module(Module) : sourcename # "Specifies a module
	that must be loaded in order to execute the tests.".

:- decl load_test_module(Module, PredNames) : ( sourcename(Module),
	    list(PredNames, predname) ) # "Specifies a module and the list
	of predicates that must be loaded in order to execute the
	tests".

:- decl load_package_module(Module) : sourcename # "Specifies a
	package that must be used in order to execute the tests.".
