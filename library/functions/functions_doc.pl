:- use_package(assertions).
:- comment(nodoc,assertions).


:- comment(title,"Functional notation").

:- comment(module, "This library package allows the use of functional
   notation in a Ciao module/program.

All function applications (definitions) are translated to goals
(predicate definitions) where another argument to hold the result of the
function is added to the right.  Function applications need to be
preceded by the operator @tt{~}, or the functor be declared as such by
using the declaration @pred{function/1}.  There is an exception: all
functions understood by @pred{is/2} are considered as functions by
default.  This feature can be disabled by a declaration @tt{:-
function(arith(false))} (and reverted by using @tt{true} instead of
@tt{false}).  A functor normally considered as a function call can be
escaped using the prefix operator @tt{^}.  Function definitions can be
written by using the binary operator @tt{:=}, and can have also body.
Example of use:
@begin{verbatim}
@includeverbatim{examples/fib_fun.pl}
@end{verbatim}
  ").

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*7+103,2001/05/21,19:22*58+'CEST'), "Fixed a bug
   related to disabling arithmetic functions and defining new predefined
   function names.  (Daniel Cabeza Gras)").

