% The pcpe runtime-query package
:- use_package(assertions).

:- doc(title,"PCPE runtime query specification").
:- doc(author,"Claudio Ochoa").

:- doc(module, "This library package provides syntax allowing to
	specify runtime queries, needed for evaluating the multiple
	solutions provided by PCPE when time efficiency is being
	measured").


:- doc(usage, "Typically, this library is used including the
   'pcpe_rtquery' package into the packages list of the module, or using the
   @decl{use_package/1} declaration:
@begin{description}
@item{In a module:}
@begin{verbatim}
	:- module(bar, [main/1], []).
@end{verbatim}
        or
@begin{verbatim}
        :- module(bar, [main/1]).
        :- include(library(pcpe_rtquery)).
@end{verbatim}
@item{In a @em{user} file:}
@begin{verbatim}
	:- use_package([pcpe_rtquery]).
@end{verbatim}
        or
@begin{verbatim}
        :- include(library(pcpe_rtquery)).
@end{verbatim}
@end{description}
   This loads the run-time and compile-time versions of the library
   (@tt{pcpe_rtquery.pl}.").

:- decl pcpe_rtquery(Query) # "Declares a runtime query to be run when
	evaluating the specialized programs obtained by PCPE when
	measuring time-efficiency. @var{Query} should be a predicate
	defined in the current program, called with partially
	instantiated arguments. 

   @bf{Example:}

@begin{verbatim} 
:- pcpe_rtquery(test(A,1,2,3)).  
@end{verbatim}

".
