:- use_package(assertions).
:- comment(nodoc,assertions).
:- comment(title, "Constraint programming over reals").
:- comment(author, "Christian Holzbaur").
:- comment(author, "Daniel Cabeza").
:- comment(module, "

@bf{Note:} This package is currently being adapted to the new
characteristics of the Ciao module system. This new version now works
right now to some extent, but it under further development at the
moment. Use with (lots of) caution.

").

:- comment(bug, "clp(Q) and clp(R) cannot be used simultaneously in
the same program, or even within the same toplevel session.").


:- comment(appendix, "

@subsection{Some CLP(R) examples}

@noindent
(Other examples can be found in the source and library directories.)

@begin{itemize}
@item 'Reversible' Fibonacci (clpr):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/fib_r}
@end{verbatim}

@begin{itemize}
@item Dirichlet problem for Laplace's equation (clpr):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/laplace}
@end{verbatim}

").


:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+227,2003/12/22,16:55*52+'CET'), "First
   revision.  (Edison Mera)").

