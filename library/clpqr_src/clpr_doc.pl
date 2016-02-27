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

