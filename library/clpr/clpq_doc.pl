:- use_package(assertions).
:- comment(nodoc,assertions).
:- comment(title, "Constraint programming over rationals").
:- comment(author, "Christian Holzbaur").
:- comment(author, "Daniel Cabeza").
:- comment(module, "

@bf{Note:} This package is currently being adapted to the new
characteristics of the Ciao module system. This new version now works
right now to some extent, but it is under further development at the
moment. Use with (lots of) caution. 

").

:- comment(appendix, "

@subsection{Some CLP(Q) examples}

@noindent
(Other examples can be found in the source and library directories.)

@begin{itemize}
@item 'Reversible' Fibonacci (clpq):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/fib_q}
@end{verbatim}


@begin{itemize}
@item Matrix multiplication (clpq):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/mmatrix_q}
@end{verbatim}


@begin{itemize}
@item Queens (clpq):
@end{itemize}

@noindent
@begin{verbatim}
@includeverbatim{examples/nqueens_q}
@end{verbatim}


").

%% @begin{itemize}
%% @item Critical (cpm critical path routine, clpq):
%% @end{itemize}
%% 
%% @noindent
%% @begin{verbatim}
%% @includeverbatim{examples/critical}
%% @end{verbatim}
