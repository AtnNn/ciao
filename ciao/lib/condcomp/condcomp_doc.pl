:- use_package([assertions]).
:- doc(nodoc, assertions).

:- doc(title, "Conditional Compilation").

:- doc(author, "Jose F. Morales").

:- doc(usage, "The conditional compilation directives are enabled
by including the @tt{condcomp} package in the package list of a module
or by means of an explicit @tt{:- use_package(condcomp)}").

:- doc(bug, "This package implements a @tt{:- define(X)} directive
and @tt{defined(X)} condition. However, the syntax and semantic of
this feature has still to be decided. Do not use it.").

:- doc(bug, "Errors do not show line numbers").

% NOTE: All code (even that not included) must be syntactically
% correct.
%
% WARNING: Do not use it to select code for different architectures or
% your bootstraps won't be portable! (you will make bytecode depends
% on architecture)
%
% Little example:
% :- define(testing).
% :- if(testing ; unstable).
% :- else.
% :- endif.

:- doc(module, "This package defines a serie of directives for
conditional compilation that allow the inclusion or exclusion of code
blocks (which may contain nested conditional directives) based on the
truth value at compile time of special goals called
@em{conditions}. The syntax for conditional directives is:

@begin{verbatim}
:- if(Cond1).
  <<Block1>>
:- elif(Cond2).
  <<Block2>>
:- else.
  <<BlockN>>
:- endif.
@end{verbatim}

@noindent where @tt{elif(_)} can appear zero or more times and the
@tt{else} part is optional. The valid conditions are:

@begin{itemize}
@item calls to @tt{current_prolog_flag/2}.
@item conjunctions, disjunctions, or negations of conditions.
@end{itemize}

The sentences in @tt{Block1} are included if the condition in
@tt{Cond1} is satisfied, else @tt{Block2} is included if @tt{Cond2} is
satisfied (and so on for each @tt{elif}), and @tt{BlockN} if no
previous condition is satisfied.").


