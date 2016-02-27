:- use_package([assertions]).
:- comment(nodoc,assertions).

:- comment(title, "The Ciao Make Package").
:- comment(subtitle,"REFERENCE MANUAL").
:- comment(subtitle,"@em{Generated/Printed on:} @today{}").

:- comment(author, "Manuel Hermenegildo").

%% :- include(library('ClipAddress')).

:- comment(copyright,"Copyright @copyright{} M. Hermenegildo

@include{Copyright.Manuals}

").

%% :- comment(summary, "...").

:- comment(module,"@noindent This package is used mainly in two main ways:

@begin{itemize} 

@item When writing @file{Makefile}s for @apl{lpmake}.

@item When writing @em{applications} which use the @lib{make} library.

@end{itemize}

@noindent In both cases, this is the package that defines the syntax
and meaning of the dependency rules used.

").

:- comment(usage, "

@begin{itemize} 

@item When writing @file{Makefile}s for @apl{lpmake}, such makefiles
      start with:

@begin{verbatim} 
:- make.
@end{verbatim} 
 
      @noindent (This uses the feature that an undefined declaration
      at the beginning of a file is interpreted by Ciao as a
      @decl{use_package/1} of a package with the same name.)

@item When writing @em{applications} which use the @lib{make} this
      library is used as any other package within the application.

@end{itemize}

@noindent @bf{Note:} it is often useful to use the @lib{functions}
package inside a @file{Makefile} (or when when using the @lib{make}
library in other applications). If both @lib{make} and @lib{functions}
are used, then @lib{make} should appear before @lib{functions} in the
list of packages.

").

:- include(library(make)).

:- comment(appendix,"

@subsection{The Dependency Rules}

The package allows defining the following types of rules:

@begin{itemize}

@item @tt{@em{TargetSuffix} <= @em{SourceSuffix} :: @em{SourceRoot} :- @em{BodyLiterals}.} 

A rule of this form declares that in order to produce the file with
suffix @em{TargetSuffix} from a source file with the suffix
@em{SourceSuffix} and root name @em{SourceRoot} the commands in
@em{BodyLiterals} must be executed. @em{BodyLiterals} is a standard
Ciao Prolog clause body, i.e., a comma-separated conjunction of
literals. When writing the script, @em{SourceRoot} is typically left
as a variable, to be instantiated by @apl{lpmake} when the script is
run to the root of name of the file to be processed. This allows using
the value of @em{SourceRoot} in @em{BodyLiterals}.  For example, the
following rule:

@begin{verbatim}
dvi <= tex :: File :-
	atom_concat(['latex ',File,'.tex'],Command),
	system(Command).
@end{verbatim}

states that we can generate a file @em{File}@tt{.dvi} if we have a
file named @em{File}@tt{.tex} and that the command to do so is
@tt{latex }@em{File}@tt{.tex}. Thus, if this rule appears in the file
@file{Makefile.pl} and we issue the command @tt{lpmake paper.dvi} 
the following occurs:

@begin{itemize} 

@item 

@item 

@end{itemize}

@begin{verbatim}
dvi <= tex :: FileRoot :- 
	system(~atom_concat(['latex ',File,'.tex'])).
@end{verbatim}

@item @tt{@em{Target} <- :- @em{BodyLiterals}.}

A rule of this form declares that in order to produce the file
@em{Target} the commands in @em{BodyLiterals} must be
executed. @em{BodyLiterals} is a standard Ciao Prolog clause body,
i.e., a comma-separated conjunction of literals. For example, the
following rule:

@begin{verbatim}
foo.pl <- :- 
	
@end{verbatim}

@item @tt{@em{Target} <- @em{Deps} :- @em{BodyLiterals}.}

@end{itemize}

In addition to these rules, the file can of course define predicates
in the usual way, or import predicates from other modules, all of
which can be called from the bodies of the dependency rules. In
particular, the @lib{system_extra} defines many system predicates in a
form which is useful inside @file{Makefile}s, specially if the
@lib{functions} package is used.

---------

If @apl{lpmake} is called without an explicit target as argument, then
the first target rule in the Makefile is used. This is useful in that
the first rule can be see a as the default rule.

@subsection{An Example of a Makefile}

@begin{verbatim}
@includeverbatim{MakefileExample.pl}
@end{verbatim}

").



