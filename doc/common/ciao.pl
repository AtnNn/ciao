
:- use_package(assertions).

:- comment(title,"The Ciao Prolog System").

:- comment(subtitle,"@em{A Next Generation Logic Programming Environment}").
:- comment(subtitle,"REFERENCE MANUAL").
:- comment(subtitle,"@bf{The Ciao System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP 3/97.1").
:- comment(subtitle,"@em{Generated/Printed on:} @today{}").
%% :- comment(subtitle,"@em{Preliminary version printed on:} @today{}").

:- comment(author, "F. Bueno").
:- comment(author, "D. Cabeza").
:- comment(author, "M. Carro").
:- comment(author, "M. Hermenegildo").
:- comment(author, "P. L@'{o}pez").
:- comment(author, "G. Puebla").

:- include(library('ClipAddress')).

:- include(library('Copyright')).

%% :- comment(bug,"Although the documentation is acceptable at this
%%    point, we are still really in beta mode in this regard.").

:- comment(summary,"

      @include{CiaoDesc.lpdoc}

   ").

:- comment(module,"

   @section{About this manual}
   @cindex{status, this manual}

   This is the @em{Reference Manual} for the Ciao Prolog development
   system. It contains basic information on how to install Ciao Prolog
   and how to write, debug, and run Ciao Prolog programs from the
   command line, from inside GNU @apl{emacs}, or form a windowing
   desktop. It also documents all the libraries available in the
   standard distribution.

      @include{AboutLPdoc.lpdoc}

   @section{About the Ciao Prolog development system}
   @cindex{ciao, global description}

      @include{AboutCiao.lpdoc}

   @section{ISO-Prolog compliance versus extensibility}
   @cindex{iso-prolog, compliance}
   @cindex{extensibility}

      @include{Compliance.lpdoc}

   @section{About the name of the System}
   @cindex{why the name Ciao}
   @cindex{Ciao, why this name}

      @include{AboutName.lpdoc}

   @section{Syntax terminology and notational conventions}
   @cindex{notation}

      @include{Conventions.lpdoc}

   @section{A tour of the manual}
   @cindex{manual, tour}
   @cindex{tour, of the manual}

   The rest of the introductory chapters after this one provide a
   first ``getting started'' introduction for newcomers to the Ciao
   system. The rest of the chapters in the manual are organized into a
   sequence of major parts as follows:

      @subsection{PART I - The program development environment}

         @include{DevEnv.lpdoc}

      @subsection{PART II - The Ciao basic language (engine)}

         @include{Builtins.lpdoc}

      @subsection{PART III - ISO-Prolog library (iso)}

         @include{IsoProlog.lpdoc}

      @subsection{PART IV - Classic Prolog library (classic)}

         @include{ClassicProlog.lpdoc}

      @subsection{PART V - Annotated Prolog library (assertions)}

         @include{AnnotatedProlog.lpdoc}

      @subsection{PART VI - Ciao Prolog library miscellanea}

         @include{MiscProlog.lpdoc}

      @subsection{PART VII - Ciao Prolog extensions}

         @include{ExtendProlog.lpdoc}

      @subsection{PART VIII - Interfaces to other languages and systems}

         @include{Interfaces.lpdoc}

      @subsection{PART IX - Abstract data types}

         @include{ADTs.lpdoc}

      @subsection{PART X - Miscellaneous standalone utilities}

         @include{ciao-utilities.lpdoc}

      @subsection{PART XI - Contributed libraries}

         @include{Contrib.lpdoc}


      @subsection{PART XII - Appendices}

         @include{Append.lpdoc}


   @section{Acknowledgments} 
   @cindex{acknowledgments}

      @include{Acknowledgments.lpdoc}

").

main.

%% --------------------------------------------------------------------------- 

:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*5+94,2000/03/28,23:19*20+'CEST'), "The manual
   intro now provides an overview of the different parts of the
   manual.  (Manuel Hermenegildo)").

%% Changes for 1.6 gathered up to patch 94

:- comment(version(1*5+0,1999/11/29,16:16*23+'MEST'),"Development
   version following even 1.4 distribution.

   Major changes so far (which will be in 1.6):

@begin{itemize}
@item Source-level debugger in emacs, breakpts.
@item Debugger embeddable in executables.
@item Greatly improved emacs interface.
@item Menu-based documenter interface.
@item Threads now available in Win32.
@item Many improvements to threads.
@item Modular clp(R) / clp(Q).
@item Improved syntax for pred. abstractions.
@item Library of higher order list predicates.
@item Better code expansion facilities (macros).
@item Compressed object code/executables. @comment{(.po)}
@item The size of atoms is now unbound.
@item Fast creation of new unique atoms.
@item # of clauses/pred. essentially unbound.
@item Delayed goals with freeze restored.
@item Faster compilation and execution.
@comment{@item Libraries not recompiled or checked unless 
         @tt{-x} option is used.}
@item Much faster fast write/read. 
@item Improved documentation.
@item Other new libraries. @comment{graphs tcltk(new version) xmrefs davinci}
@item Improved installation/deinstallation.
@item Many improvements to autodocumenter.
@item Many minor bug fixes.
@comment{@item Contrib stuff?}
@end{itemize}

   ").

:- comment(version(1*4+0,1999/11/27,19:00*00+'MEST'),"
@begin{itemize}
@item Documentation greatly improved.
@item Automatic (re)compilation of foreign files.
@item Concurrency primitives revamped; restored &Prolog-like 
      multiengine capability. 
@item Windows installation and overall operation greatly improved.
@item New version of O'Ciao class/object library, with improved performance.
@item Added support for ""predicate abstractions"" in call/N. 
@item Implemented reexportation through reexport declarations.
@item Changed precedence of importations, last one is now higher.
@item Modules can now implicitly export all predicates.
@item Many minor bugs fixed.
@end{itemize}").

:- comment(version(1*3+0,1999/06/16,17:05*58+'MEST'), "Development
   version following even 1.2 distribution.").

:- comment(version(1*2+0,1999/06/14,16:54*55+'MEST'), " Temporary
   version distributed locally for extensive testing of reexportation
   and other 1.3 features.").

:- comment(version(1*1+0,1999/06/04,13:30*37+'MEST'), "Development
   version following even 1.0 distribution.").

:- comment(version(1*0+0,1999/06/04,13:27*42+'MEST'), "
@begin{itemize}
@item Added Tcl/Tk interface library to distribution.
@item Added push_prolog_flag/2 and pop_prolog_flag/1 declarations/builtins.
@item Filename processing in Windows improved.
@item Added redefining/1 declaration to avoid redefining warnings.
@item Changed syntax/1 declaration to use_package/1.
@item Added add_clause_trans/1 declaration.
@item Changed format of .itf files such that a '+' stands for all the
standard imports from engine, which are included in c_itf source
internally (from engine(builtin_exports)).  Further changes in itf data
handling, so that once an .itf file is read in a session, the file is
cached and next time it is needed no access to the file system is required.
@item Many bugs fixed.
@end{itemize}").

:- comment(version(0*9+32,1999/04/05,20:38*17+'MEST'), "Improved
   uninstallation makefiles so that (almost) nothing is left behind.
   (Manuel Hermenegildo)").

:- comment(version(0*9+0,1999/03/10,17:03*49+'CET'), "
@begin{itemize}
@item Test version before 1.0 release. Many bugs fixed.
@end{itemize}").

:- comment(version(0*8+0,1998/10/27,13:12*36+'MET'), "
@begin{itemize}
@item Changed compiler so that only one pass is done, eliminated @tt{.dep}
      files.
@item New concurrency primitives.
@item Changed assertion comment operator to #.
@item Implemented high-order with call/N.
@item Integrated SQL-interface to external databases with 
      persistent predicate concept. 
@item First implementation of object oriented programming package.
@item Some bugs fixed.
@end{itemize}").

:- comment(version(0*7+0,1998/09/15,12:12*33+'MEST'), "
@begin{itemize}
@item Improved debugger capabilities and made easier to use.
@item Simplified assertion format.
@item New arithmetic functions added, which complete all ISO functions.
@item Some bugs fixed.
@end{itemize}
").

:- comment(version(0*6+0,1998/07/16,21:12*07+'MET DST'), "
@begin{itemize}
@item Defining other path aliases (in addition to 'library') which can
      be loaded dynamically in executables is now possible.
@item Added the posibility to define multifile predicates in the shell.
@item Added the posibility to define dynamic predicates dynamically.
@item Added addmodule meta-argument type.
@item Implemented persistent data predicates.
@item New version of PiLLoW WWW library (XML, templates, etc.).
@item Ported active modules from ``distributed Ciao'' (independent 
      development version of Ciao).
@item Implemented lazy loading in executables.
@item Modularized engine(builtin).
@item Some bugs fixed.
@end{itemize}").

:- comment(version(0*5+0,1998/3/23), "
@begin{itemize}
@item First Windows version.
@item Integrated debugger in toplevel.
@item Implemented DCG's as (Ciao-style) expansions.
@item Builtins renamed to match ISO-Prolog.
@item Made ISO the default syntax/package.
@end{itemize}").

:- comment(version(0*4+0,1998/2/24), "
@begin{itemize}
@item First version with the new Ciao emacs mode.
@item Full integration of concurrent engine and compiler/library.
@item Added new_declaration/1 directive.
@item Added modular syntax enhancements.
@item Shell script interpreter separated from toplevel shell.
@item Added new compilation warnings.
@end{itemize}
").

:- comment(version(0*3+0,1997/8/20), "
@begin{itemize}
@item Ciao builtins modularized.
@item New prolog flags can be defined by libraries.
@item Standalone comand-line compiler available, with automatic ""make"".
@item Added assertions and regular types.
@item First version using the automatic documentation generator.
@end{itemize}
").

:- comment(version(0*2+0,1997/4/16), "
@begin{itemize}
@item First module system implemented.
@item Implemented exceptions using catch/3 and throw/1.
@item Added functional & record syntax.
@item Added modular sentence, term, and goal translations.
@item Implemented attributed variables.
@item First CLPQ/CLPR implementation.
@item Added the posibility of linking external .so files.
@item Changes in syntax to allow @tt{P(X)} and @tt{""string""||L}.
@item Changed to be more similar to ISO-Prolog.
@item Implemented Prolog shell scripts.
@item Implemented data predicates.
@end{itemize}").

:- comment(version(0*1+0,1997/2/13), "First fully integrated,
   standalone Ciao distribution. Based on integrating into an
   evolution of the &-Prolog engine/libraries/preprocessor
   @cite{Hampaper,ngc-and-prolog} many functionalities from several
   previous independent development versions of Ciao @cite{ciao-prolog-compulog,ciao-ppcp,att-var-iclp,ciao-manual-tr,ciao-comp-dist-tr-deliv,ciao-ilps95,ciao-jicslp96-ws-update,pillow-ws,ciao-novascience}.

").
%% --------------------------------------------------------------------------- 
