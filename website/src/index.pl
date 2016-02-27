:- use_package(assertions).

:- doc(filetype, documentation).
:- doc(title,"The Ciao System").
%:- doc(author,"The CLIP Group").

%:- doc(pragma, section_image('ciao2-96-shadow-reduced.png')).

% Ciao is a general-purpose programming language which supports logic,
% constraint, functional, higher-order, and object-oriented programming
% styles. Its main design objectives are high expressive power,
% extensibility, safety, reliability, and efficient execution.
% It is also one of the most complete and powerful ISO-Prolog systems
% available.
% 
% @ref{The Ciao Prolog System} includes
% @ref{The Ciao Prolog Preprocessor}
% (which performs program analysis and optimization, static debugging,
% and static and dynamic verification of assertions) and
% @ref{The lpdoc Documentation Generator}.
% 
% The Ciao programming environment (see some @ref{Screenshots}) provides
% direct, graphical access to syntax-based editing, compiler,
% interactive top-level, source-level debugger, preprocessor,
% autodocumenter, etc.
% 
% @section{Features}

% TODO: This text comes from the 'summary' of Ciao, but it is not synchronized!
%       Possible solutions:
%       - make subsections individual files and include from
%         several places, etc.
%       - one big text, with optional parts depending on the final format
%         (e.g., hide text in HTML)

:- doc(module, "
Ciao is a general-purpose programming language which supports logic,
constraint, functional, higher-order, and object-oriented programming
styles. Its main design objectives are high expressive power,
extensibility, safety, reliability, and efficient execution.

@section{Main Features}

@subsection{Expressive Language...}

@begin{itemize}
@item @bf{Ciao} offers a complete Prolog system, supporting
   @em{ISO-Prolog}, but its novel modular design allows both
   @em{restricting} and @em{extending} the language. As a result,
   it allows working with @em{fully declarative subsets} of Prolog
   and also to @em{extend} these subsets (or ISO-Prolog) both
   syntactically and semantically. Most importantly, these
   restrictions and extensions can be activated separately on each
   program module so that several extensions can coexist in the same
   application for different modules.

@item @bf{Ciao} also supports (through such extensions) programming with
   functions, higher-order (with predicate abstractions), constraints,
   and objects, as well as feature terms (records), persistence,
   several control rules (breadth-first search, iterative deepening,
   ...), concurrency (threads/engines), a good base for distributed
   execution (agents), and parallel execution. Libraries also support
   WWW programming, sockets, external interfaces (C, Java, TclTk,
   relational databases, etc.), etc.
@end{itemize}

@subsection{Safe and Reliable Development...}

@begin{itemize}
@item @bf{Ciao} offers support for @em{programming in
   the large} with a robust module/object system, module-based
   separate/incremental compilation (automatically --no need for
   makefiles), an assertion language for declaring (@em{optional})
   program properties (including types and modes, but also
   determinacy, non-failure, cost, etc.), automatic static inference
   and static/dynamic checking of such assertions,
   etc. 

@item @bf{Ciao} also offers support for @em{programming in
   the small} producing small executables (including only those
   builtins used by the program) and support for writing scripts in
   Prolog. 

@item The @bf{Ciao} programming environment includes a
   classical top-level and a rich emacs interface with an embeddable
   source-level debugger and a number of execution visualization
   tools. 

@item The novel modular design of Ciao enables, in addition to modular
   program development, effective global program analysis and static
   debugging and optimization via source to source program
   transformation. These tasks are performed by the @bf{Ciao
   preprocessor} (@tt{ciaopp}, distributed separately).

@item The @bf{Ciao} programming environment also
   includes @tt{lpdoc}, an automatic documentation generator for
   LP/CLP programs. It processes Prolog files adorned with
   (@bf{Ciao}) assertions and machine-readable comments and
   generates manuals in many formats including @tt{postscript},
   @tt{pdf}, @tt{texinfo}, @tt{info}, @tt{HTML},
   @tt{man}, etc. , as well as on-line help, ascii @tt{README}
   files, entries for indices of manuals (@tt{info}, WWW, ...),
   and maintains WWW distribution sites.
@end{itemize}

@subsection{Portable and Efficient Execution...}

@begin{itemize}
@item The @bf{Ciao} compiler (which can be run outside
   the top level shell) generates several forms of
   architecture-independent and stand-alone executables, which run
   with speed, efficiency and executable size which are very
   competitive with other commercial and academic Prolog/CLP
   systems. Library modules can be compiled into compact bytecode or C
   source files, and linked statically, dynamically, or
   autoloaded. 
@end{itemize}

").


