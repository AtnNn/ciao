:- use_package(assertions).

:- doc(filetype, application). % TODO: or 'documentation'?

:- doc(title,"The LPdist Bundle Management Framework").
:- doc(subtitle, "An Architecture for Building (Ciao) Applications and Libraries").

% TODO: Put yourself here
:- doc(author, "The CLIP group").
:- doc(author, "Jose F. Morales (new design)").

% :- doc(logo, 'ciao-shadow-64h').

:- doc(subtitle_extra,"REFERENCE MANUAL").
:- doc(subtitle_extra,"@bf{The Ciao Documentation Series}").
:- doc(subtitle_extra,"@href{http://ciao-lang.org/}").
:- doc(subtitle_extra,"@em{Generated/Printed on:} @today{}").
%:- doc(subtitle_extra,"Technical Report CLIP 3/97-@version{}").

% :- include(library('ClipAddress')).

% :- include(library('Copyright')).

:- doc(summary,"
   @begin{alert}
   WRITE A SUMMARY
   @end{alert}
   ").

:- doc(module,"
   @begin{alert}
   This is just a stub, complete.
   @end{alert}

   @apl{LPdist} is a system for building and packaging Ciao libraries
   and programs. @apl{LPdist} works on structured collections of code,
   dubbed as @concept{bundle}s.

   Automating the treatment of large collections of code is a frequent
   and common problem both that has been addressed many times. This
   tool is inspired in other popular systems (see @ref{Relation with
   Other Systems}).

   @section{Bundles}

   Modules allow programs to be separated and combined in a flexible
   way. However, modules alone are not enough to describe large
   libraries and applications. For example, many definitions
   concerning namespaces, compilation options, documentation,
   licensing, authorship, etc. are usually global for a collection of
   modules. Moreover, applications often depend on external tools and
   data files whose dependencies cannot be easly specified.

   A @tt{bundle} (see @bf{note} below) is the equivalent in Ciao of
   both a @em{source project} and specification of a @em{software
   package}. It usually comprise:

   @begin{itemize}
   @item source code (as modules and packages)
   @item namespace definitions (alias paths)
   @item dependencies to other bundles
   @item documentation (in LPdoc or other formats).
   @item custom code for build/installation
   @item at least one @tt{Manifest.pl} file that describes this
     @em{meta-information}
   @end{itemize}

   @bf{Note:} when talking about Ciao programs, we will avoid the term
   @em{package} as a collection of software, in order to avoid
   confusion with @em{packages} as language extensions for Ciao.

   @section{Packaged Bundles}

   A @concept{pbundle}, or packaged bundle, is any of the possible
   packaged (ready to be distributed) versions of a bundle. You can
   think of them as the equivalent of @em{biological vectors} for
   bundles.

   It may include source code, pre-compiled binaries for specific
   architecture, and installers for different operating systems. It
   may deliver a whole bundle, a combination of them, or parts of it
   (e.g., just manuals).

   @section{Relation with Other Systems}

   There exists many @href{http://en.wikipedia.org/wiki/List_of_software_package_management_systems}{software package management systems}. Many
   of them are general tools that target software installation in the
   scope of a given operating system.

   Although there exists outstanding software package systems, both in
   terms of their capabilities and the size of their libraries, none
   has proved so far to be an @em{universal} solution. Another class
   of systems focuses on treatment of software written for a
   particular language. They offer a support layer between the
   language and other general software package systems (with many
   benefits, such that multiple, evolving, software package systems
   can be targeted by means of automatic translation). Often, the
   integration with the language simplifies many tasks, like the
   specification of dependencies for compilation.

   @apl{LPdist} falls in this category. Some similar systems
   for different languages are the following (in alphabetical order):

   @begin{itemize}
   @item @href{http://www.haskell.org/cabal/}{The Haskell Cabal}
   @item @href{http://peak.telecommunity.com/DevCenter/PythonEggs#building-eggs}{Python Eggs}
   @item @href{http://docs.rubygems.org/read/book/1}{Ruby Gems}
   @end{itemize}

   @section{Internals (installation)}

   @begin{alert}
   This section does not belong here.
   @end{alert}

   Different installation areas are supported. For personal
   installations, the installation area can overlap with the build
   staging area, such that no extra space is necessary.

@begin{verbatim}

          .............         install             ...........
          .           .---------------------------->.         .
          .           .        (generated           .         .
          .............         files such          ...........
       Build Staging Area       as binaries)      Installation Area
               .^.                                 _
                | build                            /|
                |                                 /
          .............          install         /
          .           .-------------------------'
          .           .         (source files
          .............          such as examples,
           Source Code           images, etc.)

@end{verbatim}

").


