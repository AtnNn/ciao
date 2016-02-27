:- use_package(assertions).

:- comment(filetype, part).

:- comment(title,"PART I - The program development environment").

:- comment(module,"This part documents the components of the basic Ciao
   program development environment.  They include:

   @begin{description}

   @item{ciaoc:} the standalone compiler, which creates executables
   without having to enter the interactive top-level.

   @item{ciaosh:} (also invoked simply as @tt{ciao}) is an interactive
   top-level shell, similar to the one found on most Prolog systems
   (with some enhancements). 

   @item{debugger.pl:} a Byrd box-type debugger, similar to the one
   found on most Prolog systems (also with some enhancements). This is
   not a standalone application, but is rather included in
   @apl{ciaosh}, as in other Prolog systems.

   @item{ciao-shell:} an interpreter/compiler for @em{Prolog scripts}
   (i.e., files containing Prolog code which run without needing
   explicit compilation).

   @item{ciao.el:} a GNU @concept{emacs interface}, which provides
   syntax coloring, atomatic location of errors, access to on-line
   manuals, etc. and greatly facilitates the interaction with the
   tools above.

   @end{description}

The Ciao program development environment also includes @apl{ciaopp},
the @concept{preprocessor}, and @apl{lpdoc}, the
@concept{documentation generator}, which are described in separate
manuals.

  ").

%% --------------------------------------------------------------------------- 
:- comment(version_maintenance,dir('../../version')).
%% --------------------------------------------------------------------------- 
