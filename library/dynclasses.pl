%%------------------------------------------------------------------------
%%
%% O'CIAO : OBJECT ORIENTED PROGRAMMING IN CIAO/Prolog
%%
%% RUNTIME SUPPORT FOR OBJECT MANIPULATION AT CIAO-TOPLEVEL
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : March 1999
%%
%%------------------------------------------------------------------------

:- module(dynclasses,[use_class/1],[assertions]).

:- use_module(library(dynmods),[use_module/1]).
:- use_module(library(filenames),[no_path_file_name/2,file_name_extension/3]).
:- use_module(library('objects/objects_rt'),[destroy/1]).

:- multifile 'id$fromclass'/2.
:- data      'id$fromclass'/2.

:- comment(hide,'id$fromclass'/2).

%%------------------------------------------------------------------------

:- comment(title,
	"Using classes from the CIAO toplevel shell").

:- comment(author,
	"Angel Fernandez Pineda").

:- comment(module,
	"The CIAO toplevel shell will be a usefull tool in order to
         compile and test O'CIAO classes. Those classes may be dynamically
         loaded, so some instances of them may be created to perform any 
         needed test.").

:- comment(usage,
	"This library will be automatically loaded when using the 
         @em{objects} package from the toplevel shell:
@begin{verbatim}
	?- use_package(objects).
@end{verbatim}
	
          It may be usefull for you to put a @tt{:- use_package(objects)}
          declaration on your @em{.cshrc} initialization file. 
          In such case, absolutely nothing is needed to load this library.

          The Emacs ciao-mode will also automatically load this library
          when a @em{Load buffer} command is issued (@tt{C-c l}).
         ").          

%%------------------------------------------------------------------------
%%
%% ESTABLISH DYNAMIC USAGE RELATIONSHIP WITH CLASS
%%
%% This predicate is intended to be used at CIAO toplevel.
%% Usage in user programs will cause the predicate to fail.
%%------------------------------------------------------------------------

:- meta_predicate use_class(addmodule).

:- comment(use_class/1,
	"This predicate is something very similar to @pred{use_module/1}.

         It will (re)compile and/or (re)load a class source file from the
         CIAO shell so instances of that class can be now created.

         There is an implicit usage relationship between the
         CIAO shell and @bf{any and all loaded} classes. This seems that
         user is allowed to create (@pred{new/1}) and call any public
         predicate even when the involved class was not specifically loaded
         using the use_class/1 predicate.For example:
@begin{verbatim}
   :- module(test,[example/1],[objects]).

   :- use_class(test_class).

    example(Obj) :-
        Obj new test_class,
        Obj:do_something.
@end{verbatim}

        Whenever this code is loaded:

@begin{verbatim}
	?- use_module(test).
@end{verbatim}

       user is allowed to handle instances of @em{test_class} 
       (and ascendants):

@begin{verbatim}
	?- test:example(SomeObj), SomeObj:test_something.
@end{verbatim}

        Note that @em{test_class} was not loaded using use_class/1 predicate,
        but public predicate @em{test_something/0} may be called.

        Some other usefull notes:

        @begin{itemize}

        @item Every time a class is reloaded using use_class/1, 
              all previously created instances of that class will be 
              automatically destroyed.

        @item Predicate use_class/1 will fail if called from user programs. 

        @item use_class/1 will have no effect whether given source is
              not a class.

        @end{itemize}
	").

:- pred use_class(Source) #
	"Compile and load @var{Source} class".

use_class(ClassSource,FromModule) :-
	functor(FromModule,user,_),
	class_from_source(ClassSource,Class),
	use_class_aux(Class),
	use_module(ClassSource).

% destroy previously allocated instances

use_class_aux(Class) :-
	'id$fromclass'(ID,Class),
	destroy(ID),
	message(note,[ID,' instance from ',Class,' has been deleted']),nl,
	fail.

use_class_aux(_).

%%------------------------------------------------------------------------

class_from_source(File,Class) :-
	( atom(File) -> Source = File ; arg(1,File,Source)),
	atom_codes(Source,SourceCodes),
	no_path_file_name(SourceCodes,ClassWithExtCodes),
	( file_name_extension(ClassWithExtCodes,ClassCodes,_) -> 
	  true
	;
	  ClassCodes = ClassWithExtCodes
	),
	atom_codes(Class,ClassCodes).

%%------------------------------------------------------------------------

:- comment(bug,
	"Not really a bug: when loading code which declares 
         static instances (@pred{use_module/1}), those instances may be not
         correctly created, and predicates will fail whenever they are not
         supposed to do. This may be avoided by reloading again 
         the involved module, but make sure it is modified and saved to
         disk before doing so.
        ").

%%------------------------------------------------------------------------

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*3+21,1999/07/07,10:27*51+'MEST'), "Documentation first
   created.  (Angel Fernandez Pineda)").

%%------------------------------------------------------------------------

:- comment(version_maintenance,dir('../../version')).
