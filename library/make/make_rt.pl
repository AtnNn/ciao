:- module(make_rt,[make/1,target/1,make_option/1,
	           verbose_message/2,
                   call_unknown/1,
%                   fancy_display/1,
%% Not used any more
%%		   dyn_load_cfg_file_into_make/1,
   	           dyn_load_cfg_module_into_make/1],[]).

%% ---------------------------------------------------------------------------
:- use_package(assertions).
:- use_package(regtypes).

:- comment(title,"Predicates Available When Using The Make Package").

:- comment(usage,"This module is loaded automatically when the
   @lib{make} library package is used.").

:- comment(module,"This is the run-time module which implements the
   predicates which are provided when using the @lib{make} library
   package in a given application. For example, they are used
   internally by @apl{lpmake}.").

%% ---------------------------------------------------------------------------

%% ISO Prolog-like modules
%% :- use_module(library(compiler), [ensure_loaded/1,use_module/1]).
:- use_module(library(compiler), [use_module/1]).

%% CIAO libraries
:- use_module(library(filenames),[file_name_extension/3]).
:- use_module(library(terms),    [atom_concat/2]).
:- use_module(library(system),   [file_exists/1,modif_time0/2]).
:- use_module(library(messages), [simple_message/2,warning_message/2]).
:- use_module(library(format),   [format_control/1]).
%% :- use_module(library('assertions/assrt_lib'),[set_libs/2]).

:- regtype target(T) # "@var{T} is a Makefile target.".

target(X) :- atm(X).

:- pred make(TargetList) : list(target) 

   # "This is the main entry point to the make library. Makes the list
      of targets one by one and any needed intermediate targets as
      dictated by the dependency rules.".

make([]) :- 
	!.
make([Target|Targets]) :- 
	!,
	make(Target),
	make(Targets).

make(Target) :- 
	call_unknown(_:target_deps(Target,[])), 
	!,
	( find_file(Target,PathFile)
	-> verbose_message("unconditional target ~w exists",[PathFile])
        ;  verbose_message("making unconditional target ~w",[Target]),
	   call_unknown_nofail(_:target_comment(Target)),
	   _:do_target(Target)).
make(Target) :- 
	call_unknown(_:target_deps(Target,Preconds)), 
	!,
	verbose_message("making conditional target ~w < ~w",[Target,Preconds]),
	make_dep_target(Target,Preconds).
make(Target) :- 
	file_name_extension(Target,FileBase,Extension),
	atom_codes(Extension,ExtensionS),
	ExtensionS = [0'.|TSuffixS],
	atom_codes(TSuffix,TSuffixS),
	call_unknown(_:dependency_exists(TSuffix,_SSuffix)),
	make_dep_suffix(TSuffix,FileBase,TSuffix).
make(Target) :- 
	find_file(Target,PathFile),
	verbose_message("~w has no ancestors and exists",[PathFile]).
make(Target) :- 
	throw(make_error("Could not complete ~w",[Target])).


%% ---------------------------------------------------------------------------
%% Procesing target dependencies
%% ---------------------------------------------------------------------------

make_dep_target(Target,Preconds) :-
	make(Preconds),
	( (  \+ find_file(Target,PathFile) ; 
	     ( find_file(Target,PathFile), \+ newer(Preconds,PathFile)) )
	-> verbose_message("processing ~w",[Target]),
	   call_unknown_nofail(_:target_comment(Target)),
	   _:do_target(Target)
	;  verbose_message("~w is up to date",[Target]) ).

newer([],_Target).
newer([File|Files],Target) :-
	verbose_message("checking if ~w is newer",[File]),	
	find_file(File,PathFile),
	needs_processing(Target,PathFile),
	modif_time0(PathFile, STime),
	modif_time0(Target, TTime),
	STime < TTime,
	newer(Files,Target).

%% ---------------------------------------------------------------------------
%% Procesing suffix dependencies
%% ---------------------------------------------------------------------------

make_dep_suffix(TSuffix,FileBase,_OTSuffix) :-
	\+ call_unknown(_:dependency_exists(TSuffix,_SSuffix)),
	atom_concat([FileBase,'.',TSuffix],Target),
	\+ call_unknown(_:target_deps(Target,_Preconds)), 
	verbose_message("~w has no ancestors",[Target]).
make_dep_suffix(TSuffix,FileBase,_OTSuffix) :-
	\+ call_unknown(_:dependency_exists(TSuffix,_SSuffix)),
	atom_concat([FileBase,'.',TSuffix],Target),
	call_unknown(_:target_deps(Target,Preconds)), 
	verbose_message("making conditional target ~w < ~w",[Target,Preconds]),
        make_dep_target(Target,Preconds).
make_dep_suffix(TSuffix,FileBase,OTSuffix) :- 
	call_unknown(_:dependency_exists(TSuffix,SSuffix)),
	verbose_message("Found that ~w.~w can be generated from ~w.~w",
	                [FileBase,TSuffix,FileBase,SSuffix]),

	%% Recurse finding ultimate ancestor
        make_dep_suffix(SSuffix,FileBase,OTSuffix),

	verbose_message("Checking if ~w.~w should be generated from ~w.~w",
	                [FileBase,TSuffix,FileBase,SSuffix]),

	atom_concat([FileBase,'.',SSuffix],Source),
        handle_suffix(Source,SSuffix,TSuffix,FileBase,OTSuffix).

handle_suffix(Source,_SSuffix,TSuffix,FileBase,OTSuffix) :-
	\+ find_file(Source,_),
	TSuffix \== OTSuffix,
	warning_message("ancestor ~w of ~w.~w not found",
	                [Source,FileBase,TSuffix]).
handle_suffix(Source,_SSuffix,TSuffix,FileBase,OTSuffix) :-
	\+ find_file(Source,_),
	TSuffix == OTSuffix,
	atom_concat([FileBase,'.',TSuffix],Target),
	(  file_exists(Target) -> 
	   %% *** Check that this is correct in all cases...
	   warning_message("ancestor ~w of ~w not found",[Source,Target]) 
	   %% simple_message("~w is up to date",[Target]) 
	;  %% verbose_message("neither ~w nor ~w exist",[Source,Target]),
	   warning_message("neither ~w nor ~w exist",[Source,Target]),
	   fail ).
handle_suffix(Source,SSuffix,TSuffix,FileBase,OTSuffix) :-
	find_file(Source,_PathSource),
	atom_concat([FileBase,'.',TSuffix],Target),
        %% dependency_precond/3 used (see e.g., lpdoc)
 	call_unknown_nofail(_:dependency_precond(SSuffix,TSuffix,FileBase)),
	(  needs_processing(Source,Target)
	-> verbose_message("Generating ~w from ~w",[Target,Source]),
	   call_unknown_nofail(_:dependency_comment(SSuffix,TSuffix,FileBase)),
	   _:do_dependency(TSuffix,SSuffix,FileBase)
	;  (  TSuffix == OTSuffix
	   -> simple_message("~w is up to date",[Target]) 
	   ;  verbose_message("~w is up to date",[Target]),
	      true ) ).


%% Assumes that source file exists
needs_processing(Source,Target) :-
	( \+ file_exists(Target)
	; modif_time0(Source, STime),
	  modif_time0(Target, TTime),
	  STime > TTime ).

find_file(File,File) :-
	file_exists(File).
find_file(File,PathFile) :-
	call_unknown(_:vpath(Path)),
	atom_concat([Path,'/',File],PathFile),
	file_exists(PathFile).

%% ---------------------------------------------------------------------------
%% Support code
%% ---------------------------------------------------------------------------

:- pred dyn_load_cfg_module_into_make(ConfigFile) : sourcename 

   # "Used to load dynamically a module (typically, a @file{Makefile})
      into the make library from the application using the library.".

dyn_load_cfg_module_into_make(ConfigFile) :-
	use_module(ConfigFile).

%% :- pred dyn_load_cfg_file_into_make(ConfigFile) : sourcename 
%% 
%%    # "Used to load dynamically a user file (typically, a @file{Makefile})
%%       into the make library from the application using the library.".
%% 
%% Needed to access predicates generated in user Makefile.pl files
%% :- import(user,[do_dependency/3,dependency_exists/2,do_target/1,
%%                 target_exists/1,target_deps/2,target_comment/1,
%%                 dependency_comment/3]).
%% 
%% dyn_load_cfg_file_into_make(ConfigFile) :-
%% 	ensure_loaded(ConfigFile).

:- pred make_option(Option) : atm 

   # "Asserting/retracting facts of this predicate sets/clears library 
      options. Default is no options (i.e., the predicate is undefined). The 
      following values are supported:
@begin{verbatim}
make_option('-v'). % Verbose: prints progress messages (for debugging rules).
@end{verbatim}
  ".

:- data make_option/1.

%% Default is silent. Typically asserted by 
%% make_option('-v').

:- pred verbose_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a message, using
     the arguments in @var{ArgList}, if @tt{make_option('-v')} is
     defined. Otherwise nothing is printed.".

verbose_message(Mess,Args) :-
	(  call_unknown(make_option('-v'))
	-> simple_message(Mess,Args)
	;  true ).


%:- meta_predicate call_unknown_nofail(goal).

call_unknown_nofail(G) :-
	call_unknown(G),
	!.
call_unknown_nofail(_G).


%% :- meta_predicate call_unknown(goal).

% This is a local copy, to make package independent.
% Complication is so that flags are left as they were also upon failure.
call_unknown(G) :-
	prolog_flag(unknown,Old,fail),
	prolog_flag(quiet,QOld,error),
	(  %% nl, display('*** Calling (unknown): '), display(G), nl,
	   %% %% call(_:G), 
	   call(G),
	   %% display('*** ...success with '), display(G), nl,
	   prolog_flag(unknown,_,Old),
	   prolog_flag(quiet,_,QOld)
	;  prolog_flag(unknown,_,Old),
	   prolog_flag(quiet,_,QOld),
	   %% display('*** ...failure.'), nl,
	   fail ).

%%------------------------------------------------------------------------
%% VERSION CONTROL
%%------------------------------------------------------------------------
 
:- comment(version_maintenance,dir('../../version')).

:- comment(version(1*9+26,2002/11/20,12:57*08+'CET'), "Major
   improvement to @lib{make} library, and adaptation to Ciao
   1.9. Added new examples. Improved documentation of dependency
   rules.  (Manuel Hermenegildo)").

:- comment(version(1*9+25,2002/11/20,12:55*34+'CET'), "In @lib{make}
   lib: not supporting the use of 'user' makefiles any more (too hard
   to adapt everything to their scoping rules), i.e., at the moment
   makefiles must be modules. May add support for user files again in
   the future.  (Manuel Hermenegildo)").

%%------------------------------------------------------------------------

