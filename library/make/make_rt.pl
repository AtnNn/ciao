:- module(make_rt,[make/1,make_option/1,
	           make_toplevel/1,dyn_load_cfg_module_into_make/1],[]).

%% ISO Prolog-like modules
:- use_module(library(format),[format/3]).
:- use_module(library(compiler),[ensure_loaded/1,use_module/1]).
:- use_module(library(aggregates),[findall/3]).

%% CIAO libraries
:- use_module(library(errhandle),[handle_error/2]).
:- use_module(library(lists),[append/3]).
:- use_module(library(filenames),[file_name_extension/3]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(system),[file_exists/1,modif_time0/2]).
:- use_module(library(messages),
	      [simple_message/2,warning_message/2,error_message/2]).
:- use_module(library('assertions/assrt_lib'),[set_libs/2]).

%% ---------------------------------------------------------------------------

make([]) :- 
	!.
make([Target|Targets]) :- 
	!,
	make(Target),
	make(Targets).

make(Target) :- 
	call_unknown(_:target_deps(Target,[])), 
	!,
	verbose_message("making unconditional target ~w",[Target]),
	call_unknown_nofail(_:target_comment(Target)),
	_:do_target(Target).
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
%% Procesing file dependencies
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
	verbose_message("~w.~w has no ancestors",[FileBase,TSuffix]).
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
	   simple_message("~w is up to date",[Target]) 
	;  verbose_message("neither ~w nor ~w exist",[Source,Target]),
	   fail ).
handle_suffix(Source,SSuffix,TSuffix,FileBase,OTSuffix) :-
	find_file(Source,_PathSource),
	atom_concat([FileBase,'.',TSuffix],Target),
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
%% Top-level: for using lpmake as a standalone utility
%% ---------------------------------------------------------------------------

make_toplevel(ApplName) :-
	prolog_flag(argv, Args, _),
 	catch(parse_args(Args,ApplName), E, handle_make_error(E)).

handle_make_error(make_args_error(Format,Args,ApplName)) :- 
	append("~nERROR: ",Format,T1),
	append(T1,"~n~n",T2),
	format(user_error,T2,Args),
	report_usage(ApplName,_Type,'').
handle_make_error(make_error(Format,Args)) :- 
	error_message(Format,Args).
handle_make_error(error(Error,Where)) :- 
	handle_error(Error, Where).

parse_args(['-h'|Args],ApplName) :- 
	parse_other_args_and_load(Args,Type,ConfigFile,[]),
	!,
	report_usage(ApplName,Type,ConfigFile).
parse_args(['-help'|Args],ApplName) :- 
	parse_other_args_and_load(Args,Type,ConfigFile,[]),
	!,
	report_usage(ApplName,Type,ConfigFile).
parse_args(Args,_ApplName) :- 
	parse_other_args_and_load(Args,_Type,_ConfigFile,Targets),
	!,
        process_targets(Targets).
parse_args(Args,ApplName) :-
	throw(make_args_error("~nIllegal arguments: ~w~n~n",[Args],ApplName)).
	

parse_other_args_and_load([Type,ConfigFile|Targets],Type,ConfigFile,Targets):- 
	Type = '-m',
	!,
	load_config_file(Type,"module",ConfigFile).
parse_other_args_and_load([Type,ConfigFile|Targets],Type,ConfigFile,Targets):- 
	Type = '-u',
	!,
	load_config_file(Type,"user file",ConfigFile).
parse_other_args_and_load(Targets,Type,ConfigFile,Targets) :- 
	\+ member('-h', Targets),
	\+ member('-u', Targets),
	\+ member('-m', Targets),
	!,
	Type = '-m',
	ConfigFile = 'Makefile.pl',
	load_config_file(Type,"(default) module",ConfigFile).

load_config_file(Type,Text,ConfigFile) :-
	(  file_exists(ConfigFile) 
	-> verbose_message("loading ~s ~w",[Text,ConfigFile]),
	(  Type = '-m'
	   -> use_module(ConfigFile)
	   ;  ensure_loaded(ConfigFile) )
	;  throw(make_error("file ~w does not exist",[ConfigFile])) ).

%% This used to force make from outside to load dynamically a module 
dyn_load_cfg_module_into_make(ConfigFile) :-
	use_module(ConfigFile).

%% If no target process default if defined
process_targets([]) :-
	call_unknown(_:target_exists(default)),
	!,
	make(default).
%% else process first target
process_targets([]) :-
	call_unknown(_:target_exists(Target)),
	!,
	make(Target).
%% If targets specified, process them
process_targets(Targets) :-
	!,
	make(Targets).

%% This is in narrow format because that way it looks nicer in a man page.
usage_message("

Supported command line options:

[ -u <.../Configfile.pl> ] <command1> ... <commandn>

  Process commands <command1> ... <commandn>, using user 
  file <.../Configfile.pl> as configuration file. If no 
  configuration file is specified a file 'Makefile.pl' in 
  the current directory will be used. 

[ -m <.../Configfile.pl> ] <command1> ... <commandn>

  Same as above, but the configuration file is a module. 
  Making this file a module is useful to implement 
  inherintance across diferent configuration files, i.e., 
  the values declared in a configuration file can be 
  easily made to override those defined in another.

-h     [ -u <.../Configfile.pl> ]
-h     [ -m <.../Configfile.pl> ]
-help  [ -u <.../Configfile.pl> ]
-help  [ -m <.../Configfile.pl> ]

  Print this help message. If a configuration file is given, 
  and the commands in it are commented, then information on 
  these commands is also printed.

").

report_usage(ApplName,Type,LoadedFile) :-
	format(user_error,"~nUsage:~n~n       ~w <option(s)> <command(s)>~n",
	                  [ApplName]),
	usage_message(Text),
	format(user_error,Text,[]),
	format(user_error,"~nSupported commands:~n",[]),
	report_commands(Type,LoadedFile).

report_commands(_Type,'') :-
	!,
	format(user_error,"~n(no configuration file loaded)~n",[]).
report_commands(Type,LoadedFile) :-
	(  Type = '-m'
	-> TypeText = "module"
	;  TypeText = "user file" ),
	format(user_error,"[From ~s: ~w]~n~n",[TypeText,LoadedFile]),
	(  findall(Target,call_unknown(_:target_exists(Target)),Targets),
	   Targets = [_ |_ ]
	-> ( member(Target,Targets),
	     format(user_error,"    ~w:~n",[Target]),
	     (  call_unknown(_:target_comment(Target))
	     -> true
	     ;  format(user_error,"    (no information available)~n",[]) ),
	     format(user_error,"~n",[]),
	     fail
	   ; true )
	;  format(user_error,
           "(no documented commands in the configuration file)~n",
	   []) ).

%% ---------------------------------------------------------------------------
%% Support code
%% ---------------------------------------------------------------------------

:- data make_option/1.

make_option('-v').

verbose_message(Mess,Args) :-
	(  call_unknown(make_option('-v'))
	-> simple_message(Mess,Args)
	;  true ).


:- meta_predicate call_unknown_nofail(goal).

call_unknown_nofail(G) :-
	call_unknown(G),
	!.
call_unknown_nofail(_G).


:- meta_predicate call_unknown(goal).

% This is a local copy, with less error checking.
% Complication is so that flags are left as it was also upon failure.
call_unknown(G) :-
	prolog_flag(unknown,Old,fail),
	prolog_flag(quiet,QOld,error),
	(  %% call(_:G), 
	   call(G),
	   prolog_flag(unknown,_,Old),
	   prolog_flag(quiet,_,QOld)
	;  prolog_flag(unknown,_,Old),
	   prolog_flag(quiet,_,QOld),
	   fail ).

%% ---------------------------------------------------------------------------
