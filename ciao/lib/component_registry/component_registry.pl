:- module(component_registry, [], [assertions]).

:- use_module(library(system)).
:- use_module(library(format)).
:- use_module(library(messages)).
:- use_module(library(read)).
:- use_module(library(streams)).

:- use_module(library(component_registry(component_registry_base))). % TODO: refine

:- doc(title, "Registry of Ciao Components").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jose F. Morales").

:- doc(module, "Load and access the registry of Ciao components
	(cached in @tt{HOME/.ciao.d/components/} or
	@tt{lib/component_registry/components/}). Each component can
	define its own library aliases and file search paths by means
	of @tt{CONFIG.pl} files stored in @tt{makedir/} directories.
	").

% ===========================================================================

:- export(show_components/0).
:- pred show_components # "Display the available components".
show_components :-
	format("Name\tPack\tType\tPath\n", []),
	format("-------\t-------\t-------\t-------\n", []),
	(
	    component_description(Name, Pack, Type, Path),
	    format("~w\t~w\t~w\t~w\n", [Name, Pack, Type, Path]),
	    fail
	;
	    true
	).

% ===========================================================================

:- doc(section, "Hooks for File Search Path and Library Alias").

:- multifile file_search_path/2.
:- dynamic file_search_path/2.

:- multifile library_directory/1.
:- dynamic library_directory/1.

file_search_path(Alias, Path) :-
	file_search_path_auto(Alias, Path).

library_directory(Path) :-
	library_directory_auto(Path).

% (This uses the file_search_path and library_directory for components)

% ===========================================================================

:- doc(section, "Component Data").

:- export(component_description/4).
:- data component_description/4. % TODO: Loaded from _auto

:- data file_search_path_auto/2. % TODO: Loaded from _auto
:- data library_directory_auto/1. % TODO: Loaded from _auto

cleanup_db :-
	retractall_fact(library_directory_auto(_)),
	retractall_fact(file_search_path_auto(_, _)),
	retractall_fact(component_description(_, _, _, _)).

% :- multifile component_description/4.

:- export(component_src/2).
:- pred component_src/2 # "Gets the path name where the sources of the
	component reside.".

component_src(Name, Path) :-
	component_description(Name, _, _, Path).

% Currently is equal to component_src/2, but in the future should be
% different

:- export(component_ins/2).
:- pred component_ins/2 # "Gets the path name where the component is
	installed.".

component_ins(Name, Path) :-
	component_description(Name, _, _, Path).

% ===========================================================================

:- doc(section, "Loading and Initialization of Component Settings").

:- initialization(load_db).
load_db :-
	cleanup_db,
	load_db_sysavail(all),
	load_db_sysavail(user).

load_db_sysavail(SysAvail) :-
	component_setup_dir(SysAvail, SetupDir),
	( file_exists(SetupDir) ->
	    directory_files(SetupDir, Files),
	    load_db_files(Files, SetupDir)
	; true
	).

load_db_files([], _).
load_db_files([File|Files], SetupDir) :-
	atom_concat(BaseFile,'.pl',File),
	atom_concat(SetupDir, BaseFile, Component),
	!,
	load_db_file(Component),
	load_db_files(Files, SetupDir).
load_db_files([_File|Files], SetupDir) :-
	load_db_files(Files, SetupDir).

load_db_file(File) :-
	catch(load_db_file_(File), E, handle_load_error(File, E)).

handle_load_error(File, E) :-
	absolute_file_name(File, AbsFile),
	show_message(warning, loc(AbsFile, 1, 1),
	    "Unable to load ~w. Thrown exception: ~w", [File, E]).

load_db_file_(Library) :-
	absolute_file_name(Library, AbsFile),
	open_input(AbsFile, DD),
	load_loop,
	close_input(DD).

load_loop :-
	repeat,
	read(Data),
	( Data = end_of_file ->
	    true % finish
	; process_data(Data),
	  fail % loop
	),
	!.

process_data(library_directory_auto(X)) :- !,
	assertz_fact(library_directory_auto(X)).
process_data(file_search_path_auto(X, Y)) :- !,
	assertz_fact(file_search_path_auto(X, Y)).
process_data(component_description(X, Y, Z, V)) :- !,
	assertz_fact(component_description(X, Y, Z, V)).
process_data(X) :-
	throw(unrecognized_data(X)).
