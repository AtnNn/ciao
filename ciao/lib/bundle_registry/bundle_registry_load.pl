:- module(bundle_registry_load, [], [assertions]).

:- doc(title, "Registry of Bundles (Loader and DB)").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jose F. Morales").

:- doc(module, "Load and access the registry of Ciao
	@concept{bundles}, which stores the registered bundles and
	their configuration values.

        Each bundle specifies configuration values (such as library
	aliases and file search paths) in the source @tt{Manifest.pl}
	files (stored in each bundle root directory).

        See @lib{bundle_registry(bundle_scan)} for the code to scan
        and register bundles.").

:- use_module(library(system)).
:- use_module(library(format)).
:- use_module(library(messages)).
:- use_module(library(read)).
:- use_module(library(streams)).

:- use_module(library(bundle_registry(bundle_registry_base)), [bundle_db_dir/2]).

% ===========================================================================

:- export(show_bundles/0).
:- pred show_bundles # "Display the registered bundles".
show_bundles :-
	format("Name\tPack\tType\tPath\n", []),
	format("-------\t-------\t-------\t-------\n", []),
	(
	    bundle_description(Name, Pack, Type, Path),
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

% (This uses the file_search_path and library_directory for bundles)

% ===========================================================================

:- doc(section, "Bundle Data").

:- export(bundle_description/4).
:- data bundle_description/4. % TODO: Loaded from _auto files

:- data file_search_path_auto/2. % TODO: Loaded from _auto files
:- data library_directory_auto/1. % TODO: Loaded from _auto files

cleanup_db :-
	retractall_fact(library_directory_auto(_)),
	retractall_fact(file_search_path_auto(_, _)),
	retractall_fact(bundle_description(_, _, _, _)).

:- export(bundle_src/2).
:- pred bundle_src/2 # "Gets the path name where the sources of the
	bundle reside.".
bundle_src(Name, Path) :-
	bundle_description(Name, _, _, Path).

% TODO: Currently is equal to bundle_src/2, but in the future should be
%       different.
:- export(bundle_ins/2).
:- pred bundle_ins/2 # "Gets the path name where the bundle is
	installed.".
bundle_ins(Name, Path) :-
	bundle_description(Name, _, _, Path).

% ===========================================================================

:- doc(section, "Loading and Initialization of Bundle Settings").

% TODO: Implement 'reload'? Can it be done safely without restart?
%       It should be possible, at least for the top level, to manage
%       (install, uninstall, etc.) bundles.

:- initialization(load_db).
load_db :-
	cleanup_db,
	ciao_lib_dir(LibDir),
	bundle_db_dir(LibDir, DbDir),
	( file_exists(DbDir) ->
	    directory_files(DbDir, Files),
	    load_db_files(Files, DbDir)
	; true
	).

load_db_files([], _).
load_db_files([File|Files], DbDir) :-
	atom_concat(BaseFile,'.pl',File),
	atom_concat(DbDir, BaseFile, Bundle),
	!,
	load_db_file(Bundle),
	load_db_files(Files, DbDir).
load_db_files([_File|Files], DbDir) :-
	load_db_files(Files, DbDir).

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
process_data(bundle_description(X, Y, Z, V)) :- !,
	assertz_fact(bundle_description(X, Y, Z, V)).
process_data(X) :-
	throw(unrecognized_data(X)).
