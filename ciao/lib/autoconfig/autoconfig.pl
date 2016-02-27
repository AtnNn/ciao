:- module(autoconfig, _, [assertions]).

:- use_module(library(system)).
:- use_module(library(format)).
:- use_module(library(messages)).
:- use_module(library(read)).
:- use_module(library(streams)).

% :- use_module(library(compiler), [use_module/1]).
:- use_module(library(autoconfig(autoconfig_base))).


:- initialization(autoconfig_components).

:- doc(author, "Edison Mera").
:- doc(title, "Automatic configuration module -base predicates").

:- doc(module, "This module is used to load automatically the
   modules contained in the folder components when a program starts.
   This can be used to define path aliases.  Such modules can
   also define the extra components of a system.").

:- data component_description/4.

:- data main_lpdoc_settings/2.

% :- multifile component_description/4.

:- pred component_src/2 # "Gets the path name where the sources of the
	component reside.".

component_src(Name, Path) :-
	component_description(Name, _, _, Path).

:- pred component_ins/2 # "Gets the path name where the component is
	installed.".


:- multifile file_search_path/2.
:- dynamic file_search_path/2.

:- multifile library_directory/1.
:- dynamic library_directory/1.

:- data file_search_path_auto/2.
:- data library_directory_auto/1.

file_search_path(Alias, Path) :-
	file_search_path_auto(Alias, Path).

library_directory(Path) :-
	library_directory_auto(Path).

% Currently is equal to component_src/2, but in the future should be
% different

component_ins(Name, Path) :-
	component_description(Name, _, _, Path).

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

autoconfig_components :-
	cleanup_autoconfig_db,
	load_components_sysavail(all),
	load_components_sysavail(user).

cleanup_autoconfig_db :-
	retractall_fact(library_directory_auto(_)),
	retractall_fact(file_search_path_auto(_, _)),
	retractall_fact(component_description(_, _, _, _)).

load_components_sysavail(SysAvail) :-
	component_setup_dir(SysAvail, SetupDir),
	(
	    file_exists(SetupDir) ->
	    directory_files(SetupDir, Files),
	    component_list(Files, SetupDir, Components),
	    list(Components, load_config_protected)
	;
	    true
	).

load_autoconfig_db(Library) :-
	absolute_file_name(Library, FileName),
	open_input(FileName, DD),
	load_autoconfig_loop,
	close_input(DD).

load_autoconfig_loop :-
	repeat,
	read(Clause),
	process_autoconfig_fact(Clause),
	!.

process_autoconfig_fact(end_of_file).
process_autoconfig_fact(Clause     ) :- assertz_fact(Clause), fail.

load_config_protected(Library) :-
	catch(load_autoconfig_db(Library), E, handle_autoconfig_error(Library, E)).

handle_autoconfig_error(Library, E) :-
	absolute_file_name(Library, FileName),
	show_message(warning, loc(FileName, 1, 1),
	    "Unable to load ~w.  Thrown exception was ~w",
	    [Library, E]).

component_list([], _, []).
component_list([File|Files], SetupDir, [Component|Components]) :-
	atom_concat(BaseFile,'.pl',File),
	atom_concat(SetupDir, BaseFile, Component),
	!,
	component_list(Files, SetupDir, Components).

component_list([_File|Files], SetupDir, Components) :-
	component_list(Files, SetupDir, Components).
