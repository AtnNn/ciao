:- module(_, [], [dcg]).

:- use_module(library(file_utils)).
:- use_module(library(read)).
:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(messages)).

:- use_module(library(component_registry(component_registry_base))).
:- use_module(library(distutils(dirutils)), [path_concat/3]).
:- use_module(library(make(system_extra))).

% ----------------------------------------------------------------------------
% Place here prolog code that do not require auto generated modules or config
% files
% ----------------------------------------------------------------------------

% TODO: Duplicated in DOCCOMMON.pl (what would be the right place to put it?)
perms(perm(rwX, rwX, rX)).

% Scan all the components under @var{Src} directory
:- export(component_scan/1).
component_scan(Src) :-
	component_setup_dir(all, SetupDir),
	perms(Perms),
	mkdir_perm(SetupDir, Perms),
	atom_concat(SetupDir, '/NOINSTALL', SetupDirFND),
	string_to_file("", SetupDirFND),
	components(Src, Components),
	config_source_components_dir(Components, Src, SetupDir).

config_source_components_dir([],                     _Src, _SetupDir).
config_source_components_dir([Component|Components], Src,  SetupDir) :-
	path_concat(Src, Component, ComponentPath),
	config_source_component(ComponentPath, SetupDir),
	config_source_components_dir(Components, Src, SetupDir).

% List all the components of CiaoDE
components(Src, Components) :-
	directory_files(Src, Files),
	find_components(Files, Src, Components).

find_components([],           _,    []).
find_components(['..'|Files], Src,  Components) :-
	!,
	find_components(Files, Src, Components).
find_components([File|Files], Src, [File|Components]) :-
	path_concat(Src, File, C1),
	atom_concat(C1, '/NOCOMPILE', NoCompile),
	\+file_exists(NoCompile),
	atom_concat(C1, '/makedir', ComponentDir),
	file_exists(ComponentDir),
	file_property(ComponentDir, type(directory)),
	atom_concat(ComponentDir, '/CONFIG.pl', ComponentConfig),
	file_exists(ComponentConfig),
	!,
	find_components(Files, Src, Components).
find_components([_File|Files], Src, Components) :-
	find_components(Files, Src, Components).

config_source_component(ComponentPath, SetupDir) :-
	atom_concat(ComponentPath, '/makedir/CONFIG.pl', ConfigFile),
	loop_read_file(ConfigFile, ConfigList),
	generate_auto_loadable_file(ConfigList, ComponentPath, SetupDir).

loop_read_file(FileName, ReadList) :-
	open(FileName, read, SO),
	loop_read(SO, ReadList),
	close(SO).

loop_read(SO, [T|Ts]) :-
	read(SO, T),
	T \== end_of_file,
	!,
	loop_read(SO, Ts).
loop_read(_SO, []).

member_chk(A, B) :-
	( member(A, B) -> true
	; warning_message("Element ~w not found in ~w", [A, B])
	).

generate_auto_loadable_file(ConfigList, ComponentPath, SetupDir) :-
	member_chk(component_name(ComponentName),     ConfigList),
	member_chk(component_pack(ComponentPack),     ConfigList),
	member_chk(component_type(ComponentType),     ConfigList),
	member_chk(src_alias_paths(SrcRelAliasPaths), ConfigList),
	abs_alias_paths(SrcRelAliasPaths, ComponentPath, SrcAliasPaths),
	(
	    member(ins_alias_paths(InsRelAliasPaths), ConfigList) ->
	    abs_alias_paths(InsRelAliasPaths, ComponentPath, InsAliasPaths)
	;
	    InsAliasPaths = []
	),
	!,
	atom_concat(SetupDir,       ComponentName,  ComponentName1),
	atom_concat(ComponentName1, '_src_auto.pl', SetupName),
	open_output(SetupName, UO),
%	display( '% Warning: automatically generated file \n\n' ),
	write_alias_paths_configfile(SrcAliasPaths),
	write_alias_paths(InsAliasPaths),
	portray_clause(component_description(ComponentName, ComponentPack,
		ComponentType, ComponentPath)),
	close_output(UO),
	perms(Perms),
	set_perms(SetupName, Perms).
