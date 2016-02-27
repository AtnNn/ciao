:- module(autoconfig_base,
	[ component_setup_dir/2
	, component_setup_dir/3
	, abs_alias_paths/3
	, install_alias_paths/4
	, install_alias_paths/3
	, uninstall_alias_paths/3
	, uninstall_alias_paths/2
	, write_alias_paths_configfile/1
	, write_alias/2
	, write_alias_paths/1
	], [assertions]).

:- use_module(library(terms)).
:- use_module(library(system)).
:- use_module(library(write)).
:- use_module(library(streams)).
:- use_module(library(distutils(find)), [path_concat/3]).
:- use_module(library(distutils(setperms))).

:- doc(author, "Edison Mera").
:- doc(title, "Automatic configuration module -base predicates").

component_setup_dir(all, SetupDir) :-
	absolute_file_name(library(autoconfig), '_opt', '.pl', '.', _, _,
	    RootDir),
	atom_concat(RootDir, '/components/', SetupDir).
component_setup_dir(user, SetupDir) :-
	getenvstr('HOME', Home),
	atom_codes(AHome, Home),
	atom_concat(AHome, '/.ciao.d/components/', SetupDir).

component_setup_dir(all, RealLibDir, SetupDir) :-
	atom_concat(RealLibDir, '/lib/autoconfig/components/', SetupDir).
component_setup_dir(user, _, SetupDir) :-
	component_setup_dir(user, SetupDir).

abs_alias_paths([], _, []).
abs_alias_paths([RelAliasPath|RelAliasPaths], Base, [AliasPath|AliasPaths]) :-
	abs_alias_path(RelAliasPath, Base, AliasPath),
	abs_alias_paths(RelAliasPaths, Base, AliasPaths).

abs_alias_path(AliasName=RelAliasPath, Base, AliasName=AliasPath) :-
	!,
	path_concat(Base, RelAliasPath, AliasPath).
abs_alias_path(RelAliasPath, Base, AliasPath) :-
	path_concat(Base, RelAliasPath, AliasPath).

install_alias_paths(ModuleName, BuildRoot, RealLibDir, AliasPaths) :-
	component_module(all, ModuleName, BuildRoot, RealLibDir, _, File),
	write_alias(File, AliasPaths).

install_alias_paths(SysAvail, Module, AliasPaths) :-
	component_setup_dir(SysAvail, ComponentDir),
	mkdir_perm(ComponentDir, perm(rwX,rwX,rX)),
	atom_concat([ComponentDir, Module, '.pl'], File),
	write_alias(File, AliasPaths).

write_alias(Module, AliasPaths) :-
	open_output(Module, UO),
% 	display('% Warning: automatically generated file \n\n'),
	write_alias_paths_configfile(AliasPaths),
	close_output(UO).

write_alias_paths_configfile(AliasPaths) :-
	write_alias_paths(AliasPaths).

write_alias_paths(AliasPaths) :-
	list(AliasPaths, write_alias_path).

write_alias_path(AliasName=AliasPath) :-
	!,
	portray_clause(file_search_path_auto(AliasName, AliasPath)).
write_alias_path(AliasPath) :-
	portray_clause(library_directory_auto(AliasPath)).

uninstall_alias_paths(ModuleName, BuildRoot, RealLibDir) :-
	component_module(all, ModuleName, BuildRoot, RealLibDir, _, Module),
	del_file_nofail(Module).

uninstall_alias_paths(SysAvail, Module) :-
	component_setup_dir(SysAvail, ComponentDir),
	atom_concat([ComponentDir, Module, '.pl'], File),
	del_file_nofail(File).

del_file_nofail(FileName) :-
	file_exists(FileName) -> delete_file(FileName) ; true.

component_module(SysAvail, ModuleName, BuildRoot, RealLibDir, BRDir, Module) :-
	component_setup_dir(SysAvail, RealLibDir, SetupDir),
	atom_concat(BuildRoot, SetupDir, BRDir),
	atom_concat(BRDir, ModuleName, Module).
