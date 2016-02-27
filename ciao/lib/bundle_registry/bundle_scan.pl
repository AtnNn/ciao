:- module(bundle_scan, [], [dcg, assertions]).

:- doc(title, "Scanning and Registering of Bundles").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jose F. Morales (modifications)").

:- doc(module, "This module contains code to scan bundles in a source
tree and register bundles in the system.

@begin{alert}
  Make sure that this code does not end in executables unless
  necessary (it should not be in the dependencies of usual user
  programs).
@end{alert}").

:- use_module(library(read)).
:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(messages)).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(bundle_registry(bundle_registry_load)), [bundle_src/2]).
:- use_module(library(bundle_registry(bundle_registry_base))).
:- use_module(library(dirutils), [path_concat/3]).
:- use_module(library(system_extra)).

% ----------------------------------------------------------------------------
% Place here Prolog code that does not require auto generated modules
% or config files.
% ----------------------------------------------------------------------------

% TODO: Duplicated in ciao_config_db.pl (what would be the right place to put it?)
perms(perm(rwX, rwX, rX)).

:- use_module(engine(system_info), [ciao_lib_dir/1]).

:- export(bundle_scan/1).
:- pred bundle_scan(Src) # "Scan all the bundles under the @var{Src}
   directory and annotate the results in the bundle database (creating
   it if it does not exist)".

bundle_scan(Src) :-
	ciao_lib_dir(LibDir),
	bundle_db_dir(LibDir, DbDir),
 	% Create the directory for the bundle database
	perms(Perms),
	mkdir_perm(DbDir, Perms),
 	mark_directory(noinstall, DbDir),
	%
	bundles_at_dir(Src, Bundles),
	install_src_bundles_cfg(Bundles, Src, DbDir).

% ---------------------------------------------------------------------------

% TODO: install_* is not a good name (it does not install anything, just registers it)
install_src_bundles_cfg([], _Src, _DbDir).
install_src_bundles_cfg([Bundle|Bundles], Src,  DbDir) :-
	path_concat(Src, Bundle, BundlePath),
	install_src_bundle_cfg(BundlePath, DbDir),
	install_src_bundles_cfg(Bundles, Src, DbDir).

% List all the bundles under @var{Src}
%:- export(bundles_at_dir/2).
bundles_at_dir(Src, Bundles) :-
	directory_files(Src, Files),
	find_bundles(Files, Src, Bundles).

find_bundles([],           _,    []).
find_bundles([File|Files], Src, [File|Bundles]) :-
	nonvar(File), \+ File = '..',
	path_concat(Src, File, C1),
	\+ directory_has_mark(nocompile, C1),
	is_bundle_dir(C1),
	!,
	find_bundles(Files, Src, Bundles).
find_bundles([_File|Files], Src, Bundles) :-
	find_bundles(Files, Src, Bundles).

% TODO: export?
% @var{C1} is the directory for a bundle (i.e., contains a
% Manifest.pl file)
is_bundle_dir(C1) :-
	atom_concat(C1, '/Manifest.pl', ManifestFile),
	file_exists(ManifestFile).

% Install the configuration file for one bundle (~instype=local)
install_src_bundle_cfg(BundlePath, DbDir) :-
	atom_concat(BundlePath, '/Manifest.pl', ManifestFile),
	loop_read_file(ManifestFile, ManifestList),
	%
	member_chk(bundle_name(BundleName), ManifestList),
	member_chk(bundle_pack(BundlePack), ManifestList),
	member_chk(bundle_type(BundleType), ManifestList),
	member_chk(src_alias_paths(SrcRelAliasPaths), ManifestList),
	abs_alias_paths(SrcRelAliasPaths, BundlePath, SrcAliasPaths),
	%
	( member(ins_alias_paths(InsRelAliasPaths), ManifestList) ->
	    true
	; InsRelAliasPaths = []
	),
	%
	abs_alias_paths(InsRelAliasPaths, BundlePath, InsAliasPaths),
	atom_concat(DbDir, BundleName, BundleName1),
	atom_concat(BundleName1, '_src_auto.pl', SetupName),
	open_output(SetupName, UO),
	write_alias_paths(SrcAliasPaths),
	write_alias_paths(InsAliasPaths),
	portray_clause(bundle_description(BundleName, BundlePack,
		BundleType, BundlePath)),
	close_output(UO),
	perms(Perms),
	set_perms(SetupName, Perms).

% Install the configuration file for one bundle (~instype=global)
:- export(install_ins_bundle_cfg/3). % from makedir_part_{lpdoc,ciaopp}
install_ins_bundle_cfg(BundleName, RootRealLibDir, Base) :-
	bundle_src(BundleName, BundlePath),
	atom_concat(BundlePath, '/Manifest.pl', ManifestFile),
	loop_read_file(ManifestFile, ManifestList),
	%
	( member(ins_alias_paths(InsRelAliasPaths), ManifestList) ->
	    true
	; InsRelAliasPaths = []
	),
	%
	abs_alias_paths(InsRelAliasPaths, Base, AliasPaths),
	%
	atom_concat(BundleName, '_ins_auto.pl', CfgName),
	bundle_cfg_file(CfgName, RootRealLibDir, File),
	open_output(File, UO),
	write_alias_paths(AliasPaths),
	close_output(UO).

write_alias_paths(AliasPaths) :-
	list(AliasPaths, write_alias_path). % TODO: port and use maplist

write_alias_path(AliasName=AliasPath) :- !,
	portray_clause(file_search_path_auto(AliasName, AliasPath)).
write_alias_path(AliasPath) :-
	portray_clause(library_directory_auto(AliasPath)).

% Remove a configuration file for a bundle (~instype=global)
:- export(uninstall_ins_bundle_cfg/2). % from makedir_part_{lpdoc,ciaopp}
uninstall_ins_bundle_cfg(BundleName, RootRealLibDir) :-
	atom_concat(BundleName, '_ins_auto.pl', CfgName),
	bundle_cfg_file(CfgName, RootRealLibDir, File),
	my_del_file_nofail(File).

% TODO: merge with system_extra:del_file_nofail?
my_del_file_nofail(FileName) :-
	( file_exists(FileName) -> delete_file(FileName) ; true ).

bundle_cfg_file(CfgName, RootRealLibDir, File) :-
	bundle_db_dir(RootRealLibDir, DbDir),
	atom_concat([DbDir, CfgName], File).

% Add @var{Base} to the beginning of each relative alias path to obtain absolute paths.
abs_alias_paths([], _, []).
abs_alias_paths([RelAliasPath|RelAliasPaths], Base, [AliasPath|AliasPaths]) :-
	abs_alias_path(RelAliasPath, Base, AliasPath),
	abs_alias_paths(RelAliasPaths, Base, AliasPaths).

abs_alias_path(AliasName=RelAliasPath, Base, AliasName=AliasPath) :- !,
	path_concat(Base, RelAliasPath, AliasPath).
abs_alias_path(RelAliasPath, Base, AliasPath) :-
	path_concat(Base, RelAliasPath, AliasPath).

% ---------------------------------------------------------------------------

% Load a Manifest.pl file
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

% ---------------------------------------------------------------------------
% Directory marks

% TODO: Complete missing cases, implement unmark? interact with SVN?

% TODO: Directory marks (move to a module)
mark_directory(noinstall, Dir) :-
 	% creates empty NOINSTALL file (which marks that this
 	% directory will not be installed)
 	atom_concat(Dir, '/NOINSTALL', F),
 	touch(F).

directory_has_mark(nocompile, Dir) :-
	atom_concat(Dir, '/NOCOMPILE', F),
	file_exists(F).

