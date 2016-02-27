:- module(_, [], [assertions, fsyntax, hiord]).
:- doc(title,  "Lpdoc library Compilation/Installation module").
:- doc(author, "Edison Mera").
:- doc(module, "This file is part of the CiaoDE installation system.").

:- use_module(ciaodesrc(makedir('ConfigValues')), [stop_if_error/1]).
:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(autoconfig)).
:- use_module(library(make(system_extra)), [no_tr_nl/2]).
:- use_module(library(terms),              [atom_concat/2]).

:- reexport(ciaodesrc(makedir('DOCCOMMON'))).

:- export(install_log/1).
install_log := ~atom_concat(~component_src(ciaode), '/install.log').

versionfileabs(ciaode, Value) :- !,
	versionfileabs(ciao, Value).
versionfileabs(Component) := ~atom_concat([~component_src(ciaode), '/',
		Component, '/version/GlobalVersion']).

:- export(vers/2).
vers(Component, Version) :-
	no_tr_nl(~file_to_string(~versionfileabs(Component)), VS),
	atom_codes(Version, VS).

patchfileabs(ciaode, Value) :- !,
	patchfileabs(ciao, Value).
patchfileabs(Component, Value) :-
	Value = ~atom_concat([~component_src(ciaode), '/', Component,
		'/version/GlobalPatch']).

:- export(patch/2).
patch(Component, Patch) :-
	no_tr_nl(~file_to_string(~patchfileabs(Component)), PS),
	atom_codes(Patch, PS).

:- export(componentversion/2).
% componentversion(ciaode,'1.13.0').
componentversion(Component) := ~atom_concat([~vers(Component), '.',
		~patch(Component)]).

:- export(versionmain/2).
versionmain(Component) := ~atom_concat([Component, '-', ~vers(Component)]).

:- export(vpmain/2).
% vpmain(ciaode,'ciaode-1.13.0').
vpmain(Component) :=
	~atom_concat([~versionmain(Component), '.', ~patch(Component)]).

:- export(win32vpmain/2).
win32vpmain(Component) := atom_concat([~vpmain(Component), 'Win32']).

:- export(engine/1).
engine := Engine :-
	file_to_string(~versionfileabs(ciao), VS),
	file_to_string(~patchfileabs(ciao),   PS),
	atom_codes(V, ~no_tr_nl(VS)),
	atom_codes(P, ~no_tr_nl(PS)),
	Engine = ~atom_concat(['ciao-', V, '.', P]).

% ciaodeversionmain('ciaode-1.13').
% ciaodeversionmain := ~versionmain(~wholesystem).
% ciaodevpmain('ciaode-1.13.0').
% ciaodevpmain := ~vpmain(~wholesystem).

:- export(version/2).
version(Component) :=
	~atom_concat([~versionpack(Component), '-', ~svn_revision_atom]).

% versionpack(ciaode, 'CiaoDE-1.13.0').
versionpack(Component) := ~atom_concat([~packname(Component), '-',
		~componentversion(Component)]).

:- export(versionpure/2).
% versionpure(ciaode, '1.13.0-7526').
versionpure(Component) := ~atom_concat([~componentversion(Component),
		'-', ~svn_revision_atom]).

:- export(wholesystem/1).
wholesystem(Name) :-
	component_description(Name, _, whole, _).

:- export(ciaodebase/1).
ciaodebase := ~packname(~wholesystem).

:- export(packname/2).
packname(Name, Pack) :-
	component_description(Name, Pack, _Type, _Path).

:- export(basiccomponent/1).
basiccomponent(Name) :-
	component_description(Name, _Pack, basic, _Path).
:- export(extracomponent/1).
extracomponent(Name) :-
	component_description(Name, _Pack, extra, _Path).

:- export(component/1).
component := ~basiccomponent.
component := ~extracomponent.

:- use_module(library(distutils(lpdoc_installer))).
:- use_module(library(distutils)).

:- export(install_docdir/1).
install_docdir(Component) :-
	SourceDir = ~build_doc_dir,
	vpmain(Component, VPComponent),
	(
	    docformat(DocFormat),
	    docformatdir(DocFormat, TargetDir),
	    FileName = ~atom_concat([VPComponent, '.', DocFormat]),
	    Source = ~atom_concat([SourceDir, '/', FileName]),
	    ( file_exists(Source) ->
		Target = ~atom_concat(TargetDir, FileName),
		( Source == Target ->
		    note(['Skipping copy of ', Target])
		;
		    make_dirpath(TargetDir),
		    copy_file_or_dir(Source, Target, [overwrite, timestamp])
		),
		install_docdir_hook(DocFormat, Target)
	    ;
		warning(['File ', Source, ' not generated yet. Skipping copy'])
	    ),
	    fail
	;
	    true
	).

:- export(uninstall_docdir/1).
uninstall_docdir(Component) :-
	vpmain(Component, VPComponent),
	(
	    docformat(DocFormat),
	    docformatdir(DocFormat, TargetDir),
	    FileName = ~atom_concat([VPComponent, '.', DocFormat]),
	    Target = ~atom_concat(TargetDir, FileName),
	    uninstall_docdir_hook(DocFormat, Target),
	    delete_dir_rec(Target),
	    fail
	;
	    true
	).

% This allow us to test the info files
install_docdir_hook(info, Target) :- !,
	install_info_dir(Target, ~build_doc_dir).
install_docdir_hook(_, _).

uninstall_docdir_hook(info, Target) :- !,
	uninstall_info_dir(Target, ~build_doc_dir).
uninstall_docdir_hook(_, _).

:- export(invoke_lpdoc/2).
invoke_lpdoc(ConfigFile, Options) :-
	invoke_lpdoc(~atom_concat([' -d stop_if_error=', ~stop_if_error,
		    ' -f ', ConfigFile, ' ', Options])).

:- use_module(ciaodesrc(makedir('ConfigValues')), [lpmake/1, lpdoc2/1,
		setlocalciao/1, command_option/1]).

:- export(invoke_lpdoc/1).
invoke_lpdoc(Options) :-
	make_subdir(~lpmake, ~lpdoc2, ~build_doc_dir, ~setlocalciao, Options, ~command_option).

% ============================================================================

:- export(build_doc_dir/1).
build_doc_dir := ~atom_concat([~component_src(ciaode), '/build/doc']).

