:- module(_, [], [ciaopaths, assertions, fsyntax, hiord]).

:- doc(title,  "Components of the System (and related operations)").
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jos@'{e} F. Morales").

:- doc(module, "This file defines an uniform interface to the
   components of the Ciao system, as well as common operations used in
   the build and installation process.").

:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(component_registry), [component_src/2, component_description/4]).
:- use_module(library(make(system_extra)), [no_tr_nl/2]).
:- use_module(library(terms),              [atom_concat/2]).

:- reexport(ciaodesrc(makedir('DOCCOMMON'))).

% ---------------------------------------------------------------------------

:- export(component/1).
% All the components
component := ~basic_component.
component := ~extra_component.

:- export(basic_component/1).
% The basic component
basic_component(Name) :-
	component_description(Name, _Pack, basic, _Path).

:- export(extra_component/1).
% The extra components (nondet)
extra_component(Name) :-
	component_description(Name, _Pack, extra, _Path).

:- export(component_wholesystem/1).
% The 'wholesystem' component
component_wholesystem(Name) :-
	component_description(Name, _, whole, _).

:- export(component_version/2).
% atom representing the version number of a component
component_version(Component) := Version :-
	atom_codes(Version, ~no_tr_nl(~file_to_string(~versionfileabs(Component)))).

versionfileabs(ciaode, Value) :- !,
	versionfileabs(ciao, Value).
versionfileabs(Component) := ~atom_concat([~component_src(ciaode), '/',
		Component, '/version/GlobalVersion']).

:- export(component_patch/2).
% atom representing the patch number of a component
component_patch(Component) := Patch :-
	atom_codes(Patch, ~no_tr_nl(~file_to_string(~patchfileabs(Component)))).

patchfileabs(ciaode, Value) :- !,
	patchfileabs(ciao, Value).
patchfileabs(Component, Value) :-
	Value = ~atom_concat([~component_src(ciaode), '/', Component,
		'/version/GlobalPatch']).

:- export(component_version_patch/2).
% atom representing the version and patch number
% Example: component_version_patch(ciaode,'1.13.0').
component_version_patch(Component) :=
    ~atom_concat([~component_version(Component), '.', ~component_patch(Component)]).

:- export(component_version_patch_rev/2).
% Example: component_version_patch_rev(ciaode, '1.13.0-7526').
component_version_patch_rev(Component) :=
    ~atom_concat([~component_version_patch(Component), '-', ~svn_revision_atom]).

:- export(component_packname/2).
% Example: component_packname(ciaode, 'CiaoDE').
component_packname(Name, Pack) :-
	component_description(Name, Pack, _Type, _Path).

:- export(component_packname_version_patch_rev/2).
% Example: component_packname_version_patch_rev(ciaode, 'CiaoDE-1.13.0-7526').
component_packname_version_patch_rev(Component) :=
	~atom_concat([~component_packname(Component), '-', ~component_version_patch_rev(Component)]).

:- export(component_name_version/2).
component_name_version(Component) := ~atom_concat([Component, '-', ~component_version(Component)]).

:- export(component_name_version_patch/2).
% Example: component_name_version_patch(ciaode,'ciaode-1.13.0').
component_name_version_patch(Component) :=
	~atom_concat([~component_name_version(Component), '.', ~component_patch(Component)]).

% ---------------------------------------------------------------------------

:- doc(subsection, "Installing the Documentation of a Component").

:- use_module(library(distutils(lpdoc_installer))).
:- use_module(library(distutils(dirutils))).

:- export(component_install_docs/1).
component_install_docs(Component) :-
	SourceDir = ~build_doc_dir,
	component_name_version_patch(Component, VPComponent),
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
		component_install_docs_hook(DocFormat, Target)
	    ;
		warning(['File ', Source, ' not generated yet. Skipping copy'])
	    ),
	    fail
	;
	    true
	).

:- export(component_uninstall_docs/1).
component_uninstall_docs(Component) :-
	component_name_version_patch(Component, VPComponent),
	(
	    docformat(DocFormat),
	    docformatdir(DocFormat, TargetDir),
	    FileName = ~atom_concat([VPComponent, '.', DocFormat]),
	    Target = ~atom_concat(TargetDir, FileName),
	    component_uninstall_docs_hook(DocFormat, Target),
	    delete_dir_rec(Target),
	    fail
	;
	    true
	).

% This allow us to test the info files
component_install_docs_hook(info, Target) :- !,
	install_info_dir(Target, ~build_doc_dir).
component_install_docs_hook(_, _).

component_uninstall_docs_hook(info, Target) :- !,
	uninstall_info_dir(Target, ~build_doc_dir).
component_uninstall_docs_hook(_, _).

:- use_module(ciaodesrc(makedir('ConfigValues')), [build_doc_dir/1]).

