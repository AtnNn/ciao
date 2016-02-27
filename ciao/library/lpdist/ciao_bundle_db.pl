:- module(_, [], [ciaopaths, assertions, fsyntax, hiord]).

:- doc(title,  "Database of Bundles"). % of higher level than bundle_registry...
:- doc(author, "Edison Mera (original author)").
:- doc(author, "Jos@'{e} F. Morales").

:- doc(module, "This module defines the operations to consult and
   modify the database of bundles (locally) available for the current
   Ciao source.").
% TODO: Indeed, this complements the bundle_registry, but with more
%       advanced operations.  Probably we want just one thing, with a
%       super-fast minimal version just for reading (i.e., for alias
%       paths).

:- use_module(library(file_utils)).
:- use_module(library(system)).
:- use_module(library(bundle_registry(bundle_registry_load)), [bundle_description/4]).
:- use_module(library(system_extra), [no_tr_nl/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(messages)).

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- use_module(library(lpdist(ciao_config_db))).

% ---------------------------------------------------------------------------
% TODO: we want dependencies between bundles, avoid 'basic' and 'extra'.

:- export(registered_bundle/1).
% All the bundles
registered_bundle := ~basic_bundle.
registered_bundle := ~extra_bundle.

:- export(basic_bundle/1).
% The 'basic' bundle
basic_bundle(Name) :-
	bundle_description(Name, _Pack, basic, _Path).

:- export(extra_bundle/1).
% The 'extra' bundles (nondet)
extra_bundle(Name) :-
	bundle_description(Name, _Pack, extra, _Path).

:- export(bundle_wholesystem/1).
% The 'wholesystem' bundle
bundle_wholesystem(Name) :-
	bundle_description(Name, _, whole, _).

:- export(bundle_version/2).
% TODO: Really slow (cache)
% atom representing the version number of a bundle
bundle_version(Bundle) := Version :-
	versionfileabs(Bundle, File),
	file_exists(File),
	atom_codes(Version, ~no_tr_nl(~file_to_string(File))).

versionfileabs(ciaode, Value) :- !,
	versionfileabs(ciao, Value).
% TODO: simplify
versionfileabs(Bundle) := 
        ~fsR(bundle_src(ciaode)/Bundle/version/'GlobalVersion').

:- export(bundle_patch/2).
% atom representing the patch number of a bundle
bundle_patch(Bundle) := Patch :-
	patchfileabs(Bundle, File), 
	file_exists(File),
	atom_codes(Patch, ~no_tr_nl(~file_to_string(File))).

patchfileabs(ciaode, Value) :- !,
	patchfileabs(ciao, Value).
% TODO: simplify
patchfileabs(Bundle, Value) :-
	Value = ~fsR(bundle_src(ciaode)/Bundle/version/'GlobalPatch').

:- export(bundle_version_patch/2).
% atom representing the version and patch number
% Example: bundle_version_patch(ciaode,'1.13.0').
% TODO: Really slow (due to bundle_version)
bundle_version_patch(Bundle) :=
    ~atom_concat([~bundle_version(Bundle), '.', ~bundle_patch(Bundle)]).

:- export(bundle_version_patch_rev/2).
% Example: bundle_version_patch_rev(ciaode, '1.13.0-7526').
% TODO: Really slow (due to bundle_version)
bundle_version_patch_rev(Bundle) :=
    ~atom_concat([~bundle_version_patch(Bundle), '-', ~bundle_svn_revatm]).

:- export(bundle_packname/2).
% Example: bundle_packname(ciaode, 'CiaoDE').
bundle_packname(Name, Pack) :-
	bundle_description(Name, Pack, _Type, _Path).

:- export(bundle_packname_version_patch_rev/2).
% Example: bundle_packname_version_patch_rev(ciaode, 'CiaoDE-1.13.0-7526').
% TODO: Really slow (due to bundle_version)
bundle_packname_version_patch_rev(Bundle) :=
	~atom_concat([~bundle_packname(Bundle), '-', ~bundle_version_patch_rev(Bundle)]).

:- export(bundle_name_version/2).
% TODO: Really slow (due to bundle_version)
bundle_name_version(Bundle) := ~atom_concat([Bundle, '-', ~bundle_version(Bundle)]).

:- export(bundle_name_version_patch/2).
% Example: bundle_name_version_patch(ciaode,'ciaode-1.13.0').
% TODO: Really slow (due to bundle_version)
bundle_name_version_patch(Bundle) :=
	~atom_concat([~bundle_name_version(Bundle), '.', ~bundle_patch(Bundle)]).

% ---------------------------------------------------------------------------

:- use_module(library(lpdist(svn_tools)),
	[svn_repository_root/2, svn_revision_string/2]).

:- export(bundle_svn_repository/1).
:- pred bundle_svn_repository/1 # "Specifies the subversion repository
	where the sources can be obtained, or the working copy.".

% bundle_svn_repository := ~fsR(bundle_src(ciaode)).
% TODO: This could be cached (if necessary)
% TODO: The CONFIG.pl file for the bundle could specify a fixed repository,
%       or the kind of repository (I'm not sure about this)
bundle_svn_repository(String) :-
	String = ~svn_repository_root(~fsR(bundle_src(ciaode))),
	String \= "",
	!.
bundle_svn_repository :=
	"svn+ssh://clip.dia.fi.upm.es/home/clip/SvnReps/Systems/CiaoDE/trunk".

:- export(bundle_svn_revatm/1).
bundle_svn_revatm(A) :-
	bundle_svn_revstr(S),
	atom_codes(A, S).

:- export(revision_file/1).
revision_file := ~fsR(sys_dir(build)/'REVISION').

:- export(bundle_svn_revstr/1).
bundle_svn_revstr(Revision) :-
	% Note: svnversion is done only over makedir to make it faster
	Path = ~fsR(bundle_src(ciaode)/'makedir'),
	( Revision = ~svn_revision_string(Path) -> true
	; show_message(warning, "Cannot get revision number (svn_revision_string/1 failed).")
	),
	nonvar(Revision),
	Revision \== "exported",
	!.
bundle_svn_revstr(Revision) :-
	revision_file(RevisionFile),
	file_exists(RevisionFile),
	!,
	no_tr_nl(~file_to_string(RevisionFile), Revision).
bundle_svn_revstr("Unknown").

% ---------------------------------------------------------------------------

:- use_module(library(format)).

:- use_module(library(system_extra)).
:- use_module(library(lpdist(ciao_config_options))).

:- export(gen_bundle_revision/0).
:- pred gen_bundle_revision/0 # "(Re)generate @tt{REVISION} file if current
   repository version has changed".
gen_bundle_revision :-
	bundle_svn_revstr(Revision),
	revision_file(RevisionFile),
	( file_exists(RevisionFile),
	    file_to_string(RevisionFile, RevisionOld) ->
	    ( Revision = RevisionOld ->
		format("NOTE: Revision file is up to date (~s)\n",
		    [Revision])
	    ; format("NOTE: Updating revision file (~s->~s)\n",
		    [RevisionOld, Revision]),
	      write_revision(Revision, RevisionFile)
	    )
	; format("NOTE: Creating revision file (~s)\n", [Revision]),
	  write_revision(Revision, RevisionFile)
	).

write_revision(Revision, VersionFile) :-
	string_to_file(Revision, VersionFile).
