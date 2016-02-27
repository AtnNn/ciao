:- module(_, [], [ciaopaths, assertions, basicmodes, nativeprops, fsyntax, hiord, regtypes]).

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

% TODO: use human-readable persistent predicates for this
% TODO: Really slow (cache)
:- export(bundle_version/2).
% Version number of a bundle (as a atom) (fails if missing)
bundle_version(Bundle) := Version :-
	bundle_info_file(Bundle, version, File),
	file_exists(File),
	atom_codes(Version, ~no_tr_nl(~file_to_string(File))).

:- export(bundle_patch/2).
% Patch number of a bundle (as a atom) (fails if missing)
bundle_patch(Bundle) := Patch :-
	bundle_info_file(Bundle, patch, File), 
	file_exists(File),
	atom_codes(Patch, ~no_tr_nl(~file_to_string(File))).

bundle_info_file(ciaode, Field, Value) :- !,
	bundle_info_file(ciao, Field, Value).
% TODO: Move those files as part of Manifest.pl
bundle_info_file(Bundle, version) := R :- !,
        R = ~fsR(bundle_src(Bundle)/version/'GlobalVersion').
bundle_info_file(Bundle, patch) := R :- !,
        R = ~fsR(bundle_src(Bundle)/version/'GlobalPatch').

% ---------------------------------------------------------------------------
% Different names of bundles (not all of them are unique per commit)

:- export(bundle_version_patch/2).
% Version and patch number
% TODO: Really slow (due to bundle_version and bundle_patch)
bundle_version_patch(Bundle) := ~atom_concat([~bundle_version(Bundle), '.', ~bundle_patch(Bundle)]).

:- export(bundle_packname/2).
% packname of the bundle
bundle_packname(Name, Pack) :-
	bundle_description(Name, Pack, _Type, _Path).

:- export(bundle_versioned_packname/2).
% packname with version information (using COMMIT_DESC, which show
% descriptive names with the necessary data to uniquely identify
% stable and devel versions).
bundle_versioned_packname(Bundle) := ~atom_concat([~bundle_packname(Bundle), '-', ~bundle_commit_info(Bundle, desc)]).

:- export(bundle_name/2).
% TODO: use (and fix, not always the identity if we want to
% distinguish between the loaded bundle and the built bundle)
bundle_name(Bundle) := Bundle.

:- export(bundle_manual_base/2).
% Base name for manuals
% TODO: move or share with LPdoc
% TODO: Really slow (due to bundle_version)
bundle_manual_base(Bundle) := R :-
	R = ~atom_concat([~bundle_name(Bundle), '-', ~bundle_version(Bundle), '.', ~bundle_patch(Bundle)]).

% ===========================================================================
% Extract and save commit information about the bundle source

% TODO: This is reimplemented in build_engine.sh; remove from that part

:- use_module(library(format)).

:- use_module(library(system_extra)).
:- use_module(library(lpdist(ciao_config_options))).

:- export(gen_bundle_commit_info/0).
:- pred gen_bundle_commit_info/0 # "Extract and save the commit information metadata".
gen_bundle_commit_info :-
	Bundle = ~bundle_wholesystem,
	save_bundle_commit_info(Bundle, branch),
	save_bundle_commit_info(Bundle, id),
	save_bundle_commit_info(Bundle, date),
	save_bundle_commit_info(Bundle, desc).

save_bundle_commit_info(Bundle, Field) :-
	bundle_commit_info(Bundle, Field, X),
	commit_info_file(Field, FieldFile),
	string_to_file(~atom_codes(X), FieldFile).

% COMMIT_ID: Git commit id or SVN revision number (0 if everything else fails)
% COMMIT_BRANCH: branch name (ignored in SVN at this moment)
% COMMIT_DATE: Git commit date (ignored for SVN)
% COMMIT_DESC: human-readable description of the commit (including
%   version, patch, branch, etc. if available)

% TODO: also changed in ciaobot code
:- export(commit_info_file/2).
commit_info_file(branch) := ~fsR(sys_dir(build)/'COMMIT_BRANCH').
commit_info_file(id) := ~fsR(sys_dir(build)/'COMMIT_ID').
commit_info_file(date) := ~fsR(sys_dir(build)/'COMMIT_DATE').
commit_info_file(desc) := ~fsR(sys_dir(build)/'COMMIT_DESC').

% Get bundle information (directly from the repository or from the
% saved commit info files).

:- export(bundle_commit_info/3).

:- data bundle_commit_info_db/3.

bundle_commit_info(Bundle, Field, Value) :-
	( current_fact(bundle_commit_info_db(Field, Bundle, Value0)) ->
	    true
	; bundle_commit_info_(Bundle, Field, Value0),
	  assertz_fact(bundle_commit_info_db(Field, Bundle, Value0))
	),
	Value = Value0.
	
bundle_commit_info_(Bundle, Field, Value) :-
	bundle_repo_kind(Bundle, RepoKind),
	bundle_commit_info__(Field, RepoKind, Bundle, Value).

bundle_commit_info__(Field, svn, Bundle, Value) :-
	svn_commit_info(Field, Bundle, Value0),
	!,
	Value = Value0.
bundle_commit_info__(Field, git, Bundle, Value) :-
	git_commit_info(Field, Bundle, Value0),
	!,
	Value = Value0.
bundle_commit_info__(Field, none, _Bundle, Date) :- % TODO: Use Bundle in commit_info_file
	commit_info_file(Field, File),
	file_exists(File),
	!,
	Date0 = ~no_tr_nl(~file_to_string(File)),
	atom_codes(Date, Date0).
bundle_commit_info__(_, _, _Bundle, 'Unknown').

% The kind of repository (git, svn, none)
% TODO: Missing: look in parent bundle
:- export(bundle_repo_kind/2).
bundle_repo_kind(_, RepoKind) :- name_value('git_repo_dir', _), !,
	RepoKind = git. % TODO: Hack for ciaobot
bundle_repo_kind(Bundle, RepoKind) :-
	Dir = ~fsR(bundle_src(Bundle)),
	( git_repo_at_dir(Dir) -> RepoKind = git
	; svn_repo_at_dir(Dir) -> RepoKind = svn
	; RepoKind = none
	).

% ---------------------------------------------------------------------------
% Extract commit information (SVN)

:- use_module(library(lpdist(svn_tools))).

svn_commit_info(branch, _Bundle, Branch) :-
	% TODO: (ignored for SVN)
	Branch = ''.
svn_commit_info(id, Bundle, Id) :-
	% TODO: This is incorrect
	% Note: svnversion is computed only over makedir/ directory (to make it faster)
	Path = ~fsR(bundle_src(Bundle)/'makedir'),
	( Id = ~svn_get_revision(Path) -> true
	; show_message(warning, "Cannot get revision number (svn_get_revision/2 failed)."),
	  fail
	),
	Id \== 'exported',
	!.
svn_commit_info(date, Bundle, Date) :-
	svn_commit_info(id, Bundle, Rev),
	SvnRepository = ~svn_repository_root(~fsR(bundle_src(Bundle))),
	\+ SvnRepository = '',
	Date0 = ~svn_revision_date(SvnRepository, Rev),
	!,
	atom_codes(Date, Date0).
svn_commit_info(desc, Bundle, Desc) :-
	( svn_commit_info(id, Bundle, Rev) ->
	    Desc = ~atom_concat([~bundle_version(Bundle), '.', ~bundle_patch(Bundle), '-', Rev])
	; Desc = ~atom_concat([~bundle_version(Bundle), '.', ~bundle_patch(Bundle)])
	).

% ---------------------------------------------------------------------------
% Extract commit information (Git)

:- use_module(library(lpdist(git_tools))).

git_commit_info(branch, Bundle, Branch) :-
	bundle_git_output(Bundle, 'rev-parse --abbrev-ref HEAD', Branch1),
	!,
	( Branch1 = 'HEAD' ->
	    Branch = '' % Detached HEAD in Git repository!
	; Branch = Branch1
	).
git_commit_info(id, Bundle, Id) :-
%	bundle_git_output(Bundle, 'show-ref --heads -s', Id0),
	bundle_git_output(Bundle, 'log -1 --format=\'%H\'', Id0),
	!,
	Id = Id0.
git_commit_info(short_id, Bundle, Id) :- % (Not stored)
	bundle_git_output(Bundle, 'log -1 --format=\'%h\'', Id0),
	!,
	Id = Id0.
git_commit_info(date, Bundle, Date) :-
	% Note: use ISO 8601 format for date (necessary for pbundle_meta_time/2)
	bundle_git_output(Bundle, 'log -1 --format=\'%ci\'', Date0),
	!,
	Date = Date0.
git_commit_info(desc, Bundle, Desc) :-
	% Describe the commit (see 'git describe')
	( bundle_git_output(Bundle, 'describe --tags', Desc0) ->
	    Desc1 = Desc0
	; Desc1 = ''
	),
	( Desc1 = '' ->
	    % Create our own human-readable commit description using the
	    % branch name (this should not be needed if we have proper
	    % tags in our commit graph).
	    git_commit_info(short_id, Bundle, ShortId),
	    git_commit_info(branch, Bundle, Branch),
	    Desc2 = ~atom_concat([Branch, '-g', ShortId])
	; Desc2 = Desc1
	),
        % Fix COMMIT_DESC (this assumes that we may include version
        % and patch in tag and branch names, and removes redundant
        % information)
	Version = ~bundle_version(Bundle),
	VersionPatch = ~atom_concat([~bundle_version(Bundle), '.', ~bundle_patch(Bundle)]),
	( % Version and patch in COMMIT_DESC, let us assume that is a
	  % particular code release (a commit with a tag, not a branch).
	  ( atom_concat(['v', VersionPatch, _], Desc2)
	  ; atom_concat([VersionPatch, _], Desc2)
	  ) ->
	    Desc = VersionPatch
	; % Version but not patch in COMMIT_DESC, just use it (it is a
	  % develoment release).
	  atom_concat(['v', Version, _], Desc2) ->
	    atom_concat('v', Desc, Desc2) % (remove 'v')
	; atom_concat([Version, _], Desc2) ->
	    Desc = Desc2
	; % No version info in commit desc, just append it (also a
	  % development release).
	  Desc = ~atom_concat([Version, '-', Desc2])
	).

:- use_module(library(make(make_rt)), [name_value/2]).

% Execute a git_output on the source directory of the specified bundle
bundle_git_output(Bundle, Command, R) :-
	( % TODO: Hack required by 'ciaobot' (please, configure in a nicer way)
	  name_value('git_repo_dir', Path) ->
	    true
	; Path = ~fsR(bundle_src(Bundle))
	),
	git_output(Path, Command, R).

% ---------------------------------------------------------------------------
% TODO: Merge with code in autodoc:get_last_version/3

:- export(show_bundles/0).
:- pred show_bundles # "Display the registered bundles".
show_bundles :-
	format("Name\tPack\tType\tVersion\tPath\n", []),
	format("-------\t-------\t-------\t-------\t-------\n", []),
	(
	    bundle_description(Name, Pack, Type, Path),
	    Version = ~bundle_version(Name),
	    Patch = ~bundle_patch(Name),
	    format("~w\t~w\t~w\t~w.~w\t~a\n", [Name, Pack, Type, Version, Patch, Path]),
	    fail
	;
	    true
	).

% ===========================================================================


