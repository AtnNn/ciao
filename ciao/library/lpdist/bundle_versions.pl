:- module(bundle_versions, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

:- use_module(library(file_utils)).
:- use_module(library(system_extra)).
:- use_module(library(terms), [atom_concat/2]).

:- doc(title, "Obtain Version Information of Bundles").

% % ---------------------------------------------------------------------------
% :- export(bundle_obtain_version/2).
% :- pred bundle_obtain_version_rev(BundleDir, Version) => atm(Version)
%    # "@var{Version} is version of bundle at directory @var{BundleDir},
%      with format @em{version}.@em{patch}.@em{revision}".
% 
% bundle_obtain_version_rev(BundleDir, Version) :-
% 	get_version_string(BundleDir, V),
% 	get_patch_string(BundleDir, P),
% 	get_revision_string(BundleDir, R),
% 	atom_codes(VA, V),
% 	atom_codes(PA, P),
% 	atom_codes(RA, R),
% 	atom_concat([VA, '.', PA, '#', RA], Version).

% ---------------------------------------------------------------------------
% TODO: Merge with autodoc:get_last_version/3

:- export(bundle_obtain_version/2).
:- pred bundle_obtain_version(BundleDir, NiceVersion) => atm(NiceVersion)
   # "@var{Version} is version of bundle at directory @var{BundleDir},
     with format @em{version}.@em{patch}".

bundle_obtain_version(BundleDir, NiceVersion) :-
	get_version_string(BundleDir, V),
	get_patch_string(BundleDir, P),
	atom_codes(VA, V),
	atom_codes(PA, P),
	atom_concat([VA, '.', PA], NiceVersion).

:- pred get_patch_string(BundleDir, Patch)
	: (atm(BundleDir), var(Patch))
	=> string(Patch)

# "Returns in @var{Patch} the patch string for the bundle at @var{BundleDir}".

get_patch_string(BundleDir, Patch) :-
	atom_concat(BundleDir, 'version/GlobalPatch', GP),
	no_tr_nl(~file_to_string(GP), Patch).

:- pred get_version_string(BundleDir, Version)
	: (atm(BundleDir), var(Version))
	=> string(Version)

# "Returns in @var{Version} the version string for the bundle at @var{BundleDir}".

get_version_string(BundleDir, Version) :-
	atom_concat(BundleDir, 'version/GlobalVersion', GV),
	no_tr_nl(~file_to_string(GV), Version).

% % ---------------------------------------------------------------------------
% :- pred get_revision_string(BundleDir, Revision)
% 	: (atm(BundleDir), var(Revision))
% 	=> string(Revision)
% 
% # "Returns the revision number from SVN of @var{BundleDir} in
%    @var{Revision}. If it is no possible to get it, because it is a
%    non-working copy, then empty string is returned. ".
% 
% % TODO: Cache results? It may call SVN in each call!
% % TODO: somewhat duplicated in makedir/ciao_bundle_db.pl
% % Case 1: There exist a REVISION file under BundleDir
% get_revision_string(BundleDir, Revision) :-
% 	atom_concat(BundleDir, 'REVISION', RevisionFile),
% 	file_exists(RevisionFile),
% 	!,
% 	no_tr_nl(~file_to_string(RevisionFile), Revision).
% % Case 2: there is a .svn, so it is a working copy
% get_revision_string(BundleDir, Revision) :-
% 	Cmd = ~atom_concat(['which svnversion > /dev/null 2>&1 && svnversion ', BundleDir, 'makedir']),
% 	Revision = ~no_tr_nl(~stream_to_string(~popen(Cmd, read))),
% 	Revision \== "",
% 	!.
% % Case 3: Unkown
% get_revision_string(_, "").

