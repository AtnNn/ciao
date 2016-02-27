:- module(component_versions, [], [assertions, basicmodes,
		nativeprops, fsyntax, hiord, regtypes]).

:- use_module(library(file_utils)).
:- use_module(library(make(system_extra))).
:- use_module(library(terms), [atom_concat/2]).

:- doc(title, "Obtain and Format Component Versions").

% % ---------------------------------------------------------------------------
% :- export(component_obtain_version/2).
% :- pred component_obtain_version_rev(PkgDir, Version) => atm(Version)
%    # "@var{Version} is version of component at directory @var{PkgDir},
%      with format @em{version}.@em{patch}.@em{revision}".
% 
% component_obtain_version_rev(PkgDir, Version) :-
% 	get_version_string(PkgDir, V),
% 	get_patch_string(PkgDir, P),
% 	get_revision_string(PkgDir, R),
% 	atom_codes(VA, V),
% 	atom_codes(PA, P),
% 	atom_codes(RA, R),
% 	atom_concat([VA, '.', PA, '#', RA], Version).

% ---------------------------------------------------------------------------
% TODO: Merge with autodoc:get_last_version/3

:- export(component_obtain_version/2).
:- pred component_obtain_version(PkgDir, NiceVersion) => atm(NiceVersion)
   # "@var{Version} is version of component at directory @var{PkgDir},
     with format @em{version}.@em{patch}".

component_obtain_version(PkgDir, NiceVersion) :-
	get_version_string(PkgDir, V),
	get_patch_string(PkgDir, P),
	atom_codes(VA, V),
	atom_codes(PA, P),
	atom_concat([VA, '.', PA], NiceVersion).

:- pred get_patch_string(PkgDir, Patch)
	: (atm(PkgDir), var(Patch))
	=> string(Patch)

# "Returns in @var{Patch} the patch of the package
   @var{PkgDir}".

get_patch_string(PkgDir, Patch) :-
	atom_concat(PkgDir, 'version/GlobalPatch', GP),
	no_tr_nl(~file_to_string(GP), Patch).

:- pred get_version_string(PkgDir, Version)
	: (atm(PkgDir), var(Version))
	=> string(Version)

# "Returns in @var{Version} the version of the package
   @var{PkgDir}".

get_version_string(PkgDir, Version) :-
	atom_concat(PkgDir, 'version/GlobalVersion', GV),
	no_tr_nl(~file_to_string(GV), Version).

% % ---------------------------------------------------------------------------
% :- pred get_revision_string(PkgDir, Revision)
% 	: (atm(PkgDir), var(Revision))
% 	=> string(Revision)
% 
% # "Returns the revision number from SVN of @var{PkgDir} in
%    @var{Revision}. If it is no possible to get it, because it is a
%    non-working copy, then empty string is returned. ".
% 
% % TODO: Cache results? It may call SVN in each call!
% % TODO: somewhat duplicated in makedir/makedir_component.pl
% % Case 1: There exist a REVISION file under PkgDir
% get_revision_string(PkgDir, Revision) :-
% 	atom_concat(PkgDir, 'REVISION', RevisionFile),
% 	file_exists(RevisionFile),
% 	!,
% 	no_tr_nl(~file_to_string(RevisionFile), Revision).
% % Case 2: there is a .svn, so it is a working copy
% get_revision_string(PkgDir, Revision) :-
% 	Cmd = ~atom_concat(['which svnversion > /dev/null 2>&1 && svnversion ', PkgDir, 'makedir']),
% 	Revision = ~no_tr_nl(~stream_to_string(~popen(Cmd, read))),
% 	Revision \== "",
% 	!.
% % Case 3: Unkown
% get_revision_string(_, "").

