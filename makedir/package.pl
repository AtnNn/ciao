:- module(_, _, [fsyntax]).

% SETTINGS-like file for the whole system package.

:- use_module(library(autoconfig)).
:- use_module(library(aggregates)).
:- use_module(library(distutils)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).

% To have the documentation values

% The directory from where the tar will be created the package
% source dir (Ciao, CiaoDE, Lpdoc, xdvipresent...)
distpkg_root_dir := ~atom_concat(~component_src(ciaode), '/').

% This needs to be separated from SETTINGS.pl because it is not
% related with documentation generation.

% TODO: See distutils/distpkg_versions.pl
distpkg_name := ~packname(~wholesystem).
distpkg_name_version := ~version(~wholesystem).
distpkg_version := ~atom_concat('-', ~versionpure(~wholesystem)).
distpkg_version_nice :=
	~atom_concat([~componentversion(~wholesystem),
	              ' (r', ~svn_revision_atom, ')']).

:- include(ciaosrc(makedir(platdep_modules))).

exclude_file(_) := 'ciao/SETTINGS'.
exclude_file(_) := 'ciao/SETTINGS_AUTO'.
exclude_file(noa, File) :-
	platdep_module(_, _, ModulePl),
	atom_concat(Module, '.pl', ModulePl),
	exclude_ext(Ext),
	atom_concat(['ciao/', Module, Ext], File).

exclude_ext := '.po'|'.itf'|'.asr'|'.ast'.

exclude_files(PackageType) := ~findall(File, exclude_file(PackageType, File)).

distpkg_codeitem_kind := tgz|rpm_x86|deb_x86|win|dmg.
distpkg_docitem_kind := 
    docpart("Ciao Manual", ciao, [ps, pdf])|
    docpart("CiaoPP Manual", ciaopp, [ps, pdf])|
    docpart("LPdoc Manual", lpdoc, [ps, pdf]).
% TODO: HTML is not included in the distpkg, sure?

% TODO: STILL VALID OPTIONS? THOSE WERE FOR LPDOC!
filepath := ~atom_concat(~distpkg_root_dir, 'doc/').
doc_structure := 'ciaode_doc'.

% TODO: remove? used?
commonopts := '-nobiblio'.
commonopts := '-nobugs'.
commonopts := '-nochangelog'.
commonopts := '-modes'.
commonopts := '-nopatches'.
commonopts := '-noisoline'.
commonopts := '-noengmods'.
commonopts := '-nosysmods'.
commonopts := '-nobullet'.

% TODO: remove? used?
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

% TODO: remove? used?
index := concept|global.

% ---------------------------------------------------------------------------

%:- pred distpkg_repository/1 # "Specifies the repository with
%	precompiled packages to use.".

distpkg_repository := 'ciaotester@cliptest1.dia.fi.upm.es:/shared/ciaopacks'.
