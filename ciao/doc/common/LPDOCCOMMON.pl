:- module(_, _, [fsyntax, assertions]).

:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(autoconfig)).
:- use_module(ciaodesrc(makedir('ConfigValues'))).

:- reexport(ciaodesrc(makedir('DOCCOMMON'))).

% ---------------------------------------------------------------------------
% Distpkg (distribution package) definitions

distpkg_name := 'ciao'.
% TODO: this avoids duplicated code; language support would make it nicer
:- include(library(distutils(distpkg_versions__template))).
:- use_module(library(distutils(distpkg_versions))).
:- export(distpkg_version/1).
:- export(distpkg_version_nice/1).
:- export(distpkg_name_version/1).

% ----------------------------------------------------------------------------
% Directories

:- pred distpkg_root_dir(PackageRootDir) => atm(PackageRootDir)

# "@var{PackageRootDir} defines the root directory from where the
  package actions (generations of tars, documentation, version ...)
  will take place (do not forget the / at the end of the path).

  Example: distpkg_root_dir := '/home/clip/Systems/CiaoDE/'.".

% TODO: review
distpkg_root_dir := ~atom_concat(~component_src(ciao), '/').

% ----------------------------------------------------------------------------
% Paths and options for components

% filepath   := ~atom_concat(~component_ins(ciao), ~ciaofilepath   ).
systempath := ~atom_concat(~component_ins(ciao),   ~ciaosystempath).
systempath := ~atom_concat(~component_ins(ciaopp), '/doc/readmes').

ciaofilepath_common :=
	''|
	'/doc/common'|
	'/doc/readmes'.

ciaosystempath := '/lib'|'/library'|'/contrib'.

index := concept|lib|pred|prop|regtype|decl|author|global.
% index := prop.
% index := modedef.

infodir_headfile := ~atom_concat([~component_ins(ciao),
		'/doc/common/CiaoHead.info']).
infodir_tailfile := ~atom_concat([~component_ins(ciao),
		'/doc/common/CiaoTail.info']).

% commonopts     := '-v'.
% commonopts     := '-nobugs'.
commonopts :=
	'-modes'|
	'-nopatches'|
	'-noisoline'|
	'-noengmods'|
	'-propmods'|
	'-nochangelog'.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

pathsfile := ~atom_concat(~component_ins(ciao), '/doc/common/doc_ops.pl').

startpage := 1.
papertype := afourpaper.

% compresscommand := 'gzip -f'.
% compressext := 'gz'.

perms := perm(rwX, rwX, rX).

owner := ~get_pwnam.
group := ~get_grnam.


