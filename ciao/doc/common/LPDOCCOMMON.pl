:- module(_, _, [ciaopaths, fsyntax, assertions]).

:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(component_registry), [component_src/2, component_ins/2]).
:- use_module(ciaodesrc(makedir('ConfigValues'))).

:- reexport(ciaodesrc(makedir('DOCCOMMON'))).

% the component that contains this manual 
% TODO: This could be inferred (looking for a makedir/CONFIG.pl in a parent dir)
:- export(parent_component/1).
parent_component := 'ciao'.

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

% commonopts     := verbose.
% commonopts     := no_bugs.
commonopts :=
	modes|
	no_patches|
	no_isoline|
	no_engmods|
	propmods|
	no_changelog.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

pathsfile := ~atom_concat(~component_ins(ciao), '/doc/common/doc_ops.pl').

startpage := 1.
papertype := afourpaper.

perms := perm(rwX, rwX, rX).

owner := ~get_pwnam.
group := ~get_grnam.

docformat := texi|ps|pdf|manl|info|html.

