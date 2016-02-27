:- module(_, _, [ciaopaths, fsyntax, assertions]).

:- use_module(library(system)).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- use_module(library(lpdist(ciao_config_options))). % TODO: used?

:- reexport(library(lpdist(ciao_config_options)), 
    [bibfile/1, htmldir/1, docdir/1, infodir/1, mandir/1, lpdoclib/1]).

% the bundle that contains this manual 
% TODO: This could be inferred (looking for a Manifest.pl in a parent dir)
:- export(parent_bundle/1).
parent_bundle := 'ciao'.

% ----------------------------------------------------------------------------
% Paths and options

% filepath := ...
% TODO: This should be automatic
systempath := ~fsR(bundle_ins(ciao)/'lib').
systempath := ~fsR(bundle_ins(ciao)/'library').
systempath := ~fsR(bundle_ins(ciao)/'contrib').
systempath := ~fsR(bundle_ins(ciaopp)/'doc'/'readmes').

ciaofilepath_common := ~fsR(bundle_src(ciao)).
ciaofilepath_common := ~fsR(bundle_src(ciao)/'doc'/'common').
ciaofilepath_common := ~fsR(bundle_src(ciao)/'doc'/'readmes').

index := concept|lib|pred|prop|regtype|decl|author|global.
% index := prop.
% index := modedef.

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

% TODO: Used? why?
pathsfile := ~fsR(bundle_ins(ciao)/doc/common/'doc_ops.pl').

startpage := 1.
papertype := afourpaper.

perms := perm(rwX, rwX, rX).

owner := ~get_pwnam.
group := ~get_grnam.

docformat := texi|ps|pdf|manl|info|html.

