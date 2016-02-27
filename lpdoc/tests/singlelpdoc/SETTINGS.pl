:- module(_, _, [ciaopaths, regtypes, fsyntax, assertions]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

% TODO: lpdoc all is not working for this file. The .lpdoc file is
%       ignored unless lpdoc singlelpdoc.texic is put as target.

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

% :- use_module(library(lpdist(ciao_config_options))).
% :- use_module(library(lpdist(ciao_configure))).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
output_name(_) :- fail.
libtexinfo(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

filepath := ~fsR(bundle_src(ciaode)/lpdoc/tests/singlelpdoc).

htmldir := ''.

doc_structure := 'singlelpdoc'.

doc_mainopts := no_patches.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog.

%docformat := texi. % html or others are not working
docformat := texi|ps|pdf|manl|info|html.

