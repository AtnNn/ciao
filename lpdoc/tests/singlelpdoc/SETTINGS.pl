:- module(_, _, [ciaopaths, regtypes, fsyntax, assertions]).

:- include(lpdocsrc(lib('SETTINGS_schema'))).
% ***************************************************************************
% This is a LPdoc configuration file. See SETTINGS_schema for documentation *
% ***************************************************************************

% TODO: lpdoc all is not working for this file. The .lpdoc file is
%       ignored unless lpdoc singlelpdoc.texic is put as target.

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(component_registry), [component_src/2]).

% :- use_module(ciaodesrc(makedir('MenuOptions'))).
% :- use_module(ciaodesrc(makedir('ConfigMenu'))).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
output_name(_) :- fail.
libtexinfo(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

filepath := ~atom_concat(~component_src(ciaode), ~filepathref).

htmldir := ''.

filepathref := '/lpdoc/tests/singlelpdoc'.

doc_structure := 'singlelpdoc'.

doc_mainopts := '-nopatches'.
doc_compopts := '-noisoline'|'-noengmods'|'-propmods'|'-nochangelog'.

%docformat := texi. % html or others are not working
docformat := texi|ps|pdf|manl|info|html.

