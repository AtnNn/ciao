% TODO: lpdoc all is not working for this file. The .lpdoc file is
%       ignored unless lpdoc singlelpdoc.texic is put as target.

:- module(_, _, [fsyntax, assertions]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(autoconfig)).

% :- use_module(ciaodesrc(makedir('MenuOptions'))).
% :- use_module(ciaodesrc(makedir('ConfigMenu'))).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).

filepath := ~atom_concat(~component_src(ciaode), ~filepathref).

htmldir := ''.

filepathref := '/lpdoc/tests/singlelpdoc'.

doc_structure := 'singlelpdoc'.

doc_mainopts := '-nopatches'.
doc_compopts := '-noisoline'|'-noengmods'|'-propmods'|'-nochangelog'.

% ----------------------------------------------------------------------------
% BUILD ROOT
% ----------------------------------------------------------------------------

:- pred doc_build_root(Dir) => atm(Dir)

# "@var{Dir} is the directory where documentation files will be
  placed. @var{Dir}/tmp will be used for temporary/cache files.".

doc_build_root := ~atom_concat(~component_src(ciaode), '/lpdoc/tests/singlelpdoc').
