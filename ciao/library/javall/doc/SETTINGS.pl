:- module(_, _, [fsyntax]).
:- use_module(library(terms)).
:- use_module(ciaodesrc(makedir('MenuOptions'))).
:- use_module(ciaodesrc(makedir('ConfigMenu'))).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).

:- reexport(ciaodesrc(makedir('MenuOptions')), [lpdoclib/1]).

:- redefining(_).

% -*- mode: Makefile; -*-
% ----------------------------------------------------------------------------
docformat := 'ps'.
index := 'concept' | 'pred' | 'prop' | 'regtype' | 'usage'.
% ----------------------------------------------------------------------------
% Uncomment for using the development version of lpdoc when testing...
% LIBDIR     = /home/clip/Systems/lpdoc/lib
% LPDOC      = /home/clip/Systems/lpdoc/src/lpdoc
% ----------------------------------------------------------------------------

filepath := ~atom_concat( [ ~component_src(ciao) , '/library/javall' ] ).
systempath := ~atom_concat( [ ~component_src(ciao) , '/lib' ] )|
    ~atom_concat( [ ~component_src(ciao) , '/library' ] ).

% ******** Warning: if you change this you also need to change 
% ******** ciao.el which gives an outline of the manual
doc_structure := 
    'javall_doc'-[
      'javart',
      'jtopl',
      'javasock'
    ].
% ----------------------------------------------------------------------------
% Select lpdoc options for main file
% Leaving this blank produces most verbose manuals
% -v -nobugs -noauthors -noversion -nochangelog -nopatches -modes 
% -headprops -literalprops -nopropnames -noundefined -nopropsepln -norefs 
% -nobullet -nosysmods -noengmods -noisoline -propmods -onesided

doc_mainopts := '-nochangelog'.

% Select lpdoc opts for component file(s)
% Leaving this blank produces most verbose manuals

doc_compopts := '-nochangelog' | '-modes'.
