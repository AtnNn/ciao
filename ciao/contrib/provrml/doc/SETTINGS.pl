:- module(_, _, [fsyntax]).
% TODO: THIS SETTINGS.pl FILE IS OUTDATED!

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
:- reexport(ciaodesrc(makedir('DOCCOMMON')), [lpdoclib/1]).

:- redefining(_).

% -*- mode: Makefile; -*-
% ----------------------------------------------------------------------------
% include ../../SETTINGS
%% ignored line: %% include ../../../doc/SETTINGS.COMMON
% ----------------------------------------------------------------------------
% For using the development version of ciaodoc when testing...
% LIBDIR     = /home/clip/Systems/ciaodoc/lib
% CIAODOC    = /home/clip/Systems/ciaodoc/src/ciaodoc
% ----------------------------------------------------------------------------

doc_structure :=
    'vrml'-[
      'ProVRML_intro',
      'boundary',
      'check',
      'dictionary',
      'dictionary_tree',
      'provrmlerror',
      'field_type',
      'field_value',
      'field_value_check',
      'generator',
      'generator_util',
      'internal_types',
      'provrml_io',
      'lookup',
      'provrml_parser',
      'parser_util',
      'possible',
      'tokeniser'
    ].
