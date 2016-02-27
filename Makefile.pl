% ===========================================================================
:- module(_,_,[make,functions,assertions]).
% ===========================================================================
:- comment(title,  "Ciao Future Global Compilation/Installation LPMakefile").
:- comment(author, "Edison Mera").
% ===========================================================================

% :- use_package([isomodes]).
:- use_module(library('make/system_extra')).
:- use_module(library(lists),[list_concat/2,append/3]).
:- use_module(library(terms),[atom_concat/2]).
:- use_module(library(format), [format/2, format/3]).
:- use_module(extra_utils).
:- use_module(settings).
:- use_module(shared).
:- include(common_make).

:- comment(bug, "Now is only a collection of new options, at least
                until we migrate the Makefile to lpmake").

% :- use_module(config).
:- multifile get_ciaosrc/1.
%:- dynamic   ciaosrc/1.
get_ciaosrc := Y :- working_directory(Y,Y).

tags       := ~atom_concat(~ciaosrc, '/TAGS').

% ===========================================================================
% INSTALLATION                                                              
% ===========================================================================

my_target_comment(~tags, 'Creation of TAGS for use with the find-tag command (ESC-.) in emacs').

tags <- [~tags] :- true.

~tags <- ['Makefile.pl'] :-
	tags(Tags),
	del_file_nofail(Tags),
	etagss(~lss(~ciaosrc, '*.pl'), Tags).

my_target_comment(deltags, 'Delete the TAGS file.').

deltags <- [] :-
	del_file_nofail(~tags).

% ----------------------------------------------------------------------------
% Lpdoc changelog:
% ----------------------------------------------------------------------------

:- comment(version_maintenance,dir('version')).
