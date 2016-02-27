:- module(_,_,[make,functions]).
:- use_module(library(filenames),[no_path_file_name/2,basename/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library('make/system_extra')).
:- use_module(library(global)).
:- use_module(settings).

:- multifile get_ciaosrc/1.

% Be carefull with get_vers and get_patch: when uninstall, the patch
% version may differ with the version that we try to uninstall.

%:- multifile get_patch/1.
%:- multifile get_vers/1.

:- initialization(init_global).

init_global :-
	set_global(ciaosrc,   ~get_ciaosrc),
	set_global(vers,      ~get_vers),
	set_global(patch,     ~get_patch).

ciaosrc := ~get_global(ciaosrc).
vers    := ~get_global(vers).
patch   := ~get_global(patch).

%:- dynamic ciaosrc/1.
%:- use_module(config).
% -*- mode: Makefile; -*-
% ----------------------------------------------------------------------------
% Settings which are common to makefiles below
% but which you should not need to change
%-----------------------------------------------------------------------------
% *** This is here so that it can be seen by dist and doc
tardocformats := [ps, manl, info, infoindex, html, htmlindex].
% Define this to be the main file name (with suffix if there is one???).
main := 'ciao'.
% Headers for the executable at startup
header := 'Ciao Prolog'.
copyrt := 'UPM-CLIP'.
% ifeq ($(SRC),)
% SRC=$(PWD)
% endif
actualdir := ~no_path_file_name(~ciaosrc).
codes_atom(A, B) :- atom_codes(B, A).
%basemain := ~basename(~no_path_file_name(~main)).
basemain := ~no_path_file_name(~main).
%$(basename $(notdir ~main))
versionfile := 'version/GlobalVersion'.
versionfileabs := ~atom_concat([~ciaosrc, '/', ~versionfile]).
get_vers := V :- no_tr_nl(~readf(~versionfileabs),VS), atom_codes(V,VS).
%~codes_atom(~readf(~versionfileabs)).
%$(shell cat $(SRC)/$(VERSIONFILE))
patchfile := 'version/GlobalPatch'.
patchfileabs := ~atom_concat([~ciaosrc, '/', ~patchfile]).
get_patch := V :- no_tr_nl(~readf(~patchfileabs),VS), atom_codes(V,VS).
%~codes_atom(~readf(~patchfileabs)).
%$(shell cat $(SRC)/$(PATCHFILE))
versionmain := ~atom_concat([~basemain, '-', ~vers]).
vpmain := ~atom_concat([~versionmain, 'p', ~patch]).
% The name of the (precompiled) distribution tarfile
rawtarfile := ~atom_concat([~basemain, '-', ~vers, 'p', ~patch, '-', ~prolog]).
home := ~codes_atom(~getenvstr('HOME')).
%-----------------------------------------------------------------------------

%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

