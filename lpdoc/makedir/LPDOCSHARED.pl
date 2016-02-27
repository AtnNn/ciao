%% ---------------------------------------------------------------------------
:- module(_, _, [assertions, fsyntax]).

:- doc(module, "Settings which are common to other configuration files
	but which you should not need to change").
%% ---------------------------------------------------------------------------
%:- use_module(library(make(system_extra))).
:- use_module(library(filenames), [no_path_file_name/2]).
%:- use_module(library(terms),[atom_concat/2]).
%:- use_module(library(global)).
%:- use_module(library(persvalue)).
%:- use_module(library(distutils)).
:- use_module(ciaodesrc(makedir('CIAODESHARED'))).

%% ---------------------------------------------------------------------------
%% Define this to be the main file name (with suffix if there is one):
mainname := 'lpdoc'.
%% ----------------------------------------------------------------------------
%% Other common, derived variables used in Makefiles below:

basemain := ~no_path_file_name(~mainname).
vers := ~vers(~basemain).
patch := ~patch(~basemain).

versionmain := ~versionmain(~basemain).
vpmain := ~vpmain(~basemain).
win32vpmain := ~win32vpmain(~basemain).
rawtarfile := ~vpmain.
%% ----------------------------------------------------------------------------
%% Ciao engine version (will be reported by lpdoc on startup)
:- reexport(ciaodesrc(makedir('CIAODESHARED')), [engine/1]).
