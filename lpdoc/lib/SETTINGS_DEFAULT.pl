:- module(_, _, [ciaopaths, assertions, regtypes, fsyntax]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- use_module(library(system)).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

:- doc(title, "Default configuration file for LPdoc").
:- doc(author, "The CLIP group").
:- doc(filetype, user).

:- doc(module, "This is a default configuration file for @apl{lpdoc},
   typically used in the generation of documentation for single
   modules. The defaults listed are typically suggestions and/or the
   ones used for local installation in the CLIP group machines.  These
   settings should be changed to suit your application.").

:- doc(bug, "Definitions that are overriden by the emacs mode must fit
   in one line. Do not use emacs but LPdoc to generate this file").

filepath := '/home/clip/Systems/lpdoc/doc'|'/home/clip/Systems/ciao/doc/common'.

systempath := '/home/clip/Systems/ciao/lib'|'/home/clip/Systems/ciao/library'|'/home/clip/Systems/ciao/contrib'.

pathsfile(_) :- fail.  % kludge to avoid compilation error

output_name := 'manual_name'.

doc_structure := 'main_module'.

commonopts := no_patches. % no_bugs|no_patches
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

docformat := texi|ps|pdf|manl|info|html.

index := concept.
index := pred.
index := prop.
index := regtype.
index := modedef.
index := global.

bibfile := '/home/clip/bibtex/clip/clip'.
bibfile := '/home/clip/bibtex/clip/others'.

startpage := 1.

papertype := afourpaper.

libtexinfo := 'yes'.

htmldir := '/home/clip/public_html/Local/lpdoc_docs'.
docdir := '/home/clip/public_html/Local/lpdoc_docs'.
infodir := '/home/clip/public_html/Local/lpdoc_docs'.
mandir := '/home/clip/public_html/Local/lpdoc_docs'.

datamode(perm(rw, rw, r)).
execmode(perm(rwx, rwx, rx)).

% TODO: This is defined automatically by lpdoc, but not accessible here. Fix
lpdoclib := '/usr/local/lib/lpdoc'.

% ----------------------------------------------------------------------------
% End of SETTINGS
