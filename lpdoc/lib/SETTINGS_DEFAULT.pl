:- module(_, _, [ciaopaths, assertions, regtypes, fsyntax]).

:- include(lpdocsrc(lib('SETTINGS_schema'))).
% ***************************************************************************
% This is a LPdoc configuration file. See SETTINGS_schema for documentation *
% ***************************************************************************

:- use_module(library(system)).
:- use_module(library(terms),                   [atom_concat/2]).

:- doc(title, "Configuration File for Document Generation").

:- doc(module, "This is a sample configuration file for
   @apl{lpdoc}. The defaults listed are typically suggestions and/or
   the ones used for local installation in the CLIP group machines.
   These settings should be changed to suit your application.").

:- doc(bug, "Definitions that are overriden by the emacs mode must fit
   in one line").

:- doc(author, "Your Name Here").

:- doc(filetype, user).

filepath := '/home/clip/Systems/lpdoc/doc'|'/home/clip/Systems/ciao/doc/common'.

systempath := '/home/clip/Systems/ciao/lib'|'/home/clip/Systems/ciao/library'|'/home/clip/Systems/ciao/contrib'.

pathsfile(_) :- fail.  % kludge to avoid compilation error

output_name := 'manual_name'.

doc_structure := 'main_module'.

commonopts := no_bugs|no_patches.
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

infodir_headfile := ~atom_concat([~lpdoclib, '/Head_clip.info']).
infodir_tailfile := ~atom_concat([~lpdoclib, '/Tail_clip.info']).
% TODO: This is defined automatically by lpdoc, but not accessible here. Fix
lpdoclib := '/usr/local/lib/lpdoc'.

% ----------------------------------------------------------------------------
% End of SETTINGS
