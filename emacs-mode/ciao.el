;; -*- mode: emacs-lisp; -*-
;;
;; DO NOT CHANGE THIS FILE
;; IT HAS BEEN GENERATED AUTOMATICALLY
;;
;;---------------------------------------------------------------------------
;; Emacs support for the Ciao programming environment
;; (can be used as a general Prolog mode under Emacs)
;;---------------------------------------------------------------------------
;; Copyright (C) 1986-2002 Free Software Foundation, Inc. and M. Hermenegildo
;; and others (herme@fi.upm.es, UPM-CLIP, Spain) See 'Other comments,
;; acknowledgements and changelog/version info' below for history,
;; other authors, and log of changes. 
;; 
;; 
;; This file is part of GNU Emacs.  GNU Emacs is distributed in the
;; hope that it will be useful, but WITHOUT ANY WARRANTY.  No author
;; or distributor accepts responsibility to anyone for the
;; consequences of using it or for whether it serves any particular
;; purpose or works at all, unless he says so in writing.  Refer to
;; the GNU Emacs General Public License for full details.  Everyone is
;; granted permission to copy, modify and redistribute GNU Emacs, but
;; only under the conditions described in the GNU Emacs General Public
;; License.  A copy of this license is supposed to have been given to
;; you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.
;; 
;;---------------------------------------------------------------------------
;; To use this mode the following lines must be present in your '.emacs'
;; (the Ciao installation procedure typically builds a file 'DOTemacs.el'
;; with the correct paths which you can include directly):
;;---------------------------------------------------------------------------
;;
;;;; Ciao/Prolog mode initialization
;;;; -------------------------------
;;;; (can normally be used with other Prolog modes and the default prolog.el)
;;;; 
;;(setq load-path (cons "/home/clip/lib/ciao" load-path))
;;(autoload 'run-ciao-toplevel "ciao"
;;          "Start a Ciao/Prolog top-level sub-process." t)
;;(autoload 'ciao-startup "ciao"
;;          "The Ciao/Prolog program development system startup screens." t)
;;(autoload 'ciao "ciao"
;;          "Start a Ciao/Prolog top-level sub-process." t)
;;(autoload 'prolog "ciao"
;;          "Start a Ciao/Prolog top-level sub-process." t)
;;(autoload 'run-ciao-preprocessor "ciao"
;;          "Start a Ciao/Prolog preprocessor sub-process." t)
;;(autoload 'ciaopp "ciao"
;;          "Start a Ciao/Prolog preprocessor sub-process." t)
;;(autoload 'ciao-mode "ciao"
;;          "Major mode for editing and running Ciao/Prolog" t)
;;(autoload 'ciao-inferior-mode "ciao"
;;          "Major mode for running Ciao/Prolog, CiaoPP, LPdoc, etc." t)
;;(setq auto-mode-alist (cons '("\\.pl$" . ciao-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '("\\.pls$" . ciao-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons '("\\.lpdoc$" . ciao-mode) auto-mode-alist))
;;(setq completion-ignored-extensions
;;      (append '(".dep" ".itf" ".po" ".asr" ".cpx")
;;              completion-ignored-extensions))
;;;; ------------------------------------------------------------------------
;;;; In Un*x, the following (or similar) lines should be included in your
;;;; .cshrc or .profile to find the manuals (the Ciao installation leaves
;;;; in the Ciao library directory 'DOTcshrc' and 'DOTprofile' files with
;;;; the right paths which can be included directly in your startup scripts):
;;;; 
;;;; setenv INFOPATH /usr/local/info:/usr/info:/home/clip/public_html/Local/lpdoc_docs/Ciao
;;;; ------------------------------------------------------------------------
;;% -----------------------------------------------------------
;;% Other comments, acknowledgments and changelog/version info 
;;% -----------------------------------------------------------
;;
;;:- use_package([assertions]).
;;
;;:- comment(title,"Using Ciao inside GNU emacs").
;;
;;:- comment(subtitle,"@em{An interactive program development environment for Ciao}").
;;:- comment(subtitle,"@bf{The Ciao System Documentation Series}").
;;:- comment(subtitle,"Technical Report CLIP 4/00.5.81").
;;:- comment(subtitle,"@em{Draft printed on:} @today{}").
;;
;;:- comment(author,"Manuel Hermenegildo").
;;:- comment(author,"Manuel C. Rodriguez").
;;:- comment(author,"Daniel Cabeza").
;;
;;:- include(library('ClipAddress')).
;;
;;:- include(library('Copyright')).
;;
;;:- comment(summary,"This documents the Ciao emacs interface (or @em{mode}
;;    in @apl{emacs} terms), which provides a rich, integrated user interface
;;    to the Ciao program development environment components, including the
;;    @apl{ciaosh} interactive top level and the @apl{ciaopp}
;;    preprocessor. While most features of the Ciao development environment
;;    are available from the command line of the preprocessor and the
;;    top-level shell, using Ciao inside @apl{emacs} is highly recommended,
;;    since it greatly facilitates the process of editing, compiling,
;;    running, and debugging Ciao programs. 
;;
;;    In particular, source-level
;;    debugging and location of errors in source files, syntax highlighting,
;;    automatic access to online help, and automatic version control are only
;;    available within the Ciao emacs mode.").
;;
;;:- comment(module,"@include{CiaoMode.lpdoc}").
;;
;;main.
;;
;;:- comment(version_maintenance,dir('../version')).
;;
;;:- comment(version(1*9+323,2004/03/08,18:37*17+'CET'), "Errors now
;;   also located in source files with no suffix, or .pls etc.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*9+281,2004/02/02,15:51*32+'CET'), "Fixed problem
;;   with C-c d when module has a name with single quotes (extra set of
;;   quotes not added anymore).  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+280,2004/02/02,15:51*10+'CET'), "Better
;;   highlighting when newlines present in commands. Persistent
;;   declarations now also highlighted. (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+279,2004/02/02,15:50*46+'CET'), "Eliminated
;;   some spurious messages left over from debugging.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*9+278,2004/02/02,15:48*21+'CET'), "Fixed problem
;;   due to replace-regexp-in-string not existing in older Emacs
;;   versions (pre 20.7.1). This should also fix some problems of the
;;   development environment in Windows. (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+102,2003/09/07,18:04*22+'CEST'), "Fixed minor
;;   syntax problem in ciao.el file (thanks to @index{Sergey Plis}
;;   @email{pliz@cs.unm.edu}).  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+95,2003/08/04,18:59*47+'CEST'), "Minor mod to
;;   allow C-cC-v to work with ciaopp-1.0, which uses Ciao prompt.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+94,2003/08/04,17:53*02+'CEST'), "Fixed minor
;;   bug when recognizing that inferior mode should be used.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*9+88,2003/07/18,15:58*24+'CEST'), "Added warning
;;   if attempting to put a process buffer in ciao-mode (recommending to
;;   use ciao-inferior-mode instead(.  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+82,2003/05/27,01:07*11+'CEST'), "Minor changes
;;   to improve xemacs compatibility (some, thanks to @index{Armin
;;   Rigo}).  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+73,2003/04/04,14:11*41+'CEST'), "Added new
;;   icons in environment for preprocessor. Also, when showing output
;;   after preprocessing the cursor is left in the original window.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+61,2003/02/25,12:24*25+'CET'), "Several changes
;;   to programming environment/emacs-mode:
;;
;;   @begin{itemize}
;;
;;   @item (Experimental) Reorganized menus: all help and customization
;;         now grouped in separate menus. No menu for deprecated
;;         @emph{traditional} commands any more.
;;
;;   @item Visiting the corresponding file and marking the location is
;;         now not done by default for messages of type NOTE (but it can
;;         optionally be turned on). It is set to nil by default because
;;         sometimes the user prefers not to take any action with
;;         respect to these messages (for example, many come from the
;;         documenter, indicating that adding certain declarations the
;;         documentation would be improved).
;;
;;   @item Many other commands (make-exec, make-po etc.) now also locate
;;         errors automatically and optionally.
;;
;;   @item C-cV (Preprocess buffer and show output for ciaopp) works
;;         again, and improved. Improved also behavior of standard
;;         ciao-show-preprocessor-output when no output file was
;;         produced by preprocessor. Unified several overlapping
;;         functions.
;;
;;   @item Informative messages given now when looking for next
;;         changelog entry and at last entry.
;;
;;   @item Minor fixes to the handling of version maintenance (insertion
;;         of version maintenance directives in empty files and help
;;         strings improved).
;;
;;   @item LPdoc error location now starts correctly the first time
;;         around (was bug). Systems files now located properly.
;;
;;   @item Better compatibility with @apl{xemacs} (thanks to
;;         @index{Armin Rigo} for some related patches).
;;
;;   @item Eliminated almost all emacs compilation warnings (useful for
;;         detecting bugs when making code changes).
;;
;;   @end{itemize}
;;
;;  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+54,2003/01/17,23:55*53+'CET'), "
;;   @begin{itemize}
;;
;;   @item Major improvement to customizability of faces for
;;         syntax-based highlighting. Faces now divided in conceptual
;;         classes, and customizable individually.
;;
;;   @item Greatly improved syntax-based coloring (better regexps). 
;;
;;   @item Syntax-based coloring now work also on ascii terminals for
;;         newer versions of emacs .
;;
;;   @item Literal-level assertions now correctly colored, even if they
;;         span several lines or contain disjunctions.
;;
;;   @item Added a customizable variable @tt{ciao-user-directives} which
;;         allows listing user-defined directives, to be colored in
;;         special face.
;;
;;   @item Separated directives into several more classes, with separate
;;         faces.
;;
;;   @item Syntax errors now colored also in inferior buffers.
;;
;;   @item Customizable faces now appear in the documentation.
;;
;;   @item Added new tool bar button (and binding) to refontify
;;         block/buffer. Better definitions of what is a block for
;;         syntax-based coloring to work on.
;;
;;   @item It is now possible to select through customize whether
;;         location of any errors produced when running Ciao tools
;;         (loading or preprocessing code, running the documenter, etc.)
;;         will be initiated automatically. I.e., whether after running
;;         a command, the system will automatically highlight any error
;;         messages and the corresponding areas in source files if
;;         possible.
;;
;;   @item Error marks now cleared automatically also when generating
;;         docs. Better clearing in other cases.
;;
;;   @item It is now possible to have error location in source start
;;         automatically after running lpdoc. Other fixes to hooks in
;;         lpdoc buffer.
;;
;;   @item Fixed problem when loading in xemacs due to missing function
;;         in newer versions.
;;
;;   @item Fixed problem with location of manuals in xemacs because of
;;         changes in number of arguments.
;;
;;   @item Added portable image insertion primitives with defaults,
;;         which work in emacs and xemacs. As a result image insertion
;;         now works in xemacs also (hooray!).
;;
;;   @item Icons are now installed in a separate dir. 
;;
;;   @end{itemize}
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+43,2002/12/16,10:48*42+'CET'),
;;   "ciao-module-name not interactive any more (was an error).  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*9+37,2002/12/11,17:20*36+'CET'), "Fixed bug in
;;   filepaths with \\ when loading a main file.  (Daniel Cabeza Gras)").
;;
;;:- comment(version(1*9+35,2002/12/08,10:07*34+'CET'), "Color names are
;;   uniquified in emacs mode.  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+31,2002/11/21,12:41*47+'CET'), "Reorganized
;;   source of emacs mode. Icons now in a separate directory.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*9+18,2002/10/20,18:33*20+'CEST'), "Improved
;;   handling of @tt{.cgi} files by debugger.  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+17,2002/10/13,12:29*38+'CEST'), "Several
;;   improvements to emacs-based environment:
;;
;;   @begin{itemize} 
;;
;;   @item Improvements so that paths are compatible with newer versions
;;         of @apl{cygwin} in Windows.
;;
;;   @item Improved splash procedure (ciao-startup), with sample
;;         file. Useful for starting Ciao by double-clicking on an icon.
;;
;;   @item Several improvements to preprocessor buffer.
;;
;;   @item Paths in @file{GlobalChangeLog} are now relative (long
;;         needed!). Paths in previous earlier @file{GlobalChangeLog}
;;         files should be changed to relative paths by hand.
;;
;;   @item @file{GlobalChangeLog} now set in Ciao mode by default.
;;
;;   @item Elimination of error/debug marks improved.
;;
;;   @end{itemize}
;;
;;  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+7,2002/05/26,12:45*28+'CEST'), "Easier now to
;;   adapt to different prompts in inferior processes. In particular, a
;;   regexp describing the prompt used in OS shells can now be set or
;;   modified through the standard customization. An important
;;   consequence is that error location in source files now also works
;;   when running applications which produce Ciao-style error messages
;;   in inferior shells (such as LPdoc, the embedded debugger, or any
;;   user application that outputs messages in the corresponding
;;   format). (Manuel Hermenegildo)").
;;
;;:- comment(version(1*9+6,2002/05/26,12:35*54+'CEST'), "New entry point
;;   (@tt{ciao-startup}, with no arguments) available, useful for
;;   calling ""The Ciao Programming Environment"" from a desktop icon, by
;;   making the icon call, for example, @tt{emacs -q -l
;;   /usr/local/lib/ciao/DOTemacs.el -f ciao-startup} (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*9+5,2002/05/26,12:34*06+'CEST'), "Variables
;;   @tt{ciao-library-path} / @tt{CIAOLIB} can now be set through the
;;   customization panel. Also, the scope of these has been improved.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+219,2002/05/15,22:46*11+'CEST'), "Errors (if
;;   any) are now located automatically in source after coad loading and
;;   similar commands. (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+218,2002/05/15,22:46*02+'CEST'), "Improved
;;   ciao-check-types-modes (preprocessor progress now visible).
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+217,2002/05/15,22:41*45+'CEST'), "Fixed loading
;;   regions repeatedly (no more predicate redefinition warnings), by
;;   using same temp file name (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+216,2002/05/15,22:40*16+'CEST'), "Added entries
;;   in @apl{ciaopp} menu to set verbosity of output. (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*7+215,2002/05/15,22:40*05+'CEST'), "Fixed timing
;;   problem with ciao-check-buffer-syntax. (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+214,2002/05/15,22:36*31+'CEST'), "Fixed some
;;   additional xemacs compatibility issues (related to searches).
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+188,2002/02/08,20:06*47+'CET'), "Set
;;   comint-highlight-prompt to nil because otherwise coloring is not
;;   correctly done.  (Daniel Cabeza Gras)").
;;
;;:- comment(version(1*7+187,2002/02/08,18:42*46+'CET'), "Changed some
;;   colors (Daniel Cabeza Gras)").
;;
;;:- comment(version(1*7+172,2002/01/03,21:45*17+'CET'), "Made remaning
;;   faces local to Ciao mode for portability.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*7+163,2002/01/03,12:09*10+'CET'), "Many
;;   improvements to emacs-based environment, some taking advantage of
;;   the great capabilities of emacs 21.1 and later:
;;
;;   @begin{itemize}
;;   @item Errors reported by inferior processes are now explored in
;;         forward order (i.e., the first error rewported is the first
;;         one highlighted). Improved tracking of errors. 
;;   @item @tt{:- doc} now also supported and highlighted.
;;   @item Nicer banner in emacs 21.1 
;;   @item Improved behaviour of stored query.
;;   @item Direct access to preprocessor (checking modes/types and
;;         locating errors) from toolbar.
;;   @item New icons for visualization of generated documentation.
;;   @item Improved behaviour of recentering, finding errors, etc.
;;   @item Debugger faces are now locally defined (and better
;;         customization). This also improves comtability with xemacs
;;         (which has different faces).
;;   @item Eliminated need for calendar.el
;;   @item Fixed some remaining incompatibilities with xemacs.
;;   @item Wait for prompt has better termination characteristics.
;;   @item Added some missing library directives to fontlock list, organized
;;         this better.
;;   @item Many other minor bugs fixed.
;;   @end{itemize}
;;
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+152,2001/11/23,16:11*27+'CET'), "Modified
;;   Makefile: -\(cd ....\) was erroneously interpreted; changed to -(
;;   cd ... ) (MCL)").
;;
;;:- comment(version(1*7+148,2001/11/17,20:40*45+'CET'), "Several
;;   improvements to emacs-based environment:
;;
;;   @begin{itemize}
;;   @item Ciao-specific tool bars now shown when editing files and in inferior 
;;         processes, with icons for main fuctions (works from emacs 21.1 on). 
;;   @item Other minor adaptations for working with emacs 21.1.
;;   @item Fixes to set-query. Also, previous query now appears in prompt.
;;   @item Added new interactive entry points (M-x): ciao, prolog, ciaopp.
;;   @item Fixed minor bug with tracking of last inferior buffer used.
;;   @item Several other minor improvements.
;;
;;   @end{itemize}
;;
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+122,2001/08/30,20:49*07+'CEST'), "The commands
;;   available in the interactive buffers (used by toplevel,
;;   preprocessor, etc.) are now documented in order to help users not
;;   very familiar with interactive buffers in emacs get
;;   started. (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+121,2001/08/30,17:43*34+'CEST'), "Context help
;;   now works in Ciao/CiaoPP/LPdoc toplevel interactive buffers in
;;   emacs without needing to set the mode explicitly.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(1*7+120,2001/08/28,18:08*13+'CEST'), "Minor
;;   corrections and improvements to documentation of emacs mode and
;;   debugger. Also fixed dependency code so that emacs mode
;;   documentation is always regenerated if the source file changes.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(1*7+92,2001/04/23,18:40*54+'CEST'), "Fixed bug which
;;   caused errors with short module names.  (Daniel Cabeza
;;   Gras)").
;;
;;:- comment(version(1*5+159,2000/05/31,10:52*49+'CEST'), "Fixed bug in
;;   source-level debug search.  (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(1*5+151,2000/05/26,12:27*05+'CEST'), "Added support to
;;   use source-level debugging in an embedded debugger.  (Manuel Carlos
;;   Rodriguez)").
;;
;;:- comment(version(1*5+147,2000/05/24,11:15*57+'CEST'), "Add Font-lock
;;   support for xemacs. (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(1*5+118,2000/04/19,18:13*43+'CEST'), "No need to
;;   previously create a SETTINGS file for documenting a single buffer
;;   in LPdoc mode. Reordered LPdoc menus to reflect this. Also, certain
;;   formats now viewed directly in emacs.  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*5+113,2000/04/10,17:35*36+'CEST'), "Modify the regular
;;   expression to search by the source-level debugger to support the
;;   embedded debugger.  (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(1*5+108,2000/04/07,11:02*54+'CEST'), "Change all the
;;   expressions containing what-line for a new function which return the
;;   line number. This functions is needed because what-line does not behave
;;   in emacs and in xemacs in the same way.  (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(1*5+99,2000/03/30,16:19*56+'CEST'), "Update
;;   documentation.  (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(1*5+95,2000/03/29,15:03*26+'CEST'), "Debugger menus
;;   rearranged a bit after user feedback.  (Manuel Hermenegildo)").
;;
;;:- comment(version(1*5+93,2000/03/28,18:12*08+'CEST'), "Fixed when
;;   using debug buffer source in a user file, with no module
;;   declaration, the name of the file was wrong.  (Manuel Carlos
;;   Rodriguez)").
;;
;;:- comment(version(1*5+86,2000/03/24,10:11*51+'CET'), "Fixed a problem
;;   with O'Ciao faces.  (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(1*5+85,2000/03/24,10:05*24+'CET'), "Added
;;   ciao-buffer-name. When debugging a module get the module name of a
;;   buffer. If the file name finish with .pl supress the extension,
;;   otherwise return the file name with extension.  (Manuel Carlos
;;   Rodriguez)").
;;
;;:- comment(version(1*5+80,2000/03/23,14:34*33+'CET'), "Synchronies control
;;   version for ciao.el.body with the ciao system control version.  (Manuel
;;   Carlos Rodriguez)").
;;
;;:- comment(version(0*5+86,2000/03/21,13:36*43+'CET'), "Ciao.el.body moves
;;   from using its own control version to use the control version used in
;;   Ciao. All the comments all will be kept in CiaoMode.pl in Ciao/Prolog
;;   syntax and will be printed in the Emacs Mode Manual.  (Manuel Carlos
;;   Rodriguez)").
;;
;;:- comment(version(0*5+85,2000/03/20,00:0*00+'CET'), "Fixed a bug which sent
;;   a  when in column > 10.  (Daniel Cabeza Gras)").
;;
;;:- comment(version(0*5+84,2000/03/17,00:0*00+'CET'), "Minor mod to LPdoc
;;   mode.  (Manuel Hermenegildo).").
;; 
;;:- comment(version(0*5+83,2000/03/16,00:0*00+'CET'), "Added the Set query
;;   and main file submenu and the ciao-load-query.  (Manuel Carlos
;;   Rodriguez)").
;; 
;;:- comment(version(0*5+82,2000/03/15,00:0*00+'CET'), "Added hilit a region
;;   when the predicate is not found. The color to hilit can be customize
;;   with ciao-debug-expansion.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+81,2000/02/29,00:0*00+'CET'), "Fix a bug when using
;;   the control version system in a directory which doesn't exist. In this
;;   case the directory is created.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+80,2000/02/24,00:0*00+'CET'), "Minor changes to
;;   ciao-hook. Move from a generic function to a function for each mode, but
;;   keeping a unique inferior mode but with three diferent filters. Also
;;   added some functionality as loading within a debug process or set a
;;   breakpoint while debugging. Move from differents functions to (un)mark
;;   buffers for debug to a one function which support all
;;   functionality. Add, also, set a default query to call when reloading and
;;   added ,too, load all modules as necessary.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+79,2000/02/22,00:0*00+'CET'), "Added menu support in
;;   xemacs.  (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(0*5+78,2000/02/16,00:0*00+'CET'), "Added support for
;;   hooks on a ciaopp prompt and lpdoc prompt.  There are also two different
;;   hooks for each prompt.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+77,2000/02/11,00:0*00+'CET'), "Added support for
;;   hooks on a ciao prompt. There are two different hooks, one for the Ciao
;;   inferior buffer and other to emacs side efect when founding a prompt on
;;   Ciao buffer.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+76,2000/02/07,00:0*00+'CET'), "Minor changes to
;;   documentation.  (Manuel Carlos Rodriguez)").
;;
;;:- comment(version(0*5+75,2000/02/07,00:0*00+'CET'), "Modify the
;;   customization section to show subsection and variable's type. Also added
;;   support for emacs customization.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+74,2000/02/02,00:0*00+'CET'), "Added a customization
;;   section in Ciao Mode manual.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+73,2000/02/01,00:0*00+'CET'), "Changed fontification
;;   of ciao-inferior-mode & regexp for prompt.  (Daniel Cabeza Gras)").
;;
;;:- comment(version(0*5+72,2000/01/30,00:0*00+'CET'), "Added C-cd
;;   (ciao-debug-buffer) shortcut for beginners.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+71,2000/01/29,00:0*00+'CET'), "Changed fontification.
;;   (Daniel Cabeza Gras)").
;; 
;;:- comment(version(0*5+70,2000/01/21,00:0*00+'CET'), "Move from hilit to
;;   font-lock.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+69,2000/01/12,00:0*00+'CET'), "Fixed menus (manual
;;   access), reorganized (debugging section).  (Manuel Hermenegildo and
;;   Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+68,2000/01/04,00:0*00+'CET'), "Fixed a minor problem
;;   in ciao-debug-filter.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+67,1999/12/30,00:0*00+'CET'), "Added font-lock to to
;;   breakpoints. Also a defun to repaint all breakpoints.  (Manuel Carlos
;;   Rodriguez)").
;; 
;;:- comment(version(0*5+66,1999/12/29,00:0*00+'CET'), "Added breakpoint
;;   support, hilit lines marked with ;**.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+65,1999/12/28,00:0*00+'CET'), "Added support for
;;   font-lock in Ciao.  (Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+64,1999/12/23,00:0*00+'CET'), "Change load order for
;;   doc indices so that preds are found first.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+63,1999/12/22,00:0*00+'CET'), "Fixed case when hilit
;;   not available (minimal functionality on xemacs).  (Manuel Hermenegildo
;;   and Manuel Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+62,1999/12/22,00:0*00+'CET'), "Fixed a minor timing
;;   problem in source debugger.  (Manuel Hermenegildo and Manuel Carlos
;;   Rodriguez)").
;; 
;;:- comment(version(0*5+61,1999/12/16,00:0*00+'CET'), "Minor fixes to lpdoc
;;   mode, marked srcdbg as experimental.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+60,1999/12/13,00:0*00+'CET'), "Now reasonable guess
;;   of module name made when using :- module(_,...).  (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+59,1999/12/13,00:0*00+'CET'), "Added debugger
;;   information in Ciao buffer and port color configuration.  (Manuel Carlos
;;   Rodriguez)").
;; 
;;:- comment(version(0*5+58,1999/12/10,00:0*00+'CET'), "Incorporated Manuel
;;   Carlos' source debugger functions.  (Manuel Hermenegildo and Manuel
;;   Carlos Rodriguez)").
;; 
;;:- comment(version(0*5+57,1999/12/05,00:0*00+'CET'), "Improved the handling
;;   of temporary files in LPdoc: now it is possible to generate docs for
;;   several files separately (done using an assoc list).  (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+56,1999/11/29,00:0*00+'CET'), "Separated out CiaoPP
;;   menu and created LPdoc menu. Fixed a number of bugs. Created LPdoc
;;   functionality. Now the documentation for a file can be generated in a
;;   temporary area by simply opening the file, and selecting items from the
;;   LPdoc menu.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+55,1999/11/24,00:0*00+'CET'), "Changed handling of
;;   toplevel and preprocessor commands to allow blanks in file names.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+54,1999/11/16,00:0*00+'CET'), "Parenthesis matching
;;   now also work in the Ciao/Prolog listener.  (Daniel Cabeza Gras)").
;;
;;:- comment(version(0*5+53,1999/11/16,00:0*00+'CET'), "A shorter Ciao/Prolog
;;   menu now also appears in the inferior modes.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+52,1999/11/16,00:0*00+'CET'), "Fixed bug when
;;   locating errors reported by Ciao for which no file name could be
;;   found. (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+51,1999/11/13,00:0*00+'CET'), "Updated and
;;   Unix/Windows comments in .skel.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+50,1999/11/08,00:0*00+'CET'), "Associated .pls files
;;   (for Windows), changed name in mode line to Ciao/Prolog. Fixed other
;;   minor things for Windows.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+49,1999/11/02,00:0*00+'CET'), "Added coloring of
;;   impl_defined.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+48,1999/10/20,00:0*00+'CET'), "Added
;;   ciao-insert-script-header.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+47,1999/9/28,00:0*00+'CET'), "Added
;;   ciao-preprocess-buffer-and-show-output. This allows re-running the
;;   preprocessor and refreshing the buffer containing the output in one
;;   go. Also, ciao-show-preprocessor-output now goes to the same point in
;;   the buffer as in the previous run.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+48,1999/9/7,00:0*00+'CET'), "Added colorings for
;;   discontiguous.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+46,1999/8/4,00:0*00+'CET'), "Added colorings for
;;   debug_message.  (German Puebla)").
;; 
;;:- comment(version(0*5+45,1999/7/24,00:0*00+'CET'), "Added colorings for
;;   reexport and others.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+44,1999/7/5,00:0*00+'CET'), "Minor change to
;;   use_module/use_class.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+43,1999/5/3,00:0*00+'CET'), "C-cTAB now works also
;;   in inferior mode.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+42,1999/4/15,00:0*00+'CET'), "Use_package(ociao) ->
;;   use_package(objects).  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+41,1999/4/14,00:0*00+'CET'), "Changed .el compilation
;;   command, change include to use_package for class/objects, other minor
;;   changes to coloring.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+40,1999/4/6,00:0*00+'CET'), "Minor changes to
;;   coloring.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+39,1999/3/31,00:0*00+'CET'), "Abbreviated word-help
;;   indices since now only one manual.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+38,1999/3/30,00:0*00+'CET'), "Added explanation of
;;   alternative way of specifying version maintenance.  (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+37,1999/3/25,00:0*00+'CET'), "Coloring use_class,
;;   etc.  also searching for :- class to do use_class instead of use_module.
;;   (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+36,1999/3/15,00:0*00+'CET'), "Now coloring
;;   use_package instead of syntax for Ciao 0.9. (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+35,1999/3/6,00:0*00+'CET'), "Fixed some bugs due to
;;   changing to read-file-name for completion.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+34,1999/2/29,00:0*00+'CET'), "Added bridge to
;;   word-help-complete. Increased completion level of several
;;   commands. Simple interface to setenv for changing CIAOLIB. (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+33,1999/2/29,00:0*00+'CET'), "Updated the list of
;;   word-help indices to match those in SETTINGS.COMMON.  (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+325,1999/2/8,00:0*00+'CET'), "Updated hilit patterns
;;   to avoid coloring @@<, @@=<, @@> and @@>=.  (Daniel Cabeza Gras)").
;; 
;;:- comment(version(0*5+32,1999/2/8,00:0*00+'CET'), "Increased limit when
;;   searching for :- module, added .asr to ignored entensions in
;;   DOTemacs.pl. (Daniel Cabeza Gras)").
;; 
;;:- comment(version(0*5+31,1999/1/12,00:0*00+'CET'), "Documentation finally
;;   completed.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+30,1998/12/15,00:0*00+'CET'), "ChangeLog file now
;;   opened in ciao-mode. Some changes in menus. Syntax checking now in two
;;   commands (required by top-level).  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+29,1998/12/14,00:0*00+'CET'), "Fixed interface to
;;   word-help. Still brittle...  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+28,1998/12/14,00:0*00+'CET'), "Minor bug
;;   fixes. Improved documentation.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+27,1998/12/06,00:0*00+'CET'), "Added binding and menu
;;   entry to call ciao-check-buffer-syntax on current buffer. Improved
;;   documentation.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+26,1998/12/03,00:0*00+'CET'), "Restored file name
;;   completion in inferior mode.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+25,1998/12/03,00:0*00+'CET'), "Inferior mode now also
;;   responds to C-c`. This can be used to look for errors even if running
;;   inside a normal shell by putting the shell buffer in ciao-inferior-mode
;;   and using the C-c` binding there!.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+24,1998/12/02,00:0*00+'CET'), "Fixed bug in region
;;   commands.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+23,1998/11/26,00:0*00+'CET'), "The executable name
;;   used to run ciao and the preprocessor are now configurable from the
;;   menus.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+22,1998/11/12,00:0*00+'CET'), "Improved localization
;;   of errors. Improved coloring.  (Daniel Cabeza Gras)").
;;
;;:- comment(version(0*5+21,1998/10/14,00:0*00+'CET'), "Separated out inferior
;;   mode syntax table. Better parsing of error messages. Improved
;;   definitions of comments.  (Daniel Cabeza Gras)").
;; 
;;:- comment(version(0*5+20,1998/9/25,00:0*00+'CET'), "Made color for checked
;;   assrts a little darker.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+19,1998/9/21,00:0*00+'CET'), "Auto-documentation!.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+18,1998/9/18,00:0*00+'CET'), "Paths now edited
;;   automatically. Better behaviour under no hilightting.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(0*5+17,1998/9/11,00:0*00+'CET'), "Updating changelogs
;;   does not ask for deleting excessive backup versions.  (Manuel
;;   Hermenegildo)").
;;
;;:- comment(version(0*5+16,1998/9/11,00:0*00+'CET'), "Hilighting of lpdoc
;;   commands.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+15,1998/8/20,00:0*00+'CET'), "Version maintenance
;;   method now determined by looking at comment/2 declaration.  (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+14,1998/8/14,00:0*00+'CET'), "Improved help
;;   access.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+13,1998/8/5,00:0*00+'CET'), "Improved menus, added
;;   help, including interface to word-help.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+12,1998/7/27,00:0*00+'CET'), "Updated debugger
;;   interface for new debugger.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+11,1998/7/14,00:0*00+'CET'), "Added making active
;;   modules.  (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+10,1998/7/14,00:0*00+'CET'), "New version reporting.
;;   (Manuel Hermenegildo)").
;; 
;;:- comment(version(0*5+9,1998/7/10,00:0*00+'CET'), "Improved use of
;;   make_exec/2 (now make_exec(FILE,_) is allowed).  (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+8,1998/7/10,00:0*00+'CET'), "Fixed bug in reporting
;;   of time in comments.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+7,1998/7/8,00:0*00+'CET'), "Fixed minor bugs, ask for
;;   saving buffers before compilation.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+6,1998/6/23,00:0*00+'CET'), "Changed default in
;;   version maintenance start prompt to q.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+5,1998/6/23,00:0*00+'CET'), "Updated hilit patterns.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+4,1998/6/17,00:0*00+'CET'), "Updated hilit
;;   patterns. (Manuel Hermenegildo) ").
;;
;;:- comment(version(0*5+3,1998/6/1,00:0*00+'CET'), "Hilit now on inferior
;;   process also.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+2,1998/5/23,00:0*00+'CET'), "Fixed older commands to
;;   work again.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*5+1,1998/5/20,00:0*00+'CET'), "Fixed bugs compilation
;;   functions and bindings, changed temp file regime.  (Manuel
;;   Hermenegildo)").
;; 
;;:- comment(version(0*5+0,1998/4/27,00:0*00+'CET'), "Added menu support.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(0*4+8,1998/4/15,00:0*00+'CET'), "Time now recorded in
;;   versions.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*4+7,1998/1/27,00:0*00+'CET'), "Added further processing
;;   of versions.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*4+6,1997/10/20,00:0*00+'CET'), "Added miscellaneous
;;   regexps (continued).  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*4+5,1997/8/21,00:0*00+'CET'), "Added author signing of
;;   versions.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*4+4,1997/8/06,00:0*00+'CET'), "Added version
;;   maintenance and other stuff.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*4+3,1997/5/06,00:0*00+'CET'), "Added hiliting for
;;   assertions and other minor changes. (Manuel Hermenegildo)").
;;
;;:- comment(version(0*3+0,1995/4/01,00:0*00+'CET'), "Major change to
;;   support Ciao functionality.  (Manuel Hermenegildo)").
;;
;;:- comment(version(0*2+1,1993/11/01,00:0*00+'CET'), "Major change to
;;   support &-Prolog functionality, operators, parallelizer, etc.
;;   (Manuel Hermenegildo)").
;;
;;:- comment(version(0*1+10,1993/02/05,00:0*00+'CET'), "Use copy-keymap where
;;   appropriate").
;;
;;:- comment(version(0*1+8,1993/01/27,00:0*00+'CET'), "Change log updated.").
;;
;;:- comment(version(0*1+7,1992/10/07,00:0*00+'CET'), "Add prolog-version
;;   don't treat where specially in indent-for-new-clause.").
;;
;;:- comment(version(0*1+6,1992/08/28,00:0*00+'CET'), "Fix broken input
;;   history filter for Prolog mode.").
;;
;;:- comment(version(0*1+5,1992/04/24,00:0*00+'CET'), "Added EPROLOG env
;;   variable. Flush sicstus-switch-to-buffer-other-window. Update
;;   compile-prolog to recompile entire directory.").
;; 
;;:- comment(version(0*1+2,1992/03/20,00:0*00+'CET'), "Johan Bevemyr's
;;   adaptation to the comint package. Change from sicstus0.7 to sicstus2.1.
;;   Treat where specially in indent-for-new-clause.").
;;
;; 
;;
;;
; -*- mode: emacs-lisp; -*-
; See CiaoMode.pl for ChangeLog

;; (if (< emacs-major-version 20)
;;   (error "AUC TeX requires Emacs 20 or later"))
;; @include{/home/clip/Systems/ciaopp-0.8/doc/readmes/README.lpdoc}
;; was Systems/ciaopp/doc but ciaopp is now 1.0 and does not have
;; (yet) a README.lpdoc file
;; 
;; The values in the automatically generated lpdoc SETTINGS file must
;; be configurable.
;; - It seems that a module-qualified goal in a predicate is not correctly
;;   marked in source debugging, the previous line is marked instead.
;; - When an empty .pl file is opened, add module declaration etc.
;; Preprocess with options should take you to the other buffer.
;; When an empty .pl file is visited an empty module declaration is
;; added and perhaps even version control.
;; The same way as with versions, files could have an indication of
;; where the 'project' file is (with the main, etc.)
;; **** xemacs tool bar
;; **** inferior should not be visible? (too advanced for now...)
;; **** Inferior mode 'Ciao' menu should change depending on whether
;;      it is LPdoc or CiaoPP, or ...
;; **** Tool bar has a button so that one can be in preprocess, debug,
;;      help, etc. mode, and the toolbar changes
;; **** do not use word help
;; **** really test on macs and windows, older versions, etc.
;; 
;; *** Frame titles: / In any case title should be 'Ciao'
;; default:
;; (setq (multiple-frames "%b" ("" invocation-name "@" system-name)))
;; ideas:
;; (setq frame-title-format (concat "Ciao: " "%b"))
;; (setq frame-title-format "%b")
;; 
;; **** Coloring:
;; %-type comments after a use_module declaration.
;; 
;; Should also color variables, etc. in toplevel?
;;
;; On Mac:
;;    Ciao requires the definition of some shell environment variables
;;    through the DOTcshrc or DOTprofile.  If correctly configured, when
;;    emacs is launched from a terminal all works fine, but not when you
;;    load emacs from the desktop icon (or dockbar, finder, etc.). It seems
;;    that these applications doesn't load any profile file (in a standard
;;    way).
;; 
;;    A workaround may be starting emacs from a terminal (ensuring that Ciao
;;    works in a terminal) in this way:
;; 
;;    $ open /Applications/Emacs.app
;;    - We could see if there is a way to load the user's profile file from
;;      the .emacs file.
;;    - We should in any case document it in the manual.
;; 
;; **** Define a 'novice' or 'ciao environment' variable. When on, menus 
;;      are very simple and only talk about Ciao (nothing on emacs). 
;;      In any case, menus should be divided into not Ciao or CiaoPP,
;;      but rather in other classes, e.g., help, debug, compile,
;;      precompile, document, version control, config, gui builder, etc.
;; emacs -q -l /home/clip/lib/ciao/DOTemacs -f ciao-startup
;; **** Generate task lists from the bug lists of distributions? (Paco)
;; **** Have a good bug reporting mechanism
;; **** Eliminate a changelog entry
;; **** Need to fix better the deleted buffer problem
;; 
;; Missing buttons in toolbar for new emacs versions??
;; 
;;    '(font-latex-match-font-outside-braces		      ;;;\textit{text}
;;      (0 font-lock-keyword-face
;;         append                         ;Override? [t 'keep 'prepend 'append]
;;         ;; Can't use prepend because that overwrites syntax fontification
;;         ;; e.g. comments.
;;         t)                              ;Laxmatch? if t, do not signal error
;;      (1 font-latex-italic-face append t)
;;      (2 font-latex-bold-face append t)
;;      (3 font-lock-type-face append t))

;; **** Remember to keep track of which version of emacs we are working with
;; **** C-u <compilation command> asks for options, remembers them per
;;      buffer (or set them through a menu)?

;; --------------------------------------------------------------------------
;; The actual code of the mode starts here.
;; --------------------------------------------------------------------------

;; These are rewritten during installation:

(defvar ciao-real-lib-dir "/home/clip/lib/ciao/ciao-1.10"
  "Where the actual Ciao lib directory is (and, thus, e.g., the image files).")

(defvar ciao-info-dir "/home/clip/public_html/Local/lpdoc_docs/Ciao"
  "Where the actual Ciao (LPdoc) info directory is.")

;; In emacs this is done most reliably by setting  INFOPATH (done in
;; Ciao installation)
;; xemacs does need it for finding the Ciao manuals (does not seem to
;; read INFOPATH)
(if (boundp 'xemacs-logo)
    (progn
      (load-library "info")
      ;; (require 'info)
      (setq Info-directory-list (cons ciao-info-dir Info-directory-list))))

;; This is so that the other .el files (word-help, etc.) in the Ciao
;; lib are found (this path is updated automatically during installation):
(setq load-path (cons ciao-real-lib-dir load-path))

;; --------------------------------------------------------------------------
;; Mode documentation and acks (see also documentation in functions
;; and the CiaoMode.pl file included above)
;; --------------------------------------------------------------------------

(defun ciao-mode-documentation ()
  "This function generates documentation in lpdoc format for the
    Ciao/Prolog mode commands and their bindings."
  (interactive)
  (switch-to-buffer "*ciao-tmp*")
  (ciao-mode-nocheck) ;; so that the bindings that we document are active!
;;  (ciao-inferior-mode) ;; so that the bindings that we document are active!
  
  (insert-string "@comment{** Do not edit--generated automatically **}

The Ciao/Prolog @concept{emacs interface} (or @em{mode} @cindex{emacs mode}
in @apl{emacs} terms) provides a rich, integrated user interface to the
Ciao @index{program development environment} components, including the
@apl{ciaosh} interactive top level and the @apl{ciaopp} preprocessor. While
most features of the Ciao development environment are available from the
command line of the preprocessor and the top-level shell, using Ciao inside
@apl{emacs} is highly recommended. The facilities that this mode provides
include:

@begin{itemize}

@item @index{Syntax-based highlighting} (coloring), @cindex{coloring,
syntax} @index{auto-indentation}, @index{auto-fill}, etc. of code. This
includes the assertions used by the preprocessor and the documentation
strings used by the Ciao auto-documenter, @apl{lpdoc}.

@item Providing automatic access to @concept{on-line help} for all
predicates by accessing the Ciao system manuals in @apl{info} format.

@item Starting and communicating with @apl{ciaopp}, the @index{Ciao
preprocessor}, running in its own @concept{sub-shell}. This allows easily
performing certain kinds of @index{static checks} (useful for finding
errors in programs before running them), program analysis tasks, and
@index{program transformations} on source programs.

@item Starting and communicating with the @index{Ciao top-level}, running
in its own @concept{sub-shell}. This facilitates loading programs, checking
the @em{syntax} of programs (and of @index{assertions} within programs),
marking and unmarking modules for interactive debugging, @index{tracing the
source code} @cindex{source-level debugging} @cindex{debugging,
source-level} during debugging, making stand-alone executables, compiling
modules to dynamically linkable Prolog objects, compiling modules to active
objects, etc.

@item Syntax highlighting and coloring of the error and warning messages
produced by the top level, preprocessor, or any other tool using the same
message format (such as the @apl{lpdoc} auto-documenter), and @em{locating
automatically the points in the source files where such errors occur}.

@item Performing automatic @index{version control} and keeping a
@index{changelog} of individual files or whole applications. This is done
by automatically including changelog entries in source files, which can
then be processed by the @apl{lpdoc} auto-documenter.

@end{itemize}

This chapter explains how to use the Ciao/Prolog @apl{emacs} interface and
how to set up your @apl{emacs} environment for correct operation.  The Ciao
@apl{emacs} interface can also be used to work with other Prolog or CLP
systems.

@section{Conventions for writing Ciao programs under Emacs}
@cindex{formatting conventions, for emacs} 

This is particularly important for the @concept{source-level debugger}
and the @concept{syntax-based coloring} capabilities.  This is due to
the fact that it would be unrealistic to write a complete Prolog
parser in Emacs lisp. These conventions are the following, in order of
importance:

@begin{itemize}

@item Clauses should begin on the first column (this is used to recognize
      the beginning of a clause). 

@item C style comments should not be used in a clause, but can be used
      outside any clause.

@end{itemize}

@noindent 
The following suggestion is not strictly necessary but can improve
operation:

@begin{itemize}

@item Body literals should be indented. There should be not more than
one literal per line. This allows more precision in the location of
program points during source-level debugging, i.e., when marking
breakpoints and during line tracing.

@end{itemize}

@noindent Comments which start with @tt{%}s are indented to the right
if indentation is asked for.

@noindent For syntax-based highlighting to be performed font-lock must
be available and not disabled (the Ciao mode enables it but it may be
disabled elsewhere in, e.g., the @file{.emacs} file).

@section{Checking the installation}

Typically, a complete pre-installation of the Ciao/Prolog @apl{emacs}
interface is completed during Ciao installation. To check that
installation was done and sucessful, open a file with a @tt{.pl}
ending. You should see that @apl{emacs} enters Ciao/Prolog mode: the
mode is identified in the @concept{status bar} below the
@concept{buffer} and, if the @concept{emacs menu bar} is enabled, you
should see the Ciao/Prolog menus. You should be able from the
menu-bar, for example, to go to the Ciao manuals in the info or load
the @tt{.pl} file that you just opened into a ciao top level.

If things don't work properly, see the section @ref{Installation of the
Ciao/Prolog emacs interface} later in this chapter.

@section{Functionality and associated key sequences (bindings)}

The following sections summarize the capabilities of the Ciao/Prolog
emacs interface and the (default) @index{key sequences} used to access
those capabilities.  Most of these functions are accessible also from
the menu bar.

")

  ;; This inserts the documentation strings for the bindings.
  (ciao-do-document-bindings (nreverse ciao-documented-commands))

  (insert-string (concat "

@section{Using Ciao/Prolog mode capabilities in standard shells} 

The capabilities (commands, coloring, error location, ...) which are
active in the Ciao/Prolog @em{inferior} mode can also be made
available in any standard command line shell which is being run within
emacs. This can be enabled by going to the buffer in which the shell
is running and typing ``@key{M-x} @tt{ciao-inferior-mode}''.  This is
very useful for example when running the stand-alone compiler, the
@apl{lpdoc} auto-documenter, or even certain user applications (those
that use the standard error message library) in an emacs
sub-shell. Turning the Ciao/Prolog inferior mode on on that sub-shell
will highlight and color the error messages, and automatically find
and visit the locations in the files in which the errors are reported.

Finally, one the most useful applications of this is when using the
@concept{embedded debugger} (a version of the debugger which can be
embedded into executables so that an interactive debugging session can
be triggered at any time while running that executable without needing
the top-level shell). If an application is run in a shell buffer which
has been set with Ciao inferior mode (@key{M-x} @tt{ciao-inferior-mode}) and
this application starts emitting output from the embedded debugger
(i.e., which contains the embedded debugger and is debugging its code)
then the Ciao emacs mode will be able to follow these messages, for
example tracking execution in the source level code. This also works
if the application is written in a combination of languages, provided
the parts written in Ciao are compiled with the embedded debugger
package and is thus a covenient way of debugging multi-language
applications. The only thing needed is to make sure that the output
messages appear in a shell buffer that is in Ciao inferior mode.



@section{Customization}

This section explains all variables used in the Ciao/Prolog emacs mode
which can be customized by users. Such customization can be performed
(in later versions of @apl{emacs}) from the @apl{emacs} menus
(@tt{Help -> Customize -> Top-level Customization Group}), or also by
adding a @tt{setq} expression in the @tt{.emacs} file. Such @tt{setq}
expression should be similar to:

@tt{(setq <variable> <new_value>)}

@noindent The following sections list the different variables which can be
customized for @apl{ciao}, @apl{ciaopp} and @apl{lpdoc}.\n"))

(ciao-document-variables)

(insert-string (concat "

@section{Installation of the Ciao/Prolog emacs interface}

If opening a file ending with @tt{.pl} puts emacs in another mode
(such as @apl{perl} mode, which is the --arguably incorrect-- default
setting in some @apl{emacs} distributions), then either the emacs mode
was not installed or the installation settings are being overwritten
by other settings in your @tt{.emacs} file or in some library.  In any
case, you can set things manually so that the Ciao/Prolog mode is
loaded by default in your system. This can be done by including in
your @file{.emacs} file a line such as:

@tt{(load ""<CIAOLIBDIR>/DOTemacs"")}

@noindent This loads the above mentioned file from the Ciao library, which
contains the following lines (except that the paths are changed during
installation to appropriate values for your system):

@begin{verbatim}
@includeverbatim{DOTemacs.skel}
@end{verbatim}

If you would like to configure things in a different way, you can also
copy the contents of this file to your @file{.emacs} file and make the
appropriate changes.  For example, if you do not want @tt{.pl} files
to be put automatically in Ciao/Prolog mode, then comment out (or
remove) the line:

@tt{(setq auto-mode-alist} ... @tt{)}

@noindent You will then need to switch manually to Ciao/Prolog mode by
typing @tt{M-x ciao-mode} after opening a Prolog file.

If you are able to open the Ciao/Prolog menu but the Ciao manuals are not
found or the @apl{ciao} command (the top-level) is not found when loading
@tt{.pl} files, the probable cause is that you do not have the Ciao paths
in the @tt{INFOPATH} and @tt{MANPATH} @index{environment variables}
(whether these variables are set automatically or not for users depends on
how the Ciao system was installed). Under Un*x, you can add these paths
easily by including the line:

@tt{source <CIAOLIBDIR>/DOTcshrc}

@noindent in your @tt{.login} or @tt{.cshrc} files if you are using
@apl{csh} (or @apl{tcsh}, etc.), or, alternatively, the line:

@tt{. <CIAOLIBDIR>/DOTprofile}

@noindent in your @tt{.login} or @tt{.profile} files if you are using
@apl{sh} (or @apl{bash}, etc.). See the Ciao installation instructions
(@ref{Installing Ciao from the source distribution} or @ref{Installing
Ciao from a Win32 binary distribution}) for details.

@section{Emacs version compatibility} "

ciao-mode-emacs-version "

@section{Acknowledgments (ciao.el)}

This code is derived from the 1993 version of the emacs interface for
@concept{&-Prolog} by M. Hermenegildo, itself derived from the original
@file{prolog.el} by @index{Masanobu Umeda} with changes by @index{Johan
Andersson}, @index{Peter Olin}, @index{Mats Carlsson}, and @index{Johan
Bevemyr} of @index{SICS}, Sweden. Other changes also by Daniel Cabeza and
Manuel C. Rodriguez. See the changelog for details."

  ))
  (setq version-control 'never)
  (write-file "CiaoMode.lpdoc")
  )

;;----------------------------------------------------------------------------
;; Required packages (see also info below for xemacs, etc.)
;; ---------------------------------------------------------------------------

(require 'comint)
;; (require 'calendar)
(require 'easymenu)
(require 'word-help)
(require 'etags)
;; We use FSF Emacs overlays. XEmacs uses extents instead, but comes
;; with a package to emulate overlays.
(if (boundp 'xemacs-logo)
  (require 'overlay))
(provide 'ciao)


;; ===========================================================================
;; Mode variables
;; ===========================================================================

;; --------------------------------------------------------------------------
;; Note: version control information, title, authors, etc. is now maintained 
;;       automatically, synchronized with the overall Ciao system
;;       versions, in file CiaoMode.pl (included on installation 
;;       with this file).
;;---------------------------------------------------------------------------

;; Do not change the two lines below (Patched by installation!):
(defconst ciao-mode-version "1.10p7" 
  "This is the version number of the ciao.el file")

(defconst ciao-mode-emacs-version 

  "This mode is currently being developed within @apl{GNU emacs}
version 21.2. It should also (hopefully) work with all other 21.XX,
20.XX, and later 19.XX versions. We also try our best to keep things
working under @apl{xemacs}."

  "This is a comment describing for which emacs version this ciao.el
   file has been developed.")

;; ---------------------------------------------------------------------------
;; Basic Ciao mode variables
;; ---------------------------------------------------------------------------

(defgroup ciao-environment nil
  "The Ciao system programming environment, including Ciao/Prolog,
CiaoPP and LPdoc." 
  :tag "Prolog/Ciao"
  :group 'emacs)

(defgroup ciao-environment nil
  "The Ciao system programming environment, including Ciao/Prolog,
CiaoPP and LPdoc." 
  :tag "Prolog/Ciao"
  :group 'languages)

(defgroup ciao nil
  "The Ciao/Prolog system."
  :tag "Ciao/Prolog"
  :group 'ciao-environment)

(defgroup ciaopp nil
  "The Ciao preprocesor."
  :tag "CiaoPP"
  :group 'ciao-environment)

(defgroup lpdoc nil
  "The LPdoc documentation generator."
  :tag "LPdoc"
  :group 'ciao-environment)

;; General faces group
(defgroup ciao-highlighting-faces nil
  "Ciao environment faces for syntax highlighting, debugger, etc."
  :tag "Ciao Faces" :group 'ciao-environment)

(defcustom ciao-faces-use-variable-pitch-in-comments nil
  "Controls whether variable pitch fonts are used when highlighting
comments. Unset by default. After changing this you must exit and
reinitialize for the change to take effect."
  :group 'ciao-highlighting-faces
  :type 'boolean)

(defcustom ciao-system (or (getenv "CIAO") "ciao")
  "Name of Ciao or Prolog executable which runs the classical Prolog-like
top level."
  :group 'ciao
  :type 'string)

(defun ciao-set-ciao-system () 
  "Change the Ciao/Prolog executable used to run the Prolog-like top
level. It is set by default to @tt{ciao} or, to the environment
variable @tt{CIAO} if it is defined. @cindex{toplevel command, setting}"
  (interactive)
  (setq ciao-system
	(read-file-name "Change Ciao/Prolog top-level executable ?" 
			"" ciao-system nil ciao-system)))

(defcustom ciao-system-args (or (getenv "CIAOARGS") "")
  "Arguments passed to Ciao/Prolog toplevel executable."
  :group 'ciao
  :type 'string)

(defun ciao-set-ciao-system-args () 
  "Change the arguments passed to the Ciao/Prolog executable. They are
set by default to none or, to the environment variable @tt{CIAOARGS} if it
is defined. @cindex{toplevel command args, setting}"
  (interactive)
  (setq ciao-system-args
	(read-file-name "Change args passed to Ciao/Prolog executable ?" 
			"" ciao-system-args nil ciao-system-args)))

;; 'ignore' is because custom passes id of symbol
(defun ciao-do-set-library-path (ignore ciaolib) 
  (if (string= ciaolib "") 
      (progn
	(setenv "CIAOLIB" nil)
	(setq ciao-library-path ""))
    (setenv "CIAOLIB" ciaolib)
    (setq ciao-library-path ciaolib)))

(defun ciao-initialize-library-path (ignorea ignoreb) 
  (ciao-do-set-library-path nil (or (getenv "CIAOLIB") "")))

(defcustom ciao-library-path ""
  "Path to the Ciao/Prolog System libraries (reads/sets the CIAOLIB
environment variable ). Typically left empty, since ciao
executables know which library to use."
  :group 'ciao
  :type 'string
  :initialize 'ciao-initialize-library-path
  :set 'ciao-do-set-library-path
  )

(defun ciao-set-library-path () 
  "Change the location of the Ciao/Prolog library paths (changes the
   environment variable @tt{CIAOLIB})."
  (interactive)
  (ciao-do-set-library-path nil
   (read-file-name "Change Ciao/Prolog library path ?" 
		   "" (getenv "CIAOLIB") nil (getenv "CIAOLIB"))))

(defcustom ciao-locate-errors-after-run t
  "If set, location of any errors produced when running Ciao tools
(loading or preprocessing code, running the documenter, etc.) will be
initiated automatically. I.e., after running a command, the system
will automatically highlight any error messages and the corresponding
areas in source files if possible. If set to nil this location will
only happen after typing \\<ciao-mode-map>
\\[ciao-find-last-run-errors] or accessing the corresponding menu or
tool bar button."
  :group 'ciao
  :type 'boolean)

(defcustom ciao-locate-also-note-messages nil
  "If set, also when errors of type NOTE are detected the
corresponding file is visited and the location marked. It is set to
nil by default because sometimes the user prefers not to take any
action with respect to these messages (for example, many come from the
documenter, indicating that adding certain declarations the
documentation would be improved)."
  :group 'ciao
  :type 'boolean)

;; Prompt patterns -- left some out of custom because you need to be
;;                    really careful when changing these...

(defvar ciao-prompt "?-" 
  "Ciao prompt (simplest)")

(defvar ciao-ciaopp-prompt "ciaopp ?-" 
  "CiaoPP prompt (simplest)")

(defvar ciao-prompt-pattern "\n\\?- " 
  "Matching the ciao prompt")

(defvar ciao-ciaopp-prompt-pattern "\nciaopp \\?- " 
  "Matching the ciaopp prompt")

(defcustom ciao-os-shell-prompt-pattern "\\[[0-9]+\\]> "
  "Regular expression used to describe the shell prompt pattern, so
that error location works in inferior shells. This is useful for
example so that errors are located when generating documentation (for
lpdoc versions up to 1.9), and also when using the embedded debugger
or any other application in a shell. It is best to be as precise as
possible when defining this so that the standard ciao error location
does not get confused."
  :group 'ciao
  :type 'string)

(defvar ciao-any-prompt-pattern (concat 
  "\\("
  "^\\(\\(\\|[0-9]+ \\|ciaopp \\|lpdoc \\|| \\)\\?-\\)" 
  "\\|" 
  ciao-os-shell-prompt-pattern 
  "\\)")
  "Matching any ciao or other inferior process prompt")
; "\\(^|* *\\?- *\\)\\|\\(^ciaopp \\?- *\\)") Old Ciao/SICStus prompt patterns

;; Note this one is a function
(defun ciao-error-or-prompt-pattern ()
  (concat 
  "\\("				      
  (if ciao-locate-also-note-messages
      "^\\({?WARNING.*:\\|{?ERROR.*:\\|{?NOTE.*:\\)"
    "^\\({?WARNING.*:\\|{?ERROR.*:\\)")
  "\\|"
  ciao-any-prompt-pattern
  "\\)"))

;; Set to ciao-os-shell-prompt-pattern:
(defvar ciao-lpdoc-prompt-pattern ciao-any-prompt-pattern
  "Matching the lpdoc prompt")

;; *** For lpdoc version 2.0:
;; (defvar ciao-lpdoc-prompt-pattern "\n\\lpdoc ?- " 
;;   "Matching the lpdoc prompt")

;; End of prompt patterns --

(defcustom ciao-user-directives '( "mydirective" )
  "List of identifiers of any directives defined by users which you
would like highlighted (colored). Be careful, since wrong entries may
affect other syntax highlighting."
  :group 'ciao
  :type 'list)

;; Problem is, environment usually opened with -q -> needs a special 
;; config file for this case...
;; (i.e., typically by double-clicking on an icon) 
(defcustom ciao-create-sample-file-on-startup t
  "When starting the ciao environment using ciao-startup two buffers
are opened: one with a Prolog toplevel and another with a sample
file. This toggle controls whether the sample file, meant for novice
users, is created or not. Set by default, non-novice users will
probably want to turn it off."
  :group 'ciao
  :type 'boolean)

(defcustom ciao-toplevel-buffer-name "Ciao/Prolog"
  "Basic name of the buffer running the Ciao/Prolog toplevel inferior 
process."
  :group 'ciao
  :type 'string)

(defcustom ciao-indent-width 4
  "Indentation for a new goal."
  :group 'ciao
  :type 'integer)

(defvar ciao-temp-file-name "ciao")
(defvar ciao-last-temp-file nil)

(defvar ciao-previous-error nil
  "Stores where the last error was.")

(defvar ciao-inferior-error nil
  "Stores the line in the inferior buffer which shows the error line.")

(defvar ciao-finding-errors nil
  "Non nil if we are in the middle of the process of finding errors.
   In that case it contains the original buffer to return to in the end.")

(defvar ciao-last-process-buffer-used nil
  "Contains which is the last process buffer (preprocessor, toplevel,
   ...) used.")

(defvar ciao-last-source-buffer-used nil
  "Used to contain sometimes the last source buffer used (useful for
returning to it after processing).")

(defcustom ciao-main-filename ""
  "Name of main file in a multiple module program. Setting thsi is
very useful when working on a multi-module program because it allows 
issuing a load command after working on an inferior module which will 
reload from the main module, thus also reloading automatically all
dependent modules." 
  :group 'ciao
  :type 'string)

(defun ciao-set-main-filename ()
  "Set the current buffer as the principal file in a multiple module
programming environment."
  (interactive)
  (setq ciao-main-filename
	(read-file-name "Change Ciao/Prolog main file? " 
			"" (buffer-file-name) t (buffer-file-name))))
;;   (setq ciao-main-filename 
;; 	(read-file-name "Change Ciao/Prolog main module? " 
;; 			"" (ciao-get-module-name) nil (ciao-get-module-name))))

(defvar ciao-prompt-emacs-hook nil
  "Things to do in emacs once the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-prompt-inferior-hook nil
  "Things to do in Ciao once the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-prompt-marker-acc ""
  "Keep the last line written in ciao inferior buffer. It is used to search
for the prompt since prompt should be after a newline.")
(make-variable-buffer-local 'ciao-prompt-marker-acc)

(defcustom ciao-query ""
  "Query to use in Ciao. Setting this is useful when using a long or
complicated query because it saves from having to type it over and
over again. It is possible to set that this query will be issued 
any time a program is (re)loaded."
  :group 'ciao
  :type 'string)

(defun ciao-set-query ()
  "Set a default query. This may be useful specially during debugging
sessions. However, as mentioned elsewhere, note that commands that
repeat previous queries are also available. 

This query can be recalled at any time using \\<ciao-mode-map>
\\[ciao-load-query].  It is also possible to set things up so that
this query will be issued automatically any time a program is
(re)loaded. The functionality is available in the major mode (i.e.,
from a buffer containing a source file) and in the inferior mode
(i.e., from the buffer running the top-level shell). When called from
the major mode (i.e., from window containing a source file) then the
user is prompted in the minibuffer for the query. When called from the
inferior mode
(i.e., from a top-level window) then the query on the current line,
following the Ciao prompt, is taken as the default query.

To clear the default query use \\<ciao-mode-map> \\[ciao-clear-query]
or simply set it to an empty query: i.e., in a source buffer select
\\[ciao-set-query] and enter an empty query. In an inferior mode
simply select \\[ciao-set-query] on a line that contains only the
system prompt."


  (interactive)
  (let (beg query)
    (cond ((string= (buffer-name) (concat "*" ciao-toplevel-buffer-name "*"))
	   (save-excursion
             ;; MH This approach does not work in 21.1
             ;; (beginning-of-line) 
             (if (not (search-backward-regexp ciao-prompt-pattern nil t))
                 (setq ciao-query "")
               (goto-char (match-end 0))
	       (setq beg (point))
	       (end-of-line)
	       (setq ciao-query 
		    (buffer-substring-no-properties beg (point))))))
	  ((eq major-mode 'ciao-mode)
	   (setq query (read-string "Set default query to: " ciao-query))
	   (setq ciao-query query)
	   )))
  (if (string= ciao-query "")
      (message "Default query cleared")
    (message (concat "Default query set to: '" ciao-query "'" ))))

;; MCarlos: In xemacs function match-string-no-properties does not exist.
;; This will fix that, but when using ciao-match-string you
;; should use (funcall ciao-match-string <args>)
(defvar ciao-match-string nil)
(if (not (boundp 'xemacs-logo))
    (setq ciao-match-string 'match-string-no-properties)
  (setq ciao-match-string 'match-string))

(defun ciao-clear-query ()
  "Clear the default query."
  (interactive)
  (setq ciao-query "")
  (message "Default query cleared"))

;; ---------------------------------------------------------------------------
;; Source debugger variables
;; ---------------------------------------------------------------------------

;; CHANGE
(defvar ciao-debug-filter-defer-flag nil
  "Non-nil means don't process anything form the debugger 
right now. It is saved for when flag is not set.")

(defvar ciao-debug-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in
'ciao-debug-filter'.")

(defvar ciao-debug-delete-prompt-marker nil)

(defvar ciao-debug-last-frame nil 
  "Last file over which we have drawn.")

(defvar ciao-debug-last-line nil 
  "Temporary storage of last line (coloring).")

(defvar ciao-debug-marker-acc ""
  "Text to search for ciao-debug-marker-prompt.")
(make-variable-buffer-local 'ciao-debug-marker-acc)

(defvar ciao-debug-marker-regexp nil
  "Regular expression for looking for file position info.")
(setq ciao-debug-marker-regexp
      (concat 
	      "         In "
	      "\\(.*\\)"         ; Src file
	      " ("
	      "\\([0-9]+\\)"     ; Start line
	      "-"
	      "\\([0-9]+\\)"     ; End line
              ") "
	      "\\(.*\\)"         ; Pred name
	      "-"
	      "\\([0-9]+\\)\n"    ;)) ; n-th pred
	      ".*[\*0-9]+  [\*0-9]+"
	      "  \\([CERF][a-z]+\\):.* ? "))

(defcustom ciao-logo "ciao.xpm"
  "Ciao logo image."
  :group 'ciao
  :type 'file)

(defcustom ciao-clip-logo "clip.xpm"
  "CLIP logo image."
  :group 'ciao
  :type 'file)

;; ---------------------------------------------------------------------------
;; CiaoPP variables
;; ---------------------------------------------------------------------------

(defcustom ciao-ciaopp-system (or (getenv "CIAOPP") "ciaopp-0.8")
  "Name of Ciao preprocessor executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system () 
  "Change the executable used to run the Ciao Preprocessor
toplevel. It is set by default to @tt{ciaopp} or, to the environment 
variable @tt{CIAOPP} if it is defined. @cindex{preprocessor command, setting}"
  (interactive)
  (setq ciao-ciaopp-system
	(read-file-name "Change Ciao/Prolog preprocessor executable ?"
   		        "" ciao-ciaopp-system nil ciao-ciaopp-system))) 

(defcustom ciao-ciaopp-system-args (or (getenv "CIAOPPARGS") "")
  "Arguments passed to Ciao preprocessor executable."
  :group 'ciaopp
  :type 'string)

(defun ciao-set-ciaopp-system-args () 
  "Change the arguments passed to the Ciao preprocessor executable. They are
set by default to none or to the environment variable @tt{CIAOPPARGS} if it
is defined. @cindex{preprocessor command args, setting}"
  (interactive)
  (setq ciao-ciaopp-system-args
	(read-file-name "Change args passed to Ciao preprocessor executable ?"
	        "" ciao-ciaopp-system-args nil ciao-ciaopp-system-args))) 

(defcustom ciao-ciaopp-buffer-name "Ciao-Preprocessor"
  "Basic name of the buffer running the Ciao preprocessor inferior process."
  :group 'ciaopp
  :type 'string) 

(defvar ciao-ciaopp-prompt-emacs-hook nil
  "Things to do in emacs once the prompt is found in the CiaoPP buffer.")

(defvar ciao-ciaopp-prompt-inferior-hook nil
  "Things to do in CiaoPP once the prompt is found in the CiaoPP buffer.")

(defvar ciao-ciaopp-prompt-marker-acc ""
  "Keep the last line written in ciao inferior buffer. It is used to search
for the prompt since prompt should be after a newline.")
(make-variable-buffer-local 'ciao-ciaopp-prompt-marker-acc)

;; ---------------------------------------------------------------------------
;; LPdoc variables
;; ---------------------------------------------------------------------------

;; This for lpdoc-1.9; set to "lpdoc" for 2.0
(defcustom ciao-lpdoc-system (or (getenv "LPDOC") "gmake")
  "Name of LPdoc auto-documenter executable."
  :group 'lpdoc
  :type 'string)

(defun ciao-set-lpdoc-system () 
  "Change the executable used to run the LPdoc auto-documenter. It is
set by default to @tt{lpdoc} or to the environment  
variable @tt{LPDOC} if it is defined. @cindex{lpdoc command, setting}
@cindex{auto-documenter command, setting}"
  (interactive)
  (setq ciao-lpdoc-system
	(read-file-name "Change Ciao/Prolog LPdoc auto-documenter executable ?"
   		        "" ciao-lpdoc-system nil ciao-lpdoc-system))) 

(defcustom ciao-lpdoc-system-args (or (getenv "LPDOCARGS") "")
  "Arguments passed to LPdoc executable."
  :group 'lpdoc
  :type 'string)

(defun ciao-set-lpdoc-system-args () 
  "Change the arguments passed to the LPdoc auto-documenter. They are
set by default to none or to the environment variable @tt{LPDOCARGS} if it
is defined. @cindex{lpdoc command args, setting}
@cindex{auto-documenter command args, setting}" 
  (interactive)
  (setq ciao-lpdoc-system-args
     (read-file-name "Change args passed to LPdoc auto documenter executable ?"
	        "" ciao-lpdoc-system-args nil ciao-lpdoc-system-args))) 

(defvar ciao-lpdoc-buffer-tmpdir-list nil
  "Assoc. list relating filenames and their temporary doc dirs.")

(defcustom ciao-lpdoc-wdir-root (or (getenv "LPDOCWDIR") "/tmp")
  "Name of root working dir used by LPdoc."
  :group 'lpdoc
  :type 'directory)

(defun ciao-set-lpdoc-wdir-root () 
  "Change the root working dir used by the LPdoc auto-documenter. It is
set by default to a new dir under @tt{/tmp} or to the environment  
variable @tt{LPDOCWDIR} if it is defined. @cindex{lpdoc working dir, setting}
@cindex{auto-documenter working dir, setting}"
  (interactive)
  (setq ciao-lpdoc-wdir-root
     (read-file-name "Change root working dir used by LPdoc auto-documenter ?"
   		        "" ciao-lpdoc-wdir-root nil ciao-lpdoc-wdir-root))) 

(defcustom ciao-lpdoc-docformat (or (getenv "LPDOCFORMAT") "dvi")
  "Name of default output format used by LPdoc."
  :group 'lpdoc
  :type '(choice (const "dvi")
		 (const "ps")
		 (const "info")
		 (const "man")))

(defun ciao-set-lpdoc-docformat () 
  "Change the default output format used by the LPdoc auto-documenter. It
is set by default to @tt{dvi} or to the environment variable
@tt{LPDOCFORMAT} if it is defined. @cindex{lpdoc default format, setting}
@cindex{auto-documenter default format, setting}"
  (interactive)
  (setq ciao-lpdoc-docformat
    (read-string "Change default doc format used by LPdoc auto-documenter ?"
		 ciao-lpdoc-docformat)))

(defcustom ciao-lpdoc-libpath (or (getenv "LPDOCLIB") "/home/clip/lib")
  "Path in which the LPdoc library is installed."
  :group 'lpdoc
  :type 'directory)

(defun ciao-set-lpdoc-libpath () 
  "Change the path in which the LPdoc library is installed. It is
set by default to @tt{/home/clip/lib} or to the environment  
variable @tt{LPDOCLIB} if it is defined. @cindex{lpdoc lib path, setting}
@cindex{auto-documenter lib path, setting}"
  (interactive)
  (setq ciao-lpdoc-libpath
	(read-file-name "Change path in which LPdoc lib is installed ?"
   		        "" ciao-lpdoc-libpath nil ciao-lpdoc-libpath))) 

(defcustom ciao-lpdoc-buffer-name "LPdoc"
  "Basic name of the buffer running the auto-documenter inferior process."
  :group 'lpdoc
  :type 'string) 

(defvar ciao-lpdoc-prompt-emacs-hook nil
  "Things to do in emacs once the prompt is found in the LPdoc buffer.")

(defvar ciao-lpdoc-prompt-inferior-hook nil
  "Things to do in LPdoc once the prompt is found in the LPdoc buffer.")

(defvar ciao-lpdoc-prompt-marker-acc ""
  "Keep the last line written in ciao inferior buffer. It is used to search
for the prompt since prompt should be after a newline.")
(make-variable-buffer-local 'ciao-lpdoc-prompt-marker-acc)

(defvar update-version-comments 0 ; 0 means "uninitialized"
  "Keep the last line written in ciao inferior buffer. It is used to search
for the prompt since prompt should be after a newline.")
(make-variable-buffer-local 'update-version-comments)

;; ===========================================================================
;; Mode body
;; ===========================================================================

;; List of manuals for word-help (help on symbol under cursor). These
;; info files must be accessible from the paths in the environment
;; variable 'INFOPATH' (or in the emacs variable Info-default-directory-list).
;; 
;; Unfortunately, this is very brittle... (Stallman seems to have
;; plans for a better word-help in new versions of emacs)
(setq word-help-mode-alist
 (cons
  '("Ciao/Prolog"
    (
     ;; Indices currently in manuals: concept lib pred prop regtype decl usage
     ;; Later entries take precedece, so this is probably the right order!
     ("ciao.info" 
      "Global Index"
      "Concept Definition Index" 
      "Library/Module Definition Index" 
      "Predicate/Method Definition Index" 
      "Property Definition Index" 
      "Regular Type Definition Index" 
      "Declaration Definition Index" 
      ) 
     ; ("sicstus3" 
     ;  "Predicate Index" "Obj Index" "Concept Index")
     )
    (("[A-Za-z_]+" 0)
     ("[A-Za-z_][A-Za-z0-9_^/]+" 0))
    nil
    (("[A-Za-z_]+" 0))
    )
 (cons
  '("Ciao/Prolog/LPdoc Listener"
    (
     ;; Indices currently in manuals: concept lib pred prop regtype decl usage
     ;; Later entries take precedece, so this is the right order!
     ("ciao.info" 
      "Global Index"
      "Concept Definition Index" 
      "Library/Module Definition Index" 
      "Predicate/Method Definition Index" 
      "Property Definition Index" 
      "Regular Type Definition Index" 
      "Declaration Definition Index" 
      ) 
     )
    (("[A-Za-z_]+" 0)
     ("[A-Za-z_][A-Za-z0-9_^/]+" 0))
    nil
    (("[A-Za-z_]+" 0))
    )
  word-help-mode-alist)))

;; MH Changed to do it in current dir (so that slaves can see it, etc.!)
(defvar ciao-temp-file-counter 0)

(defun ciao-last-temp-code-file ()
  "Returns the name of a the last created temporary file in the
current dir (or creates one)."
  (if (eq ciao-last-temp-file nil)
      (setq ciao-last-temp-file (ciao-temp-code-file "."))
    ciao-last-temp-file))

(defun ciao-temp-code-file (from-dir)
  "Returns the name of a temporary file in dir given in argument."
  (concat (expand-file-name (concat from-dir "/")) 
	  ciao-temp-file-name 
	  (int-to-string ciao-temp-file-counter) "_" (make-temp-name "")))

;; Not really needed?
(defun ciao-new-temp-code-file (from-dir)
  "Builds new temporary file names in the current dir."
  (setq ciao-temp-file-counter (+ ciao-temp-file-counter 1))
  (ciao-temp-code-file from-dir))

(defun ciao-new-temp-code-dir (filename)
  "Builds new temporary dir names in lpdoc root dir."
  (setq ciao-temp-file-counter (+ ciao-temp-file-counter 1))
  (concat (expand-file-name ciao-lpdoc-wdir-root) "/lpdoc_" filename "_"
	  (int-to-string ciao-temp-file-counter) "_" (make-temp-name "")))

(defvar ciao-objects-lib-loaded nil "Stores whether objects library
has been loaded or not (see ciao-load-command).")

(defvar ciao-assrt-lib-loaded nil "Stores whether assertion library
has been loaded or not (see ciao-check-buffer-syntax).")

;;----------------------------------------------------------------------------
;; Font-lock support - regular expressions and matching
;;----------------------------------------------------------------------------

;; Just a bridge (for documentation and setting local binding)
;; but better than font-lock-fontify-buffer
(defun ciao-fontify-buffer ()
  "Undate (recompute) syntax-based highlighting (coloring)."
  (interactive)
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

(defvar ciao-predicate-directives
  '( "data" "dynamic" "multifile" "impl_defined" "meta_predicate"
     "discontiguous" "persistent")
  "Names of directives describing properties of predicates.")

(defvar ciao-module-directives
  '( "module" 
     "use_module" "ensure_loaded" "use_active_module" 
     "use_package" "include" 
     "export" "reexport" "import" "redefining" 
     "initialization" "on_abort" )
  "Names of directives describing properties of predicates.")

(defvar ciao-builtin-directives
  '( "new_declaration" "op" 
     "load_compilation_module" "add_sentence_trans" "add_term_trans"
     "add_clause_trans" "add_goal_trans"
     "set_prolog_flag" "push_prolog_flag" "pop_prolog_flag" )
  "Names of other directives.")

(defvar ciao-library-directives
  '( 
;; functions
     "function" 
;; argnames
     "argnames" 
;; make
     "make" 
;; ociao
     "virtual" "public" "inheritable" "implements" "inherit_class" "class"
     "use_class"
     )
  "Names of additional directives defined in the libraries.")
 
;; Also, 'ciao-user-directives' now customizable; see above in file.

;; Order is backwards
(defvar ciao-mode-font-lock-keywords 
  `(
    ;; scripts
    ((lambda (limit) 
       (ciao-font-lock-match limit "^#!" "^[ \t]*$"))
     . ciao-face-script-header)
    ;; comments /* */
    ((lambda (limit) 
       (ciao-font-lock-match limit "/\\*" "\\*/"))
     . 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-comment-variable-pitch
	'ciao-face-comment))
    ;; % comments starting a line
    ("^[ \t]*%.*$" . 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-comment-variable-pitch
	'ciao-face-comment))
    ;; LPdoc comments
    ;; lpdoc bug comments
    ((lambda (limit) 
       (ciao-font-lock-match limit
		     "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*bug\\>"
                     "[^\\\"]\"[ \t\n]*)[ \t\n]*\\.[ \t]*$"))
     . ciao-face-lpdoc-bug-comment)
    ;; lpdoc version comments (and other related directives)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*version(" 
	"[^\\\"]\"[ \t\n]*)[ \t\n]*\\.[ \t]*$"))
     . ciao-face-lpdoc-version-comment)
    ;; other lpdoc comments
    ;; "^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*\\(version\\(_maintenance\\)?\\|doinclude\\|hide\\|filetype\\|nodoc\\)\\>" 
    ;; These ones have a string in the second argument
    ((lambda (limit)
       (ciao-font-lock-match 
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)("
	"[^\\\"]\"[ \t\n]*)[ \t\n]*\\.[ \t]*$"))
     . 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment))
    ;; These ones do not have a string in the second argument
    ((lambda (limit)
       (ciao-font-lock-match 
	limit
	"^[ \t]*:-[ \t\n]+\\(comment\\|doc\\)([ \t\n]*\\(version_maintenance\\|doinclude\\|hide\\|filetype\\|nodoc\\)\\>"
	"[ \t\n]*)[ \t\n]*\\.[ \t]*$"))
     . 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment))
    ;; comment strings in assertions
    ("#[ \n]+\\(\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\"\\)" 
     1 
     ,(if ciao-faces-use-variable-pitch-in-comments
	  'ciao-face-lpdoc-comment-variable-pitch
	'ciao-face-lpdoc-comment))
    ;; lpdoc commands in comments
    ("@[^ \t\n{}@=<>]*{[^{}@]*}"
     0 ciao-face-lpdoc-command t)
    ("@\\([}{@]\\|\\([A-Za-z]+\\|[?!]\\)[ \t\n]\\)"
     0 ciao-face-lpdoc-command t)
    ((lambda (limit) 
       (ciao-font-lock-match limit "@include[^ {}@]*{" "[^}@]*}"))
     0 ciao-face-lpdoc-include t)
    ((lambda (limit) 
       (ciao-font-lock-match limit "@begin{verbatim}" "@end{verbatim}"))
     0 ciao-face-lpdoc-verbatim t)
    ("@\\(cite\\|ref\\|section\\|subsection\\){[^{}@]*}"
     0 ciao-face-lpdoc-crossref t)
    ;; Directives 
    ((lambda (limit)
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-builtin-directives t) "\\>")
        "^[ \t]*$\\|\\.$"))
     . ciao-face-builtin-directive)
    ((lambda (limit) 
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-predicate-directives t) "\\>")
        "^[ \t]*$\\|\\.$"))
      . ciao-face-predicate-directive)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit
	(concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-module-directives t) "\\>")
	"^[ \t]*$\\|\\.$"))
     . ciao-face-module-directive)
    ((lambda (limit) 
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" 
		(regexp-opt ciao-library-directives t) "\\>")
        "^[ \t]*$\\|\\.$"))
     . ciao-face-library-directive)
    ((lambda (limit) 
       (ciao-font-lock-match
        limit
        (concat "^[ \t]*:-[ \t\n]*" (regexp-opt ciao-user-directives t) "\\>")
        "^[ \t]*$\\|\\.$"))
     . ciao-face-user-directive)
    ;; Assertions
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "checked") (ciao-end-assrt-regexp)))
     . ciao-face-checked-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*checked("))
     . ciao-face-checked-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "true") (ciao-end-assrt-regexp)))
     . ciao-face-true-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*true("))
     . ciao-face-true-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "false") (ciao-end-assrt-regexp)))
     . ciao-face-false-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*false("))
     . ciao-face-false-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "trust") (ciao-end-assrt-regexp)))
     . ciao-face-trust-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*trust("))
     . ciao-face-trust-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp "check") (ciao-end-assrt-regexp)))
     . ciao-face-check-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "^[ \t\n]*check("))
     . ciao-face-check-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match
	limit (ciao-begin-assrt-regexp 
	       "\\(decl\\|pred\\|comp\\|calls\\|success\\)") 
	(ciao-end-assrt-regexp)))
     . ciao-face-check-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "prop") (ciao-end-assrt-regexp)))
     . ciao-face-prop-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "regtype") (ciao-end-assrt-regexp)))
     . ciao-face-regtype-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "entry") (ciao-end-assrt-regexp)))
     . ciao-face-entry-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match 
	limit (ciao-begin-assrt-regexp "modedef") (ciao-end-assrt-regexp)))
     . ciao-face-modedef-assrt)
    ((lambda (limit) 
       (ciao-font-lock-match-until-matching-sexp limit "\\<debug_message("))
     . ciao-face-debug-mess)
    ;; Clause heads
    ("^[a-z][a-zA-Z0-9_]*" . ciao-face-clauseheadname)
    ("^'\\([^']\\|''\\)*'" . ciao-face-clauseheadname) 
    ;; Strings
    ("\\(^\\|[^']\\)\\(\"\\([^\\\"]\\|\"\"\\|\\\\\\(.\\|\n\\)\\)*\"\\)"
     2 ciao-face-string)
    ;; quoted atoms
    ("\\(^\\|[^0-9]\\)\\('\\([^\n\\']\\|\\\\.\\|''\\)*'\\)"
     2 ciao-face-quoted-atom)
    ;; comments not starting a line
    ("[ \t]%.*$" . 
      ,(if ciao-faces-use-variable-pitch-in-comments
	   'ciao-face-comment-variable-pitch
	 'ciao-face-comment))
    ;; Characters 0'...
    ("0'\\(\\\\.\\|.\\)" . ciao-face-string)
    ;; Variables
    ("\\<\\([A-Z_][a-zA-Z0-9_]*\\)" 1 ciao-face-variable)
    ;; Concurrency ops
    ("\\([ \t]&&\\|[ \t]&>\\|[ \t]<&\\|[ \t]&\\|[ \t]@[ \t]\\)"
    ;; ("\\(&\\|&>\\|<&\\|@[^=<>]\\)"
     . ciao-face-concurrency-op) 
    ;; Cut
    ("!" . ciao-face-cut)
    ;; Declaration neck (somewhat of a warning --recognized ones
    ;; colored above)
    ("^[ \t]*:-" . ciao-face-lpdoc-bug-comment)
    ;; Necks
    ("\\(:-\\|-->\\)" . ciao-face-prompt)
    ))

(defun ciao-begin-assrt-regexp (identifier)
  (concat "^[ \t]*:-[ \t\n]*" identifier "[ \t\n]"))

;older
;"^[ \t]*:-[ \t]*\\(check\\)?[ \t]*\\(decl\\|pred\\|comp\\|calls\\|success\\) "

(defun ciao-end-assrt-regexp ()
  "[ \t]#[ \t\n]\\|^#[ \t\n]\\|\\.[ \t]*$")

(defun ciao-font-lock-match (limit beginexp endexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (if (not (search-forward-regexp endexp limit t))
	  nil
	(setq end (cdr (match-data)))
	(set-match-data (cons begin end))
	t
      ))))

;; Matches corresponding closing delimiter
(defun ciao-font-lock-match-until-matching-sexp (limit beginexp)
  (let ((begin 0) (end 0))
    (if (not (search-forward-regexp beginexp limit t))
	nil
      (setq begin (car (match-data)))
      (goto-char (- (car (cdr (match-data))) 1))
      (forward-list)
      (setq end (cons (point) nil))
      (set-match-data (cons begin end))
      t
      )))

(defvar ciao-inferior-font-lock-keywords
      `(
	("^\\([A-Z][a-zA-Z0-9_]*\\) = \\(.*\\)\\(,\\| \\?.*\\)$"
         (1 ciao-face-answer-var)               ;; Answer variable
         (2 ciao-face-answer-val)               ;; Answer value
         (3 ciao-face-prompt)                   ;; Prompt after answer
         )
	("^\\([ \t]+[0-9]+[ \t]+[0-9]+\\)\\(Call:\\).*$"
         (1 ciao-face-debug-redo)               ;; 
         (2 ciao-face-debug-call)               ;; 
         )
	(
	 ,ciao-any-prompt-pattern
	 ;; "^\\(\\(\\|[0-9]+ \\|ciaopp \\|| \\)\\?-\\)"
         . ciao-face-prompt)                    ;; Prompts
	("^yes$" . ciao-face-yes-answer)        ;; Answer
	("^no$" . ciao-face-no-answer)          ;; Answer
	("^Select[^:]*:" . ciao-face-ciaopp-option) ;; Preproc prompt
	("^{?ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^{SYNTAX ERROR.*$" . ciao-face-error-mess)  ;; Error messages
	("^\\*\\* here \\*\\*[ \t]*$" . ciao-face-error-mess)  ;; Error mes
	("^{?WARNING.*$" . ciao-face-warning-mess)  ;; Error messages
	("^{DEBUG.*$" . ciao-face-debug-mess)       ;; Error messages
	("^{?Note:.*$" . ciao-face-note-mess)       ;; Error messages
	("^{NOTE.*$" . ciao-face-note-mess)         ;; Error messages
        ("^\\({.*\\|}\\)" . ciao-face-other-mess)   ;; Error messages
;;        ("^\\*\\*\\* ---------.*\n^\\*\\*\\* .*\n\\*\\*\\* ---------.*$" 
        ("^\\*\\*\\* \\(---------\\|=========\\).*$" 
	 . ciao-face-highlight-code)              ;; LPdoc (1.9) messages
        ("^\\*\\*\\* .*$" . ciao-face-debug-call) ;; LPdoc (1.9) messages
	("^Ciao\\>.*$" . ciao-face-startup-mess);; Startup
        ; Recognizes a date at the end of the line (ciaopp still does it)
	("^(C) .* \\w\\w\\w \\w\\w\\w [1-3]?[0-9]\
 [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [A-Z][A-Z][A-Z] [1-2][0-9][0-9][0-9]$"
        . ciao-face-startup-mess)              ;; Startup, second line
;	("\\(^\\?- *[^{ ]\\|^| \\?- *\\).*\\.[ \t]*\n"
;	 . ciao-face-prompt) ;; Query doesn't work(?)
))

;;----------------------------------------------------------------------------
;; Font-lock support - (customizable) face definitions
;;----------------------------------------------------------------------------
;;  Used to have conceptual faces and then actual faces, but it was a
;;  nightmare to keep compatible between emacs and xemacs. For now,
;;  'key' definitions (the conceptual ones) made actual faces until we
;;  work out a portable fix.

;; Reminder of tty colors:
;; black, red, green, yellow, blue, magenta, cyan, white
;; (tty-color-translate color) approximates the color

;; Debugger
(defgroup ciao-highlighting-faces-debugger nil
  "Ciao faces for debugger."
  :tag "Ciao Debugger Faces" :group 'ciao-highlighting-faces)

;; This super-kludge of adding the unnecessary defvar is needed to 
(defvar ciao-face-debug-call 'ciao-face-debug-call)
(defface ciao-face-debug-call ;; ciao-face-blueish-block
  '((((type tty) (class color))
     (:background "blue" :foreground "white"))
    (((class color) (background dark))
     (:background "blue3"))
    (((class color) (background light))
     (:background "slate blue" :foreground "white"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:background "gray")))
  "Face to use when at call port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-exit 'ciao-face-debug-exit)
(defface ciao-face-debug-exit ;; ciao-face-greenish-block
  '((((type tty) (class color))
     (:background "green"))
    (((class color) (background light))
     (:background "green"))
    (((class color) (background dark))
     (:background "darkolivegreen"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at exit port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-fail 'ciao-face-debug-fail)
(defface ciao-face-debug-fail ;; ciao-face-reddish-block
  '((((type tty) (class color))
     (:background "red" :foreground "black"))
    (((class color) (background light))
     (:background "Firebrick" :foreground "White"))
    (((class color) (background dark))
     (:background "Firebrick" :foreground "White"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at fail port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-redo 'ciao-face-debug-redo)
(defface ciao-face-debug-redo ;; ciao-face-orangy-block
  '((((type tty) (class color))
     (:background "magenta" :foreground "black"))
    (((class color) (background light))
     (:background "orange"))
    (((class color) (background dark))
     (:background "orange" :foreground "black"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use when at redo port in source debugger."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-expansion 'ciao-face-debug-expansion)
(defface ciao-face-debug-expansion ;; ciao-face-yellowish-block
  '((((type tty) (class color)) 
     (:background "yellow" :foreground "black"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (((class color) (background dark))
     (:background "yellow" :foreground "black"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t)))
  "Face to use in source debugger when source literal not located."
  :group 'ciao-highlighting-faces-debugger)

(defvar ciao-face-debug-breakpoint 'ciao-face-debug-breakpoint)
(defface ciao-face-debug-breakpoint ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Red" :bold t))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (t (:inverse-video t :bold t)))
  "Face to use with breakpoints in source debugger."
  :group 'ciao-highlighting-faces-debugger)

;; Misc language stuff
(defgroup ciao-highlighting-faces-misc nil
  "Ciao faces for miscellanous language features."
  :tag "Ciao Misc Faces" :group 'ciao-highlighting-faces)

;; resolve an emacs / xemacs incompatibility
(defvar ciao-face-script-header 'ciao-face-script-header)
(defface ciao-face-script-header ;; ciao-face-forestgreen
  '((((type tty) (class color)) (:foreground "green" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "ForestGreen"))
    (t (:inverse-video t)))
  "Face to use for script headers."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-quoted-atom 'ciao-face-quoted-atom)
(defface ciao-face-quoted-atom ;; ciao-face-quoted-atom
  '((((type tty) (class color)) (:foreground "black"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "brown"))
    (((class color) (background dark)) (:foreground "Moccasin"))
    (t (:italic t)))
  "Face to use for quoted atoms."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-variable 'ciao-face-variable)
(defface ciao-face-variable ;; ciao-face-variable
  '((((type tty) (class color)) (:foreground "magenta" :bold t))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "goldenrod1"))
    (t (:italic t)))
  "Face to use for variables."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-string 'ciao-face-string)
(defface ciao-face-string ;; ciao-face-string
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Face to use for strings."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-comment 'ciao-face-comment)
(defface ciao-face-comment ;; ciao-face-comment
  '((((type tty) (class color)) (:foreground "red"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:bold t :italic t)))
  "Face to use for code comments using fixed pitch (double %)."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-comment-variable-pitch 'ciao-face-comment-variable-pitch)
(defface ciao-face-comment-variable-pitch 
  '((t (:inherit ciao-face-comment :family "helv")))
  "Face to use for code comments using variable pitch (single %)."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-clauseheadname 'ciao-face-clauseheadname)
(defface ciao-face-clauseheadname ;; ciao-face-blue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :bold t)))
  "Face to use for clause head functors."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-concurrency-op 'ciao-face-concurrency-op)
(defface ciao-face-concurrency-op ;; ciao-face-coral-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Coral" :bold t))
    (((class color) (background dark)) (:foreground "Coral" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for concurrency operators."
  :group 'ciao-highlighting-faces-misc)

(defvar ciao-face-cut 'ciao-face-cut)
(defface ciao-face-cut ;; ciao-face-royalblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "RoyalBlue"))
    (t (:inverse-video t)))
  "Face to use for cuts."
  :group 'ciao-highlighting-faces-misc)

;; LPdoc
(defgroup ciao-highlighting-faces-lpdoc nil
  "Ciao faces for documenter-specific assertions (comments, text
strings, commnds, etc.)."
  :tag "Ciao LPdoc Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-lpdoc-bug-comment 'ciao-face-lpdoc-bug-comment)
(defface ciao-face-lpdoc-bug-comment ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Red" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for LPdoc bug comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-version-comment 'ciao-face-lpdoc-version-comment)
(defface ciao-face-lpdoc-version-comment ;; ciao-face-comment
  '((((type tty) (class color)) (:foreground "red"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "chocolate1"))
    (t (:bold t :italic t)))
  "Face to use for LPdoc version comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-comment 'ciao-face-lpdoc-comment)
(defface ciao-face-lpdoc-comment ;; ciao-face-navyblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "NavyBlue"))
    (((class color) (background dark)) (:foreground "LightBlue"))
    (t (:inverse-video t)))
  "Face to use for LPdoc textual comments."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-comment-variable-pitch
  'ciao-face-lpdoc-comment-variable-pitch) 
(defface ciao-face-lpdoc-comment-variable-pitch 
  '((t (:inherit ciao-face-lpdoc-comment :family "helv")))
  "Face to use for LPdoc textual comments in variable pitch."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-verbatim 'ciao-face-lpdoc-verbatim)
(defface ciao-face-lpdoc-verbatim ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "NavyBlue" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for LPdoc verbatim text."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-include 'ciao-face-lpdoc-include)
(defface ciao-face-lpdoc-include ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "NavyBlue" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for LPdoc include commands."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-crossref 'ciao-face-lpdoc-crossref)
(defface ciao-face-lpdoc-crossref ;; ciao-face-golden
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod"))
    (t (:bold t :italic t)))
  "Face to use for LPdoc cross-references."
  :group 'ciao-highlighting-faces-lpdoc)

(defvar ciao-face-lpdoc-command 'ciao-face-lpdoc-command)
(defface ciao-face-lpdoc-command ;; ciao-face-royalblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "RoyalBlue"))
    (t (:inverse-video t)))
  "Face to use LPdoc commands inserted in documentation text."
  :group 'ciao-highlighting-faces-lpdoc)

;; Directives
(defgroup ciao-highlighting-faces-directive nil
  "Ciao faces for various directives (:- ...)."
  :tag "Ciao Directives Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-builtin-directive 'ciao-face-builtin-directive)
(defface ciao-face-builtin-directive ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Blue" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for the standard directives."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-predicate-directive 'ciao-face-predicate-directive)
(defface ciao-face-predicate-directive ;; ciao-face-royalblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "RoyalBlue"))
    (((class color) (background dark)) (:foreground "RoyalBlue"))
    (t (:inverse-video t)))
  "Face to use for the predicate-related directives."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-module-directive 'ciao-face-module-directive)
(defface ciao-face-module-directive ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "NavyBlue" :bold t))
    (((class color) (background dark)) (:foreground "Lavender" :bold t))
    (t (:inverse-video t :bold t))
     )
     "Face to use for the module-related directives."
     :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-library-directive 'ciao-face-library-directive)
(defface ciao-face-library-directive ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "NavyBlue" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for directives defined in the library."
  :group 'ciao-highlighting-faces-directive)

(defvar ciao-face-user-directive 'ciao-face-user-directive)
(defface ciao-face-user-directive ;; ciao-face-navyblue
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) (:foreground "NavyBlue"))
    (((class color) (background dark)) (:foreground "LightBlue"))
    (t (:inverse-video t)))
  "Face to use for directives defined by the user (see
   ciao-user-directives custom variable to add new ones)."
  :group 'ciao-highlighting-faces-directive)

;; Assertions
(defgroup ciao-highlighting-faces-assertions nil
  "Ciao faces for Ciao assertions."
  :tag "Ciao Assertions Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-checked-assrt 'ciao-face-checked-assrt)
(defface ciao-face-checked-assrt ;; ciao-face-darkgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "DarkGreen" :bold t))
    (((class color) (background dark)) (:foreground "LightGreen" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for checked assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-true-assrt 'ciao-face-true-assrt)
(defface ciao-face-true-assrt ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (background dark)) (:foreground "ForestGreen" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for true assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-false-assrt 'ciao-face-false-assrt)
(defface ciao-face-false-assrt ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Red" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for false assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-trust-assrt 'ciao-face-trust-assrt)
(defface ciao-face-trust-assrt ;; ciao-face-coral-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Coral" :bold t))
    (((class color) (background dark)) (:foreground "Coral" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for trust assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-entry-assrt 'ciao-face-entry-assrt)
(defface ciao-face-entry-assrt ;; ciao-face-brown-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Brown" :bold t))
    (((class color) (background dark)) (:foreground "Brown" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for entry assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-check-assrt 'ciao-face-check-assrt)
(defface ciao-face-check-assrt ;; ciao-face-navyblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "NavyBlue" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for check assertions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-prop-assrt 'ciao-face-prop-assrt)
(defface ciao-face-prop-assrt ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Blue" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for property definitions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-regtype-assrt 'ciao-face-regtype-assrt)
(defface ciao-face-regtype-assrt ;; ciao-face-mediumblue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "MediumBlue" :bold t))
    (((class color) (background dark)) (:foreground "SkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for regtype definitions."
  :group 'ciao-highlighting-faces-assertions)

(defvar ciao-face-modedef-assrt 'ciao-face-modedef-assrt)
(defface ciao-face-modedef-assrt ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (background dark)) (:foreground "ForestGreen" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for modedef definitions."
  :group 'ciao-highlighting-faces-assertions)

;; Top levels (Ciao, CiaoPP, LPdoc)
(defgroup ciao-highlighting-faces-toplevels nil
  "Ciao faces for the Ciao, CiaoPP, LPdoc and shell top levels."
  :tag "Ciao Top Levels Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-prompt 'ciao-face-prompt)
(defface ciao-face-prompt ;; ciao-face-coral-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Coral" :bold t))
    (((class color) (background dark)) (:foreground "Coral" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for prompts in top-level and shells."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-answer-var 'ciao-face-answer-var)
(defface ciao-face-answer-var ;; ciao-face-purple
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "Face to use for answer variables in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-answer-val 'ciao-face-answer-val)
(defface ciao-face-answer-val ;; ciao-face-blue-bold
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Blue" :bold t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for answer values in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-yes-answer 'ciao-face-yes-answer)
(defface ciao-face-yes-answer ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (background dark)) (:foreground "ForestGreen" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for yes answer in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-no-answer 'ciao-face-no-answer)
(defface ciao-face-no-answer ;; ciao-face-golden-bold
  '((((type tty) (class color)) (:foreground "red" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "DarkGoldenrod" :bold t))
    (((class color) (background dark)) (:foreground "LightGoldenrod" :bold t))
    (t (:bold t :italic t)))
  "Face to use for no answer in top level."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-ciaopp-option 'ciao-face-ciaopp-option)
(defface ciao-face-ciaopp-option ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "ForestGreen" :bold t))
    (((class color) (background dark)) (:foreground "ForestGreen" :bold t))
    (t (:inverse-video t :bold t)))
  "Face to use for CiaoPP option menus."
  :group 'ciao-highlighting-faces-toplevels)

(defvar ciao-face-startup-mess 'ciao-face-startup-mess)
(defface ciao-face-startup-mess ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) 
     (:foreground "ForestGreen" :bold t :family "helv" :height 1.1))
    (((class color) (background dark)) 
     (:foreground "ForestGreen" :bold t :family "helv" :height 1.1))
    (t (:inverse-video t :bold t)))
  "Face to use for system splash message."
  :group 'ciao-highlighting-faces-toplevels)

;; Messages
(defgroup ciao-highlighting-faces-messages nil
  "Ciao faces for various messages (errors, warnings, notes, etc.)."
  :tag "Ciao Messages Faces" :group 'ciao-highlighting-faces)

(defvar ciao-face-debug-mess 'ciao-face-debug-mess)
(defface ciao-face-debug-mess ;; ciao-face-forestgreen-bold
  '((((type tty) (class color)) (:foreground "green" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) 
     (:foreground "ForestGreen" :bold t :family "helv"))
    (((class color) (background dark)) 
     (:foreground "ForestGreen" :bold t :family "helv"))
    (t (:inverse-video t :bold t)))
  "Face to use for debug messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-error-mess 'ciao-face-error-mess)
(defface ciao-face-error-mess ;; ciao-face-warning
  '((((type tty) (class color)) (:foreground "red"))
    (((class color) (background light)) 
     (:foreground "Red" :bold t :family "helv"))
    (((class color) (background dark)) 
     (:foreground "Red" :bold t :family "helv"))
    (t (:inverse-video t :bold t)))
  "Face to use for error messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-warning-mess 'ciao-face-warning-mess)
(defface ciao-face-warning-mess ;; ciao-face-brown-bold
  '((((type tty) (class color)) (:foreground "magenta" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) 
     (:foreground "Brown" :bold t :family "helv"))
    (((class color) (background dark)) 
     (:foreground "Brown" :bold t :family "helv"))
    (t (:inverse-video t :bold t)))
  "Face to use for warning messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-note-mess 'ciao-face-note-mess)
(defface ciao-face-note-mess ;; ciao-face-brown
  '((((type tty) (class color)) (:foreground "cyan" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) 
     (:foreground "brown" :family "helv"))
    (((class color) (background dark)) 
     (:foreground "brown" :family "helv"))
    (t (:inverse-video t)))
  "Face to use for note messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-other-mess 'ciao-face-other-mess)
(defface ciao-face-other-mess ;; ciao-face-brown
  '((((type tty) (class color)) (:foreground "cyan" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray"))
    (((class grayscale) (background dark)) (:foreground "DimGray"))
    (((class color) (background light)) 
     (:foreground "brown" :family "helv"))
    (((class color) (background dark)) 
     (:foreground "brown" :family "helv"))
    (t (:inverse-video t)))
  "Face to use for other messages."
  :group 'ciao-highlighting-faces-messages)

(defvar ciao-face-highlight-code 'ciao-face-highlight-code)
(defface ciao-face-highlight-code ;; ciao-face-yellowish-block
  '((((type tty) (class color)) 
     (:background "yellow" :foreground "black"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (((class color) (background dark))
     (:background "yellow" :foreground "black"))
    (t (:inverse-video t)))
  "Face to use for highlighting code areas (e.g., when locating 
   the code area that an error message refers to)."
  :group 'ciao-highlighting-faces-messages)

;; ;; Just for testing -- but does not work after startup :-(
;; (defun ciao-dark-background ()
;;   (interactive)
;;   "Just for testing how Ciao faces show with dark background. Not
;; meant to be used normally."
;;   (if (boundp 'xemacs-logo)
;;       (progn
;; 	(set-face-background 'default "Black")
;; 	(set-face-foreground 'default "White"))
;;     (set-background-color "Black")
;;     (set-foreground-color "White")))
;; 
;; (defun ciao-light-background ()
;;   (interactive)
;;   "Just for testing how Ciao faces show with light background. Not
;; meant to be used normally."
;;   (if (boundp 'xemacs-logo)
;;       (progn
;; 	(set-face-background 'default "White")
;; 	(set-face-foreground 'default "Black"))
;;     (set-background-color "White")
;;     (set-foreground-color "Black")))

;;------------------------------------------------------------
;; Key and menu bindings + documentation sections
;; These nifty functions allow autodocumenting using lpdoc! MH
;;------------------------------------------------------------

(defvar ciao-mode-map (make-sparse-keymap))

(defvar ciao-inferior-mode-map nil)

(if ciao-inferior-mode-map
	     nil
	   ; HB: 930205: Use the "correct" function 'copy-keymap'
	   ; to copy a keymap.
           ;; Inherit the commands from comint.
	   (setq ciao-inferior-mode-map (copy-keymap
					 comint-mode-map)))

(defvar ciao-documented-commands nil
 "Stores the list of commands which will appear in the documentation
  for the main mode, preceded by section comments.")

(defun ciao-define-key (map binding function)
  "A call to define-key, but we store stuff in our own format, which
  is used later to generate the documentation."
  (setq ciao-documented-commands
	(cons (list binding function) ciao-documented-commands))
  (define-key map binding function))

;; (defun ciao-report-defined-key (map function &optional comment)
;;   "Store the info for an already defined key. Used to generate the
;; documentation. Optional comment overrides the function's default
;; comment."
;;   (setq ciao-documented-commands
;; 	(cons (list (substitute-command-keys 
;; 		     (concat "\\[" (symbol-name function) "]"))
;; 	       (or comment function)) ciao-documented-commands)))

(defun ciao-report-defined-key (map function &optional comment binding)
  "Store the info for an already defined key. Used to generate the
documentation. Optional comment overrides the function's default
comment. Optional binding sets and reports a different binding."
  (let ((desc 
	 (where-is-internal function map t t)
	 )) ;; used: overriding-local-map 
    (setq ciao-documented-commands
	  (cons (list (if (or desc binding)
			  (if (stringp binding)
				binding
			    (key-description desc))
			(princ (format "M-x %s" function)))
		      (or comment function))
		ciao-documented-commands)))
  (if (stringp binding)
      (define-key map binding function)
    nil)
  )

(defun ciao-documentation-section (sec-title sec-intro)
  "We store a section title and intro, used later to generate documentation."
  (setq ciao-documented-commands
	(cons (list 'section sec-title sec-intro) ciao-documented-commands)))

(defun ciao-documentation-paragraph (paragraph-contents)
  "We store paragraph, used later to generate documentation."
  (setq ciao-documented-commands
	(cons (list 'paragraph paragraph-contents) ciao-documented-commands)))

;; Should start with a section!
(defun ciao-mode-commands (map inferior-map)

  (ciao-documentation-section
    "Syntax coloring and syntax-based editing"
    
    "Syntax-based highlighting (coloring) of code is provided
automatically when opening Ciao/Prolog files.  This includes also the
assertions used by the preprocessor and the documentation strings used
by the Ciao auto-documenter, @apl{lpdoc}.  The mode should be set to
Ciao/Prolog and the Ciao mode menus should appear on the menu bar. The
colors and fonts used can be changed through the @index{customize}
options in the help menu (see @ref{Customization}).

During editing this coloring may be refreshed by calling the
appropriate function (see below).

Limited syntax-based auto-indentation and auto-fill of code and
comments is also provided. Syntax highlighting and coloring is also
available for the error and warning messages produced by the top
level, preprocessor, and auto-documenter, and, in general, for the
output produced by these tools.

@noindent
Commands:
  ")

  (ciao-define-key map "\C-ch" 'ciao-fontify-buffer)
  (ciao-define-key map "\t" 'ciao-indent-line)

  (ciao-documentation-section 
   "Getting on-line help" 

   "The following commands are useful for getting on-line help. This
is done by accessing the @apl{info} version of the Ciao manuals or the
@apl{emacs} built-in help strings. Note also that the @apl{info}
standard @tt{search} command (generally bound to @key{s}) can be used
inside @apl{info} buffers to search for a given string.
   ")

  (ciao-define-key map "\C-c\C-i" 'ciao-help-on-current-symbol)
  (ciao-define-key map "\C-c/"    'ciao-complete-current-symbol)
  (ciao-define-key map "\C-c\C-m" 'ciao-goto-ciao-manuals)
  (ciao-define-key map "\C-hm"    'ciao-describe-mode)

  (ciao-documentation-section 
   "Loading and compiling programs" 

   "These commands allow @index{loading programs}, @index{creating
executables}, etc. by issuing the appropriate commands to a Ciao/Prolog top
level shell, running in its own buffer as a subprocess. See @ref{The
interactive top-level shell} for details. The following commands implement
the communication with the Ciao/Prolog top level:
   ")

  (ciao-define-key map "\C-ct" 'run-ciao-toplevel)
  (ciao-define-key map "\C-cl" 'ciao-load-buffer)

  (ciao-define-key map "\C-cx" 'ciao-make-exec)
  (ciao-define-key map "\C-co" 'ciao-make-po)
  (ciao-define-key map "\C-ca" 'ciao-make-activemod)

  (ciao-define-key map "\C-cs" 'ciao-set-main-filename)
  (ciao-define-key map "\C-cL" 'ciao-load-from-main-module)

  (ciao-define-key map "\C-cq" 'ciao-set-query)
;; (ciao-define-key map "???" 'ciao-clear-query)
  (ciao-define-key map "\C-cQ" 'ciao-load-query)

  (ciao-documentation-section
   "Commands available in toplevel and preprocessor buffers"
    
   "The interactive top level and the preprocessor both are typically
run in an iteractive buffer, in which it is possible to communicate
with them in the same way as if they had been started from a standard
shell. These interactive buffers run in the so-called @em{Ciao/Prolog
inferior mode}. This is a particular version of the standard emacs
shell package (comint) and thus all the commands typically available
when running shells inside emacs also work in these buffers.  In
addition, many of the commands and key bindings available in buffers
containing Ciao source code are also available in these interactive
buffers, when applicable.  The Ciao/Prolog-specific commands available
include:
    ")

  ;; Not such a good idea: completion is better (see
  ;; 'comint-dynamic-complete above) 
  ;; (ciao-define-key map "\t" 'ciao-indent-line)
  (ciao-define-key inferior-map "\C-c\C-i" 'ciao-help-on-current-symbol)
  (ciao-define-key inferior-map "\C-c/"    'ciao-complete-current-symbol)
  (ciao-define-key inferior-map "\C-c`"    'ciao-find-last-run-errors)
  (ciao-define-key inferior-map "\C-ce" 'ciao-unmark-last-run-errors)
  (ciao-define-key inferior-map "\C-cq" 'ciao-set-query)
;; (ciao-define-key map "???" 'ciao-clear-query)
  (ciao-define-key inferior-map "\C-cQ" 'ciao-load-query)
  (ciao-define-key inferior-map "\C-c\C-v" 'ciao-show-preprocessor-output)
  (ciao-define-key inferior-map "\C-cv" 'ciao-report-mode-version)

  (ciao-documentation-paragraph
   (substitute-command-keys "@noindent The following are some of the
commands from the comint shell package which may be specially useful
(type \\<ciao-mode-map> @tt{\\[describe-mode]} while in a Ciao
interactive buffer for a complete list of commands):"))

  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-previous-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-next-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-previous-matching-input)
  (ciao-report-defined-key ciao-inferior-mode-map 
 			   'comint-dynamic-complete
			   "Dynamically find completion of the item at
point. Note that this completion command refers generally to filenames
 (rather than, e.g., predicate names, as in the previous functions)."
			   "\t")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-dynamic-list-filename-completions
                           "List all (filename) completions of the
item at point."
			   "\M-?")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-send-input
			   "Return at any point of the a line at the
end of a buffer sends that line as input. Return not at end copies the
rest of the current line to the end of the buffer and sends it as
input.")
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-delchar-or-maybe-eof)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-kill-input)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'backward-kill-word)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-interrupt-subjob)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-stop-subjob)
  (ciao-report-defined-key ciao-inferior-mode-map
			   'comint-quit-subjob)

  (ciao-documentation-section 
   "Locating errors and checking the syntax of assertions" 

   "These commands allow locating quickly the point in the source code
corresponding to errors flagged by the compiler or preprocessor as
well as performing several syntactic checks of assertions:
@cindex{locating errors} 
   ")

  (ciao-define-key map "\C-c`"    'ciao-find-last-run-errors)
  (ciao-define-key map "\C-ce" 'ciao-unmark-last-run-errors)
  (ciao-define-key map "\C-cE"    'ciao-check-buffer-syntax)

  (ciao-documentation-section 
   "Commands which help typing in programs" 

   "The following commands are intended to help in the process of
writing programs: @cindex{script header, inserting automatically}
   ")

  (ciao-define-key map "\C-cIS" 'ciao-insert-script-header)

  (ciao-documentation-section
   "Debugging programs"

   "These commands allow marking modules for @index{debugging} by issuing
the appropiate commands to a Ciao/Prolog top level shell, running in its
own buffer as a subprocess. There are two differents types of debugging:
traditional Prolog debugging (using the @concept{byrd-box model} and
@concept{spy-points}) and @index{source-level debugging} (same as
traditional debugging plus source tracing and
@concept{breakpoints}). @cindex{debugging, source-level} In order to use
@index{breakpoints}, source debugging must be on. The following commands
implement comunication with the Ciao/Prolog top level:
   ")

  (ciao-define-key map "\C-cd" 'ciao-debug-buffer)
  (ciao-define-key map "\C-cm" 'ciao-select-debug-mode)
  (ciao-define-key map "\C-c\M-m" 'ciao-select-buffers-for-debug)

  (ciao-define-key map "\C-cSb" 'ciao-debug-breakon)
  (ciao-define-key map "\C-cSv" 'ciao-debug-breakoff)
  (ciao-define-key map "\C-cSn" 'ciao-debug-all-breakoff)
  (ciao-define-key map "\C-cSl" 'ciao-debug-display-breakpt)
  (ciao-define-key map "\C-cSr" 'ciao-debug-uncolor-all-breakpt)

  (ciao-define-key map "\C-cSt" 'ciao-enable-trace)
  (ciao-define-key map "\C-cSd" 'ciao-enable-debug)

  (ciao-define-key map "\C-cr" 'ciao-load-region)
  (ciao-define-key map "\C-cp" 'ciao-load-predicate)

  (ciao-documentation-section 
   "Preprocessing programs" 

   "These commands allow @index{preprocessing programs} with
@apl{ciaopp}, the @index{Ciao preprocessor}.

@include{/home/clip/Systems/ciaopp-0.8/doc/readmes/README.lpdoc}

See the preprocessor manual for details. The following commands
implement the communication with the Ciao preprocessor:
  ")

  (ciao-define-key map "\C-cM" 'ciao-preprocess-buffer-menu)
  (ciao-define-key map "\C-cP" 'ciao-preprocess-buffer)
  (ciao-define-key map "\C-cT" 'ciao-check-types-modes)
  (ciao-define-key map "\C-c\C-p" 'ciao-set-ciaopp-output-pred)
  (ciao-define-key map "\C-c\C-f" 'ciao-set-ciaopp-output-full)
  (ciao-define-key map "\C-c\C-x" 'ciao-set-ciaopp-output-none)
  (ciao-define-key map "\C-c\C-v" 'ciao-show-preprocessor-output)
  (ciao-define-key map "\C-cV" 'ciao-preprocess-buffer-and-show-output)
  (ciao-define-key map "\C-c\C-r" 'run-ciao-preprocessor)

  (ciao-documentation-section 
   "Version control" 

   "The following commands can be used to carry out a simple but
effective form of @concept{version control} by keeping a @concept{log
of changes} on a file or a group of related files. Interestingly, this
log is kept in a format that is understood by @apl{lpdoc}, the Ciao
documenter @cite{lpdoc-tr}. As a result, if these version comments are
present, then @apl{lpdoc} will be able to automatically assign up to
date version numbers to the manuals that it generates. This way it is
always possible to identify to which version of the software a manual
corresponds. Also, @apl{lpdoc} can create automatically sections
describing the changes made since previous versions, which are
extracted from the comments in the changelog entries.

The main effect of these commands is to automatically associate the
following information to a set of changes performed in the file and/or
in a set of related files:

@begin{itemize}

@item a @index{version number} (such as, e.g., @tt{1.2}, where @tt{1}
is the @concept{major version number} and @tt{2} is the @concept{minor
version number}),

@item a @concept{patch number} (such as, e.g., the @tt{4} in
@tt{1.2#4}), 

@item a @concept{time stamp} (such as, e.g.,
@tt{1998/12/14,17:20*28+MET}),

@item the author of the change, @cindex{change, author} and

@item a comment explaining the change. @cindex{change, comment}
@end{itemize}

The @concept{version numbering} used can be local to a single file or
common to a number of related files. A simple version numbering policy
is implemented: when a relevant change is made, the user typically
inserts a @concept{changelog entry} for it, using the appropriate
command (or selecting the corresponding option when prompted while
saving a file). This will cause the @em{patch number} for the file (or
for the whole system that the file is part of) to be incremented
automatically and the corresponding machine-readable comment to be
inserted in the file. Major and minor version numbers can also be
changed, but this is always invoked by hand (see below).

The changelog entry is written in the form of a @decl{comment/2}
declaration.  As mentioned before, the advantage of using this kind of
changelog entries is that these declarations can be processed by the
@apl{lpdoc} automatic documenter (see the @apl{lpdoc} reference
manual @cite{lpdoc-tr} or the @lib{assertions} library documentation
for more details on these declarations). 

Whether the user is asked or not to introduce such changelog entries,
and how the patch and version numbers should be increased is
controlled by the presence in the file of a @pred{comment/2}
declaration of the type:

@tt{:- comment(version_maintenance,<type>).}

@noindent (note that this requires including the @lib{assertions}
library in the source file).  These declarations themselves are also
typically introduced automatically when using this mode (see below).

The version maintenance mode can also be set alternatively by
inserting a comment such as:

@begin{verbatim}
%% Local Variables: 
%% mode: ciao
%% update-version-comments: \"off\"
%% End:
@end{verbatim}

The lines above instruct emacs to put the buffer visiting the file in
@concept{emacs Ciao/Prolog mode} and to turn version maintenance off.
Setting the version maintenance mode in this way has the disadvantage
that @apl{lpdoc}, the auto-documenter, and other related tools will
not be aware of the type of version maintenance being performed (the
lines above are comments for Prolog). However, this can be useful in
fact for setting the @index{version maintenance mode for packages} and
other files meant for inclusion in other files, since that way the
settings will not affect the file in which the package is included.

The following commands implement the version control support:
   ")

  (ciao-define-key map "\C-x\C-s" 'ciao-save-buffer)
  (ciao-define-key map "\C-c\C-s" 'ciao-add-comment-and-save)
  (ciao-define-key map "\C-cn"    'ciao-new-version)
  (ciao-define-key map "\C-c\C-n" 'ciao-fetch-next-changelog-entry)

  (ciao-documentation-section 
   "Generating program documentation"

   "These commands provide some bindings and facilities for generating
and viewing the documentation corresponding to the current buffer. The
documentation is generated in a temporary directory, which is created
automatically.  This is quite useful while modifying the documentation
for a file, in order to check the output that will be produced,
whithout having to set up a documentation directory by hand or to
regenerate a large manual of which the file may be a part. 
   ")

  (ciao-define-key map "\C-cDB" 'ciao-gen-buffer-doc)
  (ciao-define-key map "\C-cDF" 'ciao-set-lpdoc-docformat)
  (ciao-define-key map "\C-cDS" 'ciao-visit-lpdoc-settings)
  (ciao-define-key map "\C-cDG" 'ciao-gen-doc)
  (ciao-define-key map "\C-cDV" 'ciao-start-viewer)
  (ciao-define-key map "\C-cDW" 'ciao-set-lpdoc-wdir-root)

  (ciao-documentation-section 
   "Setting top level preprocessor and documenter executables"
 
   "These commands allow @index{changing the executables used} when
starting a Prolog top-level, the preprocessor, or the auto-documenter. They
also allow changing the arguments that these executables take, and changing
the path where the libraries reside. In the case of the top-level and
preprocessor, this should be done only by users which understand the
implications, but it is very useful if several versions of Ciao/Prolog or
the preprocessor are available in the system. All these settings can be
changed through the @index{customize} options in the help menu (see
@ref{Customization}).
   ")

  (ciao-define-key map "\C-cSC"    'ciao-set-ciao-system)
  (ciao-define-key map "\C-cS\C-c" 'ciao-set-ciao-system-args)
  (ciao-define-key map "\C-cSP"    'ciao-set-ciaopp-system)
  (ciao-define-key map "\C-cS\C-p" 'ciao-set-ciaopp-system-args)
  (ciao-define-key map "\C-cSL"    'ciao-set-library-path)
  (ciao-define-key map "\C-cSD"    'ciao-set-lpdoc-system)
  (ciao-define-key map "\C-cS\C-d" 'ciao-set-lpdoc-system-args)
  (ciao-define-key map "\C-cS\C-l" 'ciao-set-lpdoc-libpath)

  (ciao-documentation-section 
   "Other commands" 
   "Some other commands which are active in the Ciao/Prolog mode:
   ") 

  (ciao-define-key map "\C-c\C-l" 'ciao-recenter-last-ciao-buffer)

  (ciao-documentation-section 
   "Traditional Prolog Mode Commands" 

   "These commands provide some bindings and facilities for loading
programs, which are present in emacs Prolog modes of other Prolog
systems (e.g., SICStus). This is useful mainly if the Ciao/Prolog
emacs mode is used with such Prolog systems.  Note that these commands
(@pred{compile/1} and @pred{consult/1}) are deprecated in Ciao (due to
the more advanced, separate compilation model in Ciao) and their use
in the Ciao top-level is not recommended.
   ")

  (ciao-define-key map "\C-cK" 'ciao-compile-buffer)
  (ciao-define-key map "\C-ck" 'ciao-compile-region)
  (ciao-define-key map "\C-c\C-k" 'ciao-compile-predicate)
  (ciao-define-key map "\C-cC" 'ciao-consult-buffer)
  (ciao-define-key map "\C-cc" 'ciao-consult-region)
  (ciao-define-key map "\C-c\C-c" 'ciao-consult-predicate)

  (ciao-documentation-section 
   "Coexistence with other Prolog interfaces" 

   "As mentioned previously, the Ciao/Prolog @apl{emacs} interface can
also be used to work with other Prolog or CLP systems. Also, the
Ciao/Prolog @apl{emacs} interface (@em{mode}) can coexist with other
Prolog-related @apl{emacs} interfaces (@em{modes}) @cindex{emacs mode,
loading several} (such as, e.g., the @apl{SICStus} Prolog
interface). Only one of the interfaces can be active at a time for a
given buffer (i.e., for each given file opened inside @apl{emacs}). In
order the change a buffer to a given interface, move the cursor to
that buffer and type @tt{M-x ...-mode} (e.g., for the Ciao/Prolog
mode, @tt{M-x ciao-mode}).

If several Prolog-related @apl{emacs} interfaces are loaded, then
typically the @em{last} one to be loaded takes precedence, in the
sense that this will be the interface in which @apl{emacs} will be set
when opening files which have a @tt{.pl} ending (this depends a bit on
how things are set up in your @tt{.emacs} file).")

  (ciao-documentation-section 
   "Getting the Ciao/Prolog mode version" 
   "@cindex{Ciao/Prolog mode version}")

  (ciao-define-key map "\C-cv" 'ciao-report-mode-version)

  )

(ciao-mode-commands ciao-mode-map ciao-inferior-mode-map)

(defconst ciao-mode-menus-sys
  (list "CiaoSys"
;;      "----"
;;      "TOP-LEVEL/COMPILER"
     ["(Re)Start ciao top level"                 run-ciao-toplevel t]
     ["(Re)Load buffer into top level"           ciao-load-buffer  t]
     ["(Re)Load main and related modules"        ciao-load-from-main-module t]
     ["Make executable from buffer as main"      ciao-make-exec t]
     "----"
     ["Go to (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     "----"
     (list "Query and main file"
	   ["Set default query"                  ciao-set-query t]
	   ["Call default query"                 ciao-load-query t]            
	   ["Clear default query"                ciao-clear-query t]
	   ["(Un)Set main module"                ciao-set-main-filename t]
	   )
     ["Check buffer syntax (incl. assertions)"   ciao-check-buffer-syntax t]
     "----"
     ["Update syntax-based coloring"        ciao-fontify-buffer t]
     ["Insert script header"                     ciao-insert-script-header t]
     "----"
;;      "CHANGELOG / VERSION CONTROL"
     ["Insert changelog entry/increase patch #" ciao-add-comment-and-save t]
     ["Increase version number"              ciao-new-version t]
     ["Go to next changelog entry"           ciao-fetch-next-changelog-entry t]
     "----"
     ["Make object file (.po) from buffer"       ciao-make-po t]
     ["Make active module from buffer"           ciao-make-activemod t]
;; Deprecated and not recommended:
;;      "----"
;;      (list "TRADITIONAL PROLOG COMMANDS (also for SICStus)"
;;            ["Compile buffer"    ciao-compile-buffer  t]
;;            ["Compile region"    ciao-compile-region  t]
;;            ["Compile predicate" ciao-compile-predicate t]
;;            ["Consult buffer"    ciao-consult-buffer  t]
;;            ["Consult region"    ciao-consult-region  t]
;;            ["Consult predicate" ciao-consult-predicate t]
;;      )
     )
  "Menus for Ciao/Prolog mode.")

(defconst ciao-mode-menus-debug
  (list "CiaoDbg" 
;;      "----"
;;      "TOP-LEVEL/DEBUGGER"
     ["(Un)Debug buffer source"               ciao-debug-buffer t]
     "----"
     ["Select debug mode"                     ciao-select-debug-mode t]
     ["Select multiple buffers for debug"     ciao-select-buffers-for-debug t]
     (list "Breakpoints"
	   ["Set breakpoint on current literal pred symb" ciao-debug-breakon t]
 	   ["Remove breakpoint from current literal"  ciao-debug-breakoff t]
 	   ["Remove all breakpoints"             ciao-debug-all-breakoff t]
 	   ["Redisplay breakpoints"              ciao-debug-display-breakpt t]
     )
     ["Toggle debug mode (jump to bkp or spypt)" ciao-enable-debug t]
     ["Toggle trace mode"                        ciao-enable-trace t]
     "----"
     ["(Re)Load region (for debug)"              ciao-load-region  t]
     ["(Re)Load predicate (for debug)"           ciao-load-predicate t]
   )
  "Ciao/Prolog debugging menus.")

(defconst ciao-mode-menus-customize
  (list "CiaoOpts"
     ["Customize all Ciao environment settings" 
                                       (customize-group 'ciao-environment)] 
     "----"
     ["Customize all Ciao system environment settings" 
                                               (customize-group 'ciao) t]
     ["Set Ciao/Prolog toplevel executable"    ciao-set-ciao-system t]
     ["Set Ciao/Prolog toplevel args"          ciao-set-ciao-system-args t]
     "----"
     ["Set Ciao library path"                  ciao-set-library-path t]
     "----"
     ["Customize all CiaoPP environment settings" (customize-group 'ciaopp) t]
     ["Set Ciao Preprocessor executable"       ciao-set-ciaopp-system t]
     ["Set Ciao Preprocessor executable args"  ciao-set-ciaopp-system-args t]
     "----"
     ["Customize all LPdoc environment settings" (customize-group 'lpdoc) t]
     ["Set LPdoc executable"                   ciao-set-lpdoc-system t]
     ["Set LPdoc executable args"              ciao-set-lpdoc-system-args t]
     ["Set LPdoc root working directory"       ciao-set-lpdoc-wdir-root t]
     ["Set LPdoc library path"                 ciao-set-lpdoc-libpath t]
     "----"
     ["Customize all Ciao colors/faces"        (customize-group 
						'ciao-highlighting-faces) t]
  )
  "Customization menus for Ciao/Prolog mode.")

(defconst ciao-mode-menus-help
  (list "CiaoHelp" 
     ["Go to manual page for symbol under cursor" ciao-help-on-current-symbol]
;; MH Not backwards compatible...
;;      :help "Go to manual page describing the symbol under the cursor" ]
;; Also, these had ( ) 
     ["Complete symbol under cursor"        ciao-complete-current-symbol t]
     ["Ciao system manual" ciao-goto-ciao-manual t]
     ["Ciao preprocessor manual" ciao-goto-ciaopp-manual t]
     ["LPdoc automatic documenter manual" ciao-goto-lpdoc-manual t]
     ["Ciao manuals area in info index" ciao-goto-ciao-manuals t]
     ["List all key bindings" ciao-describe-mode t]
     "----"
     ["Ciao environment (mode) version" ciao-report-mode-version t]
   )
  "Help menu for Ciao/Prolog mode.")


;; MH Tool bar stuff (21.1 onwards)
;; Made a function, so that it is done when maps and menus are active.
;; - This one is for adding to the default toolbar (but modifies
;;   others :-( ).
;; (defun ciao-setup-tool-bar () 
;;   (set (make-local-variable 'tool-bar-map) 
;;        (if (display-graphic-p)
;; 	   (progn 
;; 	     (tool-bar-setup) ;; from tool-bar.el
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-help-on-current-symbol "left_arrow" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-exec "ciaoexe" ciao-mode-map)
;; 	     tool-bar-map))))
;; - This one is for an independent tool bar (we add all stuff by hand):
;; *** 
(defun ciao-setup-tool-bar () 
  (set (make-local-variable 'tool-bar-map) 
       (if (display-graphic-p) 
	   (let ((tool-bar-map (make-sparse-keymap)))
;; General stuff
	     (tool-bar-add-item "icons/ciaopl" 'find-file 'find-file 
              :help "Open or create a (Prolog) file") 
	     (tool-bar-add-item-from-menu 'dired "open")
	     (tool-bar-add-item-from-menu 'kill-this-buffer "close")
	     (tool-bar-add-item-from-menu 'save-buffer "save" nil
			       :visible '(or buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
	     (tool-bar-add-item-from-menu 'write-file "saveas" nil
			       :visible '(or buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
	     (tool-bar-add-item-from-menu 'undo "undo" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 'kill-region "cut" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 'menu-bar-kill-ring-save "copy")
	     (tool-bar-add-item-from-menu 'yank "paste" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 
                               'nonincremental-search-forward "search")
	     (tool-bar-add-item-from-menu 'print-buffer "print")
;; Ciao-specific stuff
	     (tool-bar-add-item-from-menu 
	      'run-ciao-toplevel "icons/ciao" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-fontify-buffer "icons/ciaorehighlight" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-load-buffer "icons/ciaoload" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-find-last-run-errors "jump_to" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-unmark-last-run-errors "icons/clear" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-check-buffer-syntax "icons/ciaoasr" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-check-types-modes    "icons/checkassertions" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-preprocess-buffer-menu 
	      "icons/ciaopreprocask" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-preprocess-buffer    "icons/ciaopreproc" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-preprocess-buffer-and-show-output
	      "icons/ciaopreprocsee" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-debug-buffer "icons/ciaodebug" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-gen-buffer-doc "icons/lpdoc" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-start-viewer "icons/lpdocview" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-make-exec "icons/ciaoexeout" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-insert-script-header "icons/ciaoscrt" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-po "icons/ciaopo" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-exec "icons/ciaoitf" ciao-mode-map)
;;	     (tool-bar-add-item "ciaomanuals" 
	     (tool-bar-add-item "icons/manuals" 
              'ciao-goto-ciao-manuals 'ciao-goto-ciao-manuals 
	      :help "Go to area containing the Ciao system manuals")
	     (tool-bar-add-item-from-menu 
	      'ciao-help-on-current-symbol "icons/wordhelp" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-complete-current-symbol "icons/complete" ciao-mode-map)
	     (tool-bar-add-item "preferences" 
				(lambda ()
				  (interactive)
				  (customize-group 'ciao-environment))
				'ciao-customize
 	      :help "Edit  (customize) preferences for Ciao, CIaoPP, LPdoc")
	     tool-bar-map))))

;; ;; *** 
;; ;; - This one is for playing around with compatibility with xemacs.
;; (defun ciao-setup-tool-bar () 
;;   (make-local-variable 'tool-bar-map) 
;;   (if (not (display-graphic-p))
;;       (set 'tool-bar-map nil)
;;     (set 'tool-bar-map
;; 	 (let ((tool-bar-map (make-sparse-keymap)))
;; 	   (ciao-tool-bar-add-item "icons/ciaopl" 'find-file 'find-file 
;; 			      :help "Open or create a (Prolog) file") 
;; 	   (ciao-tool-bar-add-item-from-menu 'dired "open")
;; 	   (ciao-tool-bar-add-item-from-menu 'kill-this-buffer "close")
;; 	   (ciao-tool-bar-add-item-from-menu 'save-buffer "save" nil
;; 				:visible '(or buffer-file-name
;; 					      (not (eq 'special
;; 						       (get major-mode
;; 							    'mode-class)))))
;; 	   (ciao-tool-bar-add-item-from-menu 
;; 	    'run-ciao-toplevel "icons/ciao" ciao-mode-map)
;; 	   (ciao-tool-bar-add-item "icons/manuals" 
;; 		      'ciao-goto-ciao-manuals 'ciao-goto-ciao-manuals 
;; 		      :help "Go to area containing the Ciao system manuals")
;; 	   (ciao-tool-bar-add-item "preferences" 
;; 			      (lambda ()
;; 				(interactive)
;; 				(customize-group 'ciao-environment))
;; 			      'ciao-customize
;;  	      :help "Edit  (customize) preferences for Ciao, CIaoPP, LPdoc")
;; 	   tool-bar-map))))
;; 
;; ;; *** Just for testing 
;; (defun ciao-tool-bar-add-item-from-menu (comm icon &optional map &rest props)
;;   (unless map
;;     (setq map global-map))
;;   (tool-bar-add-item-from-menu comm icon map props))
;; 
;; ;; *** Just for testing 
;; (defun ciao-tool-bar-add-item (icon def key &rest props)
;;   (tool-bar-add-item icon def key props))

(defconst ciao-mode-ciaopp-menus
  (list "CiaoPP"
;;     "CIAO PREPROCESSOR (in development)"
     "Note: CiaoPP required (in development)"
     "----"
     ["Preprocess buffer (choosing options)"   ciao-preprocess-buffer-menu t]
     ["Preprocess buffer (w/previous options)" ciao-preprocess-buffer t]
     ["Check types and modes"  ciao-check-types-modes t]
     ["Go to (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Preprocess buffer (w/previous options) and show output"  
                                      ciao-preprocess-buffer-and-show-output t]
     ["Output only predicate-level analysis info" ciao-set-ciaopp-output-pred t]
     ["Output literal- and pred-level analysis info" ciao-set-ciaopp-output-full t]
     ["Do not output analysis info" ciao-set-ciaopp-output-none t]
     ["Start ciao preprocessor"                run-ciao-preprocessor t]
     )
  "Menus for CiaoPP mode.")

(defconst ciao-mode-lpdoc-menus
  (list "LPdoc"
;;      "----"
;;      "GENERATE/VIEW DOCUMENTATION"
     ["Generate documentation for buffer"        ciao-gen-buffer-doc t]
     ["View documentation in selected format"    ciao-start-viewer t]
     ["Change default doc format/visualizer"     ciao-set-lpdoc-docformat t]
     ["Goto (next) preproc/compiler error msg"   ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     ["Visit(/create) SETTINGS file"             ciao-visit-lpdoc-settings t]
     ["Generate documentation"                   ciao-gen-doc t]
     )
  "Menus for LPdoc mode.")

(defconst ciao-inferior-mode-menus
;;  (list "Ciao/Prolog"
  (list "Ciao"
     ["Update syntax-based coloring"        ciao-fontify-buffer t]
     "----"
;;     "ERRORS"
     ["Locate (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error marks in buffers"            ciao-unmark-last-run-errors t]
     "----"
;;     "COMPILER/TOP-LEVEL/DEBUGGER"
     ["Set query as default"               ciao-set-query t]
     ["Clear default query"                ciao-clear-query t]
     ["Load default query"                 ciao-load-query t]
     ["Start ciao top level"               run-ciao-toplevel t]
     "----"
;;     "PREPROCESSOR (in development)"
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Start ciao preprocessor"                run-ciao-preprocessor t]
     )
  "Menus for Ciao/Prolog (inferior) mode.")

(defun ciao-setup-inferior-tool-bar () 
  (set (make-local-variable 'tool-bar-map) 
       (if (display-graphic-p) 
	   (let ((tool-bar-map (make-sparse-keymap)))
;; General entries, with a Ciao icon
	     (tool-bar-add-item "icons/ciaopl" 'find-file 'find-file 
              :help "Open or create a (Prolog) file") 
;; General entries
	     (tool-bar-add-item-from-menu 'dired "open")
	     (tool-bar-add-item-from-menu 'kill-this-buffer "close")
;; 	     (tool-bar-add-item-from-menu 'save-buffer "save" nil
;; 			       :visible '(or buffer-file-name
;; 					     (not (eq 'special
;; 						      (get major-mode
;; 							   'mode-class)))))
	     (tool-bar-add-item-from-menu 'write-file "saveas" nil
			       :visible '(or buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
	     (tool-bar-add-item-from-menu 'undo "undo" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 'kill-region "cut" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 'menu-bar-kill-ring-save "copy")
	     (tool-bar-add-item-from-menu 'yank "paste" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 
                               'nonincremental-search-forward "search")
;; 	     (tool-bar-add-item-from-menu 'print-buffer "print")
;; Ciao-specific entries
	     (tool-bar-add-item-from-menu 
	      'run-ciao-toplevel "icons/ciao" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-fontify-buffer "icons/ciaorehighlight" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-help-on-current-symbol "icons/wordhelp" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-complete-current-symbol "icons/complete" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-load-buffer "icons/ciaoload" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-find-last-run-errors "jump_to" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-unmark-last-run-errors "icons/clear" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-set-query "icons/ciaostorequery" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-load-query "icons/ciaoprompt" ciao-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-clear-query "icons/ciaoclearquery" ciao-mode-map)
	     (tool-bar-add-item "icons/stop" 
              'comint-interrupt-subjob 'comint-interrupt-subjob
              :help "Interrupt toplevel") 
	     (tool-bar-add-item "left_arrow" 
              'comint-previous-input 'comint-previous-input
              :help "Insert previous inputs at prompt") 
	     (tool-bar-add-item "right_arrow" 
              'comint-next-input 'comint-next-input
              :help "Insert later inputs at prompt") 
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-check-buffer-syntax "icons/ciaoasr" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-debug-buffer "icons/ciaodebug" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-gen-buffer-doc "icons/new" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-start-viewer "icons/lpdocview" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-exec "icons/ciaoexeout" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-insert-script-header "icons/ciaoscrt" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-po "icons/ciaopo" ciao-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-exec "icons/ciaoitf" ciao-mode-map)
;;	     (tool-bar-add-item "icons/ciaomanuals" 
	     (tool-bar-add-item "icons/manuals" 
              'ciao-goto-ciao-manuals 'ciao-goto-ciao-manuals 
	      :help "Go to area containing the Ciao system manuals")
	     (tool-bar-add-item "preferences" 
				(lambda ()
				  (interactive)
				  (customize-group 'ciao-environment))
				'ciao-customize
 	      :help "Edit  (customize) preferences for Ciao, CIaoPP, LPdoc")
	     tool-bar-map))))

;;------------------------------------------------------------
;; Syntax and movement
;;------------------------------------------------------------

(defvar ciao-mode-syntax-table nil)
(if ciao-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?_ "w" table) ; word constituent
    (modify-syntax-entry ?\\ "." table) ; punctuation
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
;;  1 means CHAR is the start of a two-char comment start sequence.
;;  2 means CHAR is the second character of such a sequence.
;;  3 means CHAR is the start of a two-char comment end sequence.
;;  4 means CHAR is the second character of such a sequence.
    (modify-syntax-entry ?/ "." table) ; punctuation
    (modify-syntax-entry ?* "." table) ; punctuation
    (modify-syntax-entry ?+ "." table) ; punctuation
    (modify-syntax-entry ?- "." table) ; punctuation
    (modify-syntax-entry ?= "." table) ; punctuation
    (modify-syntax-entry ?% "<" table) ; comment starter
    (modify-syntax-entry ?\n ">" table); comment ender
    (modify-syntax-entry ?\^m ">" table); ; comment ender
    (modify-syntax-entry ?< "." table) ; punctuation
    (modify-syntax-entry ?> "." table) ; punctuation
    (modify-syntax-entry ?\' "\"" table) ; escape
    (setq ciao-mode-syntax-table table)))

(defvar ciao-mode-abbrev-table nil)
(define-abbrev-table 'ciao-mode-abbrev-table ())

(defun ciao-mode-variables ()
  (setq local-abbrev-table ciao-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^%%\\|^$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ciao-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
;; Obsolete since before 19.5
;;   (make-local-variable 'comment-indent-hook)
;;   (setq comment-indent-hook 'ciao-comment-indent)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'ciao-comment-indent)
;; Using make-variable-buffer-local above
;;  (make-local-variable 'update-version-comments)
;;  (setq update-version-comments 0) ; 0 means "uninitialized"
  ;; Source debugger variables
  (make-local-variable 'ciao-debug-last-frame) 
  (setq ciao-debug-last-frame nil)
  (make-local-variable 'ciao-debug-delete-prompt-marker)
  (setq ciao-debug-delete-prompt-marker (make-marker))
  )

(defun ciao-indent-line (&optional whole-exp)
  "Indent current line as Ciao/Prolog code.
With argument, indent any additional lines of the same clause
rigidly along with this one."
  (interactive "p")
  (let ((indent (ciao-indent-level))
	(pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")

    (if (zerop (- indent (current-column)))
	nil
      (delete-region beg (point))
      (indent-to indent))

    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

;; JA 890605
(defun ciao-indent-level ()
  "Compute Ciao/Prolog indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((looking-at "%%%") 0)		   ;Large comment starts
     ((looking-at "%[^%]") comment-column) ;Small comment starts
     ((bobp) 0)				   ;Beginning of buffer
     ((looking-at "\n")                    ;a new fresh line
      (ciao-indent-for-new-clause))
     (t                                    ;indent existing clause
      (forward-line -1)
      (ciao-indent-for-new-clause)))))


;; JA 890601
(defun ciao-search-for-prev-goal ()
  "Search for the most recent Ciao/Prolog symbol (in head or in body)."
  (while (and (not (bobp)) (or (looking-at "%[^%]") (looking-at "\n")))
    (forward-line -1)
    (skip-chars-forward " \t")))

;; JA 890601
(defun ciao-indent-for-new-clause ()
  "Find column for a new goal."
  (ciao-search-for-prev-goal)
  (skip-chars-forward " \t")
  (let ((prevcol (current-column)))
    (ciao-end-of-clause)
    (forward-char -1)
    (cond ((bobp) 0)
	  ((looking-at "[.]") 0)
	  ((zerop prevcol) tab-width)
	  ((looking-at "[\[{(;]")
	   (max tab-width (+ ciao-indent-width (ciao-column-of-um-lparen))))
	  ((looking-at "[,>]") (ciao-column-of-prev-term))
	  (t (ciao-column-of-um-lparen)))))

;; JA 890601
(defun ciao-column-of-prev-term ()
  (beginning-of-line)
  (skip-chars-forward " \t\[{(;")
  (current-column))

;; JA 890601
(defun ciao-column-of-um-lparen ()
  (let ((pbal 0))
    (while (and (>= pbal 0)
		(or (> (current-column) 0)
		    (looking-at "[ \t]")))
      (cond ((looking-at "[\]})]")
	     (setq pbal (1+ pbal))
	     (forward-char -1))
	    ((looking-at "[\[{(]")
	     (setq pbal (1- pbal))
	     (forward-char -1))
	    ((looking-at "'")
	     (search-backward "'" nil t)
	     (forward-char -1))
	    ((looking-at "\"")
	     (search-backward "\"" nil t)
	     (forward-char -1))
	    (t (forward-char -1)))))
  (forward-char 1)  ;; Reset buffer pointer to prev column
  (current-column))

(defun ciao-end-of-clause ()
  "Go to end of clause in this line."
  (beginning-of-line)
  (let* ((eolpos (save-excursion (end-of-line) (point))))
    (if (re-search-forward comment-start-skip eolpos 'move)
	(goto-char (match-beginning 0)))
    (skip-chars-backward " \t")))

(defun ciao-comment-indent ()
  "Compute Ciao/Prolog comment indentation."
  (cond ((looking-at "%%%") 0)
	((looking-at "%%") (ciao-indent-level))
	(t
	 (save-excursion
	       (skip-chars-backward " \t")
	       (max (1+ (current-column)) ;Insert one space at least
		    comment-column)))))


;;------------------------------------------------------------
;; Help (locating manuals, calling word-help, etc.)
;;------------------------------------------------------------

(defun ciao-help-on-current-symbol () 

  "Find help for the symbol (e.g., predicate, directive, declaration, type,
etc.) that is currently under the cursor. Opens a (hopefully) relevant part
of the Ciao manuals in @apl{info} mode. Requires that the Ciao manuals in
@apl{info} format be installed and accessible to @apl{emacs} (i.e., they
should appear somewhere in the info directory when typing @tt{M-x
info}). It also requires @file{word-help.el}, which is provided with
Ciao. Refer to the installation instructions if this is not the case."

  (interactive) 
  (call-interactively 'word-help))

(defun ciao-complete-current-symbol () 

  "Find a completion for the symbol (e.g., predicate, directive,
declaration, type, etc.) that is currently under the cursor. Uses for
completion the contents of the indices of the Ciao manuals. Same
requirements as for finding help for the symbol."

  (interactive) 
  (call-interactively 'word-help-complete))

(defun ciao-goto-ciao-manuals () 
  "Go to the part of the info directory containing the Ciao manuals."
  (interactive) 

  (ciao-locate-manual-in-info-dir "Ciao system"))

(defun ciao-goto-ciao-manual () 
  "Go to the part of the info directory containing the Ciao manual."
  (interactive) 
  (ciao-goto-particular-manual "ciao:"))

(defun ciao-goto-ciaopp-manual () 
  "Go to the part of the info directory containing the Ciao
preprocessor manual." 
  (interactive) 
  (ciao-goto-particular-manual "ciaopp:"))

(defun ciao-goto-lpdoc-manual () 
  "Go to the part of the info directory containing the lpdoc
(automatic documenter) manual." 
  (interactive) 
  (ciao-goto-particular-manual "lpdoc:"))

(defun ciao-locate-manual-in-info-dir (text) 
  "Locate a manual entry in the info dir"
  (info) 
  (Info-directory)
  (if (search-forward text nil t) 
      (recenter 0)
    (error (concat "Could not find " text " manual in info dir"))))

(defun ciao-goto-particular-manual (manual) 
  "Go to a particular manual."
  (ciao-locate-manual-in-info-dir manual)
  (if (not (boundp 'xemacs-logo))
      (Info-follow-nearest-node)
    (backward-char 3)
    (Info-follow-nearest-node (point))
    ))

(defun ciao-describe-mode () 
  "Show a short description of the Ciao/Prolog emacs mode, including all key
bindings." 
  (interactive) 
  (describe-mode))

;;------------------------------------------------------------
;; On-line comments and changelog management
;;------------------------------------------------------------

(defun ciao-new-version () 

  "Force a move to a new major/minor version number (the user will be
prompted for the new numbers). Only applicable if using
directory-based version maintenance. Note that otherwise it suffices
with introducing a changelog entry in the file and changing its
version number by hand."

  (interactive)
  (ciao-handle-version-control-option)
  (if (or (string= (ciao-version-maint-type) "off") 
	  (string= (ciao-version-maint-type) "on"))
      (error "Only supported if using version directory")
    (if (not (string= 
	      (read-string "Change major/minor version (y/n) ?" "n")
	      "y"))
	nil
     
      (message "Will first delete current Version/Patch files")
      (sleep-for 2)
      (delete-file (concat (ciao-version-maint-type) "/GlobalVersion"))
      (delete-file (concat (ciao-version-maint-type) "/GlobalPatch"))
      (message "Current Version/Patch files deleted")
      (sleep-for 2)
      (ciao-update-version (ciao-version-maint-type))
      )
    )
  )

(defun ciao-save-buffer ()

  "This is the standard @apl{emacs} command that saves a buffer by
writing the contents into the associated @tt{.pl} file.  However, in
Ciao/Prolog mode this command can be set to ask the user before saving
whether to introduce a changelog entry documenting the changes performed.

If the buffer does not already contain a comment specifying the
@concept{type of version control} to be performed, and before saving
the buffer, the Ciao/Prolog mode prompts the user to choose among the
following options:

   @begin{description} 

   @item{@key{q}} Turn off prompting for the introduction of changelog
entries for now. @apl{emacs} will not ask again while the buffer is
loaded, but it will ask again next time you load the buffer.

   @item{@key{n}} Turn off version control for this file. A version
control comment such as:

@tt{:- comment(version_maintenance,off).}

@noindent is added to the buffer and the file is saved. @apl{emacs}
will not perform any version control on this file until the line above
is removed or modified (i.e., from now on \\<ciao-mode-map>
\\[ciao-save-buffer] simply saves the buffer).

   @item{@key{y}} Turn version control on for this file. 

   @end{description}

   If @key{y} is selected, then the system prompts again regarding how
and where the version and patch number information is to be
maintained. The following options are available:

   @begin{description}

   @item{@tt{on}} All version control information will be contained
within this file. When saving a buffer \\<ciao-mode-map>
(\\[ciao-save-buffer]) emacs will ask if a changelog entry should be
added to the file before saving. If a comment is entered by the user,
a new patch number is assigned to it and the comment is added to the
file. This patch number will be the one that follows the most recent
changelog entry already in the file. This is obviously useful when
maintaining version numbers individually for each file.

   @item{@tt{<directory_name>}} Global version control will be
performed coherently on several files. When saving a buffer
\\<ciao-mode-map> (\\[ciao-save-buffer]) emacs will ask if a changelog
entry should be added to the file before saving. If a comment is
given, the global patch number (which will be kept in the file:
@tt{<directory_name>/GlobalPatch}) is atomically incremented and the
changelog entry is added to the current file, associated to that patch
number. Also, a small entry is added to a file
@tt{<directory_name>/GlobalChangeLog} which points to the current
file. This allows inspecting all changes sequentially by visiting all
the files where the changes were made (see \\<ciao-mode-map> 
\\[ciao-fetch-next-changelog-entry]). This is obviously useful when
maintaining a single thread of version and patch numbers for a set of
files.

   @item{@tt{off}} Turns off version control: \\[ciao-save-buffer] then simply
   saves the file as usual. 

   @end{description}

@bf{Some useful tips:} 

@begin{itemize}

@item If a changelog entry is in fact introduced, the cursor is left
at the point in the file where the comment was inserted and the mark
is left at the original file point. This allows inspecting (and
possibly modifying) the changelog entry, and then returning to the
original point in the file by simply typing
\\[exchange-point-and-mark].

@item @cindex{moving changelog entries} The first changelog entry is
entered by default at the end of the buffer. Later, the changelog
entries can be moved anywhere else in the file. New changelog entries
are always inserted just above the first changelog entry which appears
in the file.

@item The comments in changelog entries can be edited at any time. 

@item If a changelog entry is moved to another file, and version
numbers are shared by several files through a directory, the
corresponding file pointer in the
@tt{<directory_name>/GlobalChangeLog} file needs to be changed also,
for the entry to be locatable later using
\\[ciao-fetch-next-changelog-entry].

@end{itemize}

"
  (interactive)
  (ciao-save-buffer-option nil))

(defun ciao-add-comment-and-save ()

  "Same as \\<ciao-mode-map> \\[ciao-save-buffer] except that it
forces prompting for inclusion of a changelog entry even if the buffer
is unmodified."

  (interactive)
  (ciao-save-buffer-option t))

(defun ciao-save-buffer-option (save-option)
  "Same as above, but allows forcing save / minor version change."
  (interactive)
  (if (and (eq (buffer-modified-p) nil) (eq save-option nil))
      ;; will do nothing -- just for printing the usual message
      (save-buffer) 
    (ciao-handle-version-control-option)
    (if (and (string= (ciao-version-maint-type) "off") (eq save-option nil))
	;; just normal save
	(save-buffer)
      (if (and (eq save-option t) 
	       (not (string= (ciao-version-maint-type) "off")))
	  ;; no need to ask
	  (ciao-update-version (ciao-version-maint-type))
	(if (string= (ciao-version-maint-type) "off")
	    ;; will do nothing -- just for printing the usual message
	    (save-buffer) 
	  ;; ask 
	  (if (not (string= 
		    (read-string "Insert changelog entry (y/n) ?" "n")
		    "y"))
	      (save-buffer);; normal save and return
	    ;; update version and save
	    (ciao-update-version (ciao-version-maint-type))
	    ))))))

(defun ciao-update-version (version-dir) 
  "Inserts a changelog entry (comment and patch number change). If a
  comment is in fact introduced, the buffer is left at the file point
  of the entry for inspection and the mark is left at the original
  file point for easy return."  
  (interactive)
  (let (original-point 
	original-buffer 
	version-file 
	version-major 
	version-minor
	no-previous-version
	patch-file
	patch-buffer
	patch-number
	keep-version
	comment
	month day year time
	old-version-control
	change-file
	tmp-point)
  (setq original-point (point))
  (beginning-of-buffer)
  (cond
   ((not (or (string= version-dir "on") (string= version-dir "off")))
    ;; Previous version is in external file - get it
    ;; For locking, we are taking advantage of emacs file locking by
    ;; modifying the buffer right away.
    (setq original-buffer (current-buffer))
    (setq version-file (concat version-dir "/GlobalVersion"))
    (if (file-readable-p version-file)
	(progn 
	  (find-file version-file)
	  (beginning-of-buffer)
	  (setq tmp-point (point))
	  (search-forward-regexp "\\.")
	  (backward-char 1)
	  ;; kill-region modifies and sets lock...
	  (setq version-major
		(buffer-substring-no-properties tmp-point (point)))
	  (forward-char 1)
	  (setq tmp-point (point))
	  (end-of-line)
	  (setq version-minor
		(buffer-substring-no-properties tmp-point (point)))
	  (setq no-previous-version nil)
	  (kill-buffer (current-buffer))
	  )
      (if (string= 
	   (read-string 
	    (concat "Could not find " version-file ", create ?") "y")
	   "y")
	  (progn
	    (setq no-previous-version t))
	(error "No version file")))

    (setq patch-file (concat version-dir "/GlobalPatch"))
    (if no-previous-version
	nil
      ;; There is a previous version
      (if (file-readable-p patch-file)
	  ;; Readable patch file: get patch number
	  (progn 
	    (switch-to-buffer original-buffer) ;; So that relative paths work!
	    (find-file patch-file)
	    (beginning-of-buffer)
	    (setq patch-buffer (current-buffer))
	    (setq tmp-point (point))
	    (end-of-line)
	    (setq patch-number 
		  (buffer-substring-no-properties tmp-point (point)))
	    (kill-buffer (current-buffer))
	    )
	;; No patch file: new patch number
	(setq patch-number "-1")))

    (switch-to-buffer original-buffer)
    )
   ((search-forward-regexp "^[ \t]*:-[ \t\n]*comment([ \t\n]*version(" nil t)
    ;; A previous version exists in the file: get it
    (setq tmp-point (point))
    (search-forward-regexp "\\*")
    (backward-char 1)
    (setq version-major
	  (buffer-substring-no-properties tmp-point (point)))
    (forward-char 1)
    (setq tmp-point (point))
    (search-forward-regexp "\\+")
    (backward-char 1)
    (setq version-minor
	  (buffer-substring-no-properties tmp-point (point)))
    (forward-char 1)
    (setq tmp-point (point))
    (search-forward-regexp "[ \t\n]*,")
    (backward-char 1)
    (setq patch-number 
	  (buffer-substring-no-properties tmp-point (point)))
    (setq no-previous-version nil)
    )
   (t
    ;; No previous version exists: set it to 0.1+-1
    (setq no-previous-version t)
    )
   )

  (if no-previous-version
      (progn 
	(setq keep-version "n")
	(setq version-major "0")
	(setq version-minor "1")
	)
       (setq keep-version "y")
    )
	
  ;; If we keep the version or no comment
  (if (string= keep-version "y")
        ;; Version and patch number stay as they are
	nil
    ;; Else, get new version
    (setq version-major
	  (read-string "Major version ? " version-major))
    (setq version-minor
	  (read-string "Minor version ? " version-minor))
    ;; and reset patch number
    (setq patch-number "-1"))
   
  (setq comment (read-string (concat 
			      "Type a comment for new version "
			      version-major "." 
			      version-minor "#" 
			      (int-to-string 
			       (+ (string-to-int patch-number) 1))
			      ":"
			      ) 			     
			     ""))

  (if (string= comment "")
      nil
    ;; Increment patch number (will be 0 if new version)
    (setq patch-number (int-to-string (+ (string-to-int patch-number) 1))))

  ;; Hey, why not set them right here
  (setq month (format-time-string "%m"))
  (setq day   (format-time-string "%d"))
  (setq year  (format-time-string "%Y"))
  (setq time  (format-time-string "%H:%M*%S+'%Z'"))

  ;; If version came from changelog file in a directory, update the
  ;; version files 
  (if (or (string= version-dir "on") (string= comment ""))
      nil

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file version-file)
    (beginning-of-buffer)
    (setq tmp-point (point))
    (end-of-line)
    (delete-region tmp-point (point))
    (insert-string (concat version-major "." version-minor))
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file patch-file)
    (beginning-of-buffer)
    (setq tmp-point (point))
    (end-of-line)
    (delete-region tmp-point (point))
    (beginning-of-buffer)
    (insert-string patch-number)
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (setq change-file (concat version-dir "/GlobalChangeLog"))
    (if (file-readable-p change-file)
	  (find-file change-file)
      (find-file change-file)
      (beginning-of-buffer)
;;    Sets buffer in Ciao mode: necessary for bindings!  
      (insert-string "\n:- module(_,_,[assertions]).\n\n")
      (ciao-insert-version-control-off-comment) 
      ;;    This one would be visible by a Prolog program (not needed)
      ;;     (insert-string "\n:- comment(version_maintenance,off).\n\n")
      )
    (beginning-of-buffer)
    (ciao-insert-version-comment 
     version-major version-minor patch-number month day year time 
     (file-relative-name (buffer-file-name original-buffer)))
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))
    (switch-to-buffer original-buffer)
    )

  (if (string= comment "")
      ;; If user gave no input comments, do nothing
      (progn 
	(message "Blank comment -- no version change")
	(if (string= version-dir "on")
	    nil
	  (set-mark original-point)
	  (goto-char original-point)
          (save-buffer))
	)
    ;; Else, insert new version
    ;; in current buffer.
    ;; Position ourselves
    (ciao-goto-first-version-comment)
    ;; We are positioned: insert new comment
    (ciao-insert-version-comment 
     version-major version-minor patch-number month day year time 
     (concat comment "\n    (" user-full-name ")") )
    (fill-paragraph nil)
    (set-mark original-point)
    (save-buffer)
    )
  ))

(defun ciao-insert-version-comment 
  (version-major version-minor patch-number month day year time comment)
  "Insert a Ciao changelog entry in file at current point."
  (insert-string (concat 
		  ":- comment(version(" version-major "*" version-minor "+"
		      patch-number "," year "/" month "/" day ","
		      time "),\n   \"" comment "\").\n\n"))
  (search-backward-regexp "^[ \t]*:-[ \t\n]*comment([ \t\n]*version(")
  )

(defun ciao-goto-first-version-comment ()
  "Position ourselves at first changelog entry if it exists"
  (beginning-of-buffer)
  ;; If previous version exists
  (if (search-forward-regexp "^[ \t]*:-[ \t\n]*comment([ \t\n]*version(" nil t)
      (beginning-of-line)
    ;; If no previous version exists
;;     (beginning-of-buffer)
;;     (if (search-forward-regexp "^[ \t]*:-[ \t\n]*module(" nil t) t t)
;;     (ciao-next-blank-line-or-eof)
    (goto-char (point-max))))

(defun ciao-insert-version-control-off-comment ()
  (insert-string (concat
		  "\n%% Local Variables: \n"
		  "%% mode: CIAO\n"
		  "%% update-version-comments: \"off\"\n"
		  "%% End:\n\n")))

(defun ciao-insert-assertions-package-reminder ()
  (insert-string 
   (concat
    "\n"
    "%% *** Delete this comment after reading: it is only a reminder! ***\n"
    "%% \n" 
    "%% The \"assertions\" library needs to be included in order to support\n"
    "%% \":- comment(...,...).\" declarations such as below, i.e., insert: \n"
    "%% \n" 
    "%% :- module(_,_,[assertions]).\n" 
    "%% \n" 
    "%% At the beginning of the file:\n" 
    "%% The following version comment(s) can be moved elsewhere in the \n"
    "%% file. Subsequent version comments will always be placed above \n"
    "%% the last one inserted.\n\n"
    )))

;; (defun ciao-next-blank-line-or-eof ()
;;   (if (search-forward-regexp "^[ \t]*$" nil t)
;;       t
;;     nil))

(defun ciao-fetch-next-changelog-entry () 

   "When a unique version numbering is being maintained across several
files, this command allows inspecting all changes sequentially by
visiting all the files in which the changes were made:

    @begin{itemize}

    @item If in a source file, find the next changelog entry in the
source file, open in another window the corresponding
@file{GlobalChangeLog} file, and position the cursor at the
corresponding entry. This allows browsing the previous and following
changes made, which may perhaps reside in other files in the system.

   @item If in a @file{GlobalChangeLog} file, look for the next entry
in the file, and open in another window the source file in which the
corresponding comment resides, positioning the corresponding comment
at the top of the screen. This allows going through a section of the
@file{GlobalChangeLog} file checking all the corresponding comments in
the different files in which they occur.

    @end{itemize}

"

  (interactive)
  (let ((mbeg 0) (mend 0) original-buffer (version nil))
    (setq original-buffer (current-buffer))
    (if (not (search-forward-regexp 
	      "^[ \t]*:-[ \t]*comment([ \t\n]*version("
	      nil t))
	(message "No (more) changelog entries found.")
      (setq mbeg (match-end 0))
      (recenter 0)
      (goto-char mbeg)
      (search-forward-regexp ")[ \t\n]*,")
      (setq mend (- (match-beginning 0) 1))
      (goto-char mend)
      (setq version (buffer-substring-no-properties mbeg mend))
      (if (string-match "GlobalChangeLog" (buffer-name))
	  ;; It is a changelog buffer: find matches in files
	  (progn
	    (search-forward "\"")
	    (setq mbeg (match-end 0))
	    (goto-char mbeg)
	    (search-forward "\"")
	    (setq mend (match-beginning 0))
	    (find-file-other-window (buffer-substring-no-properties mbeg mend))
	    (beginning-of-buffer)
	    (search-forward version)
	    (beginning-of-line)
	    (recenter 0)
	    (switch-to-buffer-other-window original-buffer))
	;; It is a normal buffer: find entry in changelog buffer
	(if (or (string= (ciao-version-maint-type) "on") 
		(string= (ciao-version-maint-type) "off"))
	    (error "No GlobalChangeLog file is associated with this file")
	  (find-file-other-window 
	   (concat (ciao-version-maint-type) "/GlobalChangeLog"))
	  (ciao-mode-nocheck) 
	  ;; set buffer to ciao mode so that bindings are active!
	  (beginning-of-buffer)
	  (search-forward version)
	  (beginning-of-line)
	  (recenter 0)
	  (switch-to-buffer-other-window original-buffer))
	))))

(defun ciao-handle-version-control-option ()
  "Look and see if there is a local variable designating whether
  version control should be performed."
  (interactive)
  (save-excursion
    (let ((option nil) (option-dir nil))
      (cond
       ((not (string= (ciao-version-maint-type) nil))
	;; local var already present: just return
	;; (message (concat "Local var found;value: "
	;;  (ciao-version-maint-type)))
	)
       (t 
	;; no local var: ask for it
	(setq option 
	      (read-string 
	       "Turn on changelog prompting on this file (y/n/q) ?" "q"))
	(if (string= option "q")
	    (setq update-version-comments "off")
	  (goto-char (point-max))
	  (cond
	   ((string= option "n")
	    ;; do not maintain control
	    (ciao-insert-version-control-off-comment)
	    (message "Off - see comments inserted at end of file")
	    (setq update-version-comments "off"))
	   (t 
	    ;; maintain control - normal file
	    (setq option-dir
		  (read-file-name
		   "Name of directory with version file (ret = this file) ?" 
		   "" "on" nil "on"))
	    ;; MR Added to avoid the bug when having control version in a
	    ;; directory which doesn't exist.
	    (if (string= option-dir "on")
		t
	      ;; Make sure the directory exists. If it doesn't exist then
	      ;; create the directory
	      (if (file-directory-p option-dir)
		  t
		(make-directory option-dir)))
	    
	    (ciao-insert-assertions-package-reminder)
	    (insert-string 
	     (concat
	      "\n:- comment(version_maintenance," 
	      (if (or (equal option-dir "on") (equal option-dir "off"))
		  option-dir
		(concat "dir('" option-dir "')" ))
	      ").\n\n"))
	    (message "On - see comments inserted at end of file")
	    (setq update-version-comments option-dir))
	   )))))))

(defun ciao-version-maint-type ()
  (interactive)
  (if (not (eq update-version-comments 0))
	update-version-comments
    (save-excursion
      (beginning-of-buffer)
      (if (search-forward-regexp 
         "^[ \t]*:-[ \t\n]*comment([ \t\n]*version_maintenance[ \t\n]*,[ \t]*" 
	   nil t)
	  (let ((begin 0))
	    (search-forward-regexp "dir([ \t\n]*'*" nil t)
	    (setq begin (point))
	    (search-forward-regexp "'*[ \t]*)" nil t)
	    (goto-char (match-beginning 0))
	    (setq update-version-comments 
		  (buffer-substring-no-properties begin (point)))
	    (message (concat "DIR: " update-version-comments))
	    )
	(setq update-version-comments nil)
	update-version-comments
        ))))

(defvar ciao-mode-version-control-saving nil)

(defun ciao-mode-version-control ()
  (interactive)
  (if (and (string= (file-name-nondirectory (buffer-file-name))
	            "ciao.el.body")
	   (not ciao-mode-version-control-saving)
	   )
      (progn
	(save-excursion
	  (set-buffer (find-file-noselect "CiaoMode.pl"))
 	  (set-buffer-modified-p t)
	  (ciao-save-buffer)
	  (kill-buffer (current-buffer)))
;; To keep dependencies: touch ciao.el.body afterwards
        (setq ciao-mode-version-control-saving t)
	(sleep-for 1)
	(set-buffer-modified-p t)
	(save-buffer (current-buffer))
        (setq ciao-mode-version-control-saving nil)
	)))

(defun ciao-mode-end-version-control ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (eq filename nil))
	(if (and (string= (file-name-nondirectory filename)
		      "ciao.el.body")
		 (get-buffer "CiaoMode.pl"))
	    (kill-buffer "CiaoMode.pl"))))) 
         
(add-hook 'after-save-hook 'ciao-mode-version-control) 
(add-hook 'kill-buffer-hook 'ciao-mode-end-version-control) 

;;------------------------------------------------------------
;; Splash
;;------------------------------------------------------------

(defun ciao-startup ()
  "Like \\<ciao-mode-map> \\[run-ciao-toplevel], but starts with a
window in Ciao mode, ready to edit, and another one with the Ciao
toplevel. Useful as splash screen for the Ciao program development
system, for example when launching from a desktop (launch emacs,
calling this function)."
  (interactive)
  (let ((tmpfile 
	(concat (ciao-new-temp-code-file ciao-lpdoc-wdir-root) ".pl")))
    (delete-other-windows)
    (run-ciao-toplevel)
    (if ciao-create-sample-file-on-startup
	(progn
	  (find-file tmpfile)
	  (beginning-of-buffer)
	  (insert-string 
"% You can type code in this buffer. 
% Save with \"File->Save Buffer As...\" or \"C-x C-s\".
% Load into toplevel with \"C-c l\"
% Explore menus above and see Section \"Using Ciao inside GNU emacs\" 
% of the ciao manual (\"Ciao/Prolog->Ciao system manual\") for details

:- module(_,_).

main(Arg) :- 
	write(Arg).

")
	  )
      (switch-to-buffer (concat "*" ciao-toplevel-buffer-name "*"))
      (delete-other-windows)
    )))

;;------------------------------------------------------------
;; The actual Ciao / Prolog / &-Prolog Mode
;;------------------------------------------------------------

(defun ciao-mode ()
  "
   This is a major mode for 
   editing / debugging / documenting / compiling / running / ...
   Ciao and Prolog code.

See the Ciao manual (you can use \\<ciao-mode-map>
\\[ciao-goto-ciao-manuals]) for full information on the many features
of this mode.

The following is a summary of the keyboard commands available in the
mode (see also the mode-specific entries in the menu-bar if enabled):

\\{ciao-mode-map}

Entry to this mode calls the value of ciao-mode-hook if that value is
non-nil." 

  (interactive)
  (if (get-buffer-process (current-buffer))
      ; Exit if this is a process buffer (very likely an error)
      (message 
      "Ciao mode not for process buffers, use M-x ciao-inferior-mode instead.")
    (ciao-mode-nocheck)))

(defun ciao-mode-nocheck ()
    (kill-all-local-variables)
    (use-local-map ciao-mode-map)
    (setq major-mode 'ciao-mode)
    ;; MR added to avoid errors in xemacs
    (if (boundp 'xemacs-logo)
	(define-key ciao-mode-map 'backspace 'delete-backward-char))
    (setq mode-name ciao-toplevel-buffer-name)
    (setq case-fold-search nil)
    (set-syntax-table ciao-mode-syntax-table)
    (ciao-mode-variables)
    
    ;; This weird ordering results in same layout in emacs and xemacs...
    (easy-menu-define ciao-menu-help ciao-mode-map 
      "Ciao/Prolog Mode Help Menus" ciao-mode-menus-help)
    (easy-menu-define ciao-menu-customize ciao-mode-map 
      "Ciao/Prolog Mode Customization Menus" 
      ciao-mode-menus-customize)
    (easy-menu-define ciao-menu-lpdoc ciao-mode-map 
      "LPdoc Mode Menus" ciao-mode-lpdoc-menus)
    (easy-menu-define ciao-menu-ciaopp ciao-mode-map 
      "CiaoPP Mode Menus" ciao-mode-ciaopp-menus)
    (easy-menu-define ciao-menu-debug ciao-mode-map 
      "Ciao/Prolog Mode Debug Menus" ciao-mode-menus-debug)
    (easy-menu-define ciao-menu-sys ciao-mode-map 
      "Ciao/Prolog Mode System Menus" ciao-mode-menus-sys)
  
    (easy-menu-add ciao-menu-sys)
    (easy-menu-add ciao-menu-debug)
    (easy-menu-add ciao-menu-ciaopp)
    (easy-menu-add ciao-menu-lpdoc)
    (easy-menu-add ciao-menu-customize)
    (easy-menu-add ciao-menu-help)

    ;; MR added to support font-lock
    (if (ciao-emacs-cannot-do-font-lock)
	nil
      ;; MH Tool bar stuff (21.1 onwards)
      (if  (fboundp 'tool-bar-mode)
	  (ciao-setup-tool-bar))
      ;; 
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults 
	    '(ciao-mode-font-lock-keywords 
	      t nil nil 
	      ;; Use all buffer refontifying...
	      'beginning-of-buffer 
	      (font-lock-mark-block-function . 
 	       ;; Alternative: mark-paragraph
	       ;; Use whole buffer for refontifying...
	       (lambda () 
		(push-mark (point-max))
		(goto-char (point-min)))
	       ))))
    (run-hooks 'ciao-mode-hook))

;; Not necessary to do it this way?
(add-hook
 'ciao-mode-hook
 (function
  (lambda ()
    (define-key ciao-mode-map "\C-x\C-s" 'ciao-save-buffer)
)))

(defun ciao-emacs-cannot-do-font-lock ()
  "We are not capable of fontifying (possible in windowing system,
modern emacses, and also in ascii mode with emacs>= 21.1)."
  (and (not window-system) (not (fboundp 'tool-bar-mode))))

;; Turn on font lock
(if (ciao-emacs-cannot-do-font-lock)
    nil
  (add-hook 'ciao-mode-hook 'turn-on-font-lock)
  (add-hook 'ciao-inferior-mode-hook 'turn-on-font-lock))


;;------------------------------------------------------------
;; Inferior process management
;;------------------------------------------------------------

(defun ciao-inferior-mode ()

  "Inferior mode for interaction with Ciao/Prolog toplevel,
preprocessor, etc.

This is a major emacs mode used in Ciao-related interactive buffers,
i.e., buffers in which it is possible to interact with an inferior
process running the Ciao/Prolog top-level, Ciao preprocessor,
documenter, etc.

You can talk to the Ciao/Prolog top-level or the preprocessor by
typing commands directly in the corresponding buffer as if in a normal
shell. You can also send files or parts of files to be preprocessed or
compiled by the processes running under this inferior mode from any
buffer which is in ciao-mode (see the emacs commands available in such
buffers).

All commands available in standard emacs shell packages (comint) are
available in these interactive buffers. In addition, there are many
new commands which are specific to this mode.  The following is a list
of all the available commands:

\\{ciao-inferior-mode-map}

Entry to this mode calls the value of ciao-mode-hook with no arguments,
qif that value is non-nil.  Likewise with the value of comint-mode-hook.
ciao-mode-hook is called after comint-mode-hook.

"
  (interactive)
  ;; Makes sure ciao mode is initialized (keymaps, etc.). We then
  ;; override with inferior mode.
  (ciao-mode-nocheck)
  (cond ((not (eq major-mode 'ciao-inferior-mode))
	 (kill-all-local-variables)
	 (comint-mode)
         (setq comint-highlight-prompt nil) ; avoid unwanted coloring
	 (setq major-mode 'ciao-inferior-mode)
	 (setq mode-name "Ciao/Prolog/LPdoc Listener")
	 (setq mode-line-process '(": %s"))
	 (setq comint-input-filter 'ciao-input-filter)
         (set-syntax-table ciao-mode-syntax-table)
	 (ciao-mode-variables)
	 ;; Source debugger stuff
	 (setq ciao-debug-last-line nil)
	 (cond ((string= (buffer-name) 
			 (concat "*" ciao-toplevel-buffer-name "*"))
		(set-process-filter (get-buffer-process (current-buffer))
				    'ciao-debug-filter))
	       ((string= (buffer-name) 
			 (concat "*" ciao-ciaopp-buffer-name "*"))
		(set-process-filter (get-buffer-process (current-buffer))
				    'ciao-ciaopp-filter))
	       ;; Uncomment for supporting hooks in LPdoc
	       ((string= (buffer-name) 
			 (concat "*" ciao-lpdoc-buffer-name "*"))
		(set-process-filter (get-buffer-process (current-buffer))
				    'ciao-lpdoc-filter))
	       ;; This case is usually used in normal shell. The filter is
	       ;; to handle source-level embedded debugger messages
	       (t (set-process-filter (get-buffer-process (current-buffer))
				     'ciao-debug-filter))
	 )
	 (set-process-sentinel (get-buffer-process (current-buffer))
			       'ciao-inferior-process-sentinel)
         ;; 
	 (use-local-map ciao-inferior-mode-map)
	 ;; These are shared:
	 (easy-menu-define ciao-inferior-menu-help ciao-inferior-mode-map 
	   "Ciao/Prolog Inferior Mode Help Menu" ciao-mode-menus-help) 
	 (easy-menu-add ciao-inferior-menu-help)
	 (easy-menu-define ciao-inferior-menu-customize ciao-inferior-mode-map 
	   "Ciao/Prolog Mode Customization Menus" ciao-mode-menus-customize)
	 (easy-menu-add ciao-inferior-menu-customize)
	 (easy-menu-define ciao-inferior-menu ciao-inferior-mode-map 
	   "Ciao/Prolog Mode Menu" ciao-inferior-mode-menus)
	 (easy-menu-add ciao-inferior-menu)
	 (setq comint-prompt-regexp ciao-any-prompt-pattern)
	 
	 ;; MR added to support font-lock
 	 (if (ciao-emacs-cannot-do-font-lock)
 	     nil
	   ;; MH Tool bar stuff (21.1 onwards)
	   (if  (fboundp 'tool-bar-mode)
	       (ciao-setup-inferior-tool-bar))
	   ;; 
 	   (make-local-variable 'font-lock-defaults)
 	   (setq font-lock-defaults 
		 '(ciao-inferior-font-lock-keywords 
		   t nil nil 
		   ;; Use all buffer refontifying...
		   'beginning-of-buffer 
		   (font-lock-mark-block-function . 
	           ;; Alternative: mark-paragraph
	           ;; Use whole buffer for refontifying...
	             (lambda () 
		       (push-mark (point-max))
		       (goto-char (point-min))))
		   )))
	 (run-hooks 'ciao-mode-hook)))
  (if (string= "*" (char-to-string (elt (buffer-name) 0)))
      (setq ciao-last-process-buffer-used 
	    (substring (buffer-name) 1 (- (length (buffer-name)) 1)))
    (setq ciao-last-process-buffer-used (buffer-name))
    (rename-buffer (concat "*" (buffer-name) "*")))
  )

(defun ciao-input-filter (str)
  ;; putting "[ \t]*" instead of " *" breaks in xemacs...
  (cond ((string-match "\\`\\s *\\'" str) nil) ;whitespace
	((not (eq major-mode 'ciao-inferior-mode)) t)
	((= (length str) 1) nil)	;one character
	((string-match "\\`[rf][ \t]*[0-9]*\\'" str) nil) ;r(edo) or f(ail)
	(t t)))

(defun ciao-insert-logos-toplevel ()
  (ciao-insert-logos ciao-toplevel-buffer-name))

(defun ciao-insert-logos-ciaopp ()
  (ciao-insert-logos ciao-ciaopp-buffer-name))

(defun ciao-insert-logos (buffer-name)
  "Insert a splash screen for the Ciao program development system at
the beginning of the current buffer."
  (set-buffer (concat "*" buffer-name "*"))
  (if window-system
      (let ((beg 0) (end 0))
 	(goto-char (point-min))
 	(setq beg (point))
 	(open-line 3)
 	(next-line 1)
	(ciao-insert-image 'xpm ciao-clip-logo "CLIP")
 	(insert-string " ")
	(ciao-insert-image 'xpm ciao-logo "Ciao")
 	(setq end (point))
 	;; (put-text-property beg end 'face 'Info-title-1-face)
	(goto-char (point-max))
 	)
    (goto-char (point-max))))

(defun ciao-insert-image (type image default)
  "Portable image insertion (emacs, xemacs). Third argument is text to
be used if images not supported (e.g., in text mode)"
  (let
      (imagefile imagefile-fullpath first-char)
    (setq first-char (substring image 0 1))
    (if (or (string= first-char "/")              ;; /foo 
	    (string= first-char ".")              ;; ./foo
	    (string= first-char "\\")             ;; \foo
	    (string= (substring image 1 2) ":"))  ;; C:foo
	;; Path given: keep in all cases
	(progn
	  (setq imagefile image)
	  (setq imagefile-fullpath image))
      ;; Probably no path: look under icons for emacs, 
      ;; put full lib path for xemacs
      (setq imagefile (concat "icons/" image))
      (setq imagefile-fullpath (concat ciao-real-lib-dir "/icons/" image)))
    (cond 
     ((and (fboundp 'tool-bar-mode) window-system);; emacs, graphical
      (insert-image 
       (find-image (list (list :type type :file imagefile )))))
     ((and (boundp 'xemacs-logo) window-system);; xemacs, graphical
      (ciao-xemacs-insert-glyph ;; xemacs needs full path
       (make-glyph (vector type :file imagefile-fullpath ))))
     (t ;; text mode
      (insert-string default)))))

(defun ciao-xemacs-insert-glyph (gl)
  "Insert a glyph at the left edge of point."
  (let ((prop 'ciaoimage)        ;; ciaoimage is an arbitrary name
	extent)
    ;; First, check to see if one of our extents already exists at
    ;; point.  For ease-of-programming, we are creating and using our
    ;; own extents (multiple extents are allowed to exist/overlap at the
    ;; same point, and it's quite possible for other applications to
    ;; embed extents in the current buffer without your knowledge).
    ;; Basically, if an extent, with the property stored in "prop",
    ;; exists at point, we assume that it is one of ours, and we re-use
    ;; it (this is why it is important for the property stored in "prop"
    ;; to be unique, and only used by us).
    (if (not (setq extent (extent-at (point) (current-buffer) prop)))
	(progn
	  ;; If an extent does not already exist, create a zero-length
	  ;; extent, and give it our special property.
	  (setq extent (make-extent (point) (point) (current-buffer)))
	  (set-extent-property extent prop t)
	  ))
    ;; Display the glyph by storing it as the extent's "begin-glyph".
    (set-extent-property extent 'begin-glyph gl)
    ))

(defun ciao ()
  "Like \\<ciao-mode-map> \\[run-ciao-toplevel], but starts with a
single window."
  (interactive)
  (run-ciao-toplevel)
  (switch-to-buffer (concat "*" ciao-toplevel-buffer-name "*"))
  (delete-other-windows)
  )

(defun prolog ()
  "Start up Ciao Prolog."
  (interactive)
  (ciao))

(defun run-ciao-toplevel ()

  "Ensure that an inferior Ciao/Prolog top-level process is
running. 

   This opens a top-level window (if one did not exist already)
where queries can be input directly as in any normal Prolog top
level. Programs can be loaded into this top level by typing the
corresponding commands in this window (such as use_module, etc.), or,
more typically, by opening the file to be loaded in an emacs window
(where it can be edited) and issuing a load command (such as
\\<ciao-mode-map> \\[ciao-load-buffer] or
\\[ciao-load-from-main-module]) directly from there (see the loading
commands of this mode and their bindings).

   Note that many useful commands (e.g., to repeat and edit previous
commands, interrupt jobs, locate errors, automatic completions, etc.)
are available in this top-level window (see @ref{Commands available in
toplevel and preprocessor buffers}).

   Often, it is not necessary to use this function since execution of
any of the other functions related to the top level (e.g., loading
buffers into the top level) ensures that a top level is started
(starting one if required)."

  (interactive)
  (message "Starting Ciao toplevel... ")
  (ciao-ensure-inferior-process ciao-toplevel-buffer-name)
  (add-hook 'ciao-prompt-inferior-hook 
	    'ciao-insert-logos-toplevel t)
  (message "Starting Ciao toplevel... done."))

(defun ciaopp ()
  "Same as \\<ciao-mode-map> \\[run-ciao-preprocessor], but starts
with a single window.  Useful as splash screen for the Ciao
preprocessor, for example when launching it from a desktop (launch
emacs, calling this function)."
  (interactive)
  (run-ciao-preprocessor)
  (switch-to-buffer (concat "*" ciao-ciaopp-buffer-name "*")) 
  (delete-other-windows)
  )

(defun run-ciao-preprocessor ()
  "Ensure that an inferior Ciao preprocessor process is running. 

   This opens a preprocessor top-level window (if one did not exist
already) where preprocessing commands and preprocessing menu options
can be input directly. Programs can be preprocessed by typing commands
in this window, or, more typically, by opening the file to be
preprocessed in an emacs window (where it can be edited) and issuing a
command (such as \\<ciao-mode-map> \\[ciao-preprocess-buffer-menu] or
\\[ciao-preprocess-buffer]) directly from there (see the preprocessing
commands of this mode and their bindings).

   Note that many useful commands (e.g., to repeat and edit previous
commands, interrupt jobs, locate errors, automatic completions, etc.)
are available in this top-level window (see @ref{Commands available in
toplevel and preprocessor buffers}).

   Often, it is not necessary to use this function since execution of
any of the other functions related to the top level (e.g., loading
buffers into the top level) ensures that a top level is started
(starting one if required)."
  (interactive)
  (message "Starting Ciao preprocessor... ")
  (ciao-ensure-inferior-process ciao-ciaopp-buffer-name)
  (add-hook 'ciao-ciaopp-prompt-inferior-hook 
	    'ciao-insert-logos-ciaopp t)
  (message "Starting Ciao preprocessor... done."))

;; MH Made it recenter, and then the functions below are trivial
(defun ciao-ensure-inferior-process (buffname)
  (let (origbuff system system-args newbuff)
    (setq origbuff (buffer-name))
    (cond
     ;; Complication, because we are sharing the inferior mode
     ((string= buffname ciao-toplevel-buffer-name)
      (setq system ciao-system)
      (setq system-args ciao-system-args))
     ((string= buffname ciao-ciaopp-buffer-name)
      (setq system ciao-ciaopp-system)
      (setq system-args ciao-ciaopp-system-args))
     ;; *** Temporary until lpdoc-2.0
     ((string= buffname ciao-lpdoc-buffer-name)
      (setq system "/bin/tcsh")
      (setq system-args "")
      ;;  (setq system ciao-lpdoc-system)
      ;;  (setq system-args ciao-lpdoc-system-args)
      )
     )
    (setq 
     newbuff
     (if (equal ""
		;; Done differently because of filenames with blanks...
		;; (ciao-get-string-after-blank system)
		system-args
		)
	 (progn 
	   (make-comint buffname 
		      ;; Done differently because of filenames with blanks...
		      ;; (ciao-get-string-before-blank system)
		      system
		      ))
       (make-comint buffname 
		    ;; Done differently because of filenames with blanks...
		    ;; (ciao-get-string-before-blank system) ; command name
		    system
		    nil                                   ; filename
		    ;; Done differently because of filenames with blanks...
		    ;; (ciao-get-string-after-blank system)  ; arguments
		    system-args
		    )))
    (if (string= (buffer-name) (buffer-name newbuff)) ;; We are already there..
	()
      (switch-to-buffer-other-window newbuff))
    (ciao-inferior-mode)
    (goto-char (point-max))
    (if (string= (buffer-name) origbuff) ;; We are already there...
	()
      (switch-to-buffer-other-window origbuff))
    (setq ciao-last-process-buffer-used buffname)))

;; Had to do this differently because of filenames with blanks...
;; (defun ciao-get-string-before-blank  (string)
;;   (if (string-match " " string) 
;;       (substring string 0 (string-match " " string))
;;   string))
;; 
;; (defun ciao-get-string-after-blank  (string)
;;   (if (string-match " " string) 
;;       (substring string (+ (string-match " " string) 1) nil)
;;   nil))

(defun ciao-recenter-last-ciao-buffer () 
  "Recenter the most recently used Ciao/Prolog inferior process buffer
(top level or preprocessor)."
  (interactive)
  (if ciao-last-process-buffer-used
    (ciao-ensure-inferior-process ciao-last-process-buffer-used)
   (message "No process has been started.")
  ))

(defvar ciao-tmp-calling-buff nil
  "Temp var to pass calling buffer to hooks.")
(defvar ciao-tmp-buffername nil
  "Temp var to pass buffername to hooks.")
(defvar ciao-tmp-command nil
  "Temp var to pass command to hooks.")

;; General interface to subprocess
(defun ciao-send-command (buffername command recenter-opt)
  ;; remember the buffer we are at
  (setq ciao-tmp-calling-buff (buffer-name))
  (save-some-buffers)
  (if (eq (comint-check-proc (get-buffer (concat "*" buffername "*"))) nil)
      (progn
	(ciao-ensure-inferior-process buffername)
	(setq ciao-tmp-buffername buffername)
	(setq ciao-tmp-command command)
	(cond ((string= buffername ciao-toplevel-buffer-name) 
	       (add-hook 'ciao-prompt-inferior-hook 
			 'ciao-do-send-command-global t))
	      ((string= buffername ciao-ciaopp-buffer-name)
	       (add-hook 'ciao-ciaopp-prompt-inferior-hook 
			 'ciao-do-send-command-global t))
	      ((string= buffername ciao-lpdoc-buffer-name)
	       (add-hook 'ciao-lpdoc-prompt-inferior-hook 
			 'ciao-do-send-command-global t))))
    (ciao-do-send-command buffername command recenter-opt))
  ;; MH Added to improve tracking of last inferior buffer used.
  (setq ciao-last-process-buffer-used buffername)
  )

;; Terrible kludge to pass arguments (just for the first command)
(defun ciao-do-send-command-global ()
  (ciao-do-send-command ciao-tmp-buffername ciao-tmp-command nil))

(defun ciao-do-send-command (buffername command recenter-opt)
   (switch-to-buffer-other-window (concat "*" buffername "*"))
   (goto-char (point-max))
   (if (eq recenter-opt t) 
       (recenter 0))
   (insert-string command)
   (comint-send-input)
   (switch-to-buffer-other-window ciao-tmp-calling-buff))

;; MH Alternative (but doesn't work?)
;; (defun ciao-send-command (buffername command)
;;   (comint-proc-query buffername command))

;;------------------------------------------------------------
;; Locating errors
;;------------------------------------------------------------

(defun ciao-any-errors ()
  "True if there were any errors in the previous run."
  (save-excursion
    (let (process-buffer)
      (setq process-buffer (concat "*" ciao-last-process-buffer-used "*"))
      (if (and ciao-last-process-buffer-used 
	       (get-buffer process-buffer)) ;; buffer still exists
	  (progn
	    ;; Go to process buffer
	    (set-buffer process-buffer)
	    (end-of-buffer)
	    (move-to-column 0) ;; skip prompt if at prompt
	    ;; Go back to previous prompt or beginning of buffer
	    (search-backward-regexp ciao-any-prompt-pattern nil t)
	    (end-of-line)
	    (not (ciao-no-more-errors)))))))

(defun ciao-no-more-errors ()
  (or (not (search-forward-regexp (ciao-error-or-prompt-pattern) nil t))
      (string=  (buffer-substring-no-properties 
		 (- (point) (length ciao-prompt)) (point))
		ciao-prompt)
      (string=  (buffer-substring-no-properties 
		 (- (point) (length ciao-ciaopp-prompt)) (point))
		ciao-ciaopp-prompt)
      (eq (string-match ciao-os-shell-prompt-pattern
		    (buffer-substring-no-properties 
		     (match-beginning 0) (match-end 0))
		    ) 0)
      ))

(defun ciao-find-last-run-errors ()
  "Go to the location in the source file containing the next error reported by
the last Ciao/Prolog subprocess (preprocessor or toplevel) which was run."
  (interactive)
  (let ((process-buffer (concat "*" ciao-last-process-buffer-used "*")))
    (if (and ciao-last-process-buffer-used 
	     (get-buffer process-buffer))
	(if ciao-finding-errors
	    (progn
	      ;; Go to process buffer
	      (if (string= (buffer-name (current-buffer)) process-buffer)
		  ()
		(switch-to-buffer-other-window process-buffer)
		)
	      (ciao-find-error process-buffer))
	  ;; Mark that we are starting a finding errors session
	  (setq ciao-finding-errors (current-buffer))
	  ;; Go to process buffer, split in two
	  (if (string= (buffer-name (current-buffer)) process-buffer)
	      ()
	    ;; Start with a single window
	    (delete-other-windows)
	    (switch-to-buffer-other-window process-buffer)
	    )
	  (end-of-buffer)
	  (move-to-column 0) ;; skip prompt if at prompt
	  ;; Go back to previous prompt or beginning of buffer
	  (search-backward-regexp ciao-any-prompt-pattern nil t)
	  (end-of-line)
	  (ciao-find-error process-buffer))
      (message "No recent program processing active."))
    ))

(defun ciao-find-error (process-buffer)
  "Go to location in source file containing next error, highlight."
  (let (beginline endline filename error)
    ;; first repaint (eliminates any previous error marks in buffer)
    ;; No need to do anything if file is not being visited any more
    (if (and ciao-previous-error
	     (get-file-buffer (car (cdr (cdr ciao-previous-error)))))
	(progn 
	  (set-buffer (get-file-buffer 
		       (car (cdr (cdr ciao-previous-error)))))
	  (if (> (car ciao-previous-error) 0)
	      (progn 
		(ciao-uncolor (car ciao-previous-error)
			      (car (cdr ciao-previous-error))
			      'ciao-error)))
	  (setq ciao-previous-error nil)))
    (set-buffer process-buffer)
    ;; In process buffer, get error data
    (setq error (ciao-get-next-error-data))
    (if (eq error nil)
	;; There are no (more) errors
	(progn
	  (goto-char (point-max)) ;; goto end of process buffer
	  ;; Return to original buffer if not already there
	  (if (eq ciao-finding-errors (current-buffer))
	      ()
	    (switch-to-buffer-other-window ciao-finding-errors))
	  ;; MH Put this back in to return to single original window 
	  ;; *** (delete-other-windows) ***
	  (setq ciao-finding-errors nil)
	  (message "There were no (more) errors."))
      ;; Error located, get info, go to file, if known.
      (setq beginline (car error))
      (setq endline (car (cdr error)))
      (setq filename (car (cdr (cdr error))))
      (if (eq filename nil)
	  (message "No corresponding file could be determined.")
	(find-file-other-window filename)
	(if (< beginline 0)
	    ;; No line numbers: just visit file
	    (progn 
	      (beginning-of-buffer)
	      (message "Error within this file.")
	      (setq ciao-previous-error nil))
	  ;; Else, highlight region in opened file...
	  (push-mark (point) t)
	  (goto-line beginline)
	  (recenter 0)
	  (ciao-color beginline endline 
		      ciao-face-highlight-code 'ciao-error)
	  (setq ciao-previous-error error)
	  (goto-line (+ endline 1))
	  (backward-char 1)
	  (message "Mark set")
	  )
	))))

(defun ciao-get-next-error-data ()
  "Locates next error, and highlights it. Returns:
     nil -- if no more errors
     '(beginline endline file) -- if an error found, where
        beginline/endline = location of error in process buffer
        file = source file containing error (if nil: no file was located)"
;; ALT:
;;         beginline/endline = can also contain predicate name / clause number
;;             (this is a temporary kludge while proprocessor error
;;              reporting is improved)

  ;; If we have a previous error found and colored, uncolor it
  (if ciao-inferior-error
      (progn
	(ciao-uncolor ciao-inferior-error
		      ciao-inferior-error
		      'ciao-error)
	(goto-line ciao-inferior-error)
	(setq ciao-inferior-error nil)))

;; From 21.1 on , this does not go over the prompt. Using column instead:
;;  (beginning-of-line)
;;  (move-to-column 0)
  (end-of-line)
  (if (ciao-no-more-errors)
      ;; No (more) errors found
      (setq ciao-inferior-error nil)
    nil
    (let ((messpoint (point)) beginline endline openpoint filename)
      (recenter 1)
      (move-to-column 0)
      (if (not (search-forward "lns " (+ (point) 80) t))
;; MH OLD
	      ;; No line number info: -1 -1
	      (progn
		(setq beginline -1)
		(setq endline -1))
;; MH ALT
;;  	  (if (not (search-forward " at " (+ (point) 80) t))
;; 	      ;; No line number info: -1 -1
;; 	      (progn
;; 		(setq beginline -1)
;; 		(setq endline -1))
;; 	    ;; locate by e.g. "at partition/4/3/1" 
;; 	    ;; This is a kludge while messages from preprocessor improve
;;  	    (let ((beg (point)) predicate clausenumber)
;;  	      (search-forward "/")
;;  	      (backward-char 1)
;;  	      (setq predicate (buffer-substring-no-properties beg (point)))
;;  	      (forward-char 1)
;;  	      ;; ignore arity (approximation)
;;  	      (search-forward "/")
;;  	      (setq beg (point))
;;  	      (search-forward "/")
;;  	      (setq clausenumber
;;  		    (string-to-int (buffer-substring-no-properties beg
;;  								   (point))))
;; 	      ;; MH ***
;; 	      (message (append "ERROR DATA: " predicate " "
;; 			       (int-to-string clausenumber )))
;; 	      ;; This typically done elsewhere, but kludge to get line numbers
;; 	      (save-excursion 
;; 		(find-file-other-window filename)
;; 		(search-forward-regexp (concat "^" beginline) nil t endline)
;; 		(setq beginline (point))
;; 		(search-forward-regexp (concat "\\(^" beginline "\\|^$\\)")  nil t)
;; 		(setq endline (point)))
;; 	      )
;; 	    (progn 
;; 	      (setq beginline -1)
;; 	      (setq endline -1))
;; 	    )
	;; Get line number info.
;;	(search-forward "lns " (+ (point) 80) t)
	(let ((beg (point)))
	  (search-forward "-")
	  (backward-char 1)
	  (setq beginline 
		(string-to-int (buffer-substring-no-properties beg (point)))))
	(forward-char 1)
	(let ((beg (point)))
	  (search-forward ")")
	  (backward-char 1)
	  (setq endline 
		(string-to-int (buffer-substring-no-properties beg (point)))))
	)
      ;; Beginning of ERROR/WARNING/... line
      (move-to-column 0)
      (setq ciao-inferior-error (ciao-what-line))
      (ciao-color ciao-inferior-error
		  ciao-inferior-error
		  ciao-face-highlight-code
		  'ciao-error)

      ;; Try to find opening "{" by inserting a "}"
      (insert-string "}")
      ;; Change syntax of parenthesis
      (modify-syntax-entry ?( "_")
      (modify-syntax-entry ?) "_")
      (modify-syntax-entry ?[ "_")
      (modify-syntax-entry ?] "_")
      ;; Scan to "{"
      (condition-case nil
	  (setq openpoint (scan-sexps (point) -1))
	(error (setq openpoint 0)))
      ;; Return syntax of parenthesis
      (modify-syntax-entry ?( "()")
      (modify-syntax-entry ?) ")(")
      (modify-syntax-entry ?[ "(]")
      (modify-syntax-entry ?] ")[")      
      ;; Delete the "}" inserted
      (delete-char -1)
      (if (= openpoint 0)
	  (setq filename nil)
	(goto-char openpoint)
	(search-forward "/")
	(backward-char 1)
	(let ((beg (point)))
          (search-forward-regexp 
	   "\\(\\.\\(po\\|itf\\|asr\\|pls\\|pl\\|cgi\\)\\>\\|$\\)")
          (setq filename 
		(fix-cygwin-drive-letter
		 (concat (buffer-substring-no-properties 
			  beg (match-beginning 0)) 
			 ;; MH cygdrive case for .pls, fixed bug
			 (cond
			  ((string= (funcall ciao-match-string 0) ".po") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) ".itf") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) ".asr") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) ".pls") 
			   ".pls")
			  ((string= (funcall ciao-match-string 0) ".pl") 
			   ".pl")
			  ((string= (funcall ciao-match-string 0) "cgi") 
			   ".cgi")
			  ((string= (funcall ciao-match-string 0) "") 
			   "")
			  )))))
	(goto-char messpoint)
        ;; (beginning-of-line)
	(move-to-column 0)
	)
      (cons beginline (cons endline (cons filename nil)))
      )))

;; MH cygdrive Fixed for newer version of cygwin
;; MH //c/ and also /cygdrive/
(defun fix-cygwin-drive-letter (filename)
  (if (eq (string-match "//./" filename) 0)
      (concat (substring filename 2 3) ":" (substring filename 3))
    (if (eq (string-match "/cygdrive/" filename) 0)
	(concat (substring filename 10 11) ":" (substring filename 11))
      filename
    )))

(defun ciao-unmark-last-run-errors()
  "Remove error marks from last run (and also debugging marks if present)."
  (interactive)
  (if ciao-last-process-buffer-used
      (save-excursion
	(setq ciao-finding-errors nil)
	(if ciao-previous-error
	    (let ((error-buffer
		   (get-file-buffer 
			   (car (cdr (cdr ciao-previous-error))))))
	      (if (not error-buffer) ;; nil=buffer does not exist any more
		  ()
		(set-buffer error-buffer)
		(ciao-uncolor (car ciao-previous-error)
			      (car (cdr ciao-previous-error))
			      'ciao-error)))
	  (message "No error mark(s) found.")
	  (setq ciao-previous-error nil))
	(if ciao-inferior-error
	    (let ((last-buffer
		   (concat "*" ciao-last-process-buffer-used "*")))
	      (if (get-buffer last-buffer) ;; else already deleted
		  (progn
		    (set-buffer last-buffer)
		    (ciao-uncolor ciao-inferior-error
				  ciao-inferior-error
				  'ciao-error)))
	      (setq ciao-inferior-error nil)))
	;; This returns nil if not debugging, so it does not hurt and
	;; is handy
	(ciao-debug-remove-marks)
	)
    (message "No recent program processing active.")
    ))

;;------------------------------------------------------------
;; Assertions and syntax cheking
;;------------------------------------------------------------

(defun ciao-check-buffer-syntax ()

  "Check the @em{syntax} of the code and assertions in the current
buffer, as well as imports and exports.  This uses the standard top
level (i.e., does not call the preprocessor and thus does not require
the preprocessor to be installed). Note that full (semantic) assertion
checking must be done with the preprocessor."

  (interactive)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (if (and ciao-assrt-lib-loaded ;; if lib loaded and process still running...
	   (comint-check-proc 
	    (get-buffer-create (concat "*" ciao-toplevel-buffer-name "*"))))
      (ciao-do-check-buffer-syntax)
    (ciao-ensure-inferior-process ciao-toplevel-buffer-name)
    (add-hook 'ciao-prompt-inferior-hook 'ciao-load-assrt-lib t)
    (add-hook 'ciao-prompt-inferior-hook 'ciao-do-check-buffer-syntax t)
    )
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t)))

(defun ciao-load-assrt-lib ()
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   "use_module(library('assertions/assrt_lib'))."
   t)
  (setq ciao-assrt-lib-loaded t))

(defun ciao-do-check-buffer-syntax ()
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "prolog_flag(verbose_compilation,_Old,off),"
           "check_code_and_assrt_syntax('" (buffer-file-name) "'),"
           "prolog_flag(verbose_compilation,_,_Old)." 
	   )
   t))

;;------------------------------------------------------------
;; Some aid for inserting text (very limited for now)
;;------------------------------------------------------------

(defun ciao-insert-script-header ()

  "Insert a (Unix) header at the top of the current buffer so that the
ciao script interpreter will be called on this file if @em{run} from
the command line. It also makes the file ``executable'' (e.g.,
'@tt{chmod +x <file>}' in Unix). See @ref{The script interpreter} for
details."

  (interactive)
  (beginning-of-buffer)
  (insert-string 
   (concat "#!/bin/sh\n"
	   "exec ciao-shell $0 \"$@\" # -*- mode: ciao; -*-\n"
	   "\n"))
  (set-file-modes (buffer-file-name) 448))


;;------------------------------------------------------------
;; Preprocess buffer
;;------------------------------------------------------------

(defun ciao-preprocess-buffer-menu ()
  "Preprocess the buffer, selecting options. Instructs the
preprocessor to load the current buffer and start an interactive
dialog in which the different options available in the preprocessor
can be set. "
  (interactive)
  (ciao-do-preprocess-buffer 'menu nil))

(defun ciao-preprocess-buffer ()
  "Preprocess the buffer, using the previously selected options. If no
options were set previously, then the preprocessor defaults are used."
  (interactive)
  (ciao-do-preprocess-buffer 'nomenu nil))

(defun ciao-preprocess-buffer-and-show-output ()
  "Preprocess the buffer, using the previously selected (or default)
options, waits for preprocessing to finish and displays the
preprocessor output (leaving the cursor at the same point if already
on a preprocessor output file). This allows running the preprocessor
over and over and watching the output while modifying the source
code."
  (interactive)
  (ciao-do-preprocess-buffer 'nomenu t))

(defun ciao-check-types-modes ()
  "Uses the preprocessor to perform compile-time checking of types and
modes (pptypesfd and shfr analyses). "
  (interactive)
  (message "Checking types and modes... ")
  (ciao-do-preprocess-buffer 'typesmodes nil))

(defun ciao-do-preprocess-buffer (action showoutput)
  "Main function to call the preprocessor. Implements the others via options."
  (message "Preprocessing buffer... ")
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command 
   ciao-ciaopp-buffer-name 
    (cond
     ((eq action 'menu)        (ciao-build-ciaopp-command "[]"))
     ((eq action 'nomenu)      (ciao-build-ciaopp-command nil ))
     ((eq action 'typesmodes)  (ciao-build-ciaopp-specific-command "ctcheck"))
     (t nil))
    t)
  (if showoutput
      (add-hook 'ciao-ciaopp-prompt-inferior-hook 
		'ciao-find-errors-or-show-output t)
    (if ciao-locate-errors-after-run
	(add-hook 'ciao-ciaopp-prompt-inferior-hook 
		  'ciao-launch-find-last-run-errors-from-orig-buffer t)))

;;   (if ciao-locate-errors-after-run
;;       (add-hook 'ciao-ciaopp-prompt-inferior-hook 
;; 		'ciao-launch-find-last-run-errors-from-orig-buffer t))
;;   (if showoutput
;;       (add-hook 'ciao-ciaopp-prompt-inferior-hook 
;; 		'ciao-show-preprocessor-output t))
  )

(defun ciao-launch-find-last-run-errors-from-orig-buffer ()
  (switch-to-buffer ciao-last-source-buffer-used)
  (ciao-find-last-run-errors)
  )

(defun ciao-find-errors-or-show-output ()
  (switch-to-buffer ciao-last-source-buffer-used)
  (if (and ciao-locate-errors-after-run (ciao-any-errors))
      (ciao-find-last-run-errors)
    (ciao-show-preprocessor-output)
    ;; In this case, probably best to stay in original buffer
    (switch-to-buffer-other-window ciao-last-source-buffer-used))
  )

(defun ciao-set-ciaopp-output-pred ()
  "Make ciaopp output only predicate-level analysis information."
  (interactive)
  (ciao-send-command ciao-ciaopp-buffer-name "dump_ai(pred)." t))

(defun ciao-set-ciaopp-output-full ()
  "Make ciaopp output both literal- and predicate-level analysis information."
  (interactive)
  (ciao-send-command ciao-ciaopp-buffer-name "dump_ai(yes)." t))

(defun ciao-set-ciaopp-output-none ()
  "Make ciaopp output no analysis information."
  (interactive)
  (ciao-send-command ciao-ciaopp-buffer-name "dump_ai(no)." t))

(defun ciao-build-ciaopp-command (options)
  (concat "precompile('" (buffer-file-name)
	  (if (string= options nil)
	      "')."
	    (concat "'," options ").") )))

(defun ciao-build-ciaopp-specific-command (command-name)
  (concat command-name "('" (buffer-file-name) "').") )

(defun ciao-show-preprocessor-output ()
  "Show last output file produced by Ciao preprocessor. The preprocessor
works by producing a file which is a transformed and/or adorned (with
assertions) version of the input file. This command is often used after
running the preprocessor in order to visit the output file and see the
results from running the preprocessor."
  (interactive)
  (let ((ciaoppbuffname (concat "*" ciao-ciaopp-buffer-name "*"))
	(origbuffer (current-buffer)))
    (if (not (get-buffer ciaoppbuffname))
	(message "Preprocessor buffer not active.")
      (if (string= (buffer-name (current-buffer)) ciaoppbuffname)
	  ()
	(switch-to-buffer-other-window ciaoppbuffname))
      (save-excursion
	(let ((mbeg 0) (mend 0) (file nil))
	  (goto-char (point-max))
	  (move-to-column 0) ;; skip prompt if at prompt
;;   	  (search-backward-regexp ciao-any-prompt-pattern nil t)
;; It is safe (and more precise) to be more specific here:
	  (search-backward-regexp ciao-ciaopp-prompt-pattern nil t)
	  (end-of-line)
	  (if (search-forward-regexp "written file " nil t)
	      (progn
		(setq mbeg (match-end 0))
		(goto-char mbeg)
		(search-forward-regexp "}")
		(setq mend (match-beginning 0))
		(setq file (buffer-substring-no-properties mbeg mend))
		(if (get-file-buffer file)
		    ;; The complication is to not complain if disk more recent!
		    (progn 
		      (switch-to-buffer (get-file-buffer file))
		      (let ((local-buff-point (point)))
			(kill-buffer (get-file-buffer file))
			(find-file file)
			(goto-char local-buff-point)))
		  (find-file file)
		  ))
	    (message "No output file written out by preprocessor.")
	    ;; If not output to visit, get cursor back to original buffer
	    (if (not (eq origbuffer (current-buffer)))
		(switch-to-buffer-other-window origbuffer))
	    ))))))

(defun ciao-ciaopp-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (if (buffer-name (process-buffer proc))
      (save-excursion
	(set-buffer (process-buffer proc))
	(comint-output-filter proc string)
	
	;; Used for ciaopp hooks
	(ciao-ciaopp-if-prompt-run-hook string))))

;;------------------------------------------------------------
;; Compiler/Top-level, file based.
;;------------------------------------------------------------

(defun ciao-make-exec ()
  "Make an executable from the code in the current buffer. The buffer
must contain a @pred{main/0} or @pred{main/1} predicate. Note that
compiler options can be set to determine whether the libraries and
auxiliary files used by the executable will be statically linked,
dynamically linked, auto-loaded, etc."
  (interactive)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "make_exec('" (buffer-file-name) "',_)." 
;; This was useful but now 'make_exec(FILE,_)' works (better!)
;; 	   (substring (buffer-name) 0 (string-match ".pl" (buffer-name))) 
;; 	   "')." 
	   )
   t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t))
  )

(defun ciao-make-po ()
  "Make a Prolog object (.po) file from the code in the current
buffer.  This is useful for example while debugging during development
of a very large application which is compiled into an excutable, and
only one or a few files are modified. If the application executable is
dynamically linked, i.e., the component .po files are loaded
dynamically during startup of the application, then this command can
be used to recompile only the file or files which have changed, and
the correct version will be loaded dynamically the next time the
application is started. However, note that this must be done with care
since it only works if the inter-module interfaces have not changed.
The recommended, much safer way is to generate the executable again,
letting the Ciao compiler, which is inherently incremental, determine
what needs to be recompiled."
  (interactive) 
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command
   ciao-toplevel-buffer-name 
   (concat "make_po('" (buffer-file-name) "').") t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t))
  )

(defun ciao-make-activemod ()
  "Make an active module executable from the code in the current
buffer. An active module is a remote procedure call server (see the
@lib{activemod} library documentation for details)."
  (interactive)
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "make_actmod('" (buffer-file-name) "','" 
    (read-string "Address publishing method: " 
	         "actmods/filebased_publish")
           "')." )
   t)
  (if ciao-locate-errors-after-run
      (add-hook 'ciao-prompt-inferior-hook 
		'ciao-launch-find-last-run-errors-from-orig-buffer t))
  )

;;------------------------------------------------------------
;; Loading
;;------------------------------------------------------------

;; This is, as so many other things, an approximation...
(defun ciao-get-module-name ()
  (save-excursion 
    (beginning-of-buffer)
    (let ((mbeg 0) (mend 0) (module-name nil))
      (setq module-name
	    (if (eq (search-forward-regexp 
		     "^[ \t]*:-[ \t\n]*\\(module\\|class\\)([ \t\n]*" 
		     20000 t) nil)
		"user"
	      (goto-char (match-end 0))
	      (setq mbeg (match-end 0))
	      (search-forward-regexp "[ \t\n]*\\(,\\|)\\)")
	      (setq mend (match-beginning 0))
	      (goto-char (match-beginning 0))
	      (buffer-substring-no-properties mbeg mend)))
      (if (eq (string-match "_" module-name) 0)
	  ;; if _ take the file name
	  (file-name-nondirectory 
	   (file-name-sans-extension
	    (buffer-file-name (current-buffer))))
	;; else, module-name, but eliminate quotes if they appear
	(ciao-replace-regexp-in-string 
	 "'[ \t\n]*$" "" 
	 (ciao-replace-regexp-in-string "^[ \t\n]*'" "" module-name)))
      )))

(defun ciao-load-command (filename delfile)
  (let (command)
    (save-excursion 
      (find-file filename)
      (beginning-of-buffer)
      (setq command 
	    (concat
	     (if (string= (ciao-get-module-name) "user")
		 "ensure_loaded('"
	       (beginning-of-buffer)
	       (if (eq (search-forward-regexp 
			"^[ \t]*:-[ \t\n]*class([ \t\n]*" 10000 t) nil)
		   "use_module('"
		 (if ciao-objects-lib-loaded
		     "use_class('"
		   (setq ciao-objects-lib-loaded 't)
		   "use_package(objects).\nuse_class('")))
;; 	     (if (boundp 'xemacs-logo)
;; 		 (replace-in-string filename "\\\\" "\\\\" t)
;; 	       (replace-regexp-in-string "\\\\" "\\\\" filename t t))
	     (ciao-replace-regexp-in-string "\\\\" "\\\\" filename t t)
	     "')."))
      (if (eq delfile nil)
	  command
	(kill-buffer (buffer-name))
	command)
      )))

(defun ciao-load-buffer ()
  "Load the current buffer (and any auxiliary files it may use) into the
top level. 

The type of compilation performed (@index{compiling} or
@index{interpreting}) is selected automatically depending on whether the
buffer has been marked for debugging or not -- see below. In case you try
to load a file while in the middle of the debugging process the debugger is
first aborted and then the buffer is loaded. Also, if there is a defined
query, the user is asked whether it should be called."
  (interactive)
  (ciao-unmark-last-run-errors)
  (ciao-load-buffer-current-or-main nil))

(defun ciao-load-from-main-module ()
  "Load the module designated as @index{main module} (and all related files
that it uses) into the top level. If no main module is defined it will load
the current buffer. 

The type of compilation performed (@index{compiling} or
@index{interpreting}) is selected automatically depending on whether
the buffer has been marked for debugging or not -- see below. In case
you try to load a file while in the middle of the debugging process
the debugger is first aborted and then the buffer is loaded. Also, if
there is a defined query, the user is asked whether is should be 
called."
  (interactive)
  (ciao-unmark-last-run-errors)
  ;; Load current if main buffer undefined
  (if (string= ciao-main-filename "")
      (ciao-load-buffer-current-or-main nil)
    ;; Else, load main
    (ciao-load-buffer-current-or-main t)))

(defun ciao-load-buffer-current-or-main (main)
  (setq ciao-last-source-buffer-used (current-buffer))
  (if (eq (comint-check-proc (get-buffer 
			      (concat "*" ciao-toplevel-buffer-name "*"))) nil)
      ;; If Ciao/Prolog buffer doesn't exist then go directly to load
      (ciao-real-load-buffer-current-or-main main)
    ;; Abort while debugging and then continue the normal process
    (let ((column
           (save-excursion
             (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
             (end-of-buffer)
             (current-column))))
      (if (< column 10)
	  (ciao-real-load-buffer-current-or-main main)
	(add-hook 'ciao-prompt-inferior-hook 'ciao-enable-trace t)
	(if main
	    (add-hook 'ciao-prompt-inferior-hook 
		      'ciao-real-load-from-main-module t)
	  (add-hook 'ciao-prompt-inferior-hook 'ciao-real-load-buffer t))
	(ciao-send-command ciao-toplevel-buffer-name "a" t)))))

(defun ciao-real-load-buffer ()
  "This function really loads the buffer. And in case a default query has been
defined it asks the user if this query should be called."
  (interactive)
  (ciao-real-load-buffer-current-or-main nil))

(defun ciao-real-load-from-main-module ()
  (interactive)
  (ciao-real-load-buffer-current-or-main t))

(defun ciao-real-load-buffer-current-or-main (main)
;; SEE ABOVE
;;  ;; Uncolor previous error if there was any 
;;   (if ciao-previous-error
;;       (progn
;; 	(get-file-buffer (car (cdr (cdr ciao-previous-error))))
;; 	(if (> (car ciao-previous-error) 0)
;; 	    (ciao-uncolor (car ciao-previous-error)
;; 			  (car (cdr ciao-previous-error))
;; 			  'ciao-error))
;; 	(setq ciao-previous-error nil)))
;;   (if ciao-inferior-error
;;       (save-excursion
;; 	(switch-to-buffer (concat "*" ciao-toplevel-buffer-name "*"))
;; 	(ciao-uncolor ciao-inferior-error
;; 		      ciao-inferior-error
;; 		      'ciao-error)
;; 	(setq ciao-inferior-error nil)))
  (if main
      (ciao-send-command ciao-toplevel-buffer-name
		       (ciao-load-command ciao-main-filename nil) t)
    ;;                 (concat "use_module('" ciao-main-filename "').") t)
    (ciao-send-command ciao-toplevel-buffer-name 
		       (ciao-load-command (buffer-file-name) nil) t))
  (add-hook 'ciao-prompt-inferior-hook 'ciao-errors-or-load-query t))


(defun ciao-errors-or-load-query ()
  (if ciao-locate-errors-after-run
      (ciao-launch-find-last-run-errors-from-orig-buffer))
  (if (or (string= ciao-query "") (ciao-any-errors))
      t
    (ciao-load-query)))
  
(defun ciao-load-query ()
  "Issue predefined query."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name ciao-query t))

(defun ciao-load-query-ask ()
  "Issue predefined query (asking the user first)."
  (interactive)
  (if (y-or-n-p (concat "Do you wish call the query '" ciao-query "'? "))
      (ciao-send-command ciao-toplevel-buffer-name ciao-query t)
    t))

(defun ciao-load-region (start end)
  "Load the current region (between the cursor and a previous mark)
into the top level. Since loading a region of a file is typically done
for debugging and/or testing purposes, this command always loads the
region in debugging mode (interpreted)." 
  (interactive "r")
  (ciao-write-region start end (ciao-last-temp-code-file))
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "debug_module(user), ensure_loaded('" ciao-last-temp-file "')." ) t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-del-temp-files t))

(defun ciao-load-predicate ()
  "Load the predicate around the cursor into the top level. Since loading a 
single predicate is typically done for debugging and/or testing purposes,
this command always loads the predicate in debugging mode (interpreted)."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-load-region (car boundaries) (cdr boundaries))))

(defun ciao-select-debug-mode ()
  "Mark, or unmkark, the current buffer for debugging (traditional
debugging or source debugging). Note that if the buffer has already been
loaded while it was unmarked for debugging (and has therefore been loaded
in ``compile'' mode) it has to be loaded again. The minibuffer shows how
the module is loaded now and allows selecting another mode for it. There
are three posibilities: N for no debug, S for source debug and D for
traditional debug."
  (interactive)
  (add-hook 'ciao-prompt-emacs-hook 'ciao-real-select-debug-mode t)
  (ciao-send-command ciao-toplevel-buffer-name "display_debugged." t))

(defun ciao-real-select-debug-mode (&optional list)
  (let ((end 0) 
	(buffers-debug)
	(module (ciao-module-name))
	(actually "N")
	(string)
	(default)
	(option))
    (if list
	(setq buffers-debug list)
      (setq buffers-debug (ciao-how-debugged)))
    (if (string-match (concat "\\<" module "\\>") (car buffers-debug))
	(setq actually "D"))
    (if (string-match (concat "\\<" module "\\>") (cdr buffers-debug))
	(setq actually "S"))
    (cond ((string= actually "N")
	   (setq string "Module not selected for debug. ")
	   (setq default "S"))
	  ((string= actually "D")
	   (setq string "Module selected for trad debug. ")
	   (setq default "N"))
	  ((string= actually "S")
	   (setq string "Module selected for source debug. ")
	   (setq default "N")))
    (setq string (concat string "Select debug mode (N/S/D)? "))
    (setq option
 	  (read-string string default nil))
    (if (string= option "") (setq option default))
    ;; Was simply:  (but xemacs does not support the last argument)
    ;;	  (read-string string default nil default))
    ;; Send the apropiate command to Ciao
    (cond ((and (or (string= actually "N")
		    (string= actually "S"))
		(string= option "D"))
	   (ciao-send-command ciao-toplevel-buffer-name
			      (concat "debug_module('" module "').") t))
	  ((and (or (string= actually "N")
		    (string= actually "D"))
		(string= option "S"))
	   (ciao-send-command ciao-toplevel-buffer-name
			      (concat "debug_module_source('" module "').") t))
	  ((and (or (string= actually "S")
		    (string= actually "D"))
		(string= option "N"))
	   (ciao-send-command ciao-toplevel-buffer-name
			      (concat "nodebug_module('" module "').") t)))))

(defun ciao-mark-buffer-source-debug ()
  "Mark a module for source debug."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "debug_module_source('" (ciao-module-name)"').")
   t))

(defun ciao-un-mark-buffer-debug ()
  "Unmark a module for debug."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name
   (concat "nodebug_module('" (substring (buffer-name) 0 -3) "').") t))

(defun ciao-enable-trace ()
  "Set the debugger to the trace state. In this state, the program is
executed step by step."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "trace." t))

(defun ciao-enable-debug ()
  "Set the debugger to the debug state. In this state, the program will
only stop in breakpoints and spypoints. Breakpoints are specially supported
in @apl{emacs} and using source debug."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "debug." t))

(defun ciao-no-debug ()
  "Set the debugger to the no debug state. In this state, the program will
execute until the end, without stopping in any step of the program."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "nodebug." t))

(defun ciao-debug-buffer ()
  "Debug (or stop debugging) buffer source. This is a shortcut which
is particularly useful when using the source debugger on a single
module. It corresponds to several lower-level actions.  Those
lower-level actions depend on how the module was selected for
debugging. In case the module was not marked for source-level
debugging, it marks the module corresponding to the current buffer for
source-level debugging, reloads it to make sure that it is loaded in
the correct way for debugging (same as \\<ciao-mode-map>
\\[ciao-load-buffer]), and sets the debugger in trace mode (i.e.,
issues the @tt{trace.} command to the top-level shell). Conversely, if
the module was already marked for source-level debugging then it will
take the opposite actions, i.e., it unmarks the module for
source-level debugging, reloads it, and sets the debugger to non-debug
mode."
  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "display_debugged." t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-real-debug-buffer t))

(defun ciao-real-debug-buffer ()
  (interactive)
  (let ((end 0) 
	(buffers-debug)
	(module (ciao-module-name))
	(actually "N"))
    (setq buffers-debug (cdr (ciao-how-debugged)))
    (if (string-match (concat "\\<" module "\\>") buffers-debug)
	(setq actually "S"))
    (cond ((string= actually "S")
	   ;; Buffer is marked for source debug
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-no-debug t)
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-load-buffer t)
	   (ciao-un-mark-buffer-debug))
	  ((string= actually "N")
	   ;; Buffer is marked for traditional debug or not marked for
	   ;; debug.
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-enable-trace t)
	   (add-hook 'ciao-prompt-inferior-hook 'ciao-load-buffer t)
	   (ciao-mark-buffer-source-debug)))))

(defun ciao-module-name ()
  (let ((module-name (ciao-get-module-name)))
    (if (and (> (length module-name) 3)
             (or (string= (substring module-name -3) ".pl")
	         (string= (substring module-name -4) ".pls")
	         (string= (substring module-name -4) ".cgi")))
	(file-name-sans-extension module-name)
      module-name)))

(defun ciao-select-buffers-for-debug ()
  "Visits all Ciao/Prolog files which are currently open in a buffer
allowing selecting for each of them whether to debug them or not and
the type of debugging performed. When working on a multiple module
program, it is possible to have many modules open at a time. In this
case, you will navigate through all open Ciao/Prolog files and select
the debug mode for each of them (same as doing \\<ciao-mode-map>
\\[ciao-select-debug-mode] for each)."

  (interactive)
  (ciao-send-command ciao-toplevel-buffer-name "display_debugged." t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-real-select-buffers-for-debug
	    t))

(defvar ciao-buffers nil)

(defun ciao-real-select-buffers-for-debug ()
  (interactive)
  (let* ((buffers (ciao-how-debugged))
	 (ciao-select-ciao-buffers
	  (function (lambda (buffer)
		      (set-buffer buffer)
		      (if (eq major-mode 'ciao-mode)
			  (setq ciao-buffers (cons buffer ciao-buffers))))))
	 (select-debug-module 
	  (function (lambda (buffer)
		      (set-buffer buffer)
		      (switch-to-buffer buffer t)
		      (ciao-real-select-debug-mode buffers))))
	 module)

    (if (not ciao-buffers)
	(mapcar ciao-select-ciao-buffers (buffer-list)))
    
    (setq module (car ciao-buffers))
    (setq ciao-buffers (cdr ciao-buffers))
    (funcall select-debug-module module)
    (if ciao-buffers
	(add-hook 'ciao-prompt-inferior-hook
		  'ciao-real-select-buffers-for-debug t))))

(defun ciao-how-debugged ()
  "Return a pair containning buffers selected for traditional debug and
buffers selected for source debug."
  (interactive)
  (let (buffers-debug end)
    (save-excursion
      (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
      (search-backward "display_debugged.")
      ;; Match all tradicional debugged modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      ;; (beginning-of-line)
      (move-to-column 0)
      (if (search-forward-regexp "\\[\\(.*\\)\\]" end t)
	  (setq buffers-debug (funcall ciao-match-string 1))
	(setq buffers-debug ""))
      ;; Match all source debug modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      ;; (beginning-of-line)
      (move-to-column 0)
      (if (search-forward-regexp "\\[\\(.*\\)\\]" end t)
	  (setq buffers-debug (cons buffers-debug 
				    (funcall ciao-match-string 1)))
	(setq buffers-debug (cons buffers-debug ""))))))
  
;;------------------------------------------------------------
;; Traditional commands: Consulting
;;------------------------------------------------------------
;; These and the following commands reuse the same temp file, which is
;; left at /tmp in the end. This eliminates the  need for
;; synchronization with the Prolog process, which is complicated by
;; the SICStus "The procedure xxx/yyy is being redefined" messages
;; (but unfortunately leaves  garbage behind, in the same way as the
;; ususal prolog.el mode).

(defun ciao-consult-buffer ()
  "Consult the entire buffer."
  (interactive)
  (ciao-send-command 
   ciao-toplevel-buffer-name (concat "consult('" (buffer-file-name) "')." )
   t))

(defun ciao-consult-region (start end)
  "Consult a given region."
   (interactive "r")
  (ciao-write-region start end (ciao-last-temp-code-file))
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "consult('" ciao-last-temp-file "')." ) t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-del-temp-files t))

(defun ciao-consult-predicate ()
  "Consult the predicate around point."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-consult-region (car boundaries) (cdr boundaries))))

;;------------------------------------------------------------
;; Traditional commands: Compiling
;;------------------------------------------------------------

(defun ciao-compile-buffer ()
  "Compile the entire buffer."
  (interactive)
  (ciao-send-command 
   ciao-toplevel-buffer-name (concat "compile('" (buffer-file-name) "')." )
   t))

(defun ciao-compile-region (start end)
  "Compile a given region."
   (interactive "r")
  (ciao-write-region start end (ciao-last-temp-code-file))
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "compile('" ciao-last-temp-file "')." ) t)
  (add-hook 'ciao-prompt-inferior-hook 'ciao-del-temp-files t))

;; PO 890606
(defun ciao-compile-predicate ()
  "Compile the predicate around point."
  (interactive)
  (let ((boundaries (predicate-boundaries)))
    (ciao-compile-region (car boundaries) (cdr boundaries))))

;; Original version: JA 890531
;; (defun build-ciao-command (commstring)
;;   (concat "ciao:zap_file('"
;;   (concat "zap_file('"
;; 	  (ciao-temp-code-file) "', '"
;; 	  (or (buffer-file-name) "user") "', " commstring ")."))

;;------------------------------------------------------------
;; Region handling
;;------------------------------------------------------------

;; MH save-excursion
;; Must be improved. Cannot handle predicates with clauses
;; separated by newlines...
;; PO 890606
(defun predicate-boundaries ()
  ;; Find "beginning" of predicate
  (beginning-of-line)
  (save-excursion 
    (while (and (not (looking-at "\n")) (not (bobp)))
      (forward-line -1)
      (skip-chars-forward " \t"))
    (let ((start (point)))

	 ;; Find "end" of predicate
	 (forward-line 1)
	 (skip-chars-forward " \t")
	 (while (and (not (looking-at "\n")) (not (eobp)))
	   (forward-line 1)
	   (skip-chars-forward " \t"))
	 (cons start (point)))))

(defun ciao-write-region (minpoint maxpoint filename)
  (let (original-buffer buffercont temp-buffer)
    (setq original-buffer (current-buffer))
    (setq buffercont (buffer-substring-no-properties minpoint maxpoint))
    (setq temp-buffer (generate-new-buffer "temp-buffer"))
    (set-buffer temp-buffer)
    (insert buffercont "\n")
    (write-region (point-min) (point-max) filename nil nil)
    (kill-buffer temp-buffer)
    (set-buffer original-buffer)))

(defun ciao-del-temp-files () 
  (delete-file-if-possible ciao-last-temp-file)
  (delete-file-if-possible (concat ciao-last-temp-file ".dep"))
  (delete-file-if-possible (concat ciao-last-temp-file ".err"))
  (delete-file-if-possible (concat ciao-last-temp-file ".asr"))
  (delete-file-if-possible (concat ciao-last-temp-file ".itf"))
  (delete-file-if-possible (concat ciao-last-temp-file ".po")))

(defun delete-file-if-possible (file)
  (if (and (file-exists-p file) (file-writable-p file))
      (delete-file file)
    nil))

;; M.H. 
;; In distributed execution, need to halt siblings...
;; (setq kill-buffer-hook 'ciao-halt-process)
(defun ciao-halt-process ()
  (if (not (comint-check-proc 
	    (concat "*" ciao-toplevel-buffer-name "*"))) 
      ()
    (progn 
      (process-send-string ciao-toplevel-buffer-name "halt.")
      (sleep-for 2))
    ))

;;------------------------------------------------------------
;; Commands related to the source code debugger
;;------------------------------------------------------------

(defun ciao-debug-display-frame (buffname)
  (interactive)
  (if ciao-debug-last-frame
      (progn
	;; (ciao-debug-set-buffer)
	(let ((port    (car ciao-debug-last-frame))
	      (file    (car (cdr ciao-debug-last-frame)))
	      (l0      (car (cdr (cdr ciao-debug-last-frame))))
	      (l1      (car (cdr (cdr (cdr ciao-debug-last-frame)))))
	      (numpred (car (cdr (cdr (cdr (cdr ciao-debug-last-frame))))))
	      (pred    (cdr (cdr (cdr (cdr (cdr ciao-debug-last-frame)))))))

	  ;; (setq file (ciao-debug-transform-file-name file))
	  (ciao-debug-display-line file l0 l1 pred numpred port buffname) 
	  (setq ciao-debug-last-frame nil)))))

(defun ciao-debug-display-line (file start end pred numpred port buffname)
  (let* ((count 0) (init 0) (finish 0) (test t) (pos 0)
	 (last-nonmenu-event t)  ; Prevent use of dialog box for questions.
	 ;; Problem for embedded debugger
	 (buffer
	  (save-excursion
	    (or (string= (buffer-name) buffname) ; was (current-buffer) and eq!
		(set-buffer buffname))
	    (ciao-debug-find-file file)))
	 (window (and buffer (or (get-buffer-window buffer)
				 (display-buffer buffer)))))

    ; Unmark the last region marked
    (ciao-debug-uncolor-line)

    (if buffer
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      ;; (goto-line start)
	      ;; Due to impression in detecting the start line of a clause
	      ;; we move to the end and clause and then search backward
	      ;; until find the beginning of the clause.
	      (goto-line end)
	      (end-of-line)
	      (re-search-backward "^[a-z']" (point-min) t)

	      ;; Search the numpred-th pred and put the marker at the
	      ;; beginning of the line. Doesn't consider PRED in
	      ;; comment
 	      (end-of-line)
 	      (setq finish (point))
 	      (beginning-of-line)
 	      (setq init (point))
 	      (while (and test (not (eq count numpred)))
 		(while (and test (not (search-forward pred finish t)))
 		  (forward-line)
		  (if (or (< end (ciao-what-line))
			  (and (eq init (point)) (eq (point) finish)))
		      (setq test nil))
 		  (end-of-line)
 		  (setq finish (point))
 		  (beginning-of-line)
 		  (setq init (point)))
 		;; Found a PRED, search if it is in a comment
 		(if (and test (not (search-backward "%" init t)))
 		    (setq count (+ count 1))
 		  (forward-line)
 		  (end-of-line)
 		  (setq finish (point))
 		  (beginning-of-line)
 		  (setq init (point))))
	      
	      (if (< count numpred) 
		  ;; Not found pred, overlay the whole region
		  (progn
		    (setq overlay-arrow-string "")
		    (goto-line end)
		    (end-of-line)
		    (re-search-backward "^[a-z']" (point-min) t)
		    (ciao-color (ciao-what-line)
				end
				ciao-face-debug-expansion
				'ciao-debug)
		    ;; Save information for uncoloring the last line
		    (setq ciao-debug-last-line
			  (cons (current-buffer)
				(ciao-what-line)))

		    )
		;; Save information for uncoloring the last line
		(setq ciao-debug-last-line
		      (cons (current-buffer)
			    (ciao-what-line)))
		
		;; Color line
		(ciao-color (ciao-what-line)
			    (ciao-what-line)
			    (ciao-debug-obtain-color port)
			    'ciao-debug)
		(setq overlay-arrow-string (ciao-debug-transform-port port))

		)
	      ;; Arrow position
	      (beginning-of-line)
	      (setq pos (point))
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))

(defun ciao-what-line ()
  "Return the line number. This function is a fix for the fact that in
xemacs the function what-line does not behave as in emacs."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))
	 
(defun ciao-debug-transform-port (port)
  "Arrow to show in source file. It's determines from PORT."
  (cond ((string= "Call" port) "C=>")
	((string= "Exit" port) "E=>")
	((string= "Fail" port) "F=>")
	((string= "Redo" port) "R=>")))

(defun ciao-debug-obtain-color (port)
  (cond ((string= "Call" port) ciao-face-debug-call)
	((string= "Exit" port) ciao-face-debug-exit)
	((string= "Fail" port) ciao-face-debug-fail)
	((string= "Redo" port) ciao-face-debug-redo)))

(defun ciao-debug-uncolor-line ()
  (if ciao-debug-last-line
      (save-excursion
	(set-buffer (car ciao-debug-last-line))
	(ciao-uncolor (cdr ciao-debug-last-line)
		      (cdr ciao-debug-last-line)
		      'ciao-debug))))
  
(defun ciao-debug-remove-marks ()
  (ciao-debug-uncolor-line)
  (setq overlay-arrow-position nil))

(defun ciao-debug-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if ciao-debug-filter-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later
	    (setq ciao-debug-filter-pending-text 
		  (concat (or ciao-debug-filter-pending-text "") string))

	  (let ((ciao-debug-filter-defer-flag t))
	    ;; Process now any text we previously saved up
	    (if ciao-debug-filter-pending-text
		(setq string (concat ciao-debug-filter-pending-text string)
		      ciao-debug-filter-pending-text nil))
	    (save-excursion
	      (set-buffer (process-buffer proc))
	      ;; If we haven been so requested, delete the debugger prompt.
	      (if (marker-buffer ciao-debug-delete-prompt-marker)
		  (progn
		    (delete-region (process-mark proc)
				   ciao-debug-delete-prompt-marker)
		    (set-marker ciao-debug-delete-prompt-marker nil)))
	      
	      ; Here we obtain the output to show in the buffer
	      (setq output (ciao-debug-marker-filter string))
	      
	      (setq process-window
		    (and ciao-debug-last-frame
			 (>= (point) (process-mark proc))
			 (get-buffer-window (current-buffer))))

	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
	      (comint-output-filter proc output))

	      (add-hook 'ciao-prompt-emacs-hook 'ciao-debug-remove-marks t)

	      (ciao-if-prompt-run-hook output)

	    ;; Put the arrow on the source line.
	    ;; This must be outside of the save-excursion 
	    ;; in case the source file is our current buffer.
	    (if process-window
		(save-selected-window
		 (select-window process-window)
		 (ciao-debug-display-frame (buffer-name)))   
	      ;; We have to be in the proper buffer, (process-buffer proc),
	      ;; but not in a save-excursion, because that would restore
	      ;; point.
	      (let ((old-buf (current-buffer)))
		(set-buffer (process-buffer proc))
		(unwind-protect
		    (ciao-debug-display-frame (buffer-name))
		  (set-buffer old-buf)))))
	  ;; If we deferred text that arrived during this processing
	  ;; handle it now.
	  (if ciao-debug-filter-pending-text
	      (ciao-debug-filter proc "")))))) 
	  
(defun ciao-debug-find-file (file)
  (save-excursion
    (let ((buf (find-file-noselect (fix-cygwin-drive-letter file))))
      (set-buffer buf)
      buf)))

(defun ciao-debug-marker-filter (string)
  "Search the string for the debugging information"
  (setq ciao-debug-marker-acc (concat ciao-debug-marker-acc string))
  (let ((output ""))
    ; Process all the complete markers in this chunk
    (while (string-match ciao-debug-marker-regexp ciao-debug-marker-acc)
      (setq
       ;; Extract the frame position from the marker
       ciao-debug-last-frame
       (cons (substring ciao-debug-marker-acc (match-beginning 6)
			(match-end 6))
	     (cons (substring ciao-debug-marker-acc 
			      (match-beginning 1) (match-end 1))
	     (cons (string-to-int (substring ciao-debug-marker-acc
				       (match-beginning 2) (match-end 2)))
	     (cons (string-to-int (substring ciao-debug-marker-acc
				       (match-beginning 3) (match-end 3)))
	     (cons (string-to-int (substring ciao-debug-marker-acc
				       (match-beginning 5) (match-end 5)))
		   (substring ciao-debug-marker-acc 
			      (match-beginning 4) (match-end 4)))))))
	            
       ;; Append Any Text Before the marker to the output we're going to
       ;; return - we don't include the marker in this text
       output (concat output 
		      (substring ciao-debug-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text
       ciao-debug-marker-acc (substring ciao-debug-marker-acc (+ (match-end
							       5) 1))))

    ;; Does the remaining text look like it might end with the beginning of
    ;; another marker? If it does, the keep it in ciao-debug-marker until
    ;; we receive the rest of it. Since we know the full marker regexp
    ;; above failed, it's pretty simple to test for marker starts.
    (if (string-match "         In " ciao-debug-marker-acc)
	(progn
	  ;; Everything before the potential marker start can be output
	  (setq output (concat output (substring ciao-debug-marker-acc 0
						 (match-beginning 0))))
	  (setq ciao-debug-marker-acc (substring ciao-debug-marker-acc
						 (match-beginning 0))))
      (setq output (concat output ciao-debug-marker-acc)
	    ciao-debug-marker-acc ""))
    output))

;; Remember to check ciao-ciaopp-if-prompt-run-hook and
;; ciao-lpdoc-if-prompt-run-hook in case of any modification
(defun ciao-if-prompt-run-hook (string)
  (let (hook)
    (setq ciao-prompt-marker-acc (concat ciao-prompt-marker-acc string))
    (if (string-match ciao-prompt-pattern ciao-prompt-marker-acc)
	(progn
	  ;; We found a prompt then remove it from accumulator so don't call
	  ;; again hook.
	  ;; Wrong. Search until last \n or \n\\?-
	  (setq ciao-prompt-marker-acc 
		(substring ciao-prompt-marker-acc (match-end 0)))
	  (if ciao-prompt-inferior-hook
	      (progn
		(setq hook (car ciao-prompt-inferior-hook))
		(setq ciao-prompt-inferior-hook 
		      (cdr ciao-prompt-inferior-hook))
		(funcall hook))
	    (run-hooks 'ciao-prompt-emacs-hook)
	    (setq ciao-prompt-emacs-hook nil))))))

;; Remember to check ciao-if-prompt-run-hook and
;; ciao-lpdoc-if-prompt-run-hook in case of any modification
(defun ciao-ciaopp-if-prompt-run-hook (string)
  (let (hook)
    (setq ciao-ciaopp-prompt-marker-acc 
	  (concat ciao-ciaopp-prompt-marker-acc string))
;; Added one case:
    (if (or 
	 (string-match ciao-ciaopp-prompt-pattern
		       ciao-ciaopp-prompt-marker-acc)
	 (string-match "\nCiao/Prolog/LPdoc Listener finished"
		       ciao-ciaopp-prompt-marker-acc))
	(progn
	  ;; We found a prompt then remove it from accumulator so don't call
	  ;; again hook.
	  ;; Wrong. Search until last \n or \n\\?-
	  (setq ciao-ciaopp-prompt-marker-acc 
		(substring ciao-ciaopp-prompt-marker-acc (match-end 0)))
	  (if ciao-ciaopp-prompt-inferior-hook
	      (progn
		(setq hook (car ciao-ciaopp-prompt-inferior-hook))
		(setq ciao-ciaopp-prompt-inferior-hook 
		      (cdr ciao-ciaopp-prompt-inferior-hook))
		(funcall hook))
	    (run-hooks 'ciao-ciaopp-prompt-emacs-hook)
	    (setq ciao-ciaopp-prompt-emacs-hook nil))))))

;; Remember to check ciao-if-prompt-run-hook and
;; ciao-ciaopp-if-prompt-run-hook in case of any modification
(defun ciao-lpdoc-if-prompt-run-hook (string)
  (let (hook)
    (setq ciao-lpdoc-prompt-marker-acc 
	  (concat ciao-lpdoc-prompt-marker-acc string))
    (if (string-match ciao-lpdoc-prompt-pattern ciao-lpdoc-prompt-marker-acc)
	(progn
	  ;; We found a prompt then remove it from accumulator so don't call
	  ;; again hook.
	  ;; Wrong. Search until last \n or \n\\?-
	  (setq ciao-lpdoc-prompt-marker-acc 
		(substring ciao-lpdoc-prompt-marker-acc (match-end 0)))
	  (if ciao-lpdoc-prompt-inferior-hook
	      (progn
		(setq hook (car ciao-lpdoc-prompt-inferior-hook))
		(setq ciao-lpdoc-prompt-inferior-hook 
		      (cdr ciao-lpdoc-prompt-inferior-hook))
		(funcall hook))
	    (run-hooks 'ciao-lpdoc-prompt-emacs-hook)
	    (setq ciao-lpdoc-prompt-emacs-hook nil))))))

(defun ciao-inferior-process-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Need to reload certain things if needed.
	 (setq ciao-objects-lib-loaded nil)
	 (setq ciao-assrt-lib-loaded nil)
	 (setq ciao-inferior-error nil)
	 ;; (setq ciao-error nil)
	 (setq ciao-debug-filter-pending-text "")

	 ;; Stop displaying an arrow in a source file.
	 (ciao-debug-remove-marks)

	 ;; Reset stuff needed for prompt hook in ciao, ciaopp and lpdoc
	 (setq ciao-prompt-emacs-hook nil)
	 (setq ciao-prompt-inferior-hook nil)
	 (setq ciao-prompt-marker-acc "")
	 (setq ciao-ciaopp-prompt-emacs-hook nil)
	 (setq ciao-ciaopp-prompt-inferior-hook nil)
	 (setq ciao-ciaopp-prompt-marker-acc "")
	 (setq ciao-lpdoc-prompt-emacs-hook nil)
	 (setq ciao-lpdoc-prompt-inferior-hook nil)
	 (setq ciao-lpdoc-prompt-marker-acc "")

	 (set-process-buffer proc nil))

	((memq (process-status proc) '(signal exit))
	 ;; Need to reload certain things if needed.
	 (setq ciao-objects-lib-loaded nil)
	 (setq ciao-assrt-lib-loaded nil)
	 (setq ciao-inferior-error nil)
	 ;; (setq ciao-error nil)
	 (setq ciao-debug-filter-pending-text "")

	 ;; Stop displaying an arrow in a source file.
	 (ciao-debug-remove-marks)

	 ;; Reset stuff needed for prompt hook in ciao, ciaopp and lpdoc
	 (setq ciao-prompt-emacs-hook nil)
	 (setq ciao-prompt-inferior-hook nil)
	 (setq ciao-prompt-marker-acc "")
	 (setq ciao-ciaopp-prompt-emacs-hook nil)
	 (setq ciao-ciaopp-prompt-inferior-hook nil)
	 (setq ciao-ciaopp-prompt-marker-acc "")
	 (setq ciao-lpdoc-prompt-emacs-hook nil)
	 (setq ciao-lpdoc-prompt-inferior-hook nil)
	 (setq ciao-lpdoc-prompt-marker-acc "")

	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ":"
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 (force-mode-line-update)
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gud buffer.
	     (set-buffer obuf))))))

(defun ciao-debug-breakon ()
  "Set a breakpoint on the current literal (goal). This can be done at any
time (while debugging or not). The cursor must be @em{on the predicate
symbol of the literal}. Breakpoints are only useful when using source-level
debugging."
  (interactive)
  ;; In case we are debugging send a @ and then continue with the normal
  ;; process.
  
  (if (comint-check-proc 
	   (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (let ((column))
	(save-excursion
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq column (current-column)))
	(if (< column 6)
	    t
	  (ciao-send-command ciao-toplevel-buffer-name "@" t)
	  (sleep-for 0.01))))

  (ciao-color (ciao-what-line)
	      (ciao-what-line)
	      ciao-face-debug-breakpoint
	      'ciao-break)
  (ciao-send-command ciao-toplevel-buffer-name
		     (concat "breakpt(" (ciao-debug-breakparams (point))
			     ").") t))
  
(defun ciao-debug-breakoff ()
  "Remove a breakpoint from the current literal (goal). This can be done
at any time (while debugging or not). The cursor must be @em{on the predicate
symbol of the literal}."
  (interactive)
  ;; In case we are debugging send a @ and then continue with the normal
  ;; process.
  (if (comint-check-proc 
	   (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (let ((column))
	(save-excursion
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq column (current-column)))
	(if (< column 6)
	    t
	  (ciao-send-command ciao-toplevel-buffer-name "@" t)
	  (sleep-for 0.01))))
  
  (ciao-uncolor (ciao-what-line)
		(ciao-what-line)
		'ciao-break)
  (ciao-send-command ciao-toplevel-buffer-name
		     (concat "nobreakpt(" (ciao-debug-breakparams (point))
			     ").") t))

(defun ciao-debug-all-breakoff ()
  "Remove all breakpoints. This can be done at any time (while debugging
or not)."
  (interactive)
  ;; In case we are debugging send a @ and then continue with the normal
  ;; process.
  (if (comint-check-proc 
	   (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (let ((column))
	(save-excursion
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq column (current-column)))
	(if (eq column 3)
	    t
	  (ciao-send-command ciao-toplevel-buffer-name "@" t)
	  (sleep-for 0.01))))
  
  (ciao-send-command ciao-toplevel-buffer-name "nobreakall." t)
  (ciao-debug-uncolor-all-breakpt))
  
(defun ciao-debug-breakparams (point)
  (let* ((boundaries (ciao-debug-predicate-boundaries point))
	(pred-name (find-tag-default)) 
	(src-file (expand-file-name (buffer-name (current-buffer))))
	(begin-line (car boundaries))
	(end-line (cdr boundaries)) 
	(number 0)
	string)
    (save-excursion
      (goto-line begin-line)
      (while (< (point) point)
	(if (re-search-forward (concat "\\<" (regexp-quote pred-name) "\\>") nil nil)
	    (setq number (+ number 1)))))
    (concat  "'" pred-name "','" src-file "',"
			 (int-to-string begin-line) "," 
			 (int-to-string end-line) "," 
			 (int-to-string number) "," 
			 (int-to-string (ciao-what-line)))))

(defun ciao-debug-predicate-boundaries (point)
  (let ((start) 
	(bound)
	(begin)
	(test t))
    ;; Find the beginning of the predicate boundary
    (save-excursion
      (search-backward-regexp "^[^ \t]" 1 t)
      (setq start (ciao-what-line)))
    ;; Find the end of the predicate boundary
    (save-excursion 
      ;; Search line to line to establish limits
      (setq test t)
      (setq begin (point))
      (end-of-line)
      (setq bound (point))
      (goto-char begin)
      (while test
	(while (not (search-forward-regexp "\\.[ \t]*\\(%\\|$\\)" bound t))
	  (forward-line 1)
	  (setq begin (point))
	  (end-of-line)
	  (setq bound (point))
	  (goto-char begin))
	;; We reach this point just when find the regexp. Are we in a
	;; comment?
	(if (not (search-backward "%" begin t))
	    (setq test nil)
	  (forward-line 1)
	  (setq begin (point))
	  (end-of-line)
	  (setq bound (point))
	  (goto-char begin)))	  
      (cons start (ciao-what-line)))))

(defsubst ciao-color (startline endline color over)
  "Highlight region from STARTLINE to ENDLINE using COLOR with overlay name
OVER."
  (let (start end overlay)
    (save-excursion
      (goto-line startline)
      (setq start (point))
      (goto-line endline)
      (end-of-line)
      (if (or (eq over 'ciao-error) (eq over 'ciao-debug))
	  (setq end (+ (point) 1))
	(setq end (point))))
    (setq overlay (make-overlay start end))
    (overlay-put overlay 'face color)
    (overlay-put overlay over t)))

(defun ciao-uncolor (startline endline over)
  "Unhighlights the region from STARTLINE to ENDLINE with the overlay name
OVER."
  (let (start)
    (save-excursion
      (goto-line startline)
      (setq start (point)))
    (mapcar (function (lambda (ovr)
			(and (overlay-get ovr over) 
			     (delete-overlay ovr))))
	    (overlays-at start))))

(defun ciao-debug-uncolor-all-breakpt ()
  "Remove breakpoints color in all Ciao/Prolog files."
  (interactive)
  (save-excursion
    (mapcar (function (lambda (buffer)
			(set-buffer buffer)
			(if (eq major-mode 'ciao-mode)
			    (ciao-debug-uncolor-buffer))))
	    (buffer-list))))

(defun ciao-debug-uncolor-buffer ()
  "Remove faces breakpoint color in a ciao buffer"
  (let (beg end)
    (setq beg (point-min))
    (setq end (point-max))
    (mapcar (function (lambda (over)
			(and (overlay-get over 'ciao-break)
			     (delete-overlay over))))
	    (overlays-in beg end))))
  
(defun ciao-debug-display-breakpt ()
  "Redisplay breakpoints in all Ciao buffers. This ensures that the marks
in the source files and the Ciao/Prolog toplevel are synchronized."

  (interactive)
  (ciao-debug-uncolor-all-breakpt)
  (if (comint-check-proc 
       (get-buffer (concat "*" ciao-toplevel-buffer-name "*")))
      (progn
	(add-hook 'ciao-prompt-emacs-hook 
		  'ciao-debug-redisplay-breakpt t)
	(ciao-send-command ciao-toplevel-buffer-name "list_breakpt." t))))

(defun ciao-debug-redisplay-breakpt ()
    (let ((buffer (current-buffer)))
      (save-excursion
	(let ((file 0) (l0 0) (l1 0) (pred 0) (numpred 0) (bound 0))
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (setq bound (point))
	  (search-backward "list_breakpt.")
	  (while (search-forward-regexp 
		  (concat "Breakpoint in file \\(.*\\)" 
			  " \\([0-9]+\\)-\\([0-9]+\\) "
			  "on literal \\(.*\\)-\\([0-9]+\\)")
		  bound t)
	    (setq file (buffer-substring-no-properties (match-beginning 1)
						       (match-end 1))
		  l0 (string-to-int (buffer-substring-no-properties 
				     (match-beginning 2) (match-end 2)))
		  l1 (string-to-int (buffer-substring-no-properties 
				     (match-beginning 3) (match-end 3)))
		  pred (buffer-substring-no-properties (match-beginning 4)
						       (match-end 4))
		  numpred (string-to-int (buffer-substring-no-properties 
					  (match-beginning 5) (match-end 5))))
	    (save-excursion
	      (set-buffer (get-file-buffer file))
	      (goto-line l0)
	      ;; To change when considering comments in clause
	      (search-forward pred nil t numpred)
	      (ciao-color (ciao-what-line)
			  (ciao-what-line)
			  ciao-face-debug-breakpoint
			  'ciao-break)))))
    (switch-to-buffer buffer)))

;;------------------------------------------------------------
;; Generating documentation using LPdoc
;;------------------------------------------------------------

(defun ciao-visit-lpdoc-settings ()
  "Visit, or create, the @tt{SETTINGS} file (which controls all
auto-documenter options)."
  (interactive)
  (let ((libsettings (concat ciao-lpdoc-libpath "/lpdoc/SETTINGS"))
	(thisfile (buffer-name (current-buffer)))
	(docsettings (concat (ciao-lpdoc-buffer-tmpdir 
			      (buffer-name (current-buffer))) "/SETTINGS"))
	(sourcedir   (directory-file-name
		      (file-name-directory 
		       (buffer-file-name (current-buffer))))))
	(make-directory (ciao-lpdoc-buffer-tmpdir thisfile) t)
	(if (file-exists-p docsettings)
	    (find-file-other-window docsettings)
	  (copy-file libsettings docsettings t) 
	  (find-file-other-window docsettings)
	  (make-symbolic-link (concat ciao-lpdoc-libpath "/lpdoc/Makefile") 
			      (concat (ciao-lpdoc-buffer-tmpdir thisfile)  
				      "/Makefile") t)
	  (beginning-of-buffer)
	  (search-forward "FILEPATHS")
	  (search-forward "=")
	  (forward-char 1)
	  (insert-string sourcedir)
	  (insert-string " ")
	  (beginning-of-buffer)
	  (search-forward "SYSTEMPATHS")
	  (search-forward "=")
	  (forward-char 1)
	  (insert-string (concat ciao-real-lib-dir "/lib "))
	  (insert-string (concat ciao-real-lib-dir "/library "))
	  (beginning-of-buffer)
	  (search-forward "COMPONENTS")
	  (search-forward "=")
	  (forward-char 1)
	  ;; Just a kludge for now...
	  (kill-line)
	  (beginning-of-buffer)
	  (search-forward "MAIN")
	  (search-forward "=")
	  (forward-char 1)
	  (kill-line)
	  (insert-string thisfile)
	  (beginning-of-buffer)
	  (save-buffer)
	  )
	)
  )

(defun ciao-lpdoc-buffer-tmpdir (filename) 
  (let ((tmpdir (cdr (assoc filename ciao-lpdoc-buffer-tmpdir-list))))
    (if tmpdir
	tmpdir
      (setq tmpdir (ciao-new-temp-code-dir filename))
      (setq ciao-lpdoc-buffer-tmpdir-list
	    (cons 
	     (cons filename tmpdir)
	     ciao-lpdoc-buffer-tmpdir-list)))
      tmpdir
    ))

(defun ciao-gen-doc ()
  "Generate the documentation according to @tt{SETTINGS} in the
default format. This allows generating complex documents but it
assumes that @tt{SETTINGS} exists and that the options that it
contains (main file, component files, paths, etc.) have been set
properly. Documentation is generated in a temporary directory. Note
however that for generating complex manuals the best approach is to
set up a permanent documentation directory with the appropriate
@tt{SETTINGS} and @tt{Makefile} files (see the LPdoc manual)."
  (interactive)
  (message "Generating documentation... ")
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (let ((thisfile (buffer-name (current-buffer))))
    (if (not (file-exists-p 
	      (concat (ciao-lpdoc-buffer-tmpdir thisfile) "/SETTINGS")))
	(message "You need to first visit SETTINGS and perhaps choose options")
      ;; Not necessary and creates a problem: first time errors are
      ;; not found because sending command gets ahead of starting process
      ;; (ciao-ensure-inferior-process ciao-lpdoc-buffer-name)
      (ciao-send-command 
       ciao-lpdoc-buffer-name 
       (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " 
	       ciao-lpdoc-system " " ciao-lpdoc-docformat)
       t)
      (if ciao-locate-errors-after-run
	  (add-hook 'ciao-lpdoc-prompt-inferior-hook 
		    'ciao-launch-find-last-run-errors-from-orig-buffer t))
      ))
  (message "Generating documentation... done.")
  )

(defun ciao-gen-buffer-doc ()
  "Generate the documentation for the current buffer in the default 
format. This allows generating a simple document for the current
buffer. Basically, it creates a @tt{SETTINGS} file, sets @tt{MAIN} in
@tt{SETTINGS} to the current buffer and then generates the
documentation in a temporary directory. Note that for generating
complex manuals the best approach is to set up a permanent
documentation directory with the appropriate @tt{SETTINGS} and
@tt{Makefile} files (see the LPdoc manual)." 
  (interactive)
  (message "Generating documentation for buffer... ")
  (setq ciao-last-source-buffer-used (current-buffer))
  (ciao-unmark-last-run-errors)
  (let ((thisfile (buffer-name (current-buffer)))
	(original-buffer (current-buffer))
	(settings (concat (ciao-lpdoc-buffer-tmpdir 
			   (buffer-name (current-buffer))) "/SETTINGS")))
    (message (concat "Settings is: " settings))
    (if (file-exists-p settings)
	t
      (ciao-visit-lpdoc-settings)
      (switch-to-buffer-other-window original-buffer)
      )
    (find-file settings)
    (beginning-of-buffer)
    (search-forward "MAIN")
    (search-forward "=")
    (forward-char 1)
    (kill-line)
    (insert-string thisfile)
    (save-buffer)
    (bury-buffer)
    ;; Not necessary and creates a problem: first time errors are
    ;; not found because sending command gets ahead of starting process
    ;;    (ciao-ensure-inferior-process ciao-lpdoc-buffer-name)
    (ciao-send-command 
     ciao-lpdoc-buffer-name 
     (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " ciao-lpdoc-system 
	     " " ciao-lpdoc-docformat)
     t)
    (if ciao-locate-errors-after-run
	(add-hook 'ciao-lpdoc-prompt-inferior-hook 
		  'ciao-launch-find-last-run-errors-from-orig-buffer t))
    )
  (message "Generating documentation for buffer... done.")
  )

(defun ciao-start-viewer ()
  "Start a viewer on the documentation for the current buffer in the
   default format." 
  (interactive)
  (let ((thisfile (buffer-name (current-buffer)))
	(thisfileroot 
         (file-name-sans-extension (buffer-name (current-buffer)))))
    (if (not (file-exists-p (concat (ciao-lpdoc-buffer-tmpdir thisfile) 
				    "/SETTINGS")))
	(message "You need to first choose options in SETTINGS")
      (cond
       ((string= ciao-lpdoc-docformat "ascii") 
	(find-file-other-window 
	 (concat 
	  (ciao-lpdoc-buffer-tmpdir thisfile) "/" thisfileroot ".ascii")))
       ((string= ciao-lpdoc-docformat "info") 
	(info-other-window
	 (concat 
	  (ciao-lpdoc-buffer-tmpdir thisfile) "/" thisfileroot ".info")))
       (t
	(ciao-send-command 
	 ciao-lpdoc-buffer-name 
	 (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " 
		 ciao-lpdoc-system " " 
		 (if (string= ciao-lpdoc-docformat "dvi")
		     ;; "large" Optional, for demos
		     "")
		 ciao-lpdoc-docformat "view")
	 t))))))

(defun ciao-lpdoc-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (if (buffer-name (process-buffer proc))
      (save-excursion
	(set-buffer (process-buffer proc))
	(comint-output-filter proc string)
	
	;; Used for lpdoc hooks
	(ciao-lpdoc-if-prompt-run-hook string))))

;;------------------------------------------------------------
;; Auxiliary
;;------------------------------------------------------------

;; Functions for generating documentation for the ciao.el mode functions
;; in lpdoc format (!) M. Hermenegildo

(defun ciao-do-document-bindings (sec-commands)
  "Generate documentation for all the bindings in lpdoc format."
   (cond
    ((eq sec-commands nil) nil)
    ((equal (car (car sec-commands)) 'section)
     (insert-string "@section{")
     (insert-string (car (cdr (car sec-commands))))
     (insert-string "}\n\n")
     (insert-string (car (cdr (cdr (car sec-commands)))))
     (insert-string "\n")
     (ciao-do-document-bindings (cdr sec-commands)))
    ((equal (car (car sec-commands)) 'paragraph)
     (insert-string "\n\n")
     (insert-string (car (cdr (car sec-commands))))
     (insert-string "\n\n")
     (ciao-do-document-bindings (cdr sec-commands)))
    (t ;; else, list of bindings
     (insert-string "@begin{description}\n")
     (ciao-print-function-info (car sec-commands))
     (insert-string "@end{description} @p \n")
     (ciao-do-document-bindings (cdr sec-commands)))
    ))

(defun ciao-print-function-info (info)
  "Print the information on a function as an item in lpdoc format. If
function is a string it is taken to be the comment."
  (insert-string
   (concat 
    "\n@item{"
    (ciao-print-keys (car info))
    "} "
    (let ((function (car (cdr info))))
      (if (stringp function)
	  function
	(documentation function)))
    "\n"
    ))
  )

(defun ciao-print-keys (str) 
  "Format key binding sequences in lpdoc format."
  (cond 
   ((string= str "") "")
   ((eq (string-match "M-x" str 0) 0)
    (concat "@key{M-x} @tt{" (substring str 3) "}"))

   ((eq (string-match "M-" str 0) 0)
    (concat "@key{" (substring str 0 3) "} "
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "A-" str 0) 0)
    (concat "@key{" (substring str 0 3) "} "
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "C-" str 0) 0)
    (concat "@key{^" (upcase (substring str 2 3)) "} "
	    (ciao-print-keys (substring str 3))))

;;    ((eq (string-match " " str 0) 0)
;;     (concat "@key{SPC} " 
;; 	    (ciao-print-keys (substring str 1))))

;; Not correct, but tries to fix spurious spaces which are passed
   ((eq (string-match " " str 0) 0)
    (concat "" 
 	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "SPC" str 0) 0)
    (concat "@key{SPC} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "\t" str 0) 0)
    (concat "@key{TAB} " 
	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "TAB" str 0) 0)
    (concat "@key{TAB} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "\e" str 0) 0)
    (concat "@key{ESC} " 
	    (ciao-print-keys (substring str 1))))

   ((eq (string-match "ESC" str 0) 0)
    (concat "@key{ESC} " 
	    (ciao-print-keys (substring str 3))))

   ((eq (string-match "RET" str 0) 0)
    (concat "@key{RET} " 
	    (ciao-print-keys (substring str 3))))
   (t 
    (concat "@key{" 
	    (text-char-description (string-to-char (substring str 0 1) ))
	    "} "
	    (ciao-print-keys 
	     (substring str 1))))))

(defun ciao-document-variables ()
  "Generate documentation for all user-defined variables in lpdoc format."
  (let ((sym-list)
	(ciao-vars nil)
	(ciaopp-vars nil)
	(lpdoc-vars nil)
	(ciao-faces nil))
    
    ;; Build a list of symbols that match pattern.
    (mapatoms (function
	       (lambda (sym)
		 (if (string-match "ciao" (symbol-name sym))
		     (setq sym-list (cons sym sym-list))))))
    
    ;; Classify variables
    (mapcar (function (lambda (sym)
			(cond ;; Must be before others
			      ((string-match "face" (symbol-name sym))
			       (setq ciao-faces (cons sym ciao-faces)))
			      ((string-match "ciaopp" (symbol-name sym))
			       (setq ciaopp-vars (cons sym ciaopp-vars)))
			      ((string-match "lpdoc" (symbol-name sym))
			       (setq lpdoc-vars (cons sym lpdoc-vars)))
			      (t 
			       (setq ciao-vars (cons sym ciao-vars))))))
	    sym-list)

    ;; Generate the documentation
    (insert-string "\n@subsection{Ciao general variables}\n")
    (insert-string "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciao-vars 'string<))
    (insert-string "@end{description}\n")
    (insert-string "\n@subsection{CiaoPP variables}\n")
    (insert-string "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciaopp-vars 'string<))
    (insert-string "@end{description}\n")
    (insert-string "\n@subsection{LPdoc variables}\n")
    (insert-string "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort lpdoc-vars 'string<))
    (insert-string "@end{description}\n")
    (insert-string 
     "\n@subsection{Faces used in syntax-based highlighting (coloring)}\n")
    (insert-string "@begin{description}\n")
    (mapcar 'ciao-describe-func (sort ciao-faces 'string<))
    (insert-string "@end{description}\n")))

(defun ciao-describe-func (s)
  "Format the description of a symbol."
  (cond
   ;; It is a customizable variable 
   ((and (boundp s) (get s 'custom-type)) 
    (insert-string 
     (concat "@item{@tt{" 
	     (symbol-name s)
	     "} (@em{"))
    (if (listp (get s 'custom-type))
	(insert-string
	 (symbol-name 
	  (type-of
	   (car (cdr (car (cdr 
			   (get 's 'custom-type))))))))
      (insert-string (symbol-name (get s
				       'custom-type))))
    (insert-string "})}\n")
    (insert-string 
     (concat 
      (documentation-property s 'variable-documentation)
      "\n")))
   ;; It is a face
   ((documentation-property s 'face-documentation)
    (insert-string 
     (concat "@item{@tt{" 
	     (symbol-name s)
	     "} (@em{face})}\n"
	     (documentation-property s 'face-documentation)
	     "\n")))
   ))

(defun compile-ciao-mode ()
  "With this handy function this file can be compiled as
   emacs -batch -l ciao.el -f compile-ciao-mode"
   (message "Compiling ciao.el")
   (byte-compile-file "ciao.el")
   (message "Compiling word-help.el")
   (byte-compile-file "word-help.el"))
;; This are really no use...
;;  (byte-force-recompile "."))
;;  (byte-recompile-directory "." t))

(defun ciao-report-mode-version ()
  "Report the version of the emacs Ciao/Prolog mode."
  (interactive)
  (message (concat "Ciao, Prolog, CiaoPP, LPdoc mode version: " 
		   ciao-mode-version )))

;; Local version of replace-regexp-in-string, since it is not 
;; present in older versions of emacsen
(defun ciao-replace-regexp-in-string (regexp rep string &optional
					     fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (ciao-replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

;;------------------------------------------------------------
;; Kludge to fix old version maintenance entries...
;;------------------------------------------------------------

; Probably does not work in xemacs...
(defun ciao-fix-old-version-maintenance ()
  (interactive)
  (beginning-of-buffer)
  (if (search-forward "%% Control version comment prompting for" nil t)
      (let (tmp)
	(beginning-of-line)
	(kill-line 3)
	(next-line 1)
	(kill-line 1)
	(previous-line 1)
	(beginning-of-line)
	(set-mark (point))
	(search-forward "version-comments:")
	(search-forward "\"")
	(kill-region (mark) (point))
	(set-mark (point))
	(search-forward "\"")
	(backward-char 1)
	(setq tmp (buffer-substring-no-properties (mark) (point)))
	(kill-region (mark) (point))
	(kill-line 1)
	(insert-string 
	 (concat
	  ":- comment(version_maintenance,"
	  (cond
	   ((equal tmp "on") "on")
	   ((equal tmp "off") "off")
	   (t (concat "dir('" tmp "')")))
	  ").\n"
	  )))
    (error "Could not find version maintenance comment")))

;;;------------------------------------------------------------