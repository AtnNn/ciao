% -----------------------------------------------------------
% Other comments, acknowledgments and changelog/version info 
% -----------------------------------------------------------

:- use_package([assertions]).

:- comment(title,"Using Ciao inside GNU emacs").

:- comment(subtitle,"@em{An interactive program development environment for Ciao}").
:- comment(subtitle,"@bf{The Ciao System Documentation Series}").
:- comment(subtitle,"Technical Report CLIP 4/00.5.81").
:- comment(subtitle,"@em{Draft printed on:} @today{}").

:- comment(author,"Manuel Hermenegildo").
:- comment(author,"Manuel C. Rodriguez").
:- comment(author,"Daniel Cabeza").

:- include(library('ClipAddress')).

:- include(library('Copyright')).

:- comment(summary,"This documents the Ciao emacs interface (or @em{mode}
    in @apl{emacs} terms), which provides a rich, integrated user interface
    to the Ciao program development environment components, including the
    @apl{ciaosh} interactive top level and the @apl{ciaopp}
    preprocessor. While most features of the Ciao development environment
    are available from the command line of the preprocessor and the
    top-level shell, using Ciao inside @apl{emacs} is highly recommended,
    since it greatly facilitates the process of editing, compiling,
    running, and debugging Ciao programs. 

    In particular, source-level
    debugging and location of errors in source files, syntax highlighting,
    automatic access to online help, and automatic version control are only
    available within the Ciao emacs mode.").

:- comment(module,"@include{CiaoMode.lpdoc}").

main.

:- comment(version_maintenance,dir('../version')).

:- comment(version(1*9+323,2004/03/08,18:37*17+'CET'), "Errors now
   also located in source files with no suffix, or .pls etc.  (Manuel
   Hermenegildo)").

:- comment(version(1*9+281,2004/02/02,15:51*32+'CET'), "Fixed problem
   with C-c d when module has a name with single quotes (extra set of
   quotes not added anymore).  (Manuel Hermenegildo)").

:- comment(version(1*9+280,2004/02/02,15:51*10+'CET'), "Better
   highlighting when newlines present in commands. Persistent
   declarations now also highlighted. (Manuel Hermenegildo)").

:- comment(version(1*9+279,2004/02/02,15:50*46+'CET'), "Eliminated
   some spurious messages left over from debugging.  (Manuel
   Hermenegildo)").

:- comment(version(1*9+278,2004/02/02,15:48*21+'CET'), "Fixed problem
   due to replace-regexp-in-string not existing in older Emacs
   versions (pre 20.7.1). This should also fix some problems of the
   development environment in Windows. (Manuel Hermenegildo)").

:- comment(version(1*9+102,2003/09/07,18:04*22+'CEST'), "Fixed minor
   syntax problem in ciao.el file (thanks to @index{Sergey Plis}
   @email{pliz@cs.unm.edu}).  (Manuel Hermenegildo)").

:- comment(version(1*9+95,2003/08/04,18:59*47+'CEST'), "Minor mod to
   allow C-cC-v to work with ciaopp-1.0, which uses Ciao prompt.
   (Manuel Hermenegildo)").

:- comment(version(1*9+94,2003/08/04,17:53*02+'CEST'), "Fixed minor
   bug when recognizing that inferior mode should be used.  (Manuel
   Hermenegildo)").

:- comment(version(1*9+88,2003/07/18,15:58*24+'CEST'), "Added warning
   if attempting to put a process buffer in ciao-mode (recommending to
   use ciao-inferior-mode instead(.  (Manuel Hermenegildo)").

:- comment(version(1*9+82,2003/05/27,01:07*11+'CEST'), "Minor changes
   to improve xemacs compatibility (some, thanks to @index{Armin
   Rigo}).  (Manuel Hermenegildo)").

:- comment(version(1*9+73,2003/04/04,14:11*41+'CEST'), "Added new
   icons in environment for preprocessor. Also, when showing output
   after preprocessing the cursor is left in the original window.
   (Manuel Hermenegildo)").

:- comment(version(1*9+61,2003/02/25,12:24*25+'CET'), "Several changes
   to programming environment/emacs-mode:

   @begin{itemize}

   @item (Experimental) Reorganized menus: all help and customization
         now grouped in separate menus. No menu for deprecated
         @emph{traditional} commands any more.

   @item Visiting the corresponding file and marking the location is
         now not done by default for messages of type NOTE (but it can
         optionally be turned on). It is set to nil by default because
         sometimes the user prefers not to take any action with
         respect to these messages (for example, many come from the
         documenter, indicating that adding certain declarations the
         documentation would be improved).

   @item Many other commands (make-exec, make-po etc.) now also locate
         errors automatically and optionally.

   @item C-cV (Preprocess buffer and show output for ciaopp) works
         again, and improved. Improved also behavior of standard
         ciao-show-preprocessor-output when no output file was
         produced by preprocessor. Unified several overlapping
         functions.

   @item Informative messages given now when looking for next
         changelog entry and at last entry.

   @item Minor fixes to the handling of version maintenance (insertion
         of version maintenance directives in empty files and help
         strings improved).

   @item LPdoc error location now starts correctly the first time
         around (was bug). Systems files now located properly.

   @item Better compatibility with @apl{xemacs} (thanks to
         @index{Armin Rigo} for some related patches).

   @item Eliminated almost all emacs compilation warnings (useful for
         detecting bugs when making code changes).

   @end{itemize}

  (Manuel Hermenegildo)").

:- comment(version(1*9+54,2003/01/17,23:55*53+'CET'), "
   @begin{itemize}

   @item Major improvement to customizability of faces for
         syntax-based highlighting. Faces now divided in conceptual
         classes, and customizable individually.

   @item Greatly improved syntax-based coloring (better regexps). 

   @item Syntax-based coloring now work also on ascii terminals for
         newer versions of emacs .

   @item Literal-level assertions now correctly colored, even if they
         span several lines or contain disjunctions.

   @item Added a customizable variable @tt{ciao-user-directives} which
         allows listing user-defined directives, to be colored in
         special face.

   @item Separated directives into several more classes, with separate
         faces.

   @item Syntax errors now colored also in inferior buffers.

   @item Customizable faces now appear in the documentation.

   @item Added new tool bar button (and binding) to refontify
         block/buffer. Better definitions of what is a block for
         syntax-based coloring to work on.

   @item It is now possible to select through customize whether
         location of any errors produced when running Ciao tools
         (loading or preprocessing code, running the documenter, etc.)
         will be initiated automatically. I.e., whether after running
         a command, the system will automatically highlight any error
         messages and the corresponding areas in source files if
         possible.

   @item Error marks now cleared automatically also when generating
         docs. Better clearing in other cases.

   @item It is now possible to have error location in source start
         automatically after running lpdoc. Other fixes to hooks in
         lpdoc buffer.

   @item Fixed problem when loading in xemacs due to missing function
         in newer versions.

   @item Fixed problem with location of manuals in xemacs because of
         changes in number of arguments.

   @item Added portable image insertion primitives with defaults,
         which work in emacs and xemacs. As a result image insertion
         now works in xemacs also (hooray!).

   @item Icons are now installed in a separate dir. 

   @end{itemize}
   (Manuel Hermenegildo)").

:- comment(version(1*9+43,2002/12/16,10:48*42+'CET'),
   "ciao-module-name not interactive any more (was an error).  (Manuel
   Hermenegildo)").

:- comment(version(1*9+37,2002/12/11,17:20*36+'CET'), "Fixed bug in
   filepaths with \\ when loading a main file.  (Daniel Cabeza Gras)").

:- comment(version(1*9+35,2002/12/08,10:07*34+'CET'), "Color names are
   uniquified in emacs mode.  (Manuel Hermenegildo)").

:- comment(version(1*9+31,2002/11/21,12:41*47+'CET'), "Reorganized
   source of emacs mode. Icons now in a separate directory.  (Manuel
   Hermenegildo)").

:- comment(version(1*9+18,2002/10/20,18:33*20+'CEST'), "Improved
   handling of @tt{.cgi} files by debugger.  (Manuel Hermenegildo)").

:- comment(version(1*9+17,2002/10/13,12:29*38+'CEST'), "Several
   improvements to emacs-based environment:

   @begin{itemize} 

   @item Improvements so that paths are compatible with newer versions
         of @apl{cygwin} in Windows.

   @item Improved splash procedure (ciao-startup), with sample
         file. Useful for starting Ciao by double-clicking on an icon.

   @item Several improvements to preprocessor buffer.

   @item Paths in @file{GlobalChangeLog} are now relative (long
         needed!). Paths in previous earlier @file{GlobalChangeLog}
         files should be changed to relative paths by hand.

   @item @file{GlobalChangeLog} now set in Ciao mode by default.

   @item Elimination of error/debug marks improved.

   @end{itemize}

  (Manuel Hermenegildo)").

:- comment(version(1*9+7,2002/05/26,12:45*28+'CEST'), "Easier now to
   adapt to different prompts in inferior processes. In particular, a
   regexp describing the prompt used in OS shells can now be set or
   modified through the standard customization. An important
   consequence is that error location in source files now also works
   when running applications which produce Ciao-style error messages
   in inferior shells (such as LPdoc, the embedded debugger, or any
   user application that outputs messages in the corresponding
   format). (Manuel Hermenegildo)").

:- comment(version(1*9+6,2002/05/26,12:35*54+'CEST'), "New entry point
   (@tt{ciao-startup}, with no arguments) available, useful for
   calling ""The Ciao Programming Environment"" from a desktop icon, by
   making the icon call, for example, @tt{emacs -q -l
   /usr/local/lib/ciao/DOTemacs.el -f ciao-startup} (Manuel
   Hermenegildo)").

:- comment(version(1*9+5,2002/05/26,12:34*06+'CEST'), "Variables
   @tt{ciao-library-path} / @tt{CIAOLIB} can now be set through the
   customization panel. Also, the scope of these has been improved.
   (Manuel Hermenegildo)").

:- comment(version(1*7+219,2002/05/15,22:46*11+'CEST'), "Errors (if
   any) are now located automatically in source after coad loading and
   similar commands. (Manuel Hermenegildo)").

:- comment(version(1*7+218,2002/05/15,22:46*02+'CEST'), "Improved
   ciao-check-types-modes (preprocessor progress now visible).
   (Manuel Hermenegildo)").

:- comment(version(1*7+217,2002/05/15,22:41*45+'CEST'), "Fixed loading
   regions repeatedly (no more predicate redefinition warnings), by
   using same temp file name (Manuel Hermenegildo)").

:- comment(version(1*7+216,2002/05/15,22:40*16+'CEST'), "Added entries
   in @apl{ciaopp} menu to set verbosity of output. (Manuel
   Hermenegildo)").

:- comment(version(1*7+215,2002/05/15,22:40*05+'CEST'), "Fixed timing
   problem with ciao-check-buffer-syntax. (Manuel Hermenegildo)").

:- comment(version(1*7+214,2002/05/15,22:36*31+'CEST'), "Fixed some
   additional xemacs compatibility issues (related to searches).
   (Manuel Hermenegildo)").

:- comment(version(1*7+188,2002/02/08,20:06*47+'CET'), "Set
   comint-highlight-prompt to nil because otherwise coloring is not
   correctly done.  (Daniel Cabeza Gras)").

:- comment(version(1*7+187,2002/02/08,18:42*46+'CET'), "Changed some
   colors (Daniel Cabeza Gras)").

:- comment(version(1*7+172,2002/01/03,21:45*17+'CET'), "Made remaning
   faces local to Ciao mode for portability.  (Manuel
   Hermenegildo)").

:- comment(version(1*7+163,2002/01/03,12:09*10+'CET'), "Many
   improvements to emacs-based environment, some taking advantage of
   the great capabilities of emacs 21.1 and later:

   @begin{itemize}
   @item Errors reported by inferior processes are now explored in
         forward order (i.e., the first error rewported is the first
         one highlighted). Improved tracking of errors. 
   @item @tt{:- doc} now also supported and highlighted.
   @item Nicer banner in emacs 21.1 
   @item Improved behaviour of stored query.
   @item Direct access to preprocessor (checking modes/types and
         locating errors) from toolbar.
   @item New icons for visualization of generated documentation.
   @item Improved behaviour of recentering, finding errors, etc.
   @item Debugger faces are now locally defined (and better
         customization). This also improves comtability with xemacs
         (which has different faces).
   @item Eliminated need for calendar.el
   @item Fixed some remaining incompatibilities with xemacs.
   @item Wait for prompt has better termination characteristics.
   @item Added some missing library directives to fontlock list, organized
         this better.
   @item Many other minor bugs fixed.
   @end{itemize}

   (Manuel Hermenegildo)").

:- comment(version(1*7+152,2001/11/23,16:11*27+'CET'), "Modified
   Makefile: -\(cd ....\) was erroneously interpreted; changed to -(
   cd ... ) (MCL)").

:- comment(version(1*7+148,2001/11/17,20:40*45+'CET'), "Several
   improvements to emacs-based environment:

   @begin{itemize}
   @item Ciao-specific tool bars now shown when editing files and in inferior 
         processes, with icons for main fuctions (works from emacs 21.1 on). 
   @item Other minor adaptations for working with emacs 21.1.
   @item Fixes to set-query. Also, previous query now appears in prompt.
   @item Added new interactive entry points (M-x): ciao, prolog, ciaopp.
   @item Fixed minor bug with tracking of last inferior buffer used.
   @item Several other minor improvements.

   @end{itemize}

   (Manuel Hermenegildo)").

:- comment(version(1*7+122,2001/08/30,20:49*07+'CEST'), "The commands
   available in the interactive buffers (used by toplevel,
   preprocessor, etc.) are now documented in order to help users not
   very familiar with interactive buffers in emacs get
   started. (Manuel Hermenegildo)").

:- comment(version(1*7+121,2001/08/30,17:43*34+'CEST'), "Context help
   now works in Ciao/CiaoPP/LPdoc toplevel interactive buffers in
   emacs without needing to set the mode explicitly.  (Manuel
   Hermenegildo)").

:- comment(version(1*7+120,2001/08/28,18:08*13+'CEST'), "Minor
   corrections and improvements to documentation of emacs mode and
   debugger. Also fixed dependency code so that emacs mode
   documentation is always regenerated if the source file changes.
   (Manuel Hermenegildo)").

:- comment(version(1*7+92,2001/04/23,18:40*54+'CEST'), "Fixed bug which
   caused errors with short module names.  (Daniel Cabeza
   Gras)").

:- comment(version(1*5+159,2000/05/31,10:52*49+'CEST'), "Fixed bug in
   source-level debug search.  (Manuel Carlos Rodriguez)").

:- comment(version(1*5+151,2000/05/26,12:27*05+'CEST'), "Added support to
   use source-level debugging in an embedded debugger.  (Manuel Carlos
   Rodriguez)").

:- comment(version(1*5+147,2000/05/24,11:15*57+'CEST'), "Add Font-lock
   support for xemacs. (Manuel Carlos Rodriguez)").

:- comment(version(1*5+118,2000/04/19,18:13*43+'CEST'), "No need to
   previously create a SETTINGS file for documenting a single buffer
   in LPdoc mode. Reordered LPdoc menus to reflect this. Also, certain
   formats now viewed directly in emacs.  (Manuel Hermenegildo)").

:- comment(version(1*5+113,2000/04/10,17:35*36+'CEST'), "Modify the regular
   expression to search by the source-level debugger to support the
   embedded debugger.  (Manuel Carlos Rodriguez)").

:- comment(version(1*5+108,2000/04/07,11:02*54+'CEST'), "Change all the
   expressions containing what-line for a new function which return the
   line number. This functions is needed because what-line does not behave
   in emacs and in xemacs in the same way.  (Manuel Carlos Rodriguez)").

:- comment(version(1*5+99,2000/03/30,16:19*56+'CEST'), "Update
   documentation.  (Manuel Carlos Rodriguez)").

:- comment(version(1*5+95,2000/03/29,15:03*26+'CEST'), "Debugger menus
   rearranged a bit after user feedback.  (Manuel Hermenegildo)").

:- comment(version(1*5+93,2000/03/28,18:12*08+'CEST'), "Fixed when
   using debug buffer source in a user file, with no module
   declaration, the name of the file was wrong.  (Manuel Carlos
   Rodriguez)").

:- comment(version(1*5+86,2000/03/24,10:11*51+'CET'), "Fixed a problem
   with O'Ciao faces.  (Manuel Carlos Rodriguez)").

:- comment(version(1*5+85,2000/03/24,10:05*24+'CET'), "Added
   ciao-buffer-name. When debugging a module get the module name of a
   buffer. If the file name finish with .pl supress the extension,
   otherwise return the file name with extension.  (Manuel Carlos
   Rodriguez)").

:- comment(version(1*5+80,2000/03/23,14:34*33+'CET'), "Synchronies control
   version for ciao.el.body with the ciao system control version.  (Manuel
   Carlos Rodriguez)").

:- comment(version(0*5+86,2000/03/21,13:36*43+'CET'), "Ciao.el.body moves
   from using its own control version to use the control version used in
   Ciao. All the comments all will be kept in CiaoMode.pl in Ciao/Prolog
   syntax and will be printed in the Emacs Mode Manual.  (Manuel Carlos
   Rodriguez)").

:- comment(version(0*5+85,2000/03/20,00:0*00+'CET'), "Fixed a bug which sent
   a  when in column > 10.  (Daniel Cabeza Gras)").

:- comment(version(0*5+84,2000/03/17,00:0*00+'CET'), "Minor mod to LPdoc
   mode.  (Manuel Hermenegildo).").
 
:- comment(version(0*5+83,2000/03/16,00:0*00+'CET'), "Added the Set query
   and main file submenu and the ciao-load-query.  (Manuel Carlos
   Rodriguez)").
 
:- comment(version(0*5+82,2000/03/15,00:0*00+'CET'), "Added hilit a region
   when the predicate is not found. The color to hilit can be customize
   with ciao-debug-expansion.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+81,2000/02/29,00:0*00+'CET'), "Fix a bug when using
   the control version system in a directory which doesn't exist. In this
   case the directory is created.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+80,2000/02/24,00:0*00+'CET'), "Minor changes to
   ciao-hook. Move from a generic function to a function for each mode, but
   keeping a unique inferior mode but with three diferent filters. Also
   added some functionality as loading within a debug process or set a
   breakpoint while debugging. Move from differents functions to (un)mark
   buffers for debug to a one function which support all
   functionality. Add, also, set a default query to call when reloading and
   added ,too, load all modules as necessary.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+79,2000/02/22,00:0*00+'CET'), "Added menu support in
   xemacs.  (Manuel Carlos Rodriguez)").

:- comment(version(0*5+78,2000/02/16,00:0*00+'CET'), "Added support for
   hooks on a ciaopp prompt and lpdoc prompt.  There are also two different
   hooks for each prompt.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+77,2000/02/11,00:0*00+'CET'), "Added support for
   hooks on a ciao prompt. There are two different hooks, one for the Ciao
   inferior buffer and other to emacs side efect when founding a prompt on
   Ciao buffer.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+76,2000/02/07,00:0*00+'CET'), "Minor changes to
   documentation.  (Manuel Carlos Rodriguez)").

:- comment(version(0*5+75,2000/02/07,00:0*00+'CET'), "Modify the
   customization section to show subsection and variable's type. Also added
   support for emacs customization.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+74,2000/02/02,00:0*00+'CET'), "Added a customization
   section in Ciao Mode manual.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+73,2000/02/01,00:0*00+'CET'), "Changed fontification
   of ciao-inferior-mode & regexp for prompt.  (Daniel Cabeza Gras)").

:- comment(version(0*5+72,2000/01/30,00:0*00+'CET'), "Added C-cd
   (ciao-debug-buffer) shortcut for beginners.  (Manuel Hermenegildo)").

:- comment(version(0*5+71,2000/01/29,00:0*00+'CET'), "Changed fontification.
   (Daniel Cabeza Gras)").
 
:- comment(version(0*5+70,2000/01/21,00:0*00+'CET'), "Move from hilit to
   font-lock.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+69,2000/01/12,00:0*00+'CET'), "Fixed menus (manual
   access), reorganized (debugging section).  (Manuel Hermenegildo and
   Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+68,2000/01/04,00:0*00+'CET'), "Fixed a minor problem
   in ciao-debug-filter.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+67,1999/12/30,00:0*00+'CET'), "Added font-lock to to
   breakpoints. Also a defun to repaint all breakpoints.  (Manuel Carlos
   Rodriguez)").
 
:- comment(version(0*5+66,1999/12/29,00:0*00+'CET'), "Added breakpoint
   support, hilit lines marked with ;**.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+65,1999/12/28,00:0*00+'CET'), "Added support for
   font-lock in Ciao.  (Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+64,1999/12/23,00:0*00+'CET'), "Change load order for
   doc indices so that preds are found first.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+63,1999/12/22,00:0*00+'CET'), "Fixed case when hilit
   not available (minimal functionality on xemacs).  (Manuel Hermenegildo
   and Manuel Carlos Rodriguez)").
 
:- comment(version(0*5+62,1999/12/22,00:0*00+'CET'), "Fixed a minor timing
   problem in source debugger.  (Manuel Hermenegildo and Manuel Carlos
   Rodriguez)").
 
:- comment(version(0*5+61,1999/12/16,00:0*00+'CET'), "Minor fixes to lpdoc
   mode, marked srcdbg as experimental.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+60,1999/12/13,00:0*00+'CET'), "Now reasonable guess
   of module name made when using :- module(_,...).  (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+59,1999/12/13,00:0*00+'CET'), "Added debugger
   information in Ciao buffer and port color configuration.  (Manuel Carlos
   Rodriguez)").
 
:- comment(version(0*5+58,1999/12/10,00:0*00+'CET'), "Incorporated Manuel
   Carlos' source debugger functions.  (Manuel Hermenegildo and Manuel
   Carlos Rodriguez)").
 
:- comment(version(0*5+57,1999/12/05,00:0*00+'CET'), "Improved the handling
   of temporary files in LPdoc: now it is possible to generate docs for
   several files separately (done using an assoc list).  (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+56,1999/11/29,00:0*00+'CET'), "Separated out CiaoPP
   menu and created LPdoc menu. Fixed a number of bugs. Created LPdoc
   functionality. Now the documentation for a file can be generated in a
   temporary area by simply opening the file, and selecting items from the
   LPdoc menu.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+55,1999/11/24,00:0*00+'CET'), "Changed handling of
   toplevel and preprocessor commands to allow blanks in file names.
   (Manuel Hermenegildo)").

:- comment(version(0*5+54,1999/11/16,00:0*00+'CET'), "Parenthesis matching
   now also work in the Ciao/Prolog listener.  (Daniel Cabeza Gras)").

:- comment(version(0*5+53,1999/11/16,00:0*00+'CET'), "A shorter Ciao/Prolog
   menu now also appears in the inferior modes.  (Manuel Hermenegildo)").

:- comment(version(0*5+52,1999/11/16,00:0*00+'CET'), "Fixed bug when
   locating errors reported by Ciao for which no file name could be
   found. (Manuel Hermenegildo)").
 
:- comment(version(0*5+51,1999/11/13,00:0*00+'CET'), "Updated and
   Unix/Windows comments in .skel.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+50,1999/11/08,00:0*00+'CET'), "Associated .pls files
   (for Windows), changed name in mode line to Ciao/Prolog. Fixed other
   minor things for Windows.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+49,1999/11/02,00:0*00+'CET'), "Added coloring of
   impl_defined.  (Manuel Hermenegildo)").

:- comment(version(0*5+48,1999/10/20,00:0*00+'CET'), "Added
   ciao-insert-script-header.  (Manuel Hermenegildo)").

:- comment(version(0*5+47,1999/9/28,00:0*00+'CET'), "Added
   ciao-preprocess-buffer-and-show-output. This allows re-running the
   preprocessor and refreshing the buffer containing the output in one
   go. Also, ciao-show-preprocessor-output now goes to the same point in
   the buffer as in the previous run.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+48,1999/9/7,00:0*00+'CET'), "Added colorings for
   discontiguous.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+46,1999/8/4,00:0*00+'CET'), "Added colorings for
   debug_message.  (German Puebla)").
 
:- comment(version(0*5+45,1999/7/24,00:0*00+'CET'), "Added colorings for
   reexport and others.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+44,1999/7/5,00:0*00+'CET'), "Minor change to
   use_module/use_class.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+43,1999/5/3,00:0*00+'CET'), "C-cTAB now works also
   in inferior mode.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+42,1999/4/15,00:0*00+'CET'), "Use_package(ociao) ->
   use_package(objects).  (Manuel Hermenegildo)").
 
:- comment(version(0*5+41,1999/4/14,00:0*00+'CET'), "Changed .el compilation
   command, change include to use_package for class/objects, other minor
   changes to coloring.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+40,1999/4/6,00:0*00+'CET'), "Minor changes to
   coloring.  (Manuel Hermenegildo)").

:- comment(version(0*5+39,1999/3/31,00:0*00+'CET'), "Abbreviated word-help
   indices since now only one manual.  (Manuel Hermenegildo)").

:- comment(version(0*5+38,1999/3/30,00:0*00+'CET'), "Added explanation of
   alternative way of specifying version maintenance.  (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+37,1999/3/25,00:0*00+'CET'), "Coloring use_class,
   etc.  also searching for :- class to do use_class instead of use_module.
   (Manuel Hermenegildo)").
 
:- comment(version(0*5+36,1999/3/15,00:0*00+'CET'), "Now coloring
   use_package instead of syntax for Ciao 0.9. (Manuel Hermenegildo)").
 
:- comment(version(0*5+35,1999/3/6,00:0*00+'CET'), "Fixed some bugs due to
   changing to read-file-name for completion.  (Manuel Hermenegildo)").

:- comment(version(0*5+34,1999/2/29,00:0*00+'CET'), "Added bridge to
   word-help-complete. Increased completion level of several
   commands. Simple interface to setenv for changing CIAOLIB. (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+33,1999/2/29,00:0*00+'CET'), "Updated the list of
   word-help indices to match those in SETTINGS.COMMON.  (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+325,1999/2/8,00:0*00+'CET'), "Updated hilit patterns
   to avoid coloring @@<, @@=<, @@> and @@>=.  (Daniel Cabeza Gras)").
 
:- comment(version(0*5+32,1999/2/8,00:0*00+'CET'), "Increased limit when
   searching for :- module, added .asr to ignored entensions in
   DOTemacs.pl. (Daniel Cabeza Gras)").
 
:- comment(version(0*5+31,1999/1/12,00:0*00+'CET'), "Documentation finally
   completed.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+30,1998/12/15,00:0*00+'CET'), "ChangeLog file now
   opened in ciao-mode. Some changes in menus. Syntax checking now in two
   commands (required by top-level).  (Manuel Hermenegildo)").
 
:- comment(version(0*5+29,1998/12/14,00:0*00+'CET'), "Fixed interface to
   word-help. Still brittle...  (Manuel Hermenegildo)").
 
:- comment(version(0*5+28,1998/12/14,00:0*00+'CET'), "Minor bug
   fixes. Improved documentation.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+27,1998/12/06,00:0*00+'CET'), "Added binding and menu
   entry to call ciao-check-buffer-syntax on current buffer. Improved
   documentation.  (Manuel Hermenegildo)").

:- comment(version(0*5+26,1998/12/03,00:0*00+'CET'), "Restored file name
   completion in inferior mode.  (Manuel Hermenegildo)").

:- comment(version(0*5+25,1998/12/03,00:0*00+'CET'), "Inferior mode now also
   responds to C-c`. This can be used to look for errors even if running
   inside a normal shell by putting the shell buffer in ciao-inferior-mode
   and using the C-c` binding there!.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+24,1998/12/02,00:0*00+'CET'), "Fixed bug in region
   commands.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+23,1998/11/26,00:0*00+'CET'), "The executable name
   used to run ciao and the preprocessor are now configurable from the
   menus.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+22,1998/11/12,00:0*00+'CET'), "Improved localization
   of errors. Improved coloring.  (Daniel Cabeza Gras)").

:- comment(version(0*5+21,1998/10/14,00:0*00+'CET'), "Separated out inferior
   mode syntax table. Better parsing of error messages. Improved
   definitions of comments.  (Daniel Cabeza Gras)").
 
:- comment(version(0*5+20,1998/9/25,00:0*00+'CET'), "Made color for checked
   assrts a little darker.  (Manuel Hermenegildo)").

:- comment(version(0*5+19,1998/9/21,00:0*00+'CET'), "Auto-documentation!.
   (Manuel Hermenegildo)").

:- comment(version(0*5+18,1998/9/18,00:0*00+'CET'), "Paths now edited
   automatically. Better behaviour under no hilightting.  (Manuel
   Hermenegildo)").

:- comment(version(0*5+17,1998/9/11,00:0*00+'CET'), "Updating changelogs
   does not ask for deleting excessive backup versions.  (Manuel
   Hermenegildo)").

:- comment(version(0*5+16,1998/9/11,00:0*00+'CET'), "Hilighting of lpdoc
   commands.  (Manuel Hermenegildo)").

:- comment(version(0*5+15,1998/8/20,00:0*00+'CET'), "Version maintenance
   method now determined by looking at comment/2 declaration.  (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+14,1998/8/14,00:0*00+'CET'), "Improved help
   access.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+13,1998/8/5,00:0*00+'CET'), "Improved menus, added
   help, including interface to word-help.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+12,1998/7/27,00:0*00+'CET'), "Updated debugger
   interface for new debugger.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+11,1998/7/14,00:0*00+'CET'), "Added making active
   modules.  (Manuel Hermenegildo)").
 
:- comment(version(0*5+10,1998/7/14,00:0*00+'CET'), "New version reporting.
   (Manuel Hermenegildo)").
 
:- comment(version(0*5+9,1998/7/10,00:0*00+'CET'), "Improved use of
   make_exec/2 (now make_exec(FILE,_) is allowed).  (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+8,1998/7/10,00:0*00+'CET'), "Fixed bug in reporting
   of time in comments.  (Manuel Hermenegildo)").

:- comment(version(0*5+7,1998/7/8,00:0*00+'CET'), "Fixed minor bugs, ask for
   saving buffers before compilation.  (Manuel Hermenegildo)").

:- comment(version(0*5+6,1998/6/23,00:0*00+'CET'), "Changed default in
   version maintenance start prompt to q.  (Manuel Hermenegildo)").

:- comment(version(0*5+5,1998/6/23,00:0*00+'CET'), "Updated hilit patterns.
   (Manuel Hermenegildo)").

:- comment(version(0*5+4,1998/6/17,00:0*00+'CET'), "Updated hilit
   patterns. (Manuel Hermenegildo) ").

:- comment(version(0*5+3,1998/6/1,00:0*00+'CET'), "Hilit now on inferior
   process also.  (Manuel Hermenegildo)").

:- comment(version(0*5+2,1998/5/23,00:0*00+'CET'), "Fixed older commands to
   work again.  (Manuel Hermenegildo)").

:- comment(version(0*5+1,1998/5/20,00:0*00+'CET'), "Fixed bugs compilation
   functions and bindings, changed temp file regime.  (Manuel
   Hermenegildo)").
 
:- comment(version(0*5+0,1998/4/27,00:0*00+'CET'), "Added menu support.
   (Manuel Hermenegildo)").

:- comment(version(0*4+8,1998/4/15,00:0*00+'CET'), "Time now recorded in
   versions.  (Manuel Hermenegildo)").

:- comment(version(0*4+7,1998/1/27,00:0*00+'CET'), "Added further processing
   of versions.  (Manuel Hermenegildo)").

:- comment(version(0*4+6,1997/10/20,00:0*00+'CET'), "Added miscellaneous
   regexps (continued).  (Manuel Hermenegildo)").

:- comment(version(0*4+5,1997/8/21,00:0*00+'CET'), "Added author signing of
   versions.  (Manuel Hermenegildo)").

:- comment(version(0*4+4,1997/8/06,00:0*00+'CET'), "Added version
   maintenance and other stuff.  (Manuel Hermenegildo)").

:- comment(version(0*4+3,1997/5/06,00:0*00+'CET'), "Added hiliting for
   assertions and other minor changes. (Manuel Hermenegildo)").

:- comment(version(0*3+0,1995/4/01,00:0*00+'CET'), "Major change to
   support Ciao functionality.  (Manuel Hermenegildo)").

:- comment(version(0*2+1,1993/11/01,00:0*00+'CET'), "Major change to
   support &-Prolog functionality, operators, parallelizer, etc.
   (Manuel Hermenegildo)").

:- comment(version(0*1+10,1993/02/05,00:0*00+'CET'), "Use copy-keymap where
   appropriate").

:- comment(version(0*1+8,1993/01/27,00:0*00+'CET'), "Change log updated.").

:- comment(version(0*1+7,1992/10/07,00:0*00+'CET'), "Add prolog-version
   don't treat where specially in indent-for-new-clause.").

:- comment(version(0*1+6,1992/08/28,00:0*00+'CET'), "Fix broken input
   history filter for Prolog mode.").

:- comment(version(0*1+5,1992/04/24,00:0*00+'CET'), "Added EPROLOG env
   variable. Flush sicstus-switch-to-buffer-other-window. Update
   compile-prolog to recompile entire directory.").
 
:- comment(version(0*1+2,1992/03/20,00:0*00+'CET'), "Johan Bevemyr's
   adaptation to the comint package. Change from sicstus0.7 to sicstus2.1.
   Treat where specially in indent-for-new-clause.").

 


