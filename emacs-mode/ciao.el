;; -*- mode: emacs-lisp; -*-
;;---------------------------------------------------------------------------
;; Emacs support for the Ciao programming environment
;; (can be used as a general Prolog mode under Emacs)
;;---------------------------------------------------------------------------
;; Copyright (C) 1986-1998 Free Software Foundation, Inc. and M. Hermenegildo
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
;; Ciao / &-Prolog / Prolog mode
;;; (can be used for SICStus, and coexist with SICStus Prolog prolog.el)
;;; 
;(setq load-path (cons "\\clip/clip/Systems/ciao/emacs-mode" load-path))
;(autoload 'run-ciao-toplevel "ciao"
;          "Start a Ciao / &-Prolog / Prolog top-level sub-process." t)
;(autoload 'run-ciao-preprocessor "ciao"
;          "Start a Ciao / &-Prolog / Prolog preprocessor sub-process." t)
;(autoload 'ciao-mode "ciao"
;          "Major mode for edit/run Ciao, Prolog, &-Prolog" t)
;(autoload 'ciao-inferior-mode "ciao"
;          "Major mode for running Ciao, Prolog, lpdoc, etc." t)
;(setq auto-mode-alist (cons '("\\.pl$" . ciao-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.pls$" . ciao-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.lpdoc$" . ciao-mode) auto-mode-alist))
;(setq completion-ignored-extensions
;      (append '(".dep" ".itf" ".po" ".asr" ".cpx")
;              completion-ignored-extensions))
;;; ------------------------------------------------------------------------
;;; In Un*x, the following (or similar) lines should be included in your
;;; .cshrc or .profile to find the manuals (the Ciao installation leaves
;;; in the Ciao library directory 'DOTcshrc' and 'DOTprofile' files with
;;; the right paths which can be included directly in your startup scripts):
;;; 
;;; setenv INFOPATH /usr/local/info:/usr/info:\\clip/clip/Systems/ciao/doc/reference
;;; ------------------------------------------------------------------------
;;; Specific to Windows installation:
;;; Location of Ciao shell
;(setq ciao-system (convert-standard-filename 
;      "\\clip/clip/Systems/ciao/shell/ciaosh.bat"))
;;; Location of info manuals
;(setq Info-default-directory-list  (cons 
;      "\\clip/clip/Systems/ciao/doc/reference" 
;      Info-default-directory-list))
;;; Make things nicer (but check if you are already doing it)
;(global-font-lock-mode)
;(transient-mark-mode t)
;;; Help for using the Windows command.com as your shell
;;; (comment out if you use bash, etc.):
;(setq process-coding-system-alist
;	    '(("cmdproxy" . (raw-text-dos . raw-text-dos))))
;;; Preventing ctrln-m's from being printed in the shell
;(add-hook 'comint-output-filter-functions   'shell-strip-ctrl-m nil t)
;; ---------------------------------------------------------------------
;; -*- mode: emacs-lisp; -*-

;; --------------------------------------------------------------------------
;; The actual code of the mode starts here.
;; --------------------------------------------------------------------------

;; This is so that the other .el files (word-help, etc.) in the Ciao
;; lib are found (this path is updated automatically during installation):
(setq load-path (cons "\\clip/clip/Systems/ciao/emacs-mode" load-path))

;; --------------------------------------------------------------------------
;; Mode documentation and acks (see also documentation in functions
;; and the CiaoMode.pl file included above)
;; --------------------------------------------------------------------------

(defun ciao-mode-documentation ()
  "This function generates documentation in lpdoc format for the
    Ciao/Prolog mode commands and their bindings."
  (interactive)
  (switch-to-buffer "*ciao-tmp*")
  (ciao-mode) ;; so that the bindings that we document are active!
  
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

@item @index{Syntax highlighting} and syntax coloring, @cindex{coloring,
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

This is particularly important for the @concept{source-level debugger} and
the @concept{syntax coloring} capabilities.  This is due to the fact that
it would be unrealistic to write a complete Prolog parser in Emacs
lisp. These conventions are the following, in order of importance:

@begin{itemize}

@item Clauses should begin on the first column (this is used to recognize
      the beginning of a clause). 

@item C style comments should not be used in a clause, but can be used
      outside any clause.

@end{itemize}

The following are suggestions which are not strictly necessary but which
can improve operation:

@begin{itemize}

@item Body literals should be indented, and there should be not more than
      one literal per line. This allows more precision in the location of
      program points during source-level debugging, i.e., when marking
      breakpoints and during line tracing.

@end{itemize}

@section{Checking the installation}

It may be possible that a complete pre-installation of the Ciao/Prolog
@apl{emacs} interface was already completed during Ciao installation. To
check this, open a file with a @tt{.pl} ending. You should see that
@apl{emacs} enters Ciao/Prolog mode: the mode is identified in the
@concept{status bar} below the @concept{buffer} and, if the @concept{emacs
menu bar} is enabled, you should see the Ciao/Prolog menus. You should be
able from the menu-bar, for example, to go to the Ciao manuals in the info
or load the @tt{.pl} file that you just opened into a ciao top level.

If things don't work properly, see the section @ref{Installation of the
Ciao/Prolog emacs interface} latter in this chapter.

@section{Functionality and associated key sequences (bindings)}

The following is a summary of the capabilities of the Ciao/Prolog emacs
interface and the (default) @index{key sequences} used to access those
capabilities.  Most of these functions are accessible also from the
menu bar.

")

  ;; This inserts the documentation strings for the bindings.
  (ciao-do-document-bindings (nreverse ciao-documented-commands))

  (insert-string (concat "

@section{Commands available in toplevel and preprocessor buffers}

The interactive top level and the preprocessor both are typically run
in an iteractive buffer, in which it is possible to communicate with
them in the same way as is they had been started from a standard
shell. In addition, the commands and key bindings available in buffers
containing source code are also available in these interactive
buffers, when applicable.

@section{Using Ciao/Prolog mode capabilities in standard shells} 

The capabilities (commands, coloring, ...) which are active in the
Ciao/Prolog ``inferior'' mode (the mode of the buffers in which the top level
and the preprocessor run) can also be made available in any standard
command line shell which is being run within emacs. This can be
enabled by going to the buffer in which the shell is running and
typing ``@key{M-x} @tt{ciao-inferior-mode}''.  This is very useful for
example when running the stand-alone compiler, the @apl{lpdoc}
auto-documenter, or even certain user applications (those that use the
standard error message library) in an emacs sub-shell. Turning the
Ciao/Prolog inferior mode on on that sub-shell will highlight and color the
error messages, and automatically find and visit the
locations in the files in which the errors are reported.

@section{Coexistence with other Prolog interfaces} 

As mentioned previously, the Ciao/Prolog @apl{emacs} interface can also be
used to work with other Prolog or CLP systems. Also, the Ciao/Prolog
@apl{emacs} interface (@em{mode}) can coexist with other
Prolog-related @apl{emacs} interfaces (@em{modes}) @cindex{emacs mode,
loading several} (such as, e.g., the @apl{SICStus} Prolog
interface). Only one of the interfaces can be active at a time for a
given buffer (i.e., for each given file opened inside @apl{emacs}). In
order the change a buffer to a given interface, move the cursor to
that buffer and type @tt{M-x ...-mode} (e.g., for the Ciao/Prolog mode,
@tt{M-x ciao-mode}).

If several Prolog-related @apl{emacs} interfaces are loaded, then
typically the @em{last} one to be loaded takes precedence, in the
sense that this will be the interface in which @apl{emacs} will be set
when opening files which have a @tt{.pl} ending (this depends a bit on
how things are set up in your @tt{.emacs} file).

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
setting in some @apl{emacs} distributions), then you can set things so that
the Ciao/Prolog mode is loaded by default in your system. This can be done
by including in your @file{.emacs} file a line such as:

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
;; Required packages 
;; ---------------------------------------------------------------------------

(require 'comint)
(require 'calendar)
(require 'easymenu)
(require 'word-help)
(require 'etags)
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
(defconst ciao-mode-version "Development Version" 
  "This is the version number of the ciao.el file")

(defconst ciao-mode-emacs-version 

  "This mode is currently being developed within @apl{GNU emacs}
version 20.4. It should also work with all other 20.XX and later 19.XX
versions."

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
  :group 'ciao-environment)

(defgroup ciaopp nil
  "The Ciao preprocesor."
  :group 'ciao-environment)

(defgroup lpdoc nil
  "The LPdoc documentation generator."
  :group 'ciao-environment)

(defcustom ciao-toplevel-buffer-name "Ciao/Prolog"
  "Basic name of the buffer running the Ciao/Prolog toplevel inferior 
process."
  :group 'ciao
  :type 'string)

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
  "*Arguments passed to Ciao/Prolog toplevel executable."
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

(defcustom ciao-indent-width 4
  "Indentation for a new goal."
  :group 'ciao
  :type 'integer)

(defvar ciao-temp-file-name "ciao")

(defvar ciao-previous-error nil
  "Stores where the last error was.")

(defvar ciao-inferior-error nil
  "Stores the line in the inferior buffer which shows the error line.")

(defcustom ciao-main-buffer ""
  "Name of main module in a multiple module program. It is very useful
when reloading from an inferior module because reload is performed from the
main module, thus also reloading all dependent modules."
  :group 'ciao
  :type 'string)

(defun ciao-set-main-buffer ()
  "Set the current buffer as the principal module in a multiple module
programming environment."
  (interactive)
  (setq ciao-main-buffer
	(read-file-name "Change Ciao/Prolog main module? " 
			"" (ciao-get-module-name) nil (ciao-get-module-name))))

(defvar ciao-prompt-emacs-hook nil
  "Things to do in emacs ones the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-prompt-inferior-hook nil
  "Things to do in Ciao ones the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-prompt-marker-acc ""
  "Keep the last line written in ciao inferior buffer. It is used to search
for the prompt due than prompt should be after a newline.")
(make-variable-buffer-local 'ciao-prompt-marker-acc)

(defcustom ciao-query ""
  "Query to use in Ciao. Setting this is useful when using a long or
complicated query because it saves from having to type it over and
over again. It is possible to set that this query will be issued 
any time a program is (re)loaded."
  :group 'ciao
  :type 'string)

(defun ciao-set-query ()
  "Set a default query. It is possible to set things up so that 
this query will be issued any time a program is (re)loaded. The
functionality is available in the major mode (i.e., from a buffer containing
a source file) and in the inferior mode (i.e., from the buffer running the
top-level shell. When called from the major mode it will ask in the
minibuffer for the query and when calling from the inferior mode it will get
the query on the current line."

  (interactive)
  (cond ((string= (buffer-name) (concat "*" ciao-toplevel-buffer-name "*"))
	 (let (beg)
	   (save-excursion
	     (beginning-of-line)
	     (goto-char (+ (point) 3))
	     (setq beg (point))
	     (end-of-line)
	     (setq ciao-query 
		   (buffer-substring-no-properties beg (point))))))
	((eq major-mode 'ciao-mode)
	 (setq ciao-query (read-string "Query? " "")))))

;; MCarlos: In xemacs does not exist the match-string-no-properties
;; function. This will fix that, but when using ciao-match-string you
;; should use (funcall ciao-match-string <args>)
(defvar ciao-match-string nil)
(if (not (boundp 'xemacs-logo))
    (setq ciao-match-string 'match-string-no-properties)
  (setq ciao-match-string 'match-string))

;; ---------------------------------------------------------------------------
;; Source debugger variables
;; ---------------------------------------------------------------------------

;; CHANGE
(defvar ciao-debug-filter-defer-flag nil
  "Non-nil means don't process anything form the debugger 
right now. It is saved for when flag is not set.")

(defvar ciao-debug-filter-pending-text nil
  "Non-nil means this is text that has been saved for latter in
'ciao-debug-filter'.")

(defvar ciao-debug-delete-prompt-marker nil)

(defvar ciao-debug-last-frame nil 
  "Last file over which we have drawn.")

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

(defcustom ciao-debug-call-color 'secondary-selection
  "Color to use with the call port in source debugger."
  :group 'ciao
  :type 'face)

(defcustom ciao-debug-exit-color 'highlight
  "Color to use with the exit port in source debugger."
  :group 'ciao
  :type 'face)

(defcustom ciao-debug-fail-color 'modeline
  "Color to use with the fail port in source debugger."
  :group 'ciao
  :type 'face)

(defcustom ciao-debug-redo-color 'holiday-face
  "Color to use with the redo port in source debugger."
  :group 'ciao
  :type 'face)

(defcustom ciao-debug-expansion 'region
  "Color to use in source debugger when the predicate was not found."
  :group 'ciao
  :type 'face)

(defcustom ciao-debug-breakpoint-color 'font-lock-warning-face
  "Color to use with breakpoints in source debugger."
  :group 'ciao
  :type 'face)

;; ---------------------------------------------------------------------------
;; CiaoPP variables
;; ---------------------------------------------------------------------------

(defcustom ciao-ciaopp-buffer-name "Ciao-Preprocessor"
  "Basic name of the buffer running the Ciao preprocessor inferior process."
  :group 'ciaopp
  :type 'string) 

(defcustom ciao-ciaopp-system (or (getenv "CIAOPP") "ciaopp")
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

(defvar ciao-ciaopp-prompt-emacs-hook nil
  "Things to do in emacs ones the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-ciaopp-prompt-inferior-hook nil
  "Things to do in CiaoPP ones the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-ciaopp-prompt-marker-acc ""
  "Keep the last line written in ciao inferior buffer. It is used to search
for the prompt due than prompt should be after a newline.")
(make-variable-buffer-local 'ciao-ciaopp-prompt-marker-acc)

;; ---------------------------------------------------------------------------
;; LPdoc variables
;; ---------------------------------------------------------------------------

(defcustom ciao-lpdoc-buffer-name "LPdoc"
  "Basic name of the buffer running the auto-documenter inferior process."
  :group 'lpdoc
  :type 'string) 

;; This for lpdc-1.9; set to "lpdoc" for 2.0
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
    (read-file-name "Change default doc format used by LPdoc auto-documenter ?"
   		        "" ciao-lpdoc-docformat nil ciao-lpdoc-docformat))) 

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

(defvar ciao-lpdoc-prompt-emacs-hook nil
  "Things to do in emacs ones the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-lpdoc-prompt-inferior-hook nil
  "Things to do in CiaoPP ones the prompt is found in the Ciao/Prolog buffer.")

(defvar ciao-lpdoc-prompt-marker-acc ""
  "Keep the last line written in ciao inferior buffer. It is used to search
for the prompt due than prompt should be after a newline.")
(make-variable-buffer-local 'ciao-lpdoc-prompt-marker-acc)

;; ===========================================================================
;; Mode body
;; ===========================================================================

(defun ciao-set-library-path () 
  "Change the location of the Ciao/Prolog library paths (changes the
   environment variable @tt{CIAOLIB})."
  (interactive)
  (setenv "CIAOLIB"
	(read-file-name "Change Ciao/Prolog library path ?" 
			"" (getenv "CIAOLIB") nil (getenv "CIAOLIB")))) 

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
  '("Ciao/Prolog Listener"
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
(defun ciao-temp-code-file ()
  "Returns the name of a temporary file in the current dir."
  (concat (expand-file-name "./") "." ciao-temp-file-name 
	  (int-to-string ciao-temp-file-counter) "_" (make-temp-name "")))
(defun ciao-new-temp-code-file ()
  "Builds new temporary file names in the current dir."
  (setq ciao-temp-file-counter (+ ciao-temp-file-counter 1))
  (ciao-temp-code-file))

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
;; Font-lock support
;;----------------------------------------------------------------------------

(defvar ciao-predicate-directives
  '( "data" "dynamic" "multifile" "impl_defined" "meta_predicate"
     "discontiguous"))

(defvar ciao-builtin-directives
  '( "module" "export" "reexport" "new_declaration" "op" "initialization"
     "on_abort" "use_module" "ensure_loaded" "use_active_module"
     "load_compilation_module" "set_prolog_flag" "push_prolog_flag"
     "pop_prolog_flag" "use_package" "redefining" "include"
     "add_sentence_trans" "add_term_trans" "add_clause_trans"
     "add_goal_trans"))

; (defcustom ciao-ociao-builtin-face 'font-lock-ForestGreen
;    "Color to fontify the Ciao/Prolog builtin directives."
;     :group 'ciao
;     :type 'face)

(defvar ciao-ociao-directives
  '( "virtual" "public" "inheritable" "implements" "inherit_class" "class"
     "use_class"))
 
; (defcustom ciao-ociao-directives-face 'font-lock-NavyBlue-bold
;   "Color to fontify the Ciao/Prolog O'Ciao directives."
;   :group 'ciao
;   :type 'face)

(defvar ciao-user-directives nil)
 
; (setq ciao-mode-font-lock-keywords
(defvar ciao-mode-font-lock-keywords
  '(
    ;; scripts
    ((lambda (limit) 
       (ciao-font-lock-match limit "^#!" "^[ \t]*$"))
     . font-lock-ForestGreen)
    ;; comments
    ((lambda (limit) 
       (ciao-font-lock-match limit "/\\*" "\\*/"))
     . font-lock-comment-face)
    ("^[ \t]*%.*$" . font-lock-comment-face)
    ;; lpdoc comments
    ((lambda (limit) 
       (ciao-font-lock-match limit
                             "^ *:- *comment( *bug\\>"
                             "[^\\\"]\" *)\\. *\n"))
     . 'font-lock-warning-face)
    ((lambda (limit) 
       (ciao-font-lock-match limit
 			     "^ *:- *comment( *\\(version\\(_maintenance\\)?\\|doinclude\\|hide\\|filetype\\|nodoc\\)\\>" 
 			     ") *\\. *\n"))
     . font-lock-comment-face)
    ((lambda (limit) 
       (ciao-font-lock-match limit
                             "^ *:- *comment("
                             "[^\\\"]\" *)\\. *\n")) 
     . font-lock-NavyBlue)
    ;; characters " and @
    ("0'[\"@]" . font-lock-string-face)
    ;; comment strings in assertions
    ("#[ \n]*\\(\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\"\\)"
     1 font-lock-NavyBlue)
    ;; strings
    ("\"\\([^\\\"]\\|\\\\\\(.\\|\n\\)\\)*\""
     . font-lock-string-face)
    ;; lpdoc commands in comments
    ((lambda (limit) 
       (ciao-font-lock-match limit "@begin{verbatim}" "@end{verbatim}"))
     0 font-lock-NavyBlue-bold t) 
    ((lambda (limit) 
	     (ciao-font-lock-match limit "@include[^ {}@]*{" "[^}@]*}"))
     0 font-lock-NavyBlue-bold t)
    ("@\\(cite\\|ref\\|section\\|subsection\\){[^{}@]*}"
     0 font-lock-crossref t)
    ("@[^ \t\n{}@=<>]*{[^{}@]*}"
     0 font-lock-keyword t)
    ("@\\([}{@]\\|\\([A-Za-z]+\\|[?!]\\)[ \t\n]\\)"
     0 font-lock-keyword t)
    ;; comments not starting a line
    ("[ \t]%.*$" . font-lock-comment-face)
    ;; Ciao/Prolog constructs, Assertions, etc.
    ((lambda (limit)
       (ciao-font-lock-match
        limit
        (concat "^ *:- *" (regexp-opt ciao-builtin-directives t) "\\>")
        "^[ \t]*$\\|\\.$"))
     . font-lock-Blue-bold)
    ((lambda (limit) 
       (ciao-font-lock-match
        limit
        (concat "^ *:- *" (regexp-opt ciao-predicate-directives t) "\\>")
        "^[ \t]*$\\|\\.$"))
      . font-lock-NavyBlue-bold)
    ((lambda (limit) 
       (ciao-font-lock-match
        limit
        (concat "^ *:- *" (regexp-opt ciao-ociao-directives t) "\\>")
        "^[ \t]*$\\|\\.$"))
     . font-lock-NavyBlue-bold)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *checked " " #[ \n]\\|\\.$"))
     . font-lock-DarkGreen-bold)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *true " " #[ \n]\\|\\.$"))
     . font-lock-ForestGreen-bold)
    ("^ *true(.*$"
     . font-lock-ForestGreen)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *false " " #[ \n]\\|\\.$"))
     . font-lock-warning-face)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *trust" " #[ \n]\\|\\.$"))
     . font-lock-Brown-bold)
    ((lambda (limit) 
       (ciao-font-lock-match
        limit 
        "^ *:- *\\(check\\)? *\\(decl\\|pred\\|comp\\|calls\\|success\\) " 
        " #[ \n]\\|\\.$"))
     . font-lock-NavyBlue-bold)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *prop " " #[ \n]\\|\\.$"))
     . font-lock-Blue-bold)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *regtype " " #[ \n]\\|\\.$"))
     . font-lock-MediumBlue-bold)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *entry " " #[ \n]\\|\\.$"))
     . font-lock-Brown-bold)
    ((lambda (limit) 
       (ciao-font-lock-match limit "^ *:- *modedef " " #[ \n]\\|\\.$"))
     . font-lock-ForestGreen-bold)
    ((lambda (limit) 
       (ciao-font-lock-match limit "\\<debug_message(" "),\\|)\\."))
     . font-lock-ForestGreen-bold)  
    ("^[a-z][a-zA-Z0-9_]*" . ciao-clauseheadname-face)
    ;; Clause heads
    ("^'\\([^']\\|''\\)*'" . ciao-clauseheadname-face) ;; Clause heads
    ;;
    ;;     ("^ *:-" nil defun)
    ("\\(&\\|&>\\|<&\\|@[^=<>]\\)"
     . font-lock-ForestGreen-bold) ;; Concurrency
    ("!" . font-lock-keyword)
    ))

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

;;(setq ciao-inferior-font-lock-keywords
(defvar ciao-inferior-font-lock-keywords
      '(
	("^\\(\\(\\|[0-9]+ \\|ciaopp \\|| \\)\\?-\\)"
         . ciao-prompt-face) ;; Prompts
	("^\\([A-Z_][a-zA-Z0-9_]*\\) = \\(.*\\)\\(,\\|? *.\\)$"
         (1 font-lock-ForestGreen-bold)                  ;; Answer variable
         (2 font-lock-Blue-bold)                         ;; Answer value
         )
	("^yes$" . font-lock-ForestGreen-bold)           ;; Answer
	("^no$" . font-lock-Goldenrod-bold)              ;; Answer
	("^Select[^:]*:" . font-lock-ForestGreen)        ;; Preproc prompt
;       ("^Select" " \\? " keyword)                       ;; Preproc prompt
;	("^Select.*$" . font-lock-Blue-bold)             ;; Preproc prompt
	("^{?ERROR.*$" . font-lock-warning-face)         ;; Error messages
	("^\\*\\* here \\*\\*$" . font-lock-Brown)       ;; Error messages
	("^{?WARNING.*$" . font-lock-Brown-bold)         ;; Error messages
	("^{DEBUG.*$" . font-lock-ForestGreen)           ;; Error messages
	("^{?Note:.*$" . font-lock-Brown)                ;; Error messages
	("^{NOTE.*$" . font-lock-Brown)                  ;; Error messages
        ("^\\({.*\\|}\\)" . font-lock-Brown)
	("^Ciao\\>.*$" . font-lock-ForestGreen-bold)     ;; Startup
        ; Recognizes a date at the end of the line
	("^(C) .* \\w\\w\\w \\w\\w\\w [1-3]?[0-9]\
 [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [A-Z][A-Z][A-Z] [1-2][0-9][0-9][0-9]$"
         . font-lock-ForestGreen-bold)       ;; Startup
;	("\\(^\\?- *[^{ ]\\|^| \\?- *\\).*\\. *\n"
;	 . font-lock-Blue-bold) ;; Query don't work
))

;;----------------------------------------------------------------------------
;; Faces for supporting font-lock
;;----------------------------------------------------------------------------
(defvar font-lock-ForestGreen 'font-lock-ForestGreen)
(defface font-lock-ForestGreen
  '((((class color) (background light)) (:foreground "ForestGreen")))
  "Font Lock mode face used to highlight ."
  :group 'font-lock-highlighting-faces)

(defvar ciao-prompt-face 'ciao-prompt-face)
(defface ciao-prompt-face
  '((((class color) (background light)) (:foreground "coral" 
						     :bold t)))
  "Face used to highlight prompts in Ciao inferior mode."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-ForestGreen-bold 'font-lock-ForestGreen-bold)
(defface font-lock-ForestGreen-bold
  '((((class color) (background light)) (:foreground "ForestGreen" 
						     :bold t)))
  "Font Lock mode face used to highlight ."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-NavyBlue-bold 'font-lock-NavyBlue-bold)
(defface font-lock-NavyBlue-bold
  '((((class color) (background light)) (:foreground "NavyBlue" 
						     :bold t)))
  "Font Lock mode face used to highlight ."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-NavyBlue 'font-lock-NavyBlue)
(defface font-lock-NavyBlue
  '((((class color) (background light)) (:foreground "NavyBlue"))
    (t (:inverse-video t :bold t)))
  "Font Lock mode face used to highlight warnings."
  :group 'font-lock-highlighting-faces)

(defvar ciao-clauseheadname-face 'ciao-clauseheadname-face)
(defface ciao-clauseheadname-face
  '((((class color) (background light)) (:foreground "Blue")))
  "Face used to highlight head names in Ciao."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-Blue-bold 'font-lock-Blue-bold)
(defface font-lock-Blue-bold
  '((((class color) (background light)) (:foreground "Blue" :bold t)))
  "Font Lock mode face used to highlight ."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-MediumBlue-bold 'font-lock-MediumBlue-bold)
(defface font-lock-MediumBlue-bold
  '((((class color) (background light)) (:foreground "MediumBlue" :bold t))
    (t (:inverse-video t :bold t)))
  "Font Lock mode face used to highlight warnings."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-keyword 'font-lock-keyword)
(defface font-lock-keyword
  '((((class color) (background light)) (:foreground "RoyalBlue")))
  "Font Lock mode face used to highlight ."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-crossref 'font-lock-crossref)
(defface font-lock-crossref
  '((((class color) (background light)) (:foreground "DarkGoldenrod" )))
  "Font Lock mode face used to highlight ."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-Goldenrod-bold 'font-lock-Goldenrod-bold)
(defface font-lock-Goldenrod-bold
  '((((class color) (background light)) (:foreground "Goldenrod"
						     :bold t))
    (t (:bold t :italic t)))
  "Font Lock mode face used to highlight ."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-DarkGreen-bold 'font-lock-DarkGreen-bold)
(defface font-lock-DarkGreen-bold
  '((((class color) (background light)) (:foreground "DarkGreen" 
						     :bold t)
     (t (:inverse-video t :bold t))))
  "Font Lock mode face."
  :group 'font-lock-hightlighting-faces)

(defvar font-lock-Brown 'font-lock-Brown)
(defface font-lock-Brown
  '((((class color) (background light)) (:foreground "Brown")
     (t (:inverse-video t :bold t))))
  "Font Lock mode face."
  :group 'font-lock-hightlighting-faces)

(defvar font-lock-Brown-bold 'font-lock-Brown-bold)
(defface font-lock-Brown-bold
  '((((class color) (background light)) (:foreground "Brown"
						     :bold t)
    (t (:inverse-video t :bold t))))
  "Font Lock mode face used to highlight warnings."
  :group 'font-lock-highlighting-faces)

;;------------------------------------------------------------
;; Key and menu bindings + documentation sections
;; These nifty functions allow autodocumenting using lpdoc! MH
;;------------------------------------------------------------

(defvar ciao-mode-map (make-sparse-keymap))

(defvar ciao-documented-commands nil
 "Stores the list of commands which will appear in the documentation,
  preceded by section comments.")

(defun ciao-define-key (map binding function)
  "A call to define-key, but we store stuff in our own format, which
  is used later to generate the documentation."
  (setq ciao-documented-commands
	(cons (list binding function) ciao-documented-commands))
  (define-key map binding function))

(defun ciao-documentation-section (sec-title sec-intro)
  "We store a section title and intro, used later to generate documentation."
  (setq ciao-documented-commands
	(cons (list 'section sec-title sec-intro) ciao-documented-commands)))

;; Should start with a section!
(defun ciao-mode-commands (map)

  (ciao-documentation-section
    "Syntax coloring and syntax based editors"
    
    "Syntax-based coloring is provided automatically when opening
Ciao/Prolog files. The mode should be set to Ciao/Prolog and the Ciao mode
menus should appear on the menu bar. Limited syntax-based indentation is
also provided:")

  (ciao-define-key map "\t" 'ciao-indent-line)

  (ciao-documentation-section 
   "Getting on-line help" 

   "The following commands are useful for getting on-line help. This
is done by accessing the @apl{info} version of the Ciao manuals or the
@apl{emacs} built-in help strings. Note also that the @apl{info}
standard @tt{search} command (generally bound to @key{s}) can be used
inside @apl{info} buffers to search for a given string.")

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
the communication with the Ciao/Prolog top level:")

  (ciao-define-key map "\C-ct" 'run-ciao-toplevel)
  (ciao-define-key map "\C-cl" 'ciao-load-buffer)
  (ciao-define-key map "\C-cL" 'ciao-load-from-main-module)

  (ciao-define-key map "\C-cq" 'ciao-set-query)
  (ciao-define-key map "\C-cQ" 'ciao-load-query)
  (ciao-define-key map "\C-cs" 'ciao-set-main-buffer)

  (ciao-define-key map "\C-cx" 'ciao-make-exec)
  (ciao-define-key map "\C-co" 'ciao-make-po)
  (ciao-define-key map "\C-ca" 'ciao-make-activemod)

  (ciao-documentation-section 
   "Locating errors and checking the syntax of assertions" 

   "These commands allow several syntactic checks of assertions, and
locating quickly the point in the source code corresponding to errors
flagged by the compiler or preprocessor: @cindex{locating errors}")

  (ciao-define-key map "\C-c`"    'ciao-find-last-run-errors)
  (ciao-define-key map "\C-ce" 'ciao-unmark-last-run-errors)
  (ciao-define-key map "\C-cE"    'ciao-check-buffer-syntax)

  (ciao-documentation-section 
   "Commands which help in typing in programs" 

   "The following commands are intended to help in the process of
writing programs: @cindex{writing programs}")

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
implement comunication with the Ciao/Prolog top level:")

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

@include{/home/clip/Systems/ciaopp/doc/readmes/README.lpdoc}

See the preprocessor manual for details. The following commands
implement the communication with the Ciao preprocessor:

")

  (ciao-define-key map "\C-cM" 'ciao-preprocess-buffer-menu)
  (ciao-define-key map "\C-cP" 'ciao-preprocess-buffer)
  (ciao-define-key map "\C-c\C-v" 'ciao-show-preprocessor-output)
  (ciao-define-key map "\C-cV" 'ciao-preprocess-buffer-and-show-output)
  (ciao-define-key map "\C-c\C-r" 'run-ciao-preprocessor)

  (ciao-documentation-section 
   "Version control" 

   "The following commands can be used to carry out a simple form of
@concept{version control} by keeping a @concept{log of changes} on a
file or a group of related files. This log is kept in a format that is
understood by @apl{lpdoc}, the Ciao documenter @cite{lpdoc-tr}. As a
result, if these version comments are present, then @apl{lpdoc} will
be able to automatically assign up to date version numbers to the
manuals that it generates. This way it is always possible to identify
to which version of the software a manual corresponds. Also,
@apl{lpdoc} can create automatically sections describing the changes
made since previous versions, which are extracted from the comments in
the changelog entries.

The main effect of these commands is automatically associate the
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
the documentation corresponding to the current buffer.  This is
specially useful while modifying the documentation for a file, in order
to check the output that will be produced.")

  (ciao-define-key map "\C-cDB" 'ciao-gen-buffer-doc)
  (ciao-define-key map "\C-cDF" 'ciao-set-lpdoc-docformat)
  (ciao-define-key map "\C-cDS" 'ciao-visit-lpdoc-settings)
  (ciao-define-key map "\C-cDG" 'ciao-gen-doc)
  (ciao-define-key map "\C-cDV" 'ciao-start-viewer)
  (ciao-define-key map "\C-cDW" 'ciao-set-lpdoc-wdir-root)

  (ciao-documentation-section 
   "Setting the top-level, preprocessor, and documenter executables"
 
   "These commands allow @index{changing the executables used} when
starting a Prolog top-level, the preprocessor, or the auto-documenter. They
also allow changing the arguments that these executables take, and changing
the path where the libraries reside. In the case of the top-level and
preprocessor, this should be done only by users which understand the
implications, but it is very useful if several versions of Ciao/Prolog or
the preprocessor are available in the system. All these arguments can be
changed through the @index{customize} options in the help menu (see
@ref{Customization}). ")

  (ciao-define-key map "\C-cSC"    'ciao-set-ciao-system)
  (ciao-define-key map "\C-cS\C-c" 'ciao-set-ciao-system-args)
  (ciao-define-key map "\C-cSP"    'ciao-set-ciaopp-system)
  (ciao-define-key map "\C-cS\C-p" 'ciao-set-ciaopp-system-args)
  (ciao-define-key map "\C-cSL"    'ciao-set-library-path)
  (ciao-define-key map "\C-cSD"    'ciao-set-lpdoc-system)
  (ciao-define-key map "\C-cS\C-d" 'ciao-set-lpdoc-system-args)
  (ciao-define-key map "\C-cS\C-l" 'ciao-set-lpdoc-libpath)

  (ciao-documentation-section 
   "Traditional Prolog Mode Commands" 

   "These commands provide some bindings and facilities for loading
programs, which are present in emacs Prolog modes of other Prolog
systems (e.g., SICStus). This is useful mainly if the Ciao/Prolog emacs mode
is used with such Prolog systems.  Note that the behavior of these
commands in Ciao is slightly different from that of SICStus and their use
(@pred{compile/1} and @pred{consult/1}) in the Ciao top-level are not
recommended.")

  (ciao-define-key map "\C-cK" 'ciao-compile-buffer)
  (ciao-define-key map "\C-ck" 'ciao-compile-region)
  (ciao-define-key map "\C-c\C-k" 'ciao-compile-predicate)
  (ciao-define-key map "\C-cC" 'ciao-consult-buffer)
  (ciao-define-key map "\C-cc" 'ciao-consult-region)
  (ciao-define-key map "\C-c\C-c" 'ciao-consult-predicate)

  (ciao-documentation-section 
   "Other commands" 
   "Some other commands which are active in the Ciao/Prolog mode:") 

  (ciao-define-key map "\C-c\C-l" 'ciao-recenter-last-ciao-buffer)

  (ciao-documentation-section 
   "Getting the Ciao/Prolog mode version" 
   "@cindex{Ciao/Prolog mode version}")

  (ciao-define-key map "\C-cv" 'ciao-report-mode-version)

  )

(ciao-mode-commands ciao-mode-map)

(defconst ciao-mode-menus
  (list "Ciao/Prolog"
     ;; "----"
     "HELP"
     ["Help (for symbol under cursor)"      ciao-help-on-current-symbol t]
     ["Complete symbol under cursor"        ciao-complete-current-symbol t]
     ["Ciao system manual"                  (ciao-goto-ciao-manual) t]
     ["List all key bindings"               (ciao-describe-mode) t]
     "----"
     "TOP-LEVEL/COMPILER"
     ["Start ciao top level"                     run-ciao-toplevel t]
     ["(Re)Load buffer into top level"           ciao-load-buffer  t]
     ["(Re)Load all modules as necessary"        ciao-load-from-main-module t]
     ["Locate (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove preproc/compiler error mark"       ciao-unmark-last-run-errors t]
     (list "Query and main file"
	   ["Set query"                          ciao-set-query t]
	   ["Call query"                         ciao-load-query t]            
	   ["(Un)Set main module"                ciao-set-main-buffer t]
	   )
     ["Check buffer syntax (incl. assertions)"   ciao-check-buffer-syntax t]
     ["Make executable from buffer"              ciao-make-exec t]
     ["Make object file (.po) from buffer"       ciao-make-po t]
     ["Make active module from buffer"           ciao-make-activemod t]
     ["Insert script header"                     ciao-insert-script-header t]
     "----"
     "TOP-LEVEL/DEBUGGER"
     ["(Un)Debug buffer source"               ciao-debug-buffer t]
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
     ["(Re)Load region (for debug)"              ciao-load-region  t]
     ["(Re)Load predicate (for debug)"           ciao-load-predicate t]
     "----"
     (list "SET MODE DEFAULTS"
       ["Set Ciao/Prolog toplevel executable"    ciao-set-ciao-system t]
       ["Set Ciao/Prolog toplevel args"          ciao-set-ciao-system-args t]
       ["Set Ciao library path"                  ciao-set-library-path t]
     )
     "----"
     (list "TRADITIONAL PROLOG COMMANDS (also for SICStus)"
           ["Compile buffer"    ciao-compile-buffer  t]
           ["Compile region"    ciao-compile-region  t]
           ["Compile predicate" ciao-compile-predicate t]
           ["Consult buffer"    ciao-consult-buffer  t]
           ["Consult region"    ciao-consult-region  t]
           ["Consult predicate" ciao-consult-predicate t]
     )
     "----"
     ["Ciao/Prolog mode version" ciao-report-mode-version t]
     )
  "Menus for Ciao/Prolog mode.")

(defconst ciao-mode-ciaopp-menus
  (list "CiaoPP"
     "----"
     "HELP"
     ["CiaoPP preprocessor manual" (ciao-goto-ciaopp-manual) t]
     ["List all key bindings" (ciao-describe-mode) t]
     "----"
     "CIAO PREPROCESSOR (in development)"
     ["Preprocess buffer (choosing options)"   ciao-preprocess-buffer-menu t]
     ["Preprocess buffer (w/previous options)" ciao-preprocess-buffer t]
     ["Locate (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Preprocess buffer and show output"  
                                      ciao-preprocess-buffer-and-show-output t]
     ["Start ciao preprocessor"                run-ciao-preprocessor t]
     "----"
     (list "SET MODE DEFAULTS"
       ["Set Ciao Preprocessor executable"      ciao-set-ciaopp-system t]
       ["Set Ciao Preprocessor executable args" ciao-set-ciaopp-system-args t]
       ["Set Ciao library path"                 ciao-set-library-path t]
     )
     "----"
     ["CiaoPP mode version" ciao-report-mode-version t]
     )
  "Menus for CiaoPP mode.")

(defconst ciao-mode-lpdoc-menus
  (list "LPdoc"
     "----"
     "HELP"
     ["LPdoc automatic documenter manual" (ciao-goto-lpdoc-manual) t]
     ["List all key bindings" (ciao-describe-mode) t]
     "----"
     "CHANGELOG / VERSION CONTROL"
     ["Insert changelog entry/increase patch #" ciao-add-comment-and-save t]
     ["Increase version number"              ciao-new-version t]
     ["Go to next changelog entry"           ciao-fetch-next-changelog-entry t]
     "----"
     "GENERATE/VIEW DOCUMENTATION"
     ["Generate documentation for buffer"        ciao-gen-buffer-doc t]
     ["View documentation in selected format"    ciao-start-viewer t]
     ["Change default doc format/visualizer"     ciao-set-lpdoc-docformat t]
     ["Locate (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Visit(/create) SETTINGS file"             ciao-visit-lpdoc-settings t]
     ["Generate documentation"                   ciao-gen-doc t]
     "----"
     (list "SET MODE DEFAULTS"
       ["Set LPdoc executable"              ciao-set-lpdoc-system t]
       ["Set LPdoc executable args"         ciao-set-lpdoc-system-args t]
       ["Set LPdoc root working directory"  ciao-set-lpdoc-wdir-root t]
       ["Set LPdoc library path"            ciao-set-lpdoc-libpath t]
     )
     "----"
     ["LPdoc mode version" ciao-report-mode-version t]
     )
  "Menus for LPdoc mode.")

(defconst ciao-inferior-mode-menus
  (list "Ciao/Prolog"
     ;; "----"
     "HELP"
     ["Help (for symbol under cursor)" ciao-help-on-current-symbol t]
     ["Complete symbol under cursor" ciao-complete-current-symbol t]
     ["Ciao system manual" (ciao-goto-ciao-manual) t]
     ["CiaoPP preprocessor manual" (ciao-goto-ciaopp-manual) t]
     ["LPdoc automatic documenter manual" (ciao-goto-lpdoc-manual) t]
     ["List all key bindings" (ciao-describe-mode) t]
     "----"
     "ERRORS"
     ["Locate (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     "----"
     "PREPROCESSOR (in development)"
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Start ciao preprocessor"                run-ciao-preprocessor t]
     "----"
     "COMPILER/TOP-LEVEL/DEBUGGER"
     ["Start ciao top level"               run-ciao-toplevel t]
     ["Set query as default"               ciao-set-query t]
     ["Load default query"                 ciao-load-query t]
     "----"
     (list "SET MODE DEFAULTS"
       ["Set Ciao/Prolog executable"       ciao-set-ciao-system   t]
       ["Set Ciao/Prolog executable args"       ciao-set-ciao-system-args t]
       ["Set Ciao Preprocessor executable" ciao-set-ciaopp-system t]
       ["Set Ciao Preprocessor executable args" ciao-set-ciaopp-system-args t]
       ["Set Ciao library path"            ciao-set-library-path  t]
     )
     "----"
     ["Ciao/Prolog mode version" ciao-report-mode-version t]
     )
  "Menus for Ciao/Prolog (inferior) mode.")

;;------------------------------------------------------------
;; Syntax and movement
;;------------------------------------------------------------

(defvar ciao-mode-syntax-table nil)
(if ciao-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\^m ">" table)
    (modify-syntax-entry ?\' "\"" table)
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
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'ciao-comment-indent)
  (make-local-variable 'update-version-comments)
  (setq update-version-comments 0) ; 0 means "uninitialized"
  (make-local-variable 'last-process-buffer-used)
  (setq last-process-buffer-used ciao-toplevel-buffer-name)
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
  (ciao-locate-manual-in-info-dir "ciao:"))

(defun ciao-goto-ciao-manual () 
  "Go to the part of the info directory containing the Ciao manual."
  (interactive) 
  (ciao-locate-manual-in-info-dir "ciao:")
  (Info-follow-nearest-node))

(defun ciao-goto-ciaopp-manual () 
  "Go to the part of the info directory containing the Ciao
preprocessor manual." 
  (interactive) 
  (ciao-locate-manual-in-info-dir "ciaopp:")
  (Info-follow-nearest-node))

(defun ciao-goto-lpdoc-manual () 
  "Go to the part of the info directory containing the lpdoc
(automatic documenter) manual." 
  (interactive) 
  (ciao-locate-manual-in-info-dir "lpdoc:")
  (Info-follow-nearest-node))

(defun ciao-locate-manual-in-info-dir (text) 
  (info) 
  (Info-directory)
  (if (search-forward text nil t) 
      (recenter 0)
    (error (concat "Could not find " text " manual in info dir"))))

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
is removed or modified (i.e., from now on \\[ciao-save-buffer] simply
saves the buffer).

   @item{@key{y}} Turn version control on for this file. 

   @end{description}

   If @key{y} is selected, then the system prompts again regarding how
and where the version and patch number information is to be
maintained. The following options are available:

   @begin{description}

   @item{@tt{on}} All version control information will be contained
within this file. When saving a buffer (\\[ciao-save-buffer]) emacs
will ask if a changelog entry should be added to the file before
saving. If a comment is entered by the user, a new patch number is
assigned to it and the comment is added to the file. This patch number
will be the one that follows the most recent changelog entry already
in the file. This is obviously useful when maintaining version numbers
individually for each file.

   @item{@tt{<directory_name>}} Global version control will be
performed coherently on several files. When saving a buffer
(\\[ciao-save-buffer]) emacs will ask if a changelog entry should be
added to the file before saving. If a comment is given, the global
patch number (which will be kept in the file:
@tt{<directory_name>/GlobalPatch}) is atomically incremented and the
changelog entry is added to the current file, associated to that patch
number. Also, a small entry is added to a file
@tt{<directory_name>/GlobalChangeLog} which points to the current
file. This allows inspecting all changes sequentially by visiting all
the files where the changes were made (see
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

  "Same as \\[ciao-save-buffer] except that it forces prompting for
inclusion of a changelog entry even if the buffer is unmodified."   

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
      (if (eq save-option t)
	  ;; no need to ask
	  (ciao-update-version (ciao-version-maint-type))
	;; ask 
	(if (not (string= 
		  (read-string "Insert changelog entry (y/n) ?" "n")
		  "y"))
	    (save-buffer);; normal save and return
	  ;; update version and save
	  (ciao-update-version (ciao-version-maint-type))
	  )
	)
      )
    )
  )

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
	change-file)
  (setq original-point (point))
  (beginning-of-buffer)
  (cond
   ((not (string= version-dir "on"))
    ;; Previous version is in external file - get it
    ;; For locking, we are taking advantage of emacs file locking by
    ;; modifying the buffer right away.
    (setq original-buffer (current-buffer))
    (setq version-file (concat version-dir "/GlobalVersion"))
    (if (file-readable-p version-file)
	(progn 
	  (find-file version-file)
	  (beginning-of-buffer)
	  (set-mark (point))
	  (search-forward-regexp "\\.")
	  (backward-char 1)
	  ;; kill-region modifies and sets lock...
	  (setq version-major
		(buffer-substring-no-properties (mark) (point)))
	  (forward-char 1)
	  (set-mark (point))
	  (end-of-line)
	  (setq version-minor
		(buffer-substring-no-properties (mark) (point)))
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
	    (set-mark (point))
	    (end-of-line)
	    (setq patch-number 
		  (buffer-substring-no-properties (mark) (point)))
	    (kill-buffer (current-buffer))
	    )
	;; No patch file: new patch number
	(setq patch-number "-1")))

    (switch-to-buffer original-buffer)
    )
   ((search-forward-regexp "^ *:- *comment( *version(" nil t)
    ;; A previous version exists in the file: get it
    (set-mark (point))
    (search-forward-regexp "\\*")
    (backward-char 1)
    (setq version-major
	  (buffer-substring-no-properties (mark) (point)))
    (forward-char 1)
    (set-mark (point))
    (search-forward-regexp "\\+")
    (backward-char 1)
    (setq version-minor
	  (buffer-substring-no-properties (mark) (point)))
    (forward-char 1)
    (set-mark (point))
    (search-forward-regexp " *,")
    (backward-char 1)
    (setq patch-number 
	  (buffer-substring-no-properties (mark) (point)))
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

  ;; Hey, why not give them value right here
  (setq month (format-time-string "%m"))
  (setq day   (format-time-string "%d"))
  (setq year  (format-time-string "%Y"))
  (setq time  (format-time-string "%H:%M*%S+'%Z'"))

  ;; If version came from file in a directory, update the
  ;; version files 
  (if (or (string= version-dir "on") (string= comment ""))
      nil

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file version-file)
    (beginning-of-buffer)
    (set-mark (point))
    (end-of-line)
    (delete-region (mark) (point))
    (insert-string (concat version-major "." version-minor))
    (setq old-version-control version-control)
    (setq version-control 'never)
    (save-buffer (current-buffer))
    (setq version-control old-version-control)
    (kill-buffer (current-buffer))

    (switch-to-buffer original-buffer) ;; So that relative paths work!
    (find-file patch-file)
    (beginning-of-buffer)
    (set-mark (point))
    (end-of-line)
    (delete-region (mark) (point))
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
      (insert-string "\n:- comment(version_maintenance,off).\n\n")
      )
    (beginning-of-buffer)
    (ciao-insert-version-comment 
     version-major version-minor patch-number month day year time 
     (buffer-file-name original-buffer))
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
  (search-backward-regexp "^ *:- *comment( *version(")
  )

(defun ciao-goto-first-version-comment ()
  "Position ourselves at first changelog entry if it exists"
  (beginning-of-buffer)
  ;; If previous version exists
  (if (search-forward-regexp "^ *:- *comment( *version(" nil t)
      (beginning-of-line)
    ;; If no previous version exists
;;     (beginning-of-buffer)
;;     (if (search-forward-regexp "^ *:- *module(" nil t) t t)
;;     (ciao-next-blank-line-or-eof)
    (goto-char (point-max))
    (insert-string 
     (concat
      "\n"
      "%% Note that the \"assertions\" library needs to be included in order\n"
      "%% to support \":- comment(...,...).\" declarations such as these.\n"
      "%% These version comment(s) can be moved elsewhere in the file.\n"
      "%% Subsequent version comments will be placed above the last one\n"
      "%% inserted.\n\n"
      )
     )
    )
  )


;; (defun ciao-next-blank-line-or-eof ()
;;   (if (search-forward-regexp "^ *$" nil t)
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
  (setq original-buffer (current-buffer))
  (search-forward-regexp "^ *:- *comment( *version(")
  (recenter 0)
  (set-mark (point))
  (search-forward-regexp ") *,")
  (backward-char 1)
  (setq version (buffer-substring-no-properties (mark) (point)))
  (if (string-match "GlobalChangeLog" (buffer-name))
      ;; It is a changelog buffer: find matches in files
      (progn
	(search-forward "\"")
	(set-mark (point))
	(search-forward "\"")
	(backward-char 1)
	(setq file (buffer-substring-no-properties (mark) (point)))
	(find-file-other-window file)
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
      (ciao-mode) ;; set buffer to ciao mode so that bindings are active!
      (beginning-of-buffer)
      (search-forward version)
      (beginning-of-line)
      (recenter 0)
      (switch-to-buffer-other-window original-buffer))
    ))

(defun ciao-handle-version-control-option ()
  "Look and see if there is a local variable designating whether
  version control should be performed."
  (interactive)
  (save-excursion
    (cond
     ((not (string= (ciao-version-maint-type) nil))
      ;; local var already present: just return
      ;; (message (concat "Local var found;value: " (ciao-version-maint-type)))
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
	  (insert-string "\n:- comment(version_maintenance,off).\n\n")
	  (message "Off - see comment inserted at end of file")
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

	  (insert-string 
	   (concat
	    "\n:- comment(version_maintenance," 
	    (if (or (equal option-dir "on") (equal option-dir "off"))
		option-dir
	      (concat "dir('" option-dir "')" ))
	    ").\n\n"))
	  (message "On - see comment inserted at end of file")
	  (setq update-version-comments option-dir))
	 )
	)
      )
     )
    )
  )

(defun ciao-version-maint-type ()
  (interactive)
  (if (not (eq update-version-comments 0))
	update-version-comments
    (save-excursion
      (beginning-of-buffer)
      (if (search-forward-regexp 
	   "^ *:- *comment( *version_maintenance *, *" nil t)
	  (progn
	    (search-forward-regexp "dir( *'*" nil t)
	    (set-mark (point))
	    (search-forward-regexp "'* *)" nil t)
	    (goto-char (match-beginning 0))
	    (setq update-version-comments 
		  (buffer-substring-no-properties (mark) (point)))
	    (message (concat "DIR: " update-version-comments))
	    )
	(setq update-version-comments nil)
	update-version-comments
        ))))

(defun ciao-mode-version-control ()
  (interactive)
  (if (string= (file-name-nondirectory (buffer-file-name))
	       "ciao.el.body")
      (save-excursion
	(set-buffer (find-file-noselect "CiaoMode.pl"))
	(set-buffer-modified-p t)
	(ciao-save-buffer))))

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
;; The Ciao / Prolog / &-Prolog Mode
;;------------------------------------------------------------

(defun ciao-mode ()
  "Major mode for editing/debugging/compiling/running Ciao and Prolog code.

Tab indents for Prolog syntax. With an argument, shifts rest of
expression rigidly with the current line. Paragraphs are separated
only by blank lines and '%%'. '%'s start comments. Syntax highlighting
done (font-lock must be available and not disabled).

The following keyboard commands are available (see also the
mode-specific entries in the menu-bar if enabled):

\\{ciao-mode-map}

Entry to this mode calls the value of ciao-mode-hook if that value is
non-nil." 

  (interactive)
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
  (easy-menu-define ciao-lpdoc-menu ciao-mode-map 
		    "LPdoc Mode Menus" ciao-mode-lpdoc-menus)
  (easy-menu-define ciao-ciaopp-menu ciao-mode-map 
		    "CiaoPP Mode Menus" ciao-mode-ciaopp-menus)
  (easy-menu-define ciao-menu ciao-mode-map 
		    "Ciao/Prolog Mode Menus" ciao-mode-menus)
  (easy-menu-add ciao-menu)
  (easy-menu-add ciao-ciaopp-menu)
  (easy-menu-add ciao-lpdoc-menu)
  
  ;; MR added to support font-lock
  (if (not window-system)
      nil
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(ciao-mode-font-lock-keywords t)))
  (run-hooks 'ciao-mode-hook))

(add-hook
 'ciao-mode-hook
 (function
  (lambda ()
    (define-key ciao-mode-map "\C-x\C-s" 'ciao-save-buffer)
)))

;; MR added to support font-lock
(if (not window-system)
    nil
  (add-hook 'ciao-mode-hook 'turn-on-font-lock)
  (add-hook 'ciao-inferior-mode-hook 'turn-on-font-lock))


;;------------------------------------------------------------
;; Inferior process management
;;------------------------------------------------------------

(defvar ciao-inferior-mode-map nil)

(defun ciao-inferior-mode ()

  "Inferior mode for interaction with Ciao/Prolog toplevel and preprocessor.

This is a major emacs mode which allows interacting with an inferior
process running the Ciao/Prolog top-level or Ciao preprocessor. You can talk
to the Ciao/Prolog top-level or the preprocessor by typing commands directly
in the corresponding buffer as if in a normal shell. You can also send
files or parts of files to be preprocessed or compiled from any buffer
which is in ciao-mode (see the emacs commands available in such
buffers).

In addition, the following emacs commands are available in these
buffers: 
\\{ciao-inferior-mode-map}

Entry to this mode calls the value of ciao-mode-hook with no arguments,
qif that value is non-nil.  Likewise with the value of comint-mode-hook.
ciao-mode-hook is called after comint-mode-hook.

Other commands: 
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-delchar-or-maybe-eof] sends end-of-file as input.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops, likewise. \\[comint-quit-subjob] sends quit signal, likewise."
  (interactive)
  (cond ((not (eq major-mode 'ciao-inferior-mode))
	 (kill-all-local-variables)
	 (comint-mode)
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
;	       ((string= (buffer-name) 
;			 (concat "*" ciao-lpdoc-buffer-name "*"))
;		(set-process-filter (get-buffer-process (current-buffer))
;				    'ciao-lpdoc-filter))
	       ;; This case is usually used in normal shell. The filter is
	       ;; to handle source-level embedded debugger messages
	       (t (set-process-filter (get-buffer-process (current-buffer))
				     'ciao-debug-filter))
	 )
	 (set-process-sentinel (get-buffer-process (current-buffer))
			       'ciao-inferior-process-sentinel)
         ;; 
	 (if ciao-inferior-mode-map
	     nil
	   ; HB: 930205: Use the "correct" function 'copy-keymap'
	   ; to copy a keymap.
	   (setq ciao-inferior-mode-map (copy-keymap comint-mode-map))
	   ;; Not such a good idea: completion is better
	   ;; (define-key ciao-inferior-mode-map "\t" 'ciao-indent-line)
	   (define-key ciao-inferior-mode-map "\t" 'comint-dynamic-complete)
	   (define-key ciao-inferior-mode-map "\C-cq" 'ciao-set-query)
	   (define-key ciao-inferior-mode-map "\C-cQ" 'ciao-load-query)
	   (define-key ciao-inferior-mode-map "\C-c/" 
	     'ciao-complete-current-symbol)
	   (define-key ciao-inferior-mode-map "\C-c\C-i" 
	     'ciao-help-on-current-symbol)
	   (define-key ciao-inferior-mode-map "\M-?" 
	     'comint-dynamic-list-filename-completions)
	   (define-key ciao-inferior-mode-map "\C-c`"
	     'ciao-find-last-run-errors-here)
	     )
	 (use-local-map ciao-inferior-mode-map)
	 (easy-menu-define ciao-inferior-menu ciao-inferior-mode-map 
			   "Ciao/Prolog Mode Menus" ciao-inferior-mode-menus)
	 (setq comint-prompt-regexp 
               "^\\(\\(\\|[0-9]+ \\|ciaopp \\|| \\)\\?-\\)")
;              "\\(^|* *\\?- *\\)\\|\\(^ciaopp \\?- *\\)") 
					; Set Ciao/SICStus prompt patterns
	 
	 ;; MR added to support font-lock
 	 (if (not window-system)
 	     nil
 	   (make-local-variable 'font-lock-defaults)
 	   (setq font-lock-defaults '(ciao-inferior-font-lock-keywords t)))
	 (run-hooks 'ciao-mode-hook))))

(defun ciao-input-filter (str)
  (cond ((string-match "\\`\\s *\\'" str) nil) ;whitespace
	((not (eq major-mode 'ciao-inferior-mode)) t)
	((= (length str) 1) nil)	;one character
	((string-match "\\`[rf] *[0-9]*\\'" str) nil) ;r(edo) or f(ail)
	(t t)))

(defun run-ciao-toplevel ()
  "Ensure that an inferior Ciao/Prolog top-level process is
running. Normally, it is not necessary to use this function since
execution of any of the other functions related to the top level
ensures that a top level is started (starting one if required). This
function is useful when one would like to start a top level in order
to type a command directly by hand into it."
  (interactive)
  (ciao-ensure-inferior-process ciao-toplevel-buffer-name))

(defun run-ciao-preprocessor ()
  "Ensure that an inferior Ciao preprocessor process is running"
  (interactive)
  (ciao-ensure-inferior-process ciao-ciaopp-buffer-name))

;; MH Made it recenter, and then the functions below are trivial
(defun ciao-ensure-inferior-process (buffname)
  (setq buff (buffer-name))
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
  (switch-to-buffer-other-window 
   (if (equal ""
	      ;; Done differently because of filenames with blanks...
	      ;; (ciao-get-string-after-blank system)
	      system-args
	      )
       (make-comint buffname 
		    ;; Done differently because of filenames with blanks...
		    ;; (ciao-get-string-before-blank system)
		    system
		    ) 
     (make-comint buffname 
		  ;; Done differently because of filenames with blanks...
		  ;; (ciao-get-string-before-blank system) ; command name
		  system
		  nil                                   ; filename
		  ;; Done differently because of filenames with blanks...
		  ;; (ciao-get-string-after-blank system)  ; arguments
		  system-args
		  ))) 
  (ciao-inferior-mode)
  (goto-char (point-max))
  (switch-to-buffer-other-window buff)
  (setq last-process-buffer-used buffname))

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
(top-level or preprocessor)."
  (interactive)
  (ciao-ensure-inferior-process last-process-buffer-used)
  )

;; General interface to subprocess
(defun ciao-send-command (buffername command recenter-opt)
  (setq buff (buffer-name))
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
			 'ciao-do-send-command-global t))))
    (ciao-do-send-command buffername command recenter-opt)))

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
   (switch-to-buffer-other-window buff))

;; MH Alternative?
;; (defun ciao-send-command (buffername command)
;;   (comint-proc-query buffername command))

;; (defun ciao-wait-for-prompt (buffername)
;;   t)

;; ;; MH This works, but you do not see the progress (only the dots) :-(
(defun ciao-wait-for-prompt (buffername)
  (setq original-buffer (current-buffer))
  (setq prompt-buffer (concat "*" buffername "*"))
  (switch-to-buffer-other-window prompt-buffer)
  (ciao-do-wait-for-prompt (get-buffer-process prompt-buffer) 1)
  (switch-to-buffer-other-window original-buffer)
  )

(defun ciao-do-wait-for-prompt (proc ndots)
  (goto-char comint-last-input-end)
  (message (concat "Waiting for command completion" (dotlist ndots)))
  (if (search-forward "?- " nil t)
      t
;;    (accept-process-output proc 0 500)
    (sleep-for 0.5)
    (ciao-do-wait-for-prompt proc (+ ndots 1))
    )
  )

(defun dotlist (N) 
  (if (= N 0)
      nil
    (concat "." (dotlist (- N 1)))))
	 

;;------------------------------------------------------------
;; Locating errors
;;------------------------------------------------------------

(defun ciao-find-last-run-errors ()
  "Go to the location in the source file containing the next error reported by
the last Ciao/Prolog subprocess (preprocessor or toplevel) which was run."
  (interactive)
  (ciao-find-error last-process-buffer-used))

(defun ciao-unmark-last-run-errors()
  "Remove the error mark in a buffer."
  (interactive)
  (save-excursion
    (if ciao-previous-error
	(progn 
	  (set-buffer (get-file-buffer (car (cdr (cdr ciao-previous-error)))))
	  (ciao-uncolor (car ciao-previous-error)
			(car (cdr ciao-previous-error))
			'ciao-error))
      (setq ciao-previous-error nil))
    (if ciao-inferior-error
	(progn
	  (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	  (ciao-uncolor ciao-inferior-error
			ciao-inferior-error
			'ciao-error)
	  (setq ciao-inferior-error nil)))))

(defun ciao-find-last-run-errors-here ()
  "Go to location in source file containing next error reported by the 
Ciao/Prolog (sub)process (preprocessor or toplevel) running in the current
buffer" 
  (interactive)
  (ciao-find-error nil))

;; These not used any more...
;; (defun ciao-find-ciao-error ()
;;   "Go to location in source file containing next error reported by the 
;;    Ciao/Prolog top-level subprocess" 
;;   (interactive)
;;   (ciao-find-error ciao-toplevel-buffer-name))
;; 
;; (defun ciao-find-ciaopp-error ()
;;   "Go to location in source file containing next error reported by the 
;;    Ciao preprocessor subprocess" 
;;   (interactive)
;;   (ciao-find-error ciao-ciaopp-buffer-name))
 
(defun ciao-find-error (processbuff)
  "Go to location in source file containing next error reported by a
   Ciao/Prolog subprocess. If processbuff is nil, we are in the process
   buffer already" 
;  (save-excursion
  (let ((origbuff (current-buffer)) error beginline endline filename)
    ;; if starting from file buffer, go to process buffer
    (if (eq processbuff nil)
	()
      ;; repaint (eliminates any previous error marks in buffer)
      (if ciao-previous-error
	  (progn
	    (get-file-buffer (car (cdr (cdr ciao-previous-error))))
	    (if (> (car ciao-previous-error) 0)
		(ciao-uncolor (car ciao-previous-error)
			      (car (cdr ciao-previous-error))
			      'ciao-error))
	    (setq ciao-previous-error nil)))
      (switch-to-buffer-other-window (concat "*" processbuff "*")))
    ;; get error data
    (setq error (ciao-get-next-error-data))
    (setq beginline (car error))
    (setq endline (car (cdr error)))
    (setq filename (car (cdr (cdr error))))
    (if (eq error nil)
	;; no more errors
	(progn
	  (goto-char (point-max)) ;; goto end of process buffer
	  (if (eq processbuff nil)
	      ()
	    ;; if starting from file buffer, return to (last) file buffer
	    (switch-to-buffer-other-window origbuff))
	  (error "No (more) errors"))
      ;; error located, go to file, if known
      (if (eq filename nil)
	  (message "No corresponding file could be determined.")
	(find-file-other-window filename)
	(if (< beginline 0)
	    ;; No line numbers: just visit file
            (progn 
	      (beginning-of-buffer)
	      (setq ciao-previous-error nil))
	  ;; Else, highlight region in opened file...
	  (push-mark (point) t)
	  (goto-line beginline)
	  (recenter 0)
	  (ciao-color beginline endline 'secondary-selection 'ciao-error)
	  (setq ciao-previous-error error)
	  (goto-line (+ endline 1))
	  (backward-char 1)
	  (message "Mark set"))
	;; If started from process buffer, return to it
	(if (eq processbuff nil)
	    (switch-to-buffer-other-window origbuff)
	  t)))))

(defun ciao-get-next-error-data ()
  "Locates next error, and highlights it. Returns:
     nil -- if no more errors
     '(beginline endline file) -- if an error found, where
        beginline/endline = location of error in process buffer
        file = source file containing error (if nil: no file was located)"

  ;; Uncolor previous error
  (if ciao-inferior-error
      (progn
	(ciao-uncolor ciao-inferior-error
		      ciao-inferior-error
		      'ciao-error)
	(setq ciao-inferior-error nil)))

  (beginning-of-line)
  (if (or (not (search-backward-regexp 
               "^\\({?WARNING.*:\\|{?ERROR.*:\\|\\?-\\)" 
               nil t))
          (string=  (buffer-substring-no-properties (point) (+ (point) 1))
		    "?"))
      ;; No (more) errors found
      nil
    (let ((messpoint (point)) beginline endline openpoint filename)
      (recenter 0)
      (if (not (search-forward "lns " (+ (point) 80) t))
	  ;; No line number info: -1 -1
	  (progn
	    (setq beginline -1)
	    (setq endline -1))
	;; Get line number info.
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
      (beginning-of-line)
      (setq ciao-inferior-error (ciao-what-line))
      (ciao-color ciao-inferior-error
		  ciao-inferior-error
		  'secondary-selection
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
          (search-forward-regexp "\\.\\(po\\|itf\\|asr\\|pl\\)\\>")
          (setq filename (fix-cygwin-drive-letter
                (concat (buffer-substring-no-properties beg (match-beginning 0)) ".pl"))))
	(goto-char messpoint)
        (beginning-of-line))
      (cons beginline (cons endline (cons filename nil)))
      )))

(defun fix-cygwin-drive-letter (filename)
  (if (not (eq (string-match "//./" filename) 0))
      filename
    (concat (substring filename 2 3) ":" (substring filename 3))
    ))

;;------------------------------------------------------------
;; Assertions and syntax cheking
;;------------------------------------------------------------

(defun ciao-check-buffer-syntax ()

  "Check the @em{syntax} of the code and assertions in the current buffer,
as well as imports and exports.  Note that full (semantic) assertion
checking must be done with the preprocessor."

  (interactive)
  (if (and ciao-assrt-lib-loaded ;; if lib loaded and process still running...
	   (comint-check-proc 
	    (get-buffer-create (concat "*" ciao-toplevel-buffer-name "*"))))
      ()
    (ciao-send-command 
     ciao-toplevel-buffer-name 
     "use_module(library('assertions/assrt_lib'))."
     t)
    (ciao-wait-for-prompt ciao-toplevel-buffer-name)
    (setq ciao-assrt-lib-loaded t)
    )
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "prolog_flag(verbose_compilation,_Old,off),"
           "check_code_and_assrt_syntax('" (buffer-file-name) "'),"
           "prolog_flag(verbose_compilation,_,_Old)." 
	   )
   t)
  )

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
  "Preprocess the buffer, selecting options. Instructs
the preprocessor to load the current buffer and start an interactive dialog
in which the different options available in the preprocessor can 
be set. "
  (interactive)
  (ciao-send-command 
   ciao-ciaopp-buffer-name (ciao-build-ciaopp-command "[]") t))

(defun ciao-preprocess-buffer ()
  "Preprocess the buffer, using the previously selected options. If no
options were set previously, then the preprocessor defaults are
used."
  (interactive)
  (ciao-send-command 
   ciao-ciaopp-buffer-name (ciao-build-ciaopp-command nil) t))

(defun ciao-build-ciaopp-command (options)
  (concat "precompile('" (buffer-file-name)
	  (if (string= options nil)
	      "')."
	    (concat "'," options ").") )))

(defun ciao-show-preprocessor-output ()
  "Show last output file produced by Ciao preprocessor. The preprocessor
works by producing a file which is a transformed and/or adorned (with
assertions) version of the input file. This command is often used after
running the preprocessor in order to visit the output file and see the
results from running the preprocessor."
  (interactive)
  (switch-to-buffer-other-window (concat "*" ciao-ciaopp-buffer-name "*"))
  (ciao-show-file))

(defun ciao-show-file ()
  "Show last file produced by Ciao preprocessor. This is the same as
the previous function, but is meant to be called already from the 
buffer in which the processor is running."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (search-backward "written file ")
    (forward-char 13)
    (set-mark (point))
    (search-forward "}")
    (backward-char 1)
    (setq file (buffer-substring-no-properties (mark) (point)))
    (if (get-file-buffer file)
	;; The complication is to not complain if disk more recent!
	(progn 
	  (switch-to-buffer (get-file-buffer file))
	  (let ((local-buff-point (point)))
	    (kill-buffer (get-file-buffer file))
	    (find-file file)
	    (goto-char local-buff-point)))
      (find-file file)
      )
    ))
;; (global-set-key "\C-c\C-f" 'ciao-show-file)
(define-key comint-mode-map "\C-c\C-v" 'ciao-show-file)


(defun ciao-preprocess-buffer-and-show-output ()
  "Preprocess the buffer, using the previously selected (or default)
options, waits for preprocessing to finish and displays the preprocessor
output (leaving the cursor at the same point if already on a preprocessor
output file). This allows running the preprocessor over and over and
watching the output while modifying the source code. "
  (interactive)
  (add-hook 'ciao-ciaopp-prompt-emacs-hook 'ciao-show-preprocessor-output
	    t)
  (ciao-preprocess-buffer))

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
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "make_exec('" (buffer-file-name) "',_)." 
;; This was useful but now 'make_exec(FILE,_)' works (better!)
;; 	   (substring (buffer-name) 0 (string-match ".pl" (buffer-name))) 
;; 	   "')." 
	   )
   t))

(defun ciao-make-po ()
  "Make a Prolog object (.po) file from the code in the current buffer.
This is very useful during debugging or program development, when only
one or a few files of a large application are modified. If the
application executable is dynamically linked, i.e., the component .po
files are loaded dynamically during startup of the application, then
this command can be used to recompile only the file or files which have
changed, and the correct version will be loaded dynamically the next
time the application is started. However, note that this only works if
the inter-module interfaces have not changed.  A safer, but possibly
slower way is to generate the executable again, letting the Ciao
compiler, which is inherently incremental, determine what needs to be
recompiled."
  (interactive) 
  (ciao-send-command
   ciao-toplevel-buffer-name 
   (concat "make_po('" (buffer-file-name) "').") t))

(defun ciao-make-activemod ()
  "Make an active module executable from the code in the current
buffer. An active module is a remote procedure call server (see the
@lib{activemod} library documentation for details)."
  (interactive)
  (ciao-send-command 
   ciao-toplevel-buffer-name 
   (concat "make_actmod('" (buffer-file-name) "','" 
    (read-string "Address publishing method: " 
	         "actmods/filebased_publish")
           "')." )
   t))

;;------------------------------------------------------------
;; Loading
;;------------------------------------------------------------

;; This is, as so many other things, an approximation...
(defun ciao-get-module-name ()
  (save-excursion 
    (beginning-of-buffer)
    (let ((module-name
           (if (eq (search-forward-regexp "^ *:- *\\(module\\|class\\)( *" 
                                          10000 t) nil)
               "user"
             (set-mark (point))
             (search-forward-regexp " *\\(,\\|)\\)")
             (goto-char (match-beginning 0))
             (buffer-substring-no-properties (mark) (point)))))
      (if (eq (string-match "_" module-name) 0)
	  (file-name-nondirectory 
	   (file-name-sans-extension
	    (buffer-file-name (current-buffer))))
 	module-name)
      )))

(defun ciao-load-command (filename delfile)
  (save-excursion 
    (find-file filename)
    (beginning-of-buffer)
    (setq command 
	  (concat
	   (if (string= (ciao-get-module-name) "user")
	       "ensure_loaded('"
	     (beginning-of-buffer)
	     (if (eq (search-forward-regexp "^ *:- *class( *" 10000 t) nil)
		 "use_module('"
	       (if ciao-objects-lib-loaded
		   "use_class('"
		 (setq ciao-objects-lib-loaded 't)
		 "use_package(objects).\nuse_class('")))
	   filename
	   "')."))
    (if (eq delfile nil)
	command
      (kill-buffer (buffer-name))
      command)
    ))

(defun ciao-load-buffer ()
  "Load the current buffer (and any auxiliary files it may use) into the
top level. The type of compilation performed (@index{compiling} or
@index{interpreting}) is selected automatically depending on whether the
buffer has been marked for debugging or not -- see below. In case you try
to load a file while in the middle of the debugging process the debugger is
first aborted and then the buffer is loaded. Also, if there is a defined
query, the user is asked whether it should be called."
  (interactive)
  (if (eq (comint-check-proc (get-buffer 
			      (concat "*" ciao-toplevel-buffer-name "*"))) nil)
      ;; If Ciao/Prolog buffer doesn't exist then do the normal load
      (ciao-real-load-buffer)
    ;; Abort while debugging and then continue the normal process
    (let ((column
           (save-excursion
             (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
             (end-of-buffer)
             (current-column))))
      (if (< column 10)
	  (ciao-real-load-buffer)
	(add-hook 'ciao-prompt-inferior-hook 'ciao-enable-trace t)
	(add-hook 'ciao-prompt-inferior-hook 'ciao-real-load-buffer t)
	(ciao-send-command ciao-toplevel-buffer-name "a" t)))))

(defun ciao-real-load-buffer ()
  "This function really loads the buffer. And in case a default query has been
defined it asks the user if this query should be called."
  (interactive)
  ;; Uncolor previous error if there was any
  (if ciao-previous-error
      (progn
	(get-file-buffer (car (cdr (cdr ciao-previous-error))))
	(if (> (car ciao-previous-error) 0)
	    (ciao-uncolor (car ciao-previous-error)
			  (car (cdr ciao-previous-error))
			  'ciao-error))
	(setq ciao-previous-error nil)))
  (if ciao-inferior-error
      (save-excursion
	(switch-to-buffer (concat "*" ciao-toplevel-buffer-name "*"))
	(ciao-uncolor ciao-inferior-error
		      ciao-inferior-error
		      'ciao-error)
	(setq ciao-inferior-error nil)))

  (ciao-send-command ciao-toplevel-buffer-name 
		     (ciao-load-command (buffer-file-name) nil) t)
  (if (string= ciao-query "")
      t
    (add-hook 'ciao-prompt-inferior-hook 'ciao-load-query t)))

(defun ciao-load-from-main-module ()
  "Load the module designed as @index{main module} (and any auxiliary files
it may use) into the top level. If no main module is defined it will load
the current buffer. The type of compilation performed
(@index{compiling} or @index{interpreting}) is selected automatically
depending on whether the buffer has been marked for debugging or not -- see
below. In case you try to load a file while in the middle of the debugging
process the debugger is first aborted and then the buffer is loaded. Also,
if there is a defined query, the user is asked whether is should be called."
  (interactive)
  ;; Abort while debugging and then continue the normal process
  (let ((column
         (save-excursion
           (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
           (end-of-buffer)
           (current-column))))
    (if (< column 10)
	(ciao-real-load-from-main-module)
      (add-hook 'ciao-prompt-inferior-hook 'ciao-enable-trace t)
      (add-hook 'ciao-prompt-inferior-hook 'ciao-real-load-from-main-module t)
      (ciao-send-command ciao-toplevel-buffer-name "a" t))))

(defun ciao-real-load-from-main-module ()
  (interactive)
  (if (string= ciao-main-buffer "")
      (ciao-load-buffer)
    (ciao-send-command ciao-toplevel-buffer-name
		       (concat "use_module('" ciao-main-buffer "').") t)))

(defun ciao-load-query ()
  "Ask user if the predefined query should be issued."
  (interactive)
  (if (y-or-n-p (concat "Do you wish call the query `" ciao-query "'? "))
      (ciao-send-command ciao-toplevel-buffer-name ciao-query t)
    t))

(defun ciao-load-region (start end)
  "Load the current region (between the cursor and a previous mark)
into the top level for debugging. Since loading a 
region of a file is typically done for debugging and/or testing purposes,
this command always loads the region in debugging mode (interpreted)."
  (interactive "r")
  (message "Loading Ciao/Prolog code... ")
  (setq temp-file (ciao-new-temp-code-file))
  (ciao-write-region start end temp-file)
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "debug_module(user), ensure_loaded('" temp-file "')." ) t)
  (ciao-wait-for-prompt ciao-toplevel-buffer-name)
  (ciao-del-temp-files temp-file)
  (message "Loading Ciao/Prolog code... done"))

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
	  (read-string string default nil default))
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
  "This is a shortcut which is particularly useful when debugging a single
module or file. It corresponds to several lower-level actions.  Those
lower-level actions depend on how the module was selected for
debugging. In case the module was not marked for source-level
debugging, it marks the module corresponding to the current buffer for 
source-level debugging, reloads it to make sure that it is loaded in
the correct way for debugging (same as \\[ciao-load-buffer]), and sets 
the debugger in trace mode (i.e., issues the @tt{trace.} command to
the top-level shell). Conversely,  if the module was already marked for 
source-level debugging then it will take the opposite actions, 
i.e., it unmarks the module for source-level debugging, reloads it, 
and sets the debugger to non-debug mode."
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
	   ;; Buffer is mark for source debug
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
  (interactive)
  (let ((module-name (ciao-get-module-name)))
    (if (and (> (length module-name) 3)
             (string= (substring module-name -3) ".pl"))
	(file-name-sans-extension module-name)
      module-name)))

(defun ciao-select-buffers-for-debug ()
  "Visits all Ciao/Prolog files which are currently open in a buffer
allowing selecting for each of them whether to debug them or not and the
type of debugging performed. When working on a multiple module program, it
is possible to have many modules open at a time. In this case, you will
navigate through all open Ciao/Prolog files and select the debug mode for
each of them (same as doing \\[ciao-select-debug-mode] for each)."

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
  (let (buffers-debug)
    (save-excursion
      (set-buffer (concat "*" ciao-toplevel-buffer-name "*"))
      (search-backward "display_debugged.")
      ;; Match all tradicional debugged modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (if (search-forward-regexp "\\[\\(.*\\)\\]" end t)
	  (setq buffers-debug (funcall ciao-match-string 1))
;;	    (setq buffers-debug (match-string-no-properties 1))
	(setq buffers-debug ""))
      ;; Match all source debug modules
      (forward-line)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
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
  (setq temp-file (ciao-temp-code-file))
  (ciao-write-region start end temp-file)
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "consult('" temp-file "')." ) t))

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
  (setq temp-file (ciao-temp-code-file))
  (ciao-write-region start end temp-file)
  (ciao-send-command ciao-toplevel-buffer-name 
   (concat "compile('" temp-file "')." ) t))

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
  (setq original-buffer (current-buffer))
  (setq buffercont (buffer-substring-no-properties minpoint maxpoint))
  (setq temp-buffer (generate-new-buffer "temp-buffer"))
  (set-buffer temp-buffer)
  (insert buffercont "\n")
  (write-region (point-min) (point-max) filename nil nil)
  (kill-buffer temp-buffer)
  (set-buffer original-buffer))

(defun ciao-del-temp-files (temp-file) 
  (delete-file-if-possible temp-file)
  (delete-file-if-possible (concat temp-file ".dep"))
  (delete-file-if-possible (concat temp-file ".itf"))
  (delete-file-if-possible (concat temp-file ".po")))

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
	  (setq ciao-debug-last-last-frame ciao-debug-last-frame
		ciao-debug-last-frame nil)))))

(defun ciao-debug-display-line (file start end pred numpred port buffname)
  (let* ((count 0) (init 0) (finish 0) (test t)
	 (last-nonmenu-event t)  ; Prevent use of dialog box for questions.
	 ;; Problema para la depuracion embebida
	 (buffer
	  (save-excursion
	    (or (eq (current-buffer) buffname)
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
				ciao-debug-expansion
				'ciao-debug)
		    ;; Save information for uncolor the last line
		    (setq ciao-debug-last-line
			  (cons (current-buffer)
				(ciao-what-line)))

		    )
		;; Save information for uncolor the last line
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
  "Return the line number. This function is needed because in xemacs the
function what-line does not behave as in emacs."
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
  (cond ((string= "Call" port) ciao-debug-call-color)
	((string= "Exit" port) ciao-debug-exit-color)
	((string= "Fail" port) ciao-debug-fail-color)
	((string= "Redo" port) ciao-debug-redo-color)))

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
	      ;; That lest us inherit various comint features.
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
    (if (string-match "\n\\?- " ciao-prompt-marker-acc)
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
    (if (string-match "\nciaopp \\?- " ciao-ciaopp-prompt-marker-acc)
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
    (if (string-match "\n\\lpdoc ?- " ciao-lpdoc-prompt-marker-acc)
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
	 (setq ciao-error nil)
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
	 (setq ciao-error nil)
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
	      ciao-debug-breakpoint-color
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
	(if (not (eq column 3))
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
	(while (not (search-forward-regexp "\\. *\\(%\\|$\\)" bound t))
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
  (let (start end)
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
  "Unhighlights the region from STARTLINE to ENDLINE withe overlay name
OVER."
  (let (start)
    (save-excursion
      (goto-line startline)
      (setq start (point)))
    (mapcar (function (lambda (ovr)
			(and (overlay-get ovr over) (delete-overlay ovr))))
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
			  ciao-debug-breakpoint-color
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
assumes that @tt{SETTINGS} exists and that its options
(main file, component files, paths, etc.) have been set
properly. Documentation is generated in a temporary directory. Note
however that for generating complex manuals the best approach is to
set up a permanent documentation directory with the appropriate
@tt{SETTINGS} and @tt{Makefile} files (see the LPdoc manual)."
  (interactive)
  (let ((thisfile (buffer-name (current-buffer))))
    (if (not (file-exists-p 
	      (concat (ciao-lpdoc-buffer-tmpdir thisfile) "/SETTINGS")))
	(message "You need to first visit SETTINGS and perhaps choose options")
      (ciao-ensure-inferior-process ciao-lpdoc-buffer-name)
      (ciao-send-command 
       ciao-lpdoc-buffer-name 
       (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " 
	       ciao-lpdoc-system " " ciao-lpdoc-docformat)
       t))))

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
    (ciao-ensure-inferior-process ciao-lpdoc-buffer-name)
    (ciao-send-command 
     ciao-lpdoc-buffer-name 
     (concat "cd " (ciao-lpdoc-buffer-tmpdir thisfile) "; " ciao-lpdoc-system 
	     " " ciao-lpdoc-docformat)
       t)))


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
	(ciao-ensure-inferior-process ciao-lpdoc-buffer-name)
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
	
	;; Used for ciaopp hooks
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
     (insert-string "@subsection{")
     (insert-string (car (cdr (car sec-commands))))
     (insert-string "}\n\n")
     (insert-string (car (cdr (cdr (car sec-commands)))))
     (insert-string "\n")
     (ciao-do-document-bindings (cdr sec-commands)))
    (t 
     (insert-string "@begin{description}\n")
     (ciao-print-function-info (car sec-commands))
     (insert-string "@end{description} @p \n")
     (ciao-do-document-bindings (cdr sec-commands)))
    ))

(defun ciao-print-function-info (info)
  "Print the information on a function as an item in lpdoc format."
  (insert-string
   (concat 
    "\n@item{"
    (ciao-flatten (mapcar 'text-char-description (car info)))
    "} "
    (documentation (car (cdr info)))
    "\n"
    ))
  )

(defun ciao-flatten (list)
   (if (eq list nil)
       ()
     (concat 
      "@key{" 
      (ciao-translate-key (car list))
      "} " 
      (ciao-flatten (cdr list)))))

(defun ciao-translate-key (key)
  "Translation table for the representation of keys in the documentation."
  (cond 
   ((equal key "^I") "TAB")
   (t key)
   ))

(defun ciao-document-variables ()
  "Generate documentation for all user-defined variables in lpdoc format."
  (let ((describe-func
	 (function
	  (lambda (s)
	    ;; Print description of symbol.
	    (if (boundp s)              ; It is a variable.
		(progn
		  (if (get s 'custom-type)
		      (progn
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
			  "\n")))))))))
	(sym-list)
	(ciao-vars nil)
	(ciaopp-vars nil)
	(lpdoc-vars nil))
    
    ;; Build a list of symbols that match pattern.
    (mapatoms (function
	       (lambda (sym)
		 (if (string-match "ciao" (symbol-name sym))
		     (setq sym-list (cons sym sym-list))))))
    
    ;; Display the data.
    (mapcar (function (lambda (sym)
			(cond ((string-match "ciaopp" (symbol-name sym))
			       (setq ciaopp-vars (cons sym ciaopp-vars)))
			      ((string-match "lpdoc" (symbol-name sym))
			       (setq lpdoc-vars (cons sym lpdoc-vars)))
			      (t 
			       (setq ciao-vars (cons sym ciao-vars))))))
	    sym-list)

    (insert-string "\n@subsection{Ciao variables}\n")
    (insert-string "@begin{description}\n")
    (mapcar describe-func (sort ciao-vars 'string<))
    (insert-string "@end{description}\n")
    (insert-string "\n@subsection{CiaoPP variables}\n")
    (insert-string "@begin{description}\n")
    (mapcar describe-func (sort ciaopp-vars 'string<))
    (insert-string "@end{description}\n")
    (insert-string "\n@subsection{LPdoc variables}\n")
    (insert-string "@begin{description}\n")
    (mapcar describe-func (sort lpdoc-vars 'string<))
    (insert-string "@end{description}\n")))

(defun compile-ciao-mode ()
  "With this handy function this file can be compiled as
   emacs -batch -l ciao.el -f compile-ciao-mode"
   (byte-compile-file "ciao.el")
   (byte-compile-file "word-help.el"))
;; This are really no use...
;;  (byte-force-recompile "."))
;;  (byte-recompile-directory "." t))

(defun ciao-report-mode-version ()
  "Report the version of the emacs Ciao/Prolog mode."
  (interactive)
  (message (concat "Ciao, Prolog, CiaoPP, LPdoc mode version: " 
		   ciao-mode-version )))

;;------------------------------------------------------------
;; Kludge to fix old version maintenance entries...
;;------------------------------------------------------------

(defun ciao-fix-old-version-maintenance ()
  (interactive)
  (beginning-of-buffer)
  (if (search-forward "%% Control version comment prompting for" nil t)
      (progn
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

;; Idea/pending bugs: 
;; - Color trues etc with disjunction?
;;        true([term(T),term(X)] ;
;;             mshare([[T],[T,X],[X]])
;;            ),
;; - Checks in clause bodies not colored? 
;; 	check(mytype(X),pr_pp('end of predicate p')).
;; - Variable to list things that can be at the beginning of a file to
;;   declare a module, in addition to "module": 
;;   (setq ciao-module-decl '("class")). To use in font-lock.
;; - C-u <compilation command> asks for options, remembers them per
;;   buffer (or set them through a menu)?
;;;------------------------------------------------------------
