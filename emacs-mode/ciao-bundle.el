;;; ciao-bundle.el --- Interface for Ciao bundles
;; Copyright (C) 1986-2012 Free Software Foundation, Inc.

;; Authors: 2012      Jose F. Morales <jfran@clip.dia.fi.upm.es>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'ciao-config) ; ciao-get-config

;;===========================================================================
;;
;; **Experimental** interface for commands and functions for managing
;; Ciao bundles.
;; 
;; Remember: do not write complex logic here, this is just the user
;;   interface (everything should be reproducible from the command line
;;   or toplevel)
;;
;; TODO:
;;   - This is a stub, we need many more commands (but not too many).
;;
;;   - Some bundles could be updated from a toplevel, but core tools
;;     (like the running toplevel) need a separate process (at least
;;     at this moment)
;;
;;   - Add menus and documentation.
;;
;;   - Remove dependencies from shell scripts and POSIX tools (as much
;;     as possible)
;;
;;===========================================================================

(defun ciao-get-bundle-proc-buffer ()
  (get-buffer-create "*Ciao Bundle Manager Process*"))

;; TODO: define 'ciao-bundle-build-all'?
;; TODO: define 'ciao-bundle-update'? (should it download anything?)

(defun ciao-setup-command (cmd)
  "Execute the `cmd' ciaosetup command"
  (async-shell-command
   (concat ciao-bin-dir "/ciaosetup " cmd)
   (ciao-get-bundle-proc-buffer)))

;;;###autoload
(defun ciao-bundle-build () 
  "(Re)Build the specified Ciao bundle"
  (interactive)
  (let (target)
    (setq
     target
     (completing-read
      "(Re)Build the specified Ciao bundle: "
      '(("ciao" 1)
	("ciaoc" 2)
	("engine" 3)
	("shell" 4)
	("lpdoc" 5)
	("ciaopp" 6)
	("emacs_mode" 7))
      nil t ""))
    (ciao-setup-command (concat "build " target))))

;; TODO: provide a minor mode for this
;;;###autoload
(defun ciao-bundle-list () 
  "List the available bundles."
  (interactive) 
  (ciao-setup-command "show_bundles"))

;; ---------------------------------------------------------------------------
;; Ciao bot
;; TODO: Separate from here?

;;;###autoload
(defun ciao-bundle-bot-status () 
  "Return the status of the Ciao bot (which builds bundles
remotely for several platforms)."
  (interactive) 
  (ciao-setup-command "bot status"))


;; Provide ourselves:

(provide 'ciao-bundle)

;;; ciao-bundle.el ends here

