;; ---------------------------------------------------------------------------
;; Miscellaneous emacs auxiliary code for documentation generation (lpdoc)
;; ---------------------------------------------------------------------------

(setq max-specpdl-size 4000)
(setq max-lisp-eval-depth 3000)

;; Hook to run texinfo-multiple-files-update in batch mode
(load-library "texinfo")

(defun update-all-nodes-in-one-file ()
  "A hook to create a single, resolved .texi file in batch mode"
  (let
      ((file (my-get-option command-line-args "-file"))
       (outfile (my-get-option command-line-args "-outfile")))
    (message (concat "*** Updating nodes in " file " ..."))
    (create-single-file file)
    (texinfo-all-menus-update t)
    (save-file-as-texi file outfile)
    (kill-emacs t)))

(defun create-single-file (file)
  "Creates a single file inserting the includes"
  (find-file file)
  (goto-char (point-min))
  (insert-includes))

(defun insert-includes ()
  (let
      ((local-last-buffer-point)
       (local-buffer-limit)
       (tmp)
       (compname))
    (setq local-last-buffer-point (point))
    (goto-char (point-max))
    (setq local-buffer-limit (point))
    (goto-char local-last-buffer-point)
    (if (eq (search-forward-regexp "[^@]@include " local-buffer-limit t) nil)
	nil
      (beginning-of-line)
      (setq tmp (point))
      (search-forward " " local-buffer-limit t)
      (kill-region tmp (point))
      (setq tmp (point))
      (end-of-line)
      (setq compname (buffer-substring tmp (point)))
      (kill-region tmp (point))
      (beginning-of-line)
      (setq tmp (point))
      (insert-file-contents compname)
      (goto-char tmp)
      (message (concat "Inserted file " compname "..."))
      (insert-includes))))

(defun save-file-as-texi (file outfile)
  (find-file file)
  (write-file outfile))

(defun fxname (iname suff1 suff2)
  (concat 
   (substring iname 0 (string-match suff1 iname))
   suff2))

;; Command line option access
(defun my-get-option (command-line-args option)
  "Get the filename that appears in the command line after -option."
  (cond 
   ((eq command-line-args nil)
    (message (concat "Error: '" option "' argument not found"))
    (kill-emacs))
   ((string= (car command-line-args) option)
	(car (cdr command-line-args)))
   (t
    (my-get-option (cdr command-line-args) option))))




