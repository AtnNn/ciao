;; (This file includes functions from emacs-library.el that are now obsolete)
;; -- Jose F. Morales

;; Useful for rewriting and istallation of SETTINGS file
(defun fix-config-file-settings ()
  "Replace library path in SETTINGS during installation"
  (setq infile (my-get-option command-line-args "-infile"))
  (setq outfile (my-get-option command-line-args "-outfile"))
  (setq variable (my-get-option command-line-args "-variable"))
  (setq value (my-get-option command-line-args "-value"))
  (message (concat "Rewriting " infile " into " outfile "..."))
  (find-file infile)
  (search-forward-regexp (concat "^ *" variable "*=") nil t)
  (set-mark (point))
  (end-of-line)
  (kill-region (mark) (point))
  (insert value)
  (write-file outfile)
  (kill-emacs)
  )
  
;; Hook to fix references in .texic files
(defun update-all-references ()
  "A hook to update references in files in batch mode"
  (let* 
      ((suffix (my-get-option command-line-args "-suffix"))
       (file0 (my-get-option command-line-args "-file"))
       (file (concat file0 "." suffix))
       (refsfile (my-get-option command-line-args "-refsfile"))
       (components (my-get-option command-line-args "-components"))
       (includes))
    (message 
     (concat "*** Updating references for " file " with components " 
	     components " ..."))
    (load-file refsfile)
    (if (equal suffix "htmlc")
	(progn
	  (setq includes (gather-includes file "<a href='\\([^<>']*\\)\.new\.html'"))
	  ;; A hack... we may have both suffixes (due to subfile)
	  (update-references-in-files "htmlc" includes)
	  (update-references-in-files "new.html" includes))
      (progn
	(setq includes (gather-includes file "@include \\(.*\\)\.texic"))
	(update-references-in-files suffix includes)))
    (kill-emacs t)))

(defun update-references-in-files (suffix files)
  (if (eq files nil)
      (message "All references processed")
    (update-references-in-file suffix (car files))
    (update-references-in-files suffix (cdr files))
    ))

(defun update-references-in-file (suffix file0)
  (let
      ((file (concat file0 "." suffix)))
    (message (concat "*** Updating references in " file "..."))
    (if (file-exists-p file)
	(progn
	  (find-file file)
	  (update-references nil)
	  (save-buffer))
      )))

(defun update-references (insidelist)
  (if insidelist
      (progn 
	(forward-char 1)
	(update-reference)
	)
    (if (not (search-forward "[BibRef: " nil t))
	t
      (goto-char (match-beginning 0))
      (forward-char 1)
      (set-mark (point))
      (search-forward " ")
      (kill-region (point) (mark))
      (update-reference))))
	  
(defun update-reference ()
  (set-mark (point))
  (if (not (search-forward-regexp "\\(]\\|,\\)" nil t)) 
      (message "Error, could not find closing ']' or ',' after reference")
    (let 
	((keyword))
      (backward-char 1)
      (setq keyword (buffer-substring (point) (mark)))
      (kill-region (mark) (point))
      (insert (get-keyword-reference keyword))
      (if (equal (char-to-string (char-after (point))) ",")
	  (update-references t)
	(update-references nil))
      )))

(defun get-keyword-reference (keyword)
  (let 
      ((reference (assoc keyword refs-alist)))
    (if (eq reference nil)
	(concat "BibRef: " keyword)
       (car (cdr reference))
    )))

;; (defun update-all-nodes ()
;;   "A hook to run texinfo-multiple-files-update in batch mode"
;;   (setq file (my-get-option command-line-args "-file"))
;;   (setq components (my-get-option command-line-args "-components"))
;;   (message 
;;    (concat "*** Updating all nodes for " file " with components " 
;; 	   components " ..."))
;; ;; Changed to cope properly with References: now always multiple files.
;; ;;   (if (string= components nil)
;; ;;       (progn 
;; ;; 	(find-file file)
;; ;; 	(texinfo-all-menus-update t))
;;       (texinfo-multiple-files-update file t t)
;;       (save-files-as-ltxi (gather-includes file "ltxi"))
;; ;;      )
;;   (save-file-as-ltxi file)
;;   (kill-emacs t))

;; (defun gather-includes (file newsuffix)
;;   (find-file file)
;;   (goto-char (point-max))
;;   (setq local-buffer-limit (point))
;;   (goto-char (point-min))
;;   (get-includes local-buffer-limit newsuffix))
;; 
;; (defun get-includes (local-buffer-limit newsuffix)
;;   "Gets includes from main file and modifies includes"
;;   (if (or 
;;        (>= (point) local-buffer-limit)
;;        (eq (search-forward "@include " local-buffer-limit t) nil))
;;       nil
;;     (set-mark (point))
;;     (search-forward ".texic" local-buffer-limit t)
;;     (setq el (buffer-substring (mark) (point)))
;;     (message (concat "*** Component: " el))
;;     (set-mark (point))
;;     (backward-char 4)
;;     (kill-region (mark) (point))
;;     (insert newsuffix)
;;     (cons el (get-includes local-buffer-limit newsuffix))))

(defun gather-includes (file regexp)
  (let
      ((local-buffer-limit))
    (find-file file)
    (goto-char (point-max))
    (setq local-buffer-limit (point))
    (goto-char (point-min))
    (delete-dups (get-includes local-buffer-limit regexp))))

(defun get-includes (local-buffer-limit regexp)
  "Gets includes from main file and modifies includes"
  (let
      ((el))
    (if (or 
	 (>= (point) local-buffer-limit)
	 (eq (re-search-forward regexp local-buffer-limit t) nil))
	nil
      (setq el (buffer-substring (match-beginning 1) (match-end 1)))
      (message (concat "*** Component: " el))
      (cons el (get-includes local-buffer-limit regexp)))))

;; (defun save-files-as-ltxi (files)
;;   (if (eq files nil)
;;       (message "All component files saved")
;;     (save-file-as-ltxi (car files))
;;     (save-files-as-ltxi (cdr files))))
;; 
;; (defun save-file-as-ltxi (file)
;;   (find-file file)
;; ;;  (message (concat "*** Saving " file " as .ltxi..."))
;; ;;  (sleep-for 1)
;;   (write-file (fxname (buffer-name) "\\.texic" ".ltxi")))

