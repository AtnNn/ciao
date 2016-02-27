;; * Link the version control of ciao.el.skel with the file
;;   CiaoMode.pl
;;
;; Since we keep version control comments in SVN, this feature is no
;; longer used. I kept it here for future reference.
;;
;; --JFMC

(defvar ciao-mode-version-control-saving nil)

; TODO: Ad hoc treatment of ciao.el.skel and CiaoMode.pl is strange
(defun ciao-mode-version-control ()
  (interactive)
  (if (and (string= (file-name-nondirectory (buffer-file-name))
	            "ciao.el.skel")
	   (not ciao-mode-version-control-saving)
	   )
      (progn
	(save-excursion
	  (set-buffer (find-file-noselect "CiaoMode.pl"))
 	  (set-buffer-modified-p t)
	  (ciao-save-buffer)
	  (kill-buffer (current-buffer)))
;; To keep dependencies: touch ciao.el.skel afterwards
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
		      "ciao.el.skel")
		 (get-buffer "CiaoMode.pl"))
	    (kill-buffer "CiaoMode.pl"))))) 
         
(add-hook 'after-save-hook 'ciao-mode-version-control) 
(add-hook 'kill-buffer-hook 'ciao-mode-end-version-control) 
