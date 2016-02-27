(setq load-path (cons "library_directory" load-path))

;--begin orig 
(autoload 'set-help-file "word-help"
  "Sets the set of Texinfo files used for `word-help' to HELPFILE."
  t nil)
(autoload 'word-help "word-help"
  "Find documentation on the KEYWORD under the cursor (or passed)."
  t nil)
(autoload 'word-help-complete "word-help"
  "Perform completion on the symbol preceding the point." t nil)
(autoload 'word-help-add-keywords "word-help"
  "Add user keywords to a certain Info file." nil nil)
(define-key help-map [?\C-i] 'word-help)
(global-set-key [\C-tab] 'word-help-complete)
;--end orig 

(require 'word-help)
(define-key help-map "\C-i" 'word-help)
;; To make this effective, do M-x set-help-file
(setq word-help-mode-alist
 (cons
  '("CIAO"
    (
     ("lpdoc.info" 
      "Concept Index" "Module/Object Definition Index" 
      "Predicate/Method Definition Index" "Property Definition Index"
      "Type Definition Index" "Operator Definition Index")
;;      ("ciao-Reference.info" 
;;       "Predicate/Method Definition Index" "Property Definition Index"
;;       "Type Definition Index" "Mode Definition Index" "Concept Index") 
;;      ("ciao-lib.info" 
;;       "Predicate/Method Definition Index" "Property Definition Index"
;;       "Type Definition Index" "Mode Definition Index" "Concept Index") 
;;      ("ciao-library.info" 
;;       "Predicate/Method Definition Index" "Property Definition Index"
;;       "Type Definition Index" "Mode Definition Index" "Concept Index") 
;;      ("sicstus3" 
;;       "Predicate Index" "Obj Index" "Concept Index")
     )
    (("[A-Za-z_]+" 0)
     ("[A-Za-z_][A-Za-z0-9_^/]+" 0))
    nil
    (("[A-Za-z_]+" 0))
    )
  word-help-mode-alist))

;; (setq word-help-mode-alist (cons '("CIAO" . "prolog") word-help-mode-alist))

;; (defvar word-help-split-window-flag t
;;   "*Non-nil means that the info buffer will pop up in a separate window.
;; If nil, we will just switch to it.")
;; 
;; (defvar word-help-magic-index-flag t
;;   "*Non-nil means that the keyword will be searched for in the requested node.
;; This is done by determining whether the line the point is positioned
;; on after using `Info-goto-node', actually contains the keyword.  If
;; not, we will search for the first occurence of the keyword.  This may
;; help when the info file isn't correctly indexed.")
