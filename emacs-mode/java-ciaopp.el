;;---------------------------------------------------------------------------
;; Emacs additional support for the CiaoPP preprocessor 
;; when working on Java files (complementary to ciao.el).
;; (Very preliminary version!) --M. Hermenegildo
;;---------------------------------------------------------------------------

;; Setup: add the following lines to your .emacs after the lines
;; included automatically by the Ciao installation.
;; ** Remember to edit the load path to suit your location ** 

;; ;; Auto loading of java-ciaopp sub mode.
;; ;; ** Remember to edit the load path to suit your location ** 
;; (setq load-path 
;;       (cons "/home/herme/MySVN/Systems/CiaoDE/emacs-mode" load-path)) 
;; (defun load-java-ciaopp-mode ()
;;  (require 'java-ciaopp)
;;  (java-ciaopp-setup))
;; (add-hook 'java-mode-hook 'load-java-ciaopp-mode)

;; BUG: Line below should be updated by installation.
;; "ciao-1.13.0.info" ;; Should be "ciao-<DEVELOPMENT_VERSION>.info" 

; ---------------------------------------------------------------------------

(require 'easymenu)
(require 'ciao)
(provide 'java-ciaopp)

; ---------------------------------------------------------------------------

(defun java-ciaopp-setup ()
  (interactive)
  ;; option in the menu
  (easy-menu-define ciao-java-menu-ciaopp java-mode-map 
    "CiaoPP Mode Menus" ciao-mode-menus-java)
  (easy-menu-add ciao-java-menu-ciaopp)
  ;; toolbar
  (ciao-java-setup-tool-bar)
 )

; ---------------------------------------------------------------------------
; Key bindings

;; (define-key java-mode-map "\C-cA"    'ciao-analyze-buffer)
;; (define-key java-mode-map "\C-cT"    'ciao-check-assertions)
;; (define-key java-mode-map "\C-cO"    'ciao-optimize-buffer)
(define-key java-mode-map "\C-cM"    'java-browse-preprocessor-options)
(define-key java-mode-map "\C-c\C-v" 'ciao-show-preprocessor-output)
(define-key java-mode-map "\C-c`"    'ciao-find-last-run-errors)
(define-key java-mode-map "\C-ce"    'ciao-unmark-last-run-errors)
(define-key java-mode-map "\C-c\C-r" 'run-ciao-preprocessor)
(define-key java-mode-map "\C-ch"    'ciao-fontify-buffer)
(define-key java-mode-map "\C-c\C-i" 'ciao-help-on-current-symbol)
(define-key java-mode-map "\C-c/"    'ciao-complete-current-symbol)
(define-key java-mode-map "\C-c\C-m" 'ciao-goto-ciao-manuals)
(define-key java-mode-map "\C-ct"    'run-ciao-toplevel)
(define-key java-mode-map "\C-cl"    'ciao-load-buffer)

; ---------------------------------------------------------------------------
; Menus

(defconst ciao-mode-menus-java
  (list "CiaoPP"
     "CiaoPP Java Preprocessor (beta)"
     "----"
     "----"
;;     Commented by JNL 
;;     ["Analyze buffer"                         ciao-analyze-buffer t]
;;     ["Check buffer assertions"                ciao-check-assertions t]
;;     ["Optimize buffer"                        ciao-optimize-buffer t]
;;     ["Browse analysis/checking/optimizing options"         
;;                                           ciao-browse-preprocessor-options t]
     ["Browse analysis options"         java-browse-preprocessor-options t]
     "----"
     ["Show last preprocessor output file"     ciao-show-preprocessor-output t]
     ["Go to (next) preproc/compiler error msg" ciao-find-last-run-errors t]
     ["Remove error (and dbg) marks in buffers"  ciao-unmark-last-run-errors t]
     "----"
;;      ["Preprocess buffer (w/previous options) and show output"  
;;                                 ciao-preprocess-buffer-and-show-output t]
     ["(Re)Start Ciao preprocessor"              run-ciao-preprocessor t]
     ["Update syntax-based coloring"             ciao-fontify-buffer t]
     "----"
     ["Go to manual page for symbol under cursor" ciao-help-on-current-symbol]
     ["Complete symbol under cursor"        ciao-complete-current-symbol t]
     ["Ciao manuals area in info index" ciao-goto-ciao-manuals t]
     "----"
     ["(Re)Start Ciao top level"                 run-ciao-toplevel t]
     ["(Re)Load buffer into top level"           ciao-load-buffer  t]
     "----"
     ["Customize all Ciao environment settings" 
                                       (customize-group 'ciao-environment)] 
     ["Ciao environment (mode) version" ciao-report-mode-version t]
     )
  "Menus for CiaoPP mode.")

; ---------------------------------------------------------------------------
; Tool bar

(defun ciao-java-setup-tool-bar () 
  (interactive)
  (make-local-variable 'tool-bar-map) 
  (if (boundp 'xemacs-logo)
      (setq ciao-xemacs-tool-bar-tmp nil)
    (setq tool-bar-map (make-sparse-keymap)))
  ;; General stuff (from standard tool bar); added only in FSF emacs.
  (ciao-general-toolbar tool-bar-map)
  ;; Java/CiaoPP-specific stuff - added in both FSF and xemacs
  ;; Stuff that is not in menus will not work.
  (ciao-tool-bar-local-item-from-menu 
   'ciao-analyze-buffer "icons/ciaoanalysis" tool-bar-map java-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-check-assertions "icons/checkassertions" tool-bar-map java-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-optimize-buffer "icons/ciaopeval" tool-bar-map java-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'java-browse-preprocessor-options
   "icons/ciaocustomize" tool-bar-map java-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-find-last-run-errors "icons/jump_to" tool-bar-map java-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-unmark-last-run-errors "icons/clear" tool-bar-map java-mode-map)
  (tool-bar-add-item "icons/manuals" 
   'ciao-goto-ciao-manuals 'ciao-goto-ciao-manuals 
   :help "Go to area containing the Ciao system manuals")
  (ciao-tool-bar-local-item-from-menu 
   'ciao-help-on-current-symbol "icons/wordhelp" tool-bar-map java-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-complete-current-symbol "icons/complete" tool-bar-map java-mode-map)
  (ciao-tool-bar-local-item-from-menu 
   'ciao-customize-all
   "icons/preferences" tool-bar-map java-mode-map
   :help "Edit (customize) preferences for Ciao, CiaoPP, LPdoc")
  (ciao-tool-bar-local-item-from-menu 
   'ciao-fontify-buffer "icons/ciaorehighlight" tool-bar-map java-mode-map)
  (ciao-xemacs-toolbar-postprocess ciao-xemacs-tool-bar-tmp))

; ---------------------------------------------------------------------------

; Find CiaoPP properties while in Java files
(setq word-help-mode-alist
 (cons
 '("Java"
   (
    ("ciao-1.13.0.info" ;; Should be "ciao-<DEVELOPMENT_VERSION>.info" 
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
  word-help-mode-alist))

; ---------------------------------------------------------------------------

