(require 'hilit19)

;; Modula-2
(setq auto-mode-alist
      (cons '("\\.mod$" . modula-2-mode) auto-mode-alist))   
(setq auto-mode-alist
      (cons '("\\.def$" . modula-2-mode) auto-mode-alist))   

;; Hope
(autoload 'hope-mode "hope-mode" "Edit Hope programs" t)

(setq auto-mode-alist
      (cons '(".*\\.hop$".hope-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '(".*\\.LST$".hope-mode) auto-mode-alist))

;;command.com shell on Win95/98
;;
;;The latest end-of-line handling code in Emacs prevents
;;command.com from being used as the interactive shell on
;;Win95 out of the box. To use command.com as your shell,
;;place the following in your startup file: 

(setq process-coding-system-alist
      '(("cmdproxy" . (raw-text-dos . raw-text-dos))))

;; Preventing ctrl-m's from being printed in the shell
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; Preventing shell commands from being echoed
(defun my-comint-init ()
  (setq comint-process-echoes t))

(add-hook 'comint-mode-hook 'my-comint-init)


;; Complete directories with "\"
(add-hook 'shell-mode-hook
	  '(lambda () (setq comint-completion-addsuffix '("\\" . "")))
	  t)

