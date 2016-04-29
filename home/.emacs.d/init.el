;; Set enironment information
(setq user-full-name "Drew Short")
(setq user-email-address "warrick@sothr.com")

;; Set UTF-8 as the default encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Load common lisp
(require 'cl)

;; Package management
(load "package")
(package-initialize)
(defvar sothr/packages '(auto-complete
			 better-defaults
			 cyberpunk-theme
			 gist
			 magit
			 markdown-mode
			 org
			 org-ac
			 org-autolist
			 org-bullets
			 org-doing
			 projectile
			 slime
			 go-mode
			 rust-mode
			 yaml-mode)
  "Default packages")

;; Repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Advice from the melpa site for a package dependency validation error in emacs 24
;;(defadvice package-compute-transaction
;;  (before package-compute-transaction-reverse (package-list requirements) activate compile)
;;    "reverse the requirements"
;;    (setq requirements (reverse requirements))
;;    (print requirements))

;; Make sure default packages are installed
(defun sothr/packages-installed-p ()
  (loop for pkg in sothr/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

;; This is the logic that runs the above function
(unless (sothr/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg sothr/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Markdown Mode Configuration
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Set the markdown processor to grip
;; pip3 install grip
(setq markdown-command "grip --export -")

;; Default configuration for auto-complete
(ac-config-default)

;; Make all files show a linenum
(global-linum-mode)

;; Configure SLIME
;; Inferior Lisp interpreter is found at $CL_BIN
(setq inferior-lisp-program (getenv "CL_BIN"))
(setq slime-contribs '(slime-fancy))

;; Load my default theme
(load-theme 'cyberpunk t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-doing org-bullets org-autolist org-ac better-defaults yaml-mode slime rust-mode projectile markdown-mode magit go-mode gist cyberpunk-theme auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
