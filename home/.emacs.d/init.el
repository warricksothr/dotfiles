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
                         neotree
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

;; Set NeoTree to toggle with F8
(global-set-key [f8] 'neotree-toggle)

;; Default configuration for auto-complete
(ac-config-default)

;; Make all files show a linenum
(global-linum-mode)

;; Configure SLIME
;; Inferior Lisp interpreter is found at $CL_BIN
(setq inferior-lisp-program (getenv "CL_BIN"))
(setq slime-contribs '(slime-fancy))
  
;; Load my default theme
;(load-theme 'cyberpunk t)

;; A method create a lambda that switches between
;; themes with the press of a button
;; Emcas requires explicit closures otherwise they're
;; abandoned at runtime and cryptic errors occur
(let ((next nil) (themes nil))
      (defun make-theme-switcher (theme_list)
        (setf themes theme_list)
        #'(lambda ()
            (setf next (pop themes))
            ;; Move the first entry in the list to the last
            (setf themes (append themes (list next)))
            ;; Load the theme that was next in the list
            (load-theme next t))))

;; create a function to loop over and apply themes
(setf load-next-theme (make-theme-switcher '(cyberpunk adwaita)))

;; Load the first theme as the default theme
(funcall load-next-theme)

;; Toggle theme switch with F3
(global-set-key [f3] (lambda ()
                       (interactive)
                       (funcall load-next-theme)))

;; Windows specific configuration
;; A Linux environment is assumed by default
(defun windows-config ()
  ;; When running in Windows, we want to use an alternate shell so we
  ;; can be more unixy.
  (setq shell-file-name "c:/Tools/msys64/usr/bin/zsh")
  (setq explicit-shell-file-name shell-file-name)
  (setq explicit-zsh-args '("--login" "-i")) ; Make sure the shell is in the home
  (setenv "HOME" "c:/tools/msys64/home/neria") ; Set the home environment correctly
  (setenv "PATH"
    (concat ".:/usr/local/bin:/msys64/bin:/bin:"
      (replace-regexp-in-string " " "\\\\ "
        (replace-regexp-in-string "\\\\" "/"
          (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1"
            (getenv "PATH")))))))

;; Windows specific configurations
; (cond
;  ((string-equal system-type "windows-nt") ; MS Windows System
;   (windows-config)))

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
