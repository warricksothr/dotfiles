;; Set enironment information
(setq user-full-name "Drew Short")
(setq user-email-address "warrick@sothr.com")

;; Load common lisp
(require 'cl)

;; Package management
(load "package")
(package-initialize)
(defvar sothr/packages '(auto-complete
			 cyberpunk-theme
			 gist
			 magit
			 markdown-mode
			 projectile
			 yaml-mode)
  "Default packages")

;; Repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

;; Advice from the melpa site for a package dependency validation error in emacs 24
(defadvice package-compute-transaction
  (before package-compute-transaction-reverse (package-list requirements) activate compile)
    "reverse the requirements"
    (setq requirements (reverse requirements))
    (print requirements))

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

;; Default configuration for auto-complete
(ac-config-default)

;; Load my default theme
(load-theme 'cyberpunk t)
