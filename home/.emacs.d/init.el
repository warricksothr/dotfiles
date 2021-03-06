;;; package --- init.el
;;; commentary: My emacs setup file
;;; code:
;; Set enironment information
(setq user-full-name "Drew Short")
(setq user-email-address "warrick@sothr.com")

;; Set UTF-8 as the default encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Make sure we're not checking signatures...
;; I know, this is bad, but would somone please
;; publish their signing keys so that I can
;; comment this out!
(setq package-check-signature nil)

;; Load common lisp
(require 'cl)

;; Theme packages
(defvar sothr/themes '(cyberpunk-theme)
  "Themes")

;; Tool packages
(defvar sothr/tools '(auto-complete
                      better-defaults
                      company
                      elpy
                      flycheck
                      gist
                      projectile
                      magit
                      neotree
                      racer
                      slime)
  "Tools")

;; Additional mode packages
(defvar sothr/modes '(markdown-mode
                      go-mode
                      rust-mode
                      yaml-mode)
  "Modes")

;; Org mode specific packages
(defvar sothr/org '(org
                    org-ac
                    org-autolist
                    org-bullets
                    org-doing)
  "Org Mode Packages")

;; Combine the package lists
(defvar sothr/packages (append sothr/themes sothr/tools sothr/modes sothr/org) "Default Packages")

;; Package management
(require 'package)
;; Repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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
;; Rust development setup
;; Enable company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)
;;flycheck for syntax checking
(global-flycheck-mode)
;;(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; Racer for code completion
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
;; On demand rust completions
(add-hook 'racer-mode-hook #'company-mode)
;; must only be executed after the first rust-mode load
(with-eval-after-load "rust-mode"
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))
(setq company-tooltip-align-annotations t)

;; Disable emacs help screen
(setq inhibit-splash-screen t)

;; Emable emacs copying from X11
(setq x-select-enable-clipboard t)

;; Always follow symbolic links back to the source file in version control
(setq x-select-enable-clipboard t)

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

;; Make all files show a line number
(global-linum-mode)
;; Make sure that there is spacing in linum mode
;(setq linum-format "%4d \u2502 ")  ;; Fancy line with vertical bar
;; Dynamic width for lines for convienient right justification
(unless window-system
  (add-hook 'linum-before-numbering-hook
	    (lambda ()
	      (setq-local linum-format-fmt
			  (let ((w (length (number-to-string
					    (count-lines (point-min) (point-max))))))
			    (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))

;; Configure SLIME
;; Inferior Lisp interpreter is found at $CL_BIN
(setq inferior-lisp-program (getenv "CL_BIN"))
(setq slime-contribs '(slime-fancy))

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

;; Make sure that the frames are set to dark mode
(custom-set-variables '(frame-background-mode 'dark))

;; Toggle theme switch with F3
(global-set-key [f3] (lambda ()
                       (interactive)
                       (funcall load-next-theme)))

;; Set python tabs to none, and indent width to 4
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))
;; Enable ELPY
(setq elpy-rpc-python-command "python3")
(elpy-enable)

;; Set the autoencrypt feature of emacs for .gpg files
(require `epa-file)
(epa-file-enable)

;; Windows specific configuration
;; A Linux environment is assumed by default
;(defun windows-config ()
  ;; When running in Windows, we want to use an alternate shell so we
  ;; can be more unixy.
  ;(setq shell-file-name "c:/Tools/msys64/usr/bin/zsh")
  ;(setq explicit-shell-file-name shell-file-name)
  ;(setq explicit-zsh-args '("-i")) ; Make sure the shell is in the home
  ;(setenv "HOME" "c:/tools/msys64/home/neria")) ; Set the home environment correctly
  ;(setenv "PATH"
    ;(concat ".:/usr/local/bin:/msys64/bin:/bin:"
      ;(replace-regexp-in-string "/////" "\\\\ "
        ;(replace-regexp-in-string "\\\\" "/"
          ;(replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1"
            ;(getenv "PATH")))))))

;; Windows specific configurations
;(cond
 ;((string-equal system-type "windows-nt") ; MS Windows System
                                        ;(windows-config)))

;; Advice for open files as root if we don't have enough permissions
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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
 '(neo-file-link-face ((t (:foreground "white")))))
