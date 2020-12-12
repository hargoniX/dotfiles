;; Some performance settings for lsp-mode
(setq gc-cons-threshold 100000000
      gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024))

(setq make-backup-files nil) ;; We dont need these
(setq auto-save-default nil) ;; Not this one either
(menu-bar-mode -1) ;; The menu bar looks ugly in terminal
(tool-bar-mode -1) ;; Nobody needs this
(toggle-scroll-bar -1) ;; Or this
(setq inhibit-startup-screen t) ;; Leave me alone with your tutorials
(setq whitespace-style '(face trailing tabs))
(add-hook 'prog-mode-hook 'whitespace-mode) ;; Show trailing whitespaces in prog-mode
(setq tramp-default-method "ssh") ;; speed up tramp mode

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("elpa"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package))

(use-package general :ensure t
  :init
  ;; Space as leader key
  (general-create-definer vim-leader-def :prefix "SPC"))


(use-package evil :ensure t
  :init
  (setq evil-toggle-key "C-~") ;; so C-z works for background
  :config
  (evil-mode))

(use-package magit :ensure t
  :general
  (vim-leader-def 'normal 'global
    "gb" 'magit-blame ;; git blame
    "gs" 'magit-status ;; git status
    "gl" 'magit-log ;; git log
    "gc" 'magit-checkout ;; git checkout
    "ga" 'magit-branch) ;; git ast (as b is taken)
  :config
  (use-package evil-magit :ensure t))

(use-package org :ensure t
  ;; M-LEFT M-RIGHT deindent indent node
  ;; M-UP M-DOWN move node up down
  ;; Tab fold/unfold
  ;; M-S-RET Insert new TODO
  :general
  (vim-leader-def 'normal 'global
    "ohj" 'org-forward-heading-same-level ;; org header j"
    "ohk" 'org-backward-heading-same-level ;; org header k
    "or" 'org-meta-return ;; org return, insert new  entry
    "otr" 'org-todo ;; org todo rotate
    "opk" 'org-priority-up ;; org prio k
    "opj" 'org-priority-down ;; org prip j
    "oci" 'org-clock-in
    "oco" 'org-clock-out
    "oy"  'org-cycle) ;; Org-cYcle
  :init
  (setq org-todo-keywords '((sequence "TODO" "PROGRESS" "FEEDBACK" "|" "DONE" "DELEGATED")))
  (setq org-log-done 'time)
  )

(use-package gruvbox-theme :ensure t
  :init
  (load-theme 'gruvbox-dark-medium t)
  )

(use-package tmux-pane :ensure t
  :config
  (tmux-pane-mode))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (smartparens evil-magit magit evil general use-package))))
