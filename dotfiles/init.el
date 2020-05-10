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
  (general-create-definer vim-leader-def :prefix "SPC"))


(use-package evil :ensure t
  :init
  (setq evil-toggle-key "C-~") ;; so C-z works for background
  :config
  (evil-mode))

(use-package linum-relative :ensure t
  :diminish linum-relative-mode
  :init
  (add-hook 'find-file-hook 'linum-relative-mode)) ;; only have line numbers for files

(use-package fill-column-indicator :ensure t
  :diminish fci-mode
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "red")
  :init
  (add-hook 'prog-mode-hook 'fci-mode))

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

(use-package smartparens :ensure t ;; auto parens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil) ;; to hide this fucking highlighting
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package rust-mode :ensure t ;; rust highlighting, indentation and integration
  :init
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil))))

(use-package ivy :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :init
  (ivy-mode 1))

(use-package counsel :ensure t
  :diminish counsel-mode
  :general
  (vim-leader-def 'normal 'global
    "gg" 'counsel-git-grep) ;; GitGrep
  :init
  (counsel-mode 1))

(use-package lsp-mode :ensure t
  :general
  (vim-leader-def 'normal 'global
    "lr" 'lsp-rename) ;; LSP Rename
  :config
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-log-io t)
  (setq lsp-prefer-capf t)
  (setq lsp-eldoc-hook '(lsp-hover)) ;; disable semantic highlighting
  (setq lsp-diagnostic-package nil)
  :init
  (add-hook 'rust-mode-hook 'lsp)
  (add-hook 'ruby-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp))

(use-package company :ensure t
  :init
  (add-hook 'lsp-mode-hook 'company-mode))

(use-package lsp-ivy :ensure t
  :general
  (vim-leader-def 'normal 'global
    "ls" 'lsp-ivy-workspace-symbol) ;; LSP symbol
  )

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
    "oco" 'org-clock-out)
  :init
  (setq org-todo-keywords '((sequence "TODO" "PROGRESS" "FEEDBACK" "|" "DONE" "DELEGATED")))
  (setq org-log-done 'time)
  )
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
    (smartparens evil-magit magit fill-column-indicator linum-relative evil general use-package))))
