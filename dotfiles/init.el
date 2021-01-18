(setq make-backup-files nil) ;; We dont need these
(setq auto-save-default nil) ;; Not this one either
(menu-bar-mode -1) ;; The menu bar looks ugly in terminal
(tool-bar-mode -1) ;; Nobody needs this
(toggle-scroll-bar -1) ;; Or this
(setq inhibit-startup-screen t) ;; Leave me alone with your tutorials
(setq tramp-default-method "ssh") ;; speed up tramp mode

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Relative line numbers
(setq-default display-line-numbers 'visual
              display-line-numbers-widen t
              ;; this is the default
              display-line-numbers-current-absolute t)

(defun noct:relative ()
  (setq-local display-line-numbers 'visual))

(defun noct:absolute ()
  (setq-local display-line-numbers t))

(add-hook 'evil-insert-state-entry-hook #'noct:absolute)
(add-hook 'evil-insert-state-exit-hook #'noct:relative)

(setq-default show-trailing-whitespace t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("elpa"       . "http://elpa.gnu.org/packages/")
                         ))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t) ; always download packages

;; General things

;; Themes and icons
(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t))

(use-package all-the-icons)

;; auto indent change like vim sleuth
(use-package dtrt-indent
  :init
  (add-hook 'prog-mode-hook 'dtrt-indent-mode))

;; 80 charcater limit line in prog mode
(use-package fill-column-indicator
  :diminish fci-mode
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "red")
  :init
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package smartparens ;; auto parens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil) ;; to hide this fucking highlighting
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package general
  :init
  ;; Space as leader key
  (general-create-definer vim-leader-def :prefix "SPC"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package evil
  :init
  (setq evil-toggle-key "C-~") ;; so C-z works for background
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  :config
  (evil-mode))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper) ; TODO: possibly map this to / at some point?
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . Ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Org mode stuff

(use-package org
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
  (setq org-log-done 'time))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))



;; Development stuff

(use-package magit
  :general
  (vim-leader-def 'normal 'global
    "gb" 'magit-blame ;; git blame
    "gs" 'magit-status ;; git status
    "gl" 'magit-log ;; git log
    "gc" 'magit-checkout ;; git checkout
    "ga" 'magit-branch)) ;; git ast (as b is taken)

(use-package evil-magit
  :after magit)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-log-io t)
  (setq lsp-auto-guess-root t)
  :hook
  (rust-mode . lsp)
  (java-mode . lsp)
  :general
  (vim-leader-def 'normal 'global
    "gd" 'lsp-find-definition))

(use-package company
  :hook
  (lsp-mode . company-mode)
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  :init
  (setq gc-cons-threshold 1600000)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.4)
  :bind (:map company-active-map
	      ("C-j" . company-select-next-or-abort) ;; down
	      ("C-k" . company-select-previous-or-abort) ;; up
	      ("C-l" . company-complete-selection) ;; right, as in complete towards the right
	      ))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile)

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun company-mode/add-yasnippet ()
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package yasnippet
  :init
  :hook
  (company-mode . yas-minor-mode)
  (company-mode . company-mode/add-yasnippet))

(use-package yasnippet-snippets)

;; language specific

;; rust
(use-package rust-mode)

;; LaTeX
(use-package auctex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-scale-function 1.5))

;; Java
(use-package lsp-java)
