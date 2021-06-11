(setq make-backup-files nil) ;; We dont need these
(setq auto-save-default nil) ;; Not this one either
(menu-bar-mode -1) ;; The menu bar looks ugly in terminal
(tool-bar-mode -1) ;; Nobody needs this
(scroll-bar-mode -1)
(setq inhibit-startup-screen t) ;; Leave me alone with your tutorials
(setq tramp-default-method "ssh") ;; speed up tramp mode
(set-face-attribute 'default nil :height 125) ;; Set font size

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

(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t)

;; General things

(use-package exec-path-from-shell
  :ensure t)

;; Use ssh agent from env
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
(exec-path-from-shell-copy-env "PATH")

;; Themes and icons
(use-package doom-themes
  :ensure t
  :config
  (setq doom-gruvbox-light-variant "soft")
  (load-theme 'doom-gruvbox-light t)
  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t
  :defer 2)

;; auto indent change like vim sleuth
(use-package dtrt-indent
  :ensure t
  :hook
  (prog-mode . dtrt-indent-mode)
  (markdown-mode . dtrt-indent-mode))

;; 80 charcater limit line in prog mode
(use-package fill-column-indicator
  :ensure t
  :defer 1
  :diminish fci-mode
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "red")
  :hook
  (prog-mode . fci-mode)
  (markdown-mode . fci-mode))

(use-package smartparens ;; auto parens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil) ;; to hide this fucking highlighting
  :hook
  (prog-mode . smartparens-mode)
  (markdown-mode . smartparens-mode))

(use-package general
  :ensure t
  :init
  ;; Space as leader key
  (general-create-definer vim-leader-def :prefix "SPC"))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package evil
  :ensure t
  :init
  (setq evil-toggle-key "C-~") ;; so C-z works for background
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-matchit
  :after evil
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package ivy
  :ensure t
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
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Org mode stuff

; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(use-package org
  :ensure t
  ;; C-c C-t org rotate
  ;; Tab fold/unfold
  ;; M-S-RET Insert new TODO
  ;; https://orgmode.org/guide/index.html#Top
  ;; TODO 6.1
  :general
  (vim-leader-def 'normal 'global
    "oci" 'org-clock-in
    "oco" 'org-clock-out
    "oa"  'org-agenda
    "oca" 'org-capture) 
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-latex-listings 't)
  (setq org-agenda-files (quote ("~/org")))
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-log-repeat nil)
  ; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)
  ; Activate appointments so we get notifications
  (appt-activate t)
  :init
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))))
  (setq org-log-done 'time)
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/org/notes.org")
                 "* TODO %?\n")
                ("n" "note" entry (file "~/org/notes.org")
                 "* %? :NOTE:\n")
                ("p" "protocol" entry (file "~/org/notes.org")
                 "* Protocol of %? :PROTOCOL:\n%U\n")
                ))))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode))


;; Development stuff

(use-package magit
  :ensure t
  :general
  (vim-leader-def 'normal 'global
    "gb" 'magit-blame ;; git blame
    "gs" 'magit-status ;; git status
    "gl" 'magit-log ;; git log
    "gc" 'magit-checkout ;; git checkout
    "ga" 'magit-branch)) ;; git ast (as b is taken)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq gc-cons-threshold 100000000) ;; 100 mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-auto-guess-root t)
  (setq lsp-idle-delay 1.)
  :hook
  (rust-mode . lsp)
  (java-mode . lsp)
  (python-mode . lsp)
  :general
  (vim-leader-def 'normal 'global
    "gd" 'lsp-find-definition))

(use-package lsp-ui
  :ensure t
  :defer 2
  :config
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-ui-doc-enable nil))

(use-package lsp-ivy
  :ensure t
  :defer 2
  :after lsp-mode
  :bind(:map lsp-mode-map ("C-l g a" . lsp-ivy-workspace-symbol)))

(use-package company
  :ensure t
  :hook
  (lsp-mode . company-mode)
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (org-mode . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.4)
  :bind (:map company-active-map
	      ("C-j" . company-select-next-or-abort) ;; down
	      ("C-k" . company-select-previous-or-abort) ;; up
	      ("C-l" . company-complete-selection) ;; right, as in complete towards the right
	      ))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :init
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun company-mode/add-yasnippet ()
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package yasnippet
  :ensure t
  :init
  :hook
  (company-mode . yas-minor-mode)
  (company-mode . company-mode/add-yasnippet))

(use-package yasnippet-snippets
  :quelpa ((yasnippet-snippets :fetcher github :repo "hargoniX/yasnippet-snippets")))

(use-package flycheck
  :ensure t)

;; rust
(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . prettify-symbols-mode)
  (rust-mode . (lambda ()
		 (push '("->" . ?→) prettify-symbols-alist)
		 (push '("=>" . ?⇒) prettify-symbols-alist)
		 (push '("!=" . ?≠) prettify-symbols-alist)
		 (push '("<<=" . "<<=") prettify-symbols-alist)
		 (push '(">>=" . ">>=") prettify-symbols-alist)
		 (push '("<=" . ?≤) prettify-symbols-alist)
                 (push '(">=" . ?≥) prettify-symbols-alist))))

;; LaTeX
(use-package auctex
  :ensure t
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-scale-function 1.5))

;; Math, input methods
(use-package math-symbol-lists
  :ensure t
  :config
  (quail-define-package "hbv-math" "UTF-8" "Ω" t)
  (quail-define-rules ; add whatever extra rules you want to define here...
   ; Equality
   ("<="        ?≤)
   (">="        ?≥)
   ("~="        ?≠)
   ; Logic
   ("~"         ?¬)
   ("->"        ?→)
   ("=>"        ?⇒)
   ("<->"       ?↔)
   ("<=>"       ?⇔)
   ("/\\"       ?∧)
   ("\\/"       ?∨)
   ; Predicate Logic
   ("ALL"       ?∀)
   ("EX"        ?∃)
   ; sets of numbers
   ("\\nats"    ?ℕ)
   ("\\ints"    ?ℤ)
   ("\\rats"    ?ℚ)
   ("\\reals"   ?ℝ)
   ("\\complex" ?ℂ)
   ("\\primes"  ?ℙ))
   (mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
        (append math-symbol-list-basic math-symbol-list-extended math-symbol-list-superscripts math-symbol-list-subscripts))
)

;; Java
(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-format-on-type-enabled nil))

(add-hook 'java-mode-hook 'prettify-symbols-mode)
(add-hook 'java-mode-hook (lambda ()
		 (push '("!=" . ?≠) prettify-symbols-alist)
		 (push '("<=" . ?≤) prettify-symbols-alist)
                 (push '(">=" . ?≥) prettify-symbols-alist)))

;; Haskell
(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package lsp-haskell
  :ensure t
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp))

;; Lean
(use-package lean-mode
  :ensure t)

;; mod+i in normal mode in my i3 is bound to run this
(use-package emacs-everywhere
  :ensure t
  :hook
  (emacs-everywhere-mode . (lambda () (set-input-method "hbv-math")))
  :config
  (setq emacs-everywhere-markdown-apps nil)
  (setq emacs-everywhere-markdown-windows nil))

(use-package rainbow-mode
  :ensure t
  :hook
  (prog-mode . rainbow-mode))

(bh/org-agenda-to-appt)
