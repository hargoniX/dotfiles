;; Initial optimization blob, mostly taken from doom emacs
(let ((file-name-handler-alist nil))

;; Set the gc threshold high initially so the init.el can just be
;; loaded in one move
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; Lower the gc threshold again afterwards
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 256 1024 1024) ; 256 MB
          )))

;; This is important for e.g. lsp mode
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)


;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

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

;; straight.el bootstrap
(setq straight-check-for-modifications 'live)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; inhibit package.el load
(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(straight-use-package 'use-package)

;; General things
(use-package exec-path-from-shell
  :straight t
  :config
  ;; don't evaluate my entire zshrc
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-copy-envs '("PATH" "SSH_AGENT_PID" "SSH_AUTH_SOCK")))

;; Themes and icons
(use-package doom-themes
  :straight t
  :config
  (setq doom-gruvbox-light-variant "soft")
  (load-theme 'doom-gruvbox-light t)
  (doom-themes-org-config))

(use-package all-the-icons
  :straight t
  :defer 2)

;; auto indent change like vim sleuth
(use-package dtrt-indent
  :straight t
  :hook
  (prog-mode . dtrt-indent-mode)
  (text-mode . dtrt-indent-mode)
  (org-mode . dtrt-indent-mode)
  (markdown-mode . dtrt-indent-mode))

;; 80 charcater limit line in prog mode
(use-package fill-column-indicator
  :straight t
  :defer 1
  :diminish fci-mode
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-color "red")
  :hook
  (prog-mode . fci-mode)
  (markdown-mode . fci-mode))

;; auto parenthesis matching
(use-package electric-pair
  :config
  (setq electric-pair-open-newline-between-pairs nil)
  :hook
  (prog-mode . electric-pair-mode)
  (text-mode . electric-pair-mode)
  (org-mode . electric-pair-mode)
  (markdown-mode . electric-pair-mode))


(use-package general
  :straight t
  :init
  ;; Space as leader key
  (general-create-definer vim-leader-def :prefix "SPC"))

(use-package which-key
  :straight t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package evil
  :straight t
  :bind
  (:map evil-motion-state-map
        ("C-y" . nil))
  (:map evil-insert-state-map
        ("C-y" . nil))
  :init
  (setq evil-toggle-key "C-~") ;; so C-z works for background
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package ivy
  :straight t
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
  :straight t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package counsel
  :straight t
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
  :straight t
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
  :hook
  (org-mode . (lambda () (electric-indent-local-mode -1)))
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-latex-listings 't)
  (setq org-agenda-files (quote ("~/org")))
  (setq org-directory "~/org")
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-log-repeat nil)
  ; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)
  (setq appt-message-warning-time 16)
  (setq appt-display-interval 5)
  ; Activate appointments so we get notifications
  (appt-activate t)
  (setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (haskell . t)
     (ruby . t)
     (dot . t)
     (emacs-lisp . t)))
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
                )))
  (setq org-edit-src-content-indentation 0))

(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-fragtog
  :straight t
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-ref
  :straight t
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite))

;; Development stuff

(use-package magit
  :straight t
  :general
  (vim-leader-def 'normal 'global
    "gb" 'magit-blame ;; git blame
    "gs" 'magit-status ;; git status
    "gl" 'magit-log ;; git log
    "gc" 'magit-checkout ;; git checkout
    "ga" 'magit-branch)) ;; git ast (as b is taken)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-auto-guess-root t)
  (setq lsp-idle-delay 1.)
  (setq lsp-enable-file-watchers nil)
  :hook
  (rust-mode . lsp)
  (java-mode . lsp)
  (python-mode . lsp)
  :general
  (vim-leader-def 'normal 'global
    "gd" 'lsp-find-definition))

(use-package lsp-ivy
  :straight t
  :after lsp-mode
  :bind(:map lsp-mode-map ("C-l g a" . lsp-ivy-workspace-symbol)))

(use-package company
  :straight t
  :hook
  (lsp-mode . company-mode)
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (org-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.4)
  :bind (:map company-active-map
	      ("C-j" . company-select-next-or-abort) ;; down
	      ("C-k" . company-select-previous-or-abort) ;; up
	      ("C-l" . company-complete-selection) ;; right, as in complete towards the right
	      ))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-delay 2.0)
  (setq company-box-max-candidates 10))

(use-package projectile
  :straight t
  :after lsp
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun company-mode/add-yasnippet ()
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package yasnippet
  :straight t
  :init
  :bind (:map yas-minor-mode-map
              ("C-y" . yas-expand))
  :hook
  (company-mode . yas-minor-mode)
  (company-mode . company-mode/add-yasnippet))

(use-package yasnippet-snippets
  :straight (yasnippet-snippets :type git :host github :repo "AndreaCrotti/yasnippet-snippets"
                                :fork (:host github
                                             :repo "hargonix/yasnippet-snippets"))
  :after yasnippet)

(use-package flycheck
  :straight t
  :after lsp)

;; rust
(use-package rust-mode
  :straight t
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
  :straight t
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-scale-function 1.5))

;; Math, input methods
(use-package math-symbol-lists
  :straight t
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
  :straight t
  :after lsp
  :config
  (setq lsp-java-format-on-type-enabled nil))

(add-hook 'java-mode-hook 'prettify-symbols-mode)
(add-hook 'java-mode-hook (lambda ()
		 (push '("!=" . ?≠) prettify-symbols-alist)
		 (push '("<=" . ?≤) prettify-symbols-alist)
                 (push '(">=" . ?≥) prettify-symbols-alist)))

;; Haskell
(use-package haskell-mode
  :straight t
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package lsp-haskell
  :straight t
  :after lsp
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp))

;; Lean
(use-package lean-mode
  :straight t
  :mode ("\\.lean\\'" . lean-mode))

;; mod+i in normal mode in my i3 is bound to run this
(use-package emacs-everywhere
  :straight t
  :hook
  (emacs-everywhere-mode . (lambda () (set-input-method "hbv-math")))
  :config
  (setq emacs-everywhere-markdown-apps nil)
  (setq emacs-everywhere-markdown-windows nil))

(use-package rainbow-mode
  :straight t
  :hook
  (prog-mode . rainbow-mode))

(use-package graphviz-dot-mode
  :straight t
  :hook
  (graphviz-dot-mode . (lambda () (set-input-method "hbv-math")))
  :config
  (setq graphviz-dot-indent-width 4))

(use-package ispell
  :straight t
  :if (executable-find "hunspell")
  :config
  (setq ispell-dictionary "de_DE,en_GB,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_GB,en_US")
  :hook
  (org-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (text-mode . flyspell-mode))

(when (file-exists-p "~/.emacs.d/local.el")
    (message "Loading ~/.emacs.d/local.el")
    (load-file "~/.emacs.d/local.el"))


(run-with-idle-timer 2 nil #'bh/org-agenda-to-appt)

) ;; global let binding
