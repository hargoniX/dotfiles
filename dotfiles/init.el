;;; -*- lexical-binding: t; -*-

;; Initial optimization blob, mostly taken from doom emacs
(let ((file-name-handler-alist nil))

;; Set the gc threshold high initially so the init.el can just be
;; loaded in one move
(setq gc-cons-threshold most-positive-fixnum) ; 2^61 bytes

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
(add-to-list 'default-frame-alist '(font . "JuliaMono-11"))

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

(setq show-trailing-whitespace t)
(setq vc-follow-symlinks t)

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

;; auto indent change like vim sleuth
(use-package dtrt-indent
  :straight (dtrt-indent :type git :host github :repo "jscheid/dtrt-indent")
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
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . Ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Org mode stuff

(defun hbv/beamer-bold (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))


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
     (emacs-lisp . t)
     (octave .)
     (R . t)
     (plantuml . t)))
  (setq org-plantuml-exec-mode 'plantuml)
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
                 "* Protocol of %? :PROTOCOL:\n%U\n"))))
  (setq org-edit-src-content-indentation 0)
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))
  )

(defun hbv/org-sync ()
  (interactive)
  ;; This used to be copy-file but it doesn't play well with nautilus webdav
  (shell-command "cp ~/org/notes.org ~/webdav/notes.org")
  (shell-command "cp ~/org/goals.org ~/webdav/goals.org")
  (shell-command "cp ~/org/uni.org ~/webdav/uni.org")
  (shell-command "cp ~/org/weekly.org ~/webdav/weekly.org")
  (shell-command "cp ~/org/personal.org ~/webdav/personal.org"))

(use-package ox
  :after org
  :config
  (add-to-list 'org-export-filter-bold-functions 'hbv/beamer-bold))

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

(use-package org-super-agenda
  :straight t
  :after org
  :config
  (setq org-super-agenda-groups
	'((:name "Timed"
		 :time-grid t)
	  (:name "Today"
		 :scheduled today)
	  (:name "Due Today"
		 :deadline today)
	  (:name "Due soon"
		 :deadline future)
	  (:name "Waiting"
		 :todo "WAITING")
	  ))
  (org-super-agenda-mode))

(use-package origami
  :straight t
  :after (org-super-agenda)
  :bind
  (:map org-super-agenda-header-map
        ("o" . origami-toggle-node))
  :hook
  (org-agenda-mode . origami-mode))

(use-package org-attach-screenshot
  :straight t
  :after org
  :config
  (setq org-attach-screenshot-relative-links t)
  (setq org-attach-screenshot-dirfunction
	(lambda ()
		  (progn (cl-assert (buffer-file-name))
			 (concat (file-name-sans-extension (buffer-file-name))
				 "-att")))
  ))

(use-package org-alert
  :straight t
  :after org
  :config
  (setq alert-default-style 'libnotify)
  (org-alert-enable)
  (org-alert-check))

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
  :general
  (vim-leader-def 'normal 'global
    "gd" 'lsp-find-definition))

(use-package lsp-ivy
  :straight t
  :after lsp-mode
  :bind(:map lsp-mode-map ("C-l g a" . lsp-ivy-workspace-symbol)))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-sideline-enable nil))

(use-package projectile
  :straight t
  :after lsp
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package flycheck
  :straight t
  )

(use-package flycheck-posframe
  :straight t
  :hook
  (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(use-package company
  :straight t
  :hook
  (lsp-mode . company-mode)
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (org-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-idle-delay 0.4))

(use-package company-box
  :disabled
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-delay 2.0)
  (setq company-box-max-candidates 10))

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

;; This function is from nicm@
(defun openbsd-knf-space-indent (langelem)
  "Indents either 4 spaces or none.

This function is from nicm@ and also in gsoares@'s config.
Most useful with c-offset-alist entries that are lists such as
arglist-cont-nonempty"
  (save-excursion
    (goto-char (cdr langelem))
    (setq syntax (car (car (c-guess-basic-syntax))))
    (while (or (eq syntax 'arglist-intro)
	       (or (eq syntax 'arglist-cont)
		   (eq syntax 'arglist-cont-nonempty)))
      (forward-line -1)
      (setq syntax (car (car (c-guess-basic-syntax)))))
    (beginning-of-line)
    (re-search-forward "[^ \t]" (c-point 'eol))
    (goto-char (+ (match-beginning 0) 4))
    (vector (current-column))))

;; OpenBSD KNF that doug@ uses.
(defconst openbsd-knf-style
  '(
    ;; Debugging
    ;; (c-echo-syntactic-information-p . t) ;; display C-c C-s when hit 'tab'
    ;;
    ;; General settings that should be enabled in c-mode
    (indent-tabs-mode . t)      ;; use tabs whenever feasible
    (fill-column . 80)          ;; Assume KNF tries to maintain 80 char lines
    (show-trailing-whitespace . t)  ;; KNF says to not have trailing WS
    (tab-width . 8)             ;; When displaying literal tab, show 8 spaces
    ;; c-mode
    (c-progress-interval . 1)   ;; display progress meter during long indents
    (c-basic-offset . 8)        ;; KNF uses 8 space tabs
    (c-comment-only-line-offset . 0)  ;; don't indent comments extra
    (c-backspace-function . delete-backward-char)
    (c-delete-function . delete-char)
    (c-syntactic-indentation-in-macros . t) ;; indent inside macro defs
    (c-tab-always-indent . t)  ;; indent line: Use C-q 'tab' to insert literal
    (c-continued-statement-offset 0)
    ;; (c-electric-flag . nil)   ;; if you don't like auto-indent
    (c-electric-continued-statement . t)
    ;;
    (c-indent-comments-syntactically-p . nil)
    ;;
    ;; Offsets for the various c-mode symbols.  Offsets are sometimes based
    ;; upon other offsets.  For instance, arglist-intro is the 1st argument
    ;; line.  If you define arglist-cont, it uses arglist-intro plus that.
    ;; c-echo-syntactic-information-p is your friend when debugging indents.
    ;;
    ;; [N] means absolute column.  All the others are relative.
    ;;  0 = no extra indentation.  For literal column 0, use [0]
    ;;  N = extra N spaces.  For literal column N, use [N]
    ;; ++ = c-basic-offset * 2
    ;; -- = c-basic-offset * -2
    ;;  + = c-basic-offset * 1
    ;;  - = c-basic-offset * -1
    ;;  * = c-basic-offset * 0.5
    ;;  / = c-basic-offset * -0.5
    (c-offsets-alist . (
			;; Literal symbols
			(func-decl-cont . 0)        ; C++ style func mod
			(block-open . 0)            ; '{' for block
			(label . [1])               ; goto label in column 1
			(comment-intro . 0)         ; C comment
			(cpp-macro . [0])           ; #define in column 0
			;; Multiline macro symbols
			(cpp-define-intro . [0])    ; first list = column 0
			(cpp-macro-cont . +)        ; add'l lines in macro
			;; Function symbols
			(defun-open . 0)            ; '{' alone for func
			(defun-close . 0)           ; '}' alone for func
			(defun-block-intro . +)     ; first line of func
			(topmost-intro . 0)         ; outermost part
			(topmost-intro-cont . 0)    ; outermost part cont
			(statement . 0)             ; func stmt (already off)
			;; XXX statement-cont should be 4 unless
			;; it is part of a macro, then 8.
			(statement-cont . *)        ; continue stmt
			;; Class symbols.  XXX Should add support since there
			;; is a little C++ in the tree (GNU)
			;; Java
			;; K&R
			(knr-argdecl-intro . +)     ; rare K&R (from KNF)
			(knr-argdecl . 0)           ; add'l indent for rest
			;; Conditional construct symbols
			(block-close . 0)           ; '}' for block
			(statement-block-intro . +) ; stmt in loop/cond
			(substatement . +)          ; non-braced stmt if()
			(substatement-open . 0)     ; '{' in loop/cond
			(substatement-label . [1])  ; goto label in loop/cond
			(do-while-closure . 0)      ; 'while' alone in 'do'
			(else-clause . 0)           ; 'else' when not nested
			;; Brace list symbols
			(brace-list-close . 0)      ; enum/agg list close
			(brace-list-intro . +)      ; 1st line of enum/agg
			(brace-list-entry . 0)      ; add'l indent for entries
			(brace-list-open . 0)       ; enum/agg init open
			;; Switch statement symbols
			(statement-case-open . +)   ; '{' in case
			(statement-case-intro . +)  ; 1st line in case stmt
			(case-label . 0)            ; case label in switch
			;; Paren list symbols
			;; XXX This is typically a list so need to handle it
			;; differently from the rest.  Emacs adds the indents.
			(arglist-intro . openbsd-knf-space-indent) ; 1st line
			(arglist-cont . openbsd-knf-space-indent)
			(arglist-cont-nonempty . openbsd-knf-space-indent)
			(arglist-close . 0)         ; ')' alone
			;; External scope symbols
			(extern-lang-open . [0])    ; '{' alone in 'extern C'
			(extern-lang-close . [0])   ; '}' alone in 'extern C'
			(inextern-lang . +)         ; lines inside 'extern C'
			;; Statement block
			(inexpr-statement . +)))    ; gcc extension stmt expr
    ;; If not specified, the default is "before after".  All variables are
    ;; defined here.
    (c-hanging-braces-alist . (
			       ;; All variables
			       (defun-open before after)  ; function, enum
			       (defun-close before after) ; function
			       (class-open after) ; struct too
			       (class-close before after)
			       (inline-open after)
			       (inline-close before after)
			       (block-open after)
                               (block-close . c-snug-do-while)
			       (statement-cont after)
			       (substatement-open after)
			       (statement-case-open before after)
			       (brace-list-open after)
			       (brace-list-close before close)
			       (brace-list-intro after)
			       (brace-entry-open after)
			       (extern-lang-open after)
			       (extern-lang-close before after)
			       (namespace-open after)           ;; C++
			       (namespace-close before afetr)   ;; C++
			       (module-open after)              ;; CORBA
			       (module-close before after)      ;; CORBA
			       (composition-open after)         ;; CORBA
			       (composition-close before after) ;; CORBA
			       (inexpr-class-open after)
			       (inexpr-class-close before after)
			       (arglist-cont-nonempty before after)))
    ;; Whether to auto-insert newline before/after colon
    (c-hanging-colons-alist . ((case-label after)
                               (label after)
                               (access-label after)  ;; C++
                               (member-init-intro before)
                               (inher-intro)))
    ;; Whether to insert newlines after ';' or ','
    (c-hanging-semi&comma-criteria . (
				      ;; supress newline when next line non-blank
				      c-semi&comma-no-newlines-before-nonblanks
				      ;; suppress newline in paren (for loop etc)
				      c-semi&comma-inside-parenlist
				      ;; supress newline for one liner
				      c-semi&comma-no-newlines-for-oneline-inliners))
    ;; When autonewline mode is enabled, clean up some extra newlines
    (c-cleanup-list . (brace-else-brace    ; } else {
                       brace-elseif-brace  ; } else if {
                       brace-catch-brace   ; } catch (...) {
                       ;; empty-defun-braces ; {} instead of multiple lines
                       defun-close-semi    ; struct: no \n between '}' and ';'
                       list-close-comma    ; remove final comma
                       scope-operator
		       ;; space-before-funcall ; GNU standard
		       ;; compact-empty-funcall ; another GNU standard
		       ;; comment-close-slash ; term comment with slash
		       ))
    ))

(defun openbsd-set-knf-style ()
  "Set OpenBSD style in a 'c-mode-common-hook'.
Or interactively enable it in a buffer."
  (interactive)
  (c-add-style "OpenBSD" openbsd-knf-style t))

(use-package c-mode
  :hook
  (c-mode . openbsd-set-knf-style))

;; rust
(use-package rust-mode
  :straight t
  :if (executable-find "rustup")
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
  :if (executable-find "pdflatex")
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq preview-scale-function 1.5))

;; Haskell
(use-package haskell-mode
  :straight t
  :if (executable-find "ghc")
  :hook
  (haskell-mode . interactive-haskell-mode))

(use-package lsp-haskell
  :straight t
  :if (executable-find "ghc")
  :after lsp
  :hook
  (haskell-mode . lsp)
  (haskell-literate-mode . lsp))

(use-package lean4-mode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  :if (executable-find "elan")
  ;; to defer loading the package until required
  :commands (lean4-mode)
  :config
  (setq lsp-ui-doc-show-with-cursor nil))

(use-package proof-general
  :straight t
  :if (executable-find "coqtop")
  :config
  (setq proof-splash-seen nil)
  (setq proof-electric-terminator-enable t))

(use-package company-coq
  :straight t
  :if (executable-find "coqtop")
  :hook
  (coq-mode . company-coq-mode))

(use-package octave-mode
  :mode ("\\.m\\'" . octave-mode)
  :hook
  (octave-mode . company-mode))

(use-package vhdl-mode
  :hook
  (vhdl-mode . vhdl-stutter-mode))

(use-package tuareg
  :straight t
  :mode ("\\.ml\\'" . tuareg-mode)
  :if (executable-find "ocaml"))

(use-package merlin
  :straight t
  :if (executable-find "ocaml")
  :after tuareg
  :hook
  (tuareg-mode . merlin-mode))

(use-package nix-mode
  :straight t
  :if (executable-find "nix-shell")
  :mode "\\.nix\\'")

(use-package dhall-mode
  :straight t
  :if (executable-find "dhall")
  :config
  (setq
    ;; uncomment the next line to disable automatic format
    ;; dhall-format-at-save nil

    ;; header-line is obsoleted by lsp-mode
   dhall-use-header-line nil)
  :hook
  (dhall-mode . lsp))

(use-package go-mode
  :straight t
  :if (executable-find "go")
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  :hook
  (go-mode . lsp))

;; mod+i in normal mode in my i3 is bound to run this
(use-package emacs-everywhere
  :straight t
  :hook
  (emacs-everywhere-mode . (lambda () (set-input-method "Lean")))
  :config
  (setq emacs-everywhere-markdown-apps nil)
  (setq emacs-everywhere-markdown-windows nil))

(use-package rainbow-mode
  :straight t
  :hook
  (prog-mode . rainbow-mode))

(use-package graphviz-dot-mode
  :straight t
  :if (executable-find "dot")
  :hook
  (graphviz-dot-mode . (lambda () (set-input-method "Lean")))
  :config
  (setq graphviz-dot-indent-width 4))

(use-package ispell
  :straight t
  :if (executable-find "hunspell")
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "de_DE,en_GB,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_GB,en_US")
  :hook
  (org-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (text-mode . flyspell-mode))

(use-package vterm
  :straight t
  :commands (vterm vterm-other-window)
  :hook
  (vterm-mode . (lambda () (setq show-trailing-whitespace nil)))
  (vterm-mode . (lambda () (evil-local-mode -1))))

;; Custom garbage collection strategy until emacs has a garbage
;; collector from this century

(defconst hbv-gc-cons-threshold (* 32 1024 1024)) ;; 32 MB

(defun defer-garbage-collection ()
  (setq gcmh-high-cons-threshold most-positive-fixnum)
  (gcmh-set-high-threshold))

(defun restore-garbage-collection ()
  (setq gcmh-high-cons-threshold hbv-gc-cons-threshold)
  (gcmh-set-high-threshold))

(use-package gcmh
  :straight t
  :hook
  (after-init . gcmh-mode)
  (minibuffer-setup . defer-garbage-collection)
  (minibuffer-exit . restore-garbage-collection)
  (company-completion-started . (lambda (_) (defer-garbage-collection)))
  (company-after-completion . (lambda (_) (restore-garbage-collection)))
  :config
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold hbv-gc-cons-threshold)
  (setq gcmh-low-cons-threshold (* 1024 1024)) ;; 1 MB
  )



(when (file-exists-p "~/.emacs.d/local.el")
    (message "Loading ~/.emacs.d/local.el")
    (load-file "~/.emacs.d/local.el"))

) ;; global let binding
