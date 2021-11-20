;;; -*- lexical-binding: t; -*-

;; Initial optimization blob, mostly taken from doom emacs
(let ((file-name-handler-alist nil))

;; Set the gc threshold high initially so the init.el can just be
;; loaded in one move
(setq gc-cons-threshold most-positive-fixnum) ; 2^61 bytes

;; This is important for e.g. eglot mode
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
(add-to-list 'default-frame-alist '(font . "JuliaMono"))

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
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . Ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


;(defun hbv/enable-doom-modeline-icons (frame)
;  (unless (eq (rassoc 'window-system frame) nil)
;    (setq doom-modeline-icon doom-modeline-icon)))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15)
  ;(add-hook 'after-make-frame-functions #'hbv/enable-doom-modeline-icons)
  )

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
     (R . t)))
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
  (copy-file "~/org/notes.org" "~/webdav/notes.org" t)
  (copy-file "~/org/personal.org" "~/webdav/personal.org" t)
  (copy-file "~/org/uni.org" "~/webdav/uni.org" t)
  (copy-file "~/org/weekly.org" "~/webdav/weekly.org" t)
  (copy-file "~/webdav/mobile-notes.org" "~/org/mobile-notes.org" t))

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
  (setq org-super-agenda-groups '((:auto-group t)))
  (org-super-agenda-mode))

(use-package origami
  :straight t
  :after (org-super-agenda)
  :bind
  (:map org-super-agenda-header-map
        ("o" . origami-toggle-node))
  :hook
  (org-agenda-mode . origami-mode))

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

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  :commands (eglot))

(use-package company
  :straight t
  :hook
  (eglot-mode . company-mode)
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
  (quail-define-rules
   ; Equality and order
   ("<=" ?≤) (">=" ?≥) ("\\prec" ?≺) ("\\preceq" ?≼) ("\\succ" ?≻)
   ("\\succeq" ?≽)
   ("/=" ?≠) ("\\neq" ?≠) ("\\=n" ?≠)("\\equiv" ?≡) ("\\nequiv" ?≢)
   ("\\approx" ?≈) ("\\~~" ?≈) ("\\t=" ?≜) ("\\def=" ?≝)
   ;; Set theory
   ("\\sub" ?⊆) ("\\subset" ?⊂) ("\\subseteq" ?⊆) ("\\in" ?∈)
   ("\\inn" ?∉) ("\\:" ?∈) ("\\cap" ?∩) ("\\inter" ?∩)
   ("\\cup" ?∪) ("\\uni" ?∪) ("\\emptyset" ?∅) ("\\empty" ?∅)
   ("\\times" ?×) ("\\x" ?×)
   ;; Number stuff
   ("\\div" ?∣) ("\\infty" ?∞) ("\\sqrt" ?√) ("\\Im" ?ℑ) ("\\Re" ?ℜ)
   ; Logic
   ("\\/" ?∨) ("\\and" ?∧) ("/\\" ?∧) ("\\or" ?∨) ("\\~" ?¬) ("\\neg" ?¬)
   ("|-" ?⊢) ("|-n" ?⊬) ("\\bot" ?⊥) ("\\top" ?⊤) ("\\r" ?→) ("\\lr" ?↔)
   ("\\qed" ?∎)
   ; Predicate Logic
   ("\\all" ?∀) ("\\ex" ?∃) ("\\exn" ?∄)
   ;; functions
   ("\\to" ?→) ("\\mapsto" ?↦) ("\\circ" ?∘) ("\\comp" ?∘) ("\\integral" ?∫)
   ("\\fun" ?λ)
   ; sets of numbers
   ("\\nat" ?ℕ) ("\\N" ?ℕ) ("\\int" ?ℤ) ("\\Z" ?ℤ) ("\\rat" ?ℚ) ("\\Q" ?ℚ)
   ("\\real" ?ℝ) ("\\R" ?ℝ) ("\\complex" ?ℂ) ("\\C" ?ℂ) ("\\prime" ?ℙ)
   ("\\P" ?ℙ)
   ; Complexity
   ("\\bigo" ?𝒪)
   ; greek
   ("\\Ga" ?α) ("\\GA" ?Α) ("\\a" ?α)
   ("\\Gb" ?β) ("\\GB" ?Β) ("\\b" ?β)
   ("\\Gg" ?γ) ("\\GG" ?Γ) ("\\g" ?γ) ("\\G" ?Γ)
   ("\\Gd" ?δ) ("\\GD" ?Δ) ("\\del" ?δ) ("\\Del" ?Δ)
   ("\\Ge" ?ε) ("\\GE" ?Ε) ("\\eps" ?ε)
   ("\\Gz" ?ζ) ("\\GZ" ?Ζ)
   ("\\Gh" ?η) ("\\GH" ?Η) ("\\eta" ?η)
   ("\\Gth" ?θ) ("\\GTH" ?Θ) ("\\the" ?θ) ("\\The" ?Θ)
   ("\\Gi" ?ι) ("\\GI" ?Ι) ("\\iota" ?ι)
   ("\\Gk" ?κ) ("\\GK" ?Κ)
   ("\\Gl" ?λ) ("\\GL" ?Λ) ("\\lam" ?λ)
   ("\\Gm" ?μ) ("\\GM" Μ) ("\\mu" ?μ)
   ("\\Gx" ?ξ) ("\\GX" ?Ξ) ("\\xi" ?ξ) ("\\Xi" ?Ξ)
   ("\\Gp" ?π) ("\\GP" ?Π) ("\\pi" ?π) ("\\Pi" ?Π)
   ("\\Gr" ?ρ) ("\\GR" ?Ρ) ("\\rho" ?ρ)
   ("\\Gs" ?σ) ("\\GS" ?Σ) ("\\sig" ?σ) ("\\Sig" ?Σ)
   ("\\Gt" ?τ) ("\\GT" ?Τ) ("\\tau" ?τ)
   ("\\Gph" ?ϕ) ("\\GPH" ?Φ) ("\\phi" ?ϕ) ("\\Phi" ?Φ)
   ("\\Gc" ?χ) ("\\GC" ?Χ) ("\\chi" ?χ)
   ("\\Gp" ?ψ) ("\\GP" ?Ψ) ("\\psi" ?ψ)
   ("\\Go" ?ω) ("\\GO" ?Ω) ("\\omega" ?ω) ("\\Omega" ?Ω)
   )
   (mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
        (append math-symbol-list-superscripts math-symbol-list-subscripts))
)

;; Haskell
(use-package haskell-mode
  :straight t
  :hook
  (haskell-mode . interactive-haskell-mode))


(use-package lean4-mode
  :straight (lean4-mode :type git :host github :repo "leanprover/lean4"
             :files ("lean4-mode/lean4*.el"))
  ;; to defer loading the package until required
  :commands (lean4-mode))

(use-package octave-mode
  :mode ("\\.m\\'" . octave-mode)
  :hook
  (octave-mode . company-mode))

(use-package ess
  :straight t
  :mode ("\\.R\\'" . ess-r-mode)
  :mode ("\\.r\\'" . ess-r-mode))

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
