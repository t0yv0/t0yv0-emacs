;;; default --- Emacs setup
;;;
;;; Commentary:
;;; - nothing too interesting here
;;;
;;; Code:

;;;; Increase GC threshold while loading.
(setq gc-cons-threshold (* 64 1024 1024))


(require 't0yv0-basics)
(require 'use-package)

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package avy
  :bind (("C-c f" . avy-goto-char-timer))
  :bind (:map isearch-mode-map
              (("C-f" . avy-isearch))))

(use-package code-cells
  :bind (("A-;" . code-cells-comment-or-uncomment)
         ("A-h" . code-cells-mark-cell)
         ("A-M-p" . code-cells-move-cell-up)
         ("A-M-n" . code-cells-move-cell-down)
         ("A-p" . code-cells-backward-cell)
         ("A-j" . code-cells-eval)
         ("A-n" . code-cells-forward-cell)
         ("A-m" . code-cells-mode))
  :custom
  (code-cells-eval-region-commands
   '((python-ts-mode . (lambda (beg end) (jupyter-eval-region nil beg end)))
     (python-mode . (lambda (beg end) (jupyter-eval-region nil beg end))))))

(use-package consult
  :after dash
  :bind (("M-y"       . consult-yank-pop)
         ("C-x b"     . consult-buffer)
         ("C-x C-b"   . consult-buffer)
         ("M-s d"     . t0yv0/consult-ripgrep-current-directory)
         ("M-s p"     . consult-ripgrep)
         ("M-g g"     . consult-goto-line)
         ("M-g M-g"   . consult-goto-line)
         ("M-g h"     . consult-org-heading)
         ("M-g i"     . consult-imenu)
         ("M-g e"     . consult-flymake)
         ("M-g M-e"   . t0yv0/consult-flymake-project-errors)
         ("M-g C-SPC" . consult-mark)
         ("M-g l"     . consult-line)
         ("M-g d"     . t0yv0/consult-changed-line)
         ("C-x p b"   . consult-project-buffer)
         ("C-x p g"   . consult-ripgrep)
         ("C-x r b"   . consult-bookmark)
         ("H-'"       . consult-register)
         ("H-;"       . consult-register-store)

         ("<remap> <suspend-frame>" . consult-complex-command))
  :custom
  (consult-buffer-filter
   '("\\` "
     "\\`\\*Completions\\*\\'"
     "\\`\\*Flymake log\\*\\'"
     "\\`\\*Semantic SymRef\\*\\'"
     "\\`\\*tramp/.*\\*\\'"
     "\\`\\*scratch\\*"
     "\\`\\*copilot-balancer\\*"
     "\\`\\*Async-native-compile-log\\*"
     "\\`magit"
     "\\`\\*vterm"
     "\\`\\*Messages"
     "\\`\\*Apropos"
     "\\`\\*Help"))
  (consult-buffer-sources
   '(consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-buffer
     consult--source-recent-file
     consult--source-file-register
     consult--source-bookmark
     consult--source-project-buffer-hidden
     consult--source-project-recent-file-hidden
     t0yv0/consult-source-git-status-file))
  (consult-project-buffer-sources
   '(t0yv0/consult-source-git-status-file
     consult--source-project-buffer
     consult--source-project-recent-file)))

(use-package copilot
  :bind (("C-c p b" . copilot-previous-completion)
         ("C-c p d" . copilot-diagnose)
         ("C-c p f" . copilot-next-completion)
         ("C-c p g" . copilot-clear-overlay)
         ("C-c p l" . copilot-login)
         ("C-c p m" . global-copilot-mode)
         ("C-c p p" . copilot-accept-completion)
         ("C-c p q" . copilot-logout)
         ("C-c p /" . copilot-complete))
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-disable-predicates
   (list
    (lambda ()
      (member major-mode '(shell-mode
                           inferior-python-mode
                           eshell-mode
                           term-mode
                           vterm-mode
                           comint-mode
                           compilation-mode
                           debugger-mode
                           dired-mode-hook
                           compilation-mode
                           minibuffer-mode)))))
  :config
  (advice-add 'copilot--mode-enter
              :around (lambda (orig-fun &rest args)
                        (when (not (member major-mode '(minibuffer-mode)))
                          (apply orig-fun args))))
  (advice-add 'copilot--mode-exit
              :around (lambda (orig-fun &rest args)
                        (when (not (member major-mode '(minibuffer-mode)))
                          (apply orig-fun args))))
  (copilot-diagnose))

(use-package corfu
  :init
  (global-corfu-mode)

  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("C-j" . corfu-insert)))

(use-package dape
  :bind (("H-t" . t0yv0/go-debug-current-test)
         ("H-n" . dape-next)
         ("H-c" . dape-continue)
         ("H-o" . dape-step-out)
         ("H-i" . dape-step-in)
         ("H-b" . dape-breakpoint-toggle)
         ("H-B" . dape-breakpoint-remove-all)
         ("H-r" . dape-restart)
         ("H-q" . dape-quit))
  :config
  (dape-breakpoint-global-mode))

(use-package diminish)

(use-package doom-modeline
  :after nerd-icons
  :custom (doom-modeline-minor-modes t)
  :init (doom-modeline-mode 1))

(use-package envrc
  :diminish envrc-mode
  :init (envrc-global-mode))

(use-package eglot
  :bind (("C-c e m" . eglot-find-implementation)
         ("C-c e r" . eglot-rename)
         ("C-c e i" . eglot-code-action-inline)
         ("C-c e x" . eglot-code-action-extract))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-stay-out-of (list 'eldoc))
  (eglot-ignored-server-capabilities '(:hoverProvider
                                       :signatureHelpProvider
                                       :documentHighlightProvider
                                       :codeLensProivder
                                       :inlayHintProvier))
  :config
  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (eglot-inlay-hints-mode -1))))

(use-package emacs
  :bind* (("C-c q" . t0yv0/quit)
          ("M-`"   . other-frame)
          ("H-["   . previous-buffer)
          ("H-]"   . next-buffer)

          ("<remap> <dabbrev-expand>" . hippie-expand))

  :hook
  (before-save . delete-trailing-whitespace)

  :config
  (require-theme 'modus-themes)
  (load-theme 'modus-operandi)
  (define-key isearch-mode-map (kbd "C-j") 'isearch-exit)
  (global-hl-line-mode 1)
  (t0yv0/go-fixup-compilation-regexp)

  :custom
  (display-buffer-alist
   `(((or "\\*compilation"
          "\\*Org Links"
          "\\*Org Select"
          "\\*Gofmt"
          "\\*Occur"
          "\\*Embark"
          "\\*test"
          "\\*jupyter"
          "current-region.png"
          "\\*xref"
          "\\*vterm"
          "\\*Warnings")
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 1)
      (preserve-size . (nil . t))
      (window-height . 0.382)
      (window-parameters ((no-other-window . t)
                          (no-delete-other-windows . t))))))

  (display-buffer-base-action '((display-buffer-reuse-window
                                 display-buffer-in-previous-window
                                 display-buffer-same-window
                                 display-buffer-full-frame)))

  (auto-save-default nil)
  (blink-cursor-mode nil)
  (bookmark-default-file "~/my/bookmarks")
  (bookmark-save-flag 1)
  (column-number-mode t)
  (completion-cycle-threshold 3)
  (delete-selection-mode t)
  (display-fill-column-indicator-mode t)
  (fill-column 120)
  (gc-cons-percentage 0.2)
  (global-display-line-numbers-mode t)
  (global-mark-ring-max 6)
  (global-so-long-mode 1)
  (electric-indent-mode nil)
  (electric-pair-mode t)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (inhibit-splash-screen t)
  (inhibit-startup-message t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "")
  (major-mode 'text-mode)
  (make-backup-files nil)
  (mark-ring-max 6)
  (menu-bar-mode nil)
  (mac-command-modifier 'meta)
  (mac-option-modifier 'alt)
  (mac-pass-command-to-system nil)
  (mac-pass-control-to-system nil)
  (mac-right-command-modifier 'hyper)
  (mac-right-option-modifier 'super)
  (mouse-wheel-progressive-speed nil)
  (prefer-coding-system 'utf-8)
  (read-process-output-max 4194304)
  (scroll-bar-mode nil)
  (scroll-conservatively 10)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (suggest-key-bindings t)
  (switch-to-buffer-obey-display-actions t)
  (default-tab-width 8)
  (tab-always-indent 'complete)
  (tab-width 8)
  (tool-bar-mode nil)
  (visible-bell t)
  (winner-mode t)

  (project-switch-commands
   '((consult-project-buffer "Buffer" ?b)
     (project-find-file "File" nil)
     (t0yv0/consult-ripgrep-current-project "Ripgrep" ?g)
     (project-find-dir "Dir" nil)
     (t0yv0/vterm-proj "VTerm" ?v)
     (magit-project-status "Magit" nil)))

  (default-frame-alist
   '((menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (font . "Iosevka 13")
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil))))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)

  :init
  (require 'embark)
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  (add-to-list 'embark-target-finders 't0yv0/embark-target-gh-ref)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package epa
  :config
  (advice-add 'epa-file-write-region
              :before (lambda (&rest args)
                        (setq-local epa-file-encrypt-to '("0x9E15CD89706EE947"))
                        (message "NOTE: bypassing recipient selector and encrypting for self"))))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package forge
  :after magit
  :config
  (require 'auth-source)
  (auth-source-pass-enable))

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package gh-autolinks
  :custom (gh-autolinks-use-overlays t)
  :hook (before-save . (lambda ()
                         (when (equal major-mode 'org-mode)
                           (gh-autolinks-org-buffer)))))

(use-package gptel
  :bind (("C-c g b" . gptel)
         ("C-c g g" . gptel-send)
         ("C-c g m" . gptel-menu)
         ("C-c g t" . gptel-set-topic))
  :config
  (let ((openai-token (t0yv0/gptel-openai-token)))
    (if openai-token
        (setq
         gptel-model "gpt-4o"
         gptel-api-key openai-token)
      (setq
       gptel-model 'llama3.2:latest
       gptel-backend (gptel-make-ollama "ollama"
                       :host "localhost:11434"
                       :models '(llama3.2:latest
                                 llama2:latest))))))

(use-package git-link
  :bind (("C-c l g" . t0yv0/git-link)
         ("C-c l c" . git-link-commit)
         ("C-c l h" . git-link-homepage)))

(use-package json-ts-mode
  :mode "\\.json\\'")

(use-package jupyter)

(use-package zoom
  :custom (zoom-size '(0.618 . 0.618))
  :config
  (zoom-mode t))

(use-package go-mode
  :bind (("C-c C-a" . go-import-add))
  :config
  (setq auto-mode-alist (rassq-delete-all 'go-mode auto-mode-alist)))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :bind (("C-c t SPC" . testrun-at-point)
         ("C-c t t"   . testrun-repeat)
         ("C-c t d"   . testrun-in-current-directory)
         ("C-c t v"   . testrun-toggle-verbosity)
         ("C-c b d"   . t0yv0/build-go-current-directory))

  :config
  (t0yv0/ensure-tree-sitter-grammar-install)
  :hook
  (go-ts-mode . (lambda ()
                  (t0yv0/go-fixup-compilation-regexp)
                  (eglot-ensure)
                  (message (format "enabling treesitedit-mode"))
                  (treesitedit-mode 1)))
  (before-save . (lambda ()
                   (interactive)
                   (when (eq major-mode 'go-ts-mode)
                     (require 'go-mode)
                     (gofmt)))))

(use-package haskell-mode
  ;; TODO: run-hooks: Symbol’s function definition is void: haskell-mode-after-save-handler
  :mode "\\.hs\\'")

(use-package jinx
  :diminish jinx-mode
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; Requires a font installed from https://www.nerdfonts.com
(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package magit
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-topleft-v1)
  :bind (("C-x g" . magit-status)))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package mermaid-mode
  :bind ("C-c M" . t0yv0/mermaid-compile)
  :config
  (defun t0yv0/mermaid-compile ()
    "Compiles and displays a Mermaid diagram from current region."
    (interactive)
    (save-excursion
      (when (and (eq 'markdown-mode major-mode)
                 (not (use-region-p)))
        (markdown-mark-paragraph))
      (when (and (eq 'org-mode major-mode)
                 (not (use-region-p)))
        (org-babel-mark-block))
      (mermaid-compile-region)
      (deactivate-mark t)
      (with-current-buffer "current-region.png"
        (revert-buffer nil t))))
  :custom
  (mermaid-mmdc-location t0yv0/mermaid-mmdc-location))

(use-package minions
  :config (minions-mode +1))

(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)))

(use-package org
  :bind (("C-c c"     . org-capture)
         ("C-c a"     . org-agenda)
         ("C-c l l"   . org-store-link)
         ("C-c l C-l" . org-insert-link))
  :custom (org-src-lang-modes '(("C" . c)
                                ("C++" . c++)
                                ("asymptote" . asy)
                                ("bash" . sh)
                                ("beamer" . latex)
                                ("calc" . fundamental)
                                ("cpp" . c++)
                                ("ditaa" . artist)
                                ("desktop" . conf-desktop)
                                ("dot" . fundamental)
                                ("elisp" . emacs-lisp)
                                ("go" . go-ts)
                                ("ocaml" . tuareg)
                                ("screen" . shell-script)
                                ("shell" . sh)
                                ("sqlite" . sql)
                                ("toml" . conf-toml)))
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :config
  (setq org-startup-indented t)
  (setq org-archive-location "%s_archive.gpg::")
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (python . t)))
  (org-link-set-parameters
   "gh"
   :follow #'(lambda (path _)
               (browse-url
                (concat "https://github.com/"
                        (replace-regexp-in-string
                         (regexp-quote "#")
                         "/issues/" path nil 'literal)))))
  (setq org-default-notes-file "~/workshare/org/notes.org.gpg")
  (setq org-agenda-files '("~/workshare/org/notes.org.gpg"
                           "~/workshare/org/gtd.org.gpg"
                           "~/workshare/org/tickler.org.gpg"))
  ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/workshare/org/notes.org.gpg" "Tasks")
           "* TODO %i%?\n  %a")
          ("T" "Tickler" entry
           (file+headline "~/workshare/org/tickler.org.gpg" "Tickler")
           "* %i%? \n %(format-time-string \"<%Y-%m-%d %H:%M>\" (current-time))")))
  (setq org-refile-targets '(("~/workshare/org/gtd.org.gpg" :maxlevel . 1))))

(use-package org-present)

(use-package org-roam
  :bind (("C-c r c" . org-roam-capture)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r b" . org-roam-buffer-toggle))
  :custom
  (org-roam-directory (file-truename "~/workshare/org-roam/"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-dailies
  :bind (("C-c r t" . org-roam-dailies-capture-today)
         ("C-c r d" . org-roam-dailies-goto-today)
         ("C-c r g" . org-roam-dailies-goto-date))
  :custom
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org.gpg"
                         "#+title: %<%Y-%m-%d>\n")))))

(use-package ob-go)

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-regexp))
  (orderless-style-dispatchers (t0yv0/orderless-style-dispatchers))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c q" . ormolu-format-buffer)))

(use-package python-ts-mode
  :mode "\\.py\\'"
  :config
  (t0yv0/ensure-tree-sitter-grammar-install)
  :hook
  (python-ts-mode . (lambda ()
                      (eglot-ensure)
                      (pyvenv-mode t))))

(use-package pyvenv
  :config
  (pyvenv-mode t))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-max-menu-items 15
        recentf-max-saved-items 100)
  :init
  (recentf-mode 1))

(use-package terraform-mode
  :custom (terraform-indent-level 2)
  :config
  (defun t0yv0/terraform-mode-init ()
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 't0yv0/terraform-mode-init))

(use-package testrun
  :custom (testrun-switch-to-compilation-buffer t))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (require 'treesitedit)
  (treesitedit-mode)
  (t0yv0/ensure-tree-sitter-grammar-install)
  :hook
  (typescript-mode . (lambda ()
                       (eglot-ensure)
                       (treesit-parser-create 'typescript))))

(use-package vertico
  :init
  (vertico-mode))

(use-package vterms
  :bind (("H-<return>" . vterms-toggle)
         ("C-H-<return>" . vterms-back)
         ("C-c z" . vterms-repeat)
         ("H-." . vterms-cd)))

(use-package vterm
  :bind (:map vterm-mode-map
              ("M-c" . kill-ring-save)
              ("M-v" . yank)
              ("M-z" . undo)
              ("M-w" . kill-ring-save)
              ("M-/" . #'t0yv0/vterm-dabbrev-expand))
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               (compilation-shell-minor-mode 1)
                               (setq-local global-hl-line-mode nil))))

(use-package wgrep
  :custom ((wgrep-auto-save-buffer t))
  :bind (:map grep-mode-map
              (("e" . wgrep-change-to-wgrep-mode))))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (list "~/my/snippets" t0yv0/yas-snippets))
  (yas-reload-all)
  (yas-global-mode))


;; Done loading, decreate the threshold.
(setq gc-cons-threshold (* 2 1024 1024))


(provide 'default)
;;; default.el ends here
