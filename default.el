;;; default --- Emacs setup
;;;
;;; Commentary:
;;; - nothing too interesting here
;;;
;;; Code:

(require 'use-package)
(require 't0yv0-ware)

;;; package configuration

(use-package company
  :after eglot
  :hook (eglot-managed-mode . company-mode)
  :custom (company-idle-delay nil)
  :bind ("C-M-i" . company-complete-common-or-cycle))

(use-package consult
  :after dash
  :bind (("M-y"       . consult-yank-pop)
         ("C-x b"     . consult-buffer)
         ("M-g g"     . consult-goto-line)
         ("M-g M-g"   . consult-goto-line)
         ("M-g i"     . consult-imenu)
         ("M-g C-SPC" . consult-mark)
         ("M-g l"     . consult-line)
         ("M-g d"     . t0yv0/consult-changed-line)
         ("C-x p b"   . consult-project-buffer)
         ("C-x p g"   . consult-ripgrep))
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
   '(t0yv0/consult-source-window-buffer
     t0yv0/consult-source-git-status-file
     consult--source-hidden-buffer
     consult--source-modified-buffer
     consult--source-buffer
     consult--source-recent-file
     consult--source-file-register
     consult--source-bookmark
     consult--source-project-buffer-hidden
     consult--source-project-recent-file-hidden))
  (consult-project-buffer-sources
   '(t0yv0/consult-source-git-status-file
     consult--source-project-buffer
     consult--source-project-recent-file)))

(use-package copilot)

(use-package dash)

(use-package dap-mode
  :init (require 'dap-dlv-go))

(use-package edit-indirect)

(use-package eglot
  :bind (("M-g n"   . flymake-goto-next-error)
         ("M-g M-n" . flymake-goto-next-error)
         ("M-g p"   . flymake-goto-prev-error)
         ("M-g M-p" . flymake-goto-prev-error)
         ("M-g e"   . consult-flymake)
         ("M-g M-e" . t0yv0/consult-project-flymake))
  :custom
  (eglot-stay-out-of (list 'eldoc))
  (eglot-ignored-server-capabilities '(:hoverProvider
                                       :signatureHelpProvider
                                       :documentHighlightProvider
                                       :codeLensProivder
                                       :inlayHintProvier))
  :config
  (add-hook 'eglot-managed-mode-hook #'t0yv0/disable-eglot-inlay-hints))

(use-package emacs
  :bind* (("C-c d"     . t0yv0/diary)
          ("C-c z"     . t0yv0/vterm-repeat)
          ("C-c o"     . org-capture)
          ("C-c ?"     . t0yv0/cheatsheet)
          ("C-x C-b"   . t0yv0/window-buffer-back)
          ("C-x M-b"   . t0yv0/window-buffer-forward)
          ("C-x C-M-b" . t0yv0/window-buffer-forward)
          ("M-s d"     . t0yv0/consult-ripgrep-current-directory)
          ("M-s p"     . t0yv0/consult-ripgrep-current-project))

  :hook
  (before-save-hook . delete-trailing-whitespace)

  :custom-face
  (region ((t (:extend t :background "khaki"))))

  :custom
  (bookmark-default-file "~/my/bookmarks")
  (bookmark-save-flag 1)
  (column-number-mode t)
  (display-fill-column-indicator-mode t)
  (fill-column 100)
  (global-mark-ring-max 6)
  (global-so-long-mode 1)
  (indent-tabs-mode nil)
  (inhibit-splash-screen t)
  (inhibit-startup-message t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "")
  (major-mode 'text-mode)
  (make-backup-files nil)
  (mark-ring-max 6)
  (menu-bar-mode nil)
  (ns-command-modifier 'meta)
  (ns-right-command-modifier 'super)
  (prefer-coding-system 'utf-8)
  (scroll-bar-mode nil)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (suggest-key-bindings t)
  (switch-to-buffer-obey-display-actions t)
  (default-tab-width 8)
  (tab-width 8)
  (tool-bar-mode nil)
  (visible-bell t)
  (winner-mode t)

  (default-frame-alist '((menu-bar-lines . 0)
	                     (tool-bar-lines . 0)
	                     (font . "Iosevka 14")
	                     (vertical-scroll-bars . nil)
	                     (horizontal-scroll-bars . nil))))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
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

(use-package go-mode)

(use-package go-ts-mode
  :mode "\\.go\\'"
  :init (t0yv0/ensure-tree-sitter-grammar-install)
  :after go-mode
  :bind (("C-c C-a" . go-import-add)
         ("C-M-b"   . t0yv0/treesit-backward)
         ("C-M-f"   . t0yv0/treesit-forward)
         ("C-M-h"   . t0yv0/treesit-expand-region)
         ("C-M-p"   . t0yv0/treesit-previous)
         ("C-M-n"   . t0yv0/treesit-next)
         ("C-M-d"   . t0yv0/treesit-down)
         ("C-M-u"   . t0yv0/treesit-up))
  :hook
  (go-ts-mode . eglot-ensure)
  (before-save . t0yv0/gofmt-before-save))

(use-package haskell-mode)

(use-package hydra
  :init

  (defhydra t0yv0/link-hydra (:color blue :hint nil)
    "links"
    ("g" t0yv0/kill-github-link-at-point "gh-kill")
    ("G" t0yv0/github-link-at-point-to-register "gh-register")
    ("o" t0yv0/kill-org-link "org-kill")
    ("O" t0yv0/org-link-to-register "org-register"))

  (defhydra t0yv0/windmove-hydra (:hint nil)
    "windmove: use arrow keys or fbnp to nav, add shift to swap\n"
    ("<enter>" nil "select")
    ("C-j" nil "select")
    ("0" delete-window "delete")
    ("1" delete-other-windows "delete-other")
    ("v" split-window-vertically "v-split")
    ("h" split-window-horizontally "h-split")
    ("S-<left>" windmove-swap-states-left)
    ("S-<right>" windmove-swap-states-right)
    ("S-<up>" windmove-swap-states-up)
    ("S-<down>" windmove-swap-states-down)
    ("B" windmove-swap-states-left)
    ("F" windmove-swap-states-right)
    ("P" windmove-swap-states-up)
    ("N" windmove-swap-states-down)
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<up>" windmove-up)
    ("<down>" windmove-down)
    ("b" windmove-left)
    ("f" windmove-right)
    ("p" windmove-up)
    ("n" windmove-down))

  (defhydra t0yv0/compile-hydra (:color blue)
    "compilation"
    ("c" compile "compile")
    ("m" t0yv0/mermaid-compile "mermaid"))

  (defhydra t0yv0/vterm-hydra (:color blue)
    "vterms"
    ("v" t0yv0/vterm "vterm")
    ("p" t0yv0/vterm-proj "vterm-proj")
    ("d" t0yv0/vterm-dir "vterm-dir")
    ("r" t0yv0/vterm-repeat "vterm-repeat"))

  (defhydra t0yv0/copilot-hydra ()
    "copilot"
    ("." copilot-complete "complete")
    ("n" copilot-next-completion "next")
    ("p" copilot-previous-completion "prev")
    ("C-j" copilot-accept-completion "accept" :color blue))

  :bind (("C-c w" . t0yv0/windmove-hydra/body)
         ("C-c l" . t0yv0/link-hydra/body)
         ("C-c c" . t0yv0/compile-hydra/body)
         ("C-c v" . t0yv0/vterm-hydra/body)
         ("C-c /" . t0yv0/copilot-hydra/body)))

(use-package json-mode)

(use-package nix-mode)

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init (marginalia-mode))

(use-package mermaid-mode)

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (progn
    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((shell . t)
                                   (python . t)))
    (org-link-set-parameters "gh" :follow #'t0yv0/org-follow-gh-link)
    (setq org-default-notes-file "~/my/notes.org")
    (setq org-agenda-files '("~/my/notes.org"
                             "~/my/gtd.org"
                             "~/my/tickler.org"))

    ;; https://orgmode.org/manual/Capture-templates.html#Capture-templates
    (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline "~/my/notes.org" "Tasks")
             "* TODO %i%?\n  %a")
            ("T" "Tickler" entry
             (file+headline "~/my/tickler.org" "Tickler")
             "* %i%? \n %(format-time-string \"<%Y-%m-%d %H:%M>\" (current-time))")))
    (setq org-refile-targets '(("~/my/gtd.org" :maxlevel . 3)
                               ("~/my/someday.org" :level . 1)
                               ("~/my/tickler.org" :maxlevel . 2)))))

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

(use-package paredit
  :diminish paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook
	    #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook
	    #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook
	    #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook
            #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook
	    #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook
	    #'enable-paredit-mode)
  (add-hook 'racket-mode-hook
	    #'enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("M-s" . nil)))

(use-package recentf
  :bind
  ("C-x C-r" . consult-recent-file)
  :config
  (setq recentf-max-menu-items 15
        recentf-max-saved-items 100)
  (recentf-mode 1))

(use-package tide
  :config (add-hook 'typescript-mode-hook #'(lambda ()
					      (company-mode +1)
					      (tide-setup))))

(use-package typescript-mode)

(use-package vertico :init (vertico-mode))

(use-package vterm
  :bind (("C-x p v" . t0yv0/vterm-proj))
  :bind (:map vterm-mode-map
              ("M-/" . #'t0yv0/vterm-dabbrev-expand))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*vterm"
                 (display-buffer-reuse-window
                  t0yv0/display-buffer-same-vterm-window
                  display-buffer-reuse-mode-window
                  display-buffer-in-direction)
                 (inhibit-same-window . nil)
                 (mode vterm-mode vterm-copy-mode)
                 (direction . bottom)
                 (window . root)
                 (window-height . 0.381)
                 (dedicated . t))))

(use-package yaml-mode)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        (list "~/my/snippets"
              (concat (file-name-directory (or load-file-name (buffer-file-name))) "snippets")))
  (yas-reload-all)
  (yas-global-mode))

(custom-set-variables
 '(project-switch-commands
   '((consult-project-buffer "Buffer" ?b)
     (project-find-file "File" nil)
     (t0yv0/consult-ripgrep-current-project "Ripgrep" ?g)
     (project-find-dir "Dir" nil)
     (t0yv0/vterm-proj "VTerm" ?v)
     (magit-project-status "Magit" nil))))


(provide 'default)
;;; default.el ends here
