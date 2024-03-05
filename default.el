;;; default --- Emacs setup
;;;
;;; Commentary:
;;; - nothing too interesting here
;;;
;;; Code:

(require 'hydra)
(require 't0yv0-basics)
(require 'use-package)

(use-package avy
  :bind (("C-c s" . avy-goto-char-2))
  :bind (:map isearch-mode-map
              (("C-c s" . avy-isearch))))

(use-package consult
  :after dash
  :bind (("M-y"       . consult-yank-pop)
         ("C-x b"     . consult-buffer)
         ("C-x C-b"   . consult-buffer)
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

(use-package copilot
  :bind (("C-c /" . t0yv0/copilot-hydra/body)
         ("C-c n" . copilot-next-completion)
         ("C-c p" . copilot-previous-completion)
         ("C-c j" . copilot-accept-completion)
         ("C-c g" . copilot-clear-overlay))
  :custom (copilot-disable-predicates
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
                                   compilation-mode-hook
                                   minibuffer-mode-hook)))))
  :config
  (eval '(pretty-hydra-define
          t0yv0/copilot-hydra
          (:color amaranth :quit-key "C-g")
          ("Completions"
           (("/" copilot-complete "complete")
            ("g" copilot-clear-overlay "clear" :color blue)
            ("j" copilot-accept-completion "accept" :color blue))
           "Nav"
           (("n" copilot-next-completion "next")
            ("p" copilot-previous-completion "prev"))
           "Login"
           (("L" copilot-login "login" :color blue)
            ("Q" copilot-logout "logout" :color blue))
           "Copilot System"
           (("G" global-copilot-mode "global-mode" :color blue)
            ("X" (lambda () (interactive) (global-copilot-mode -1)) "global-mode-off" :color blue)
            ("D" copilot-diagnose "diagnose" :color blue))))))

(use-package corfu
  :init
  (global-corfu-mode)

  :bind
  (:map corfu-map
        ("C-j" . corfu-insert)))

(use-package diminish)

(use-package doom-modeline
  :after nerd-icons
  :init (doom-modeline-mode 1))

(use-package envrc
  :diminish envrc-mode
  :init (envrc-global-mode))

(use-package eglot
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
  :bind* (("C-c d" . t0yv0/diary)
          ("C-c q" . t0yv0/quit)
          ("C-c o" . org-capture)
          ("M-s d" . t0yv0/consult-ripgrep-current-directory)
          ("M-s p" . consult-ripgrep)
          ("C-h"   . t0yv0/backspace)
          ("C-c x" . execute-extended-command)
          ("M-`"   . other-frame)
          ("M-c"   . kill-ring-save)
          ("M-v"   . yank)
          ("M-x"   . kill-region)
          ("M-z"   . undo)
          ("<remap> <dabbrev-expand>" . hippie-expand))

  :hook
  (before-save . delete-trailing-whitespace)

  :config
  (require-theme 'modus-themes)
  (load-theme 'modus-operandi)
  (define-key isearch-mode-map (kbd "C-j") 'isearch-exit)
  (global-set-key (kbd "C-c h") 'help-command)

  :custom
  (display-buffer-alist
   '(("\\*Gofmt Error"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*Occur"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*Org Help"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*Embark Actions"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*Embark Collect"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*test"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*vterm"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("current-region.png"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*compilation"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*Org Links"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))
     ("\\*xref"
      (t0yv0/display-buffer-at-bottom (dedicated . t)))))

  (bookmark-default-file "~/my/bookmarks")
  (bookmark-save-flag 1)
  (column-number-mode t)
  (completion-cycle-threshold 3)
  (display-fill-column-indicator-mode t)
  (fill-column 100)
  (gc-cons-threshold 16777216)
  (gc-cons-percentage 0.2)
  (global-mark-ring-max 6)
  (global-so-long-mode 1)
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
  (mac-pass-command-to-system nil)
  (mac-pass-control-to-system nil)
  (ns-command-modifier 'meta)
  (ns-right-command-modifier 'super)
  (prefer-coding-system 'utf-8)
  (read-process-output-max 4194304)
  (scroll-bar-mode nil)
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

  (initial-frame-alist
   '((unsplittable t)))

  (default-frame-alist
   '((unsplittable . t)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (font . "Iosevka 13")
     (vertical-scroll-bars . nil)
     (horizontal-scroll-bars . nil))))

(use-package embark
  :bind
  ("C-." . embark-act)
  ("C-;" . embark-dwim)

  :bind
  ((:map embark-identifier-map
         ("x" . t0yv0/embark-execute-identifier))
   (:map embark-defun-map
         ("x" . t0yv0/embark-execute-defun)))

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

(use-package epa
  :config
  (advice-add 'epa-file-write-region
              :before (lambda (&rest args)
                        (setq-local epa-file-encrypt-to '("0x9E15CD89706EE947"))
                        (message "NOTE: bypassing recipient selector and encrypting for self"))))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package git-link
  :after major-mode-hydra
  :bind (("C-c l" . (lambda ()
                      (interactive)
                      (require 'git-link)
                      (t0yv0/link-hydra/body))))
  :config
  (eval '(pretty-hydra-define
          t0yv0/link-hydra
          (:color blue :quit-key "C-g")
          ("Git Links"
           (("g" git-link "git-link")
            ("c" git-link-commit "git-link-commit")
            ("h" git-link-homepage "git-link-homepage"))
           "Org Links"
           (("l" org-store-link "org-store-link")
            ("C-l" org-insert-link "org-insert-link [C-c C-l]"))))))

(use-package go-mode
  :bind (("C-c C-a" . go-import-add))
  :config
  (setq auto-mode-alist (rassq-delete-all 'go-mode auto-mode-alist)))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :config
  (require 'major-mode-hydra)
  (require 't0yv0-treesit)
  (t0yv0/ensure-tree-sitter-grammar-install)
  (eval '(major-mode-hydra-define
          go-ts-mode (:idle 0.5)
          ("Consult"
           (("e" consult-flymake "errors")
            ("E" (lambda () (interactive) (consult-flymake (project-current nil))) "project-errors")
            ("m" consult-imenu "imenu"))
           "Code"
           (("c" (lambda () (interactive) (compile "go build .")) "compile")
            ("t" (lambda () (interactive) (t0yv0/embark-execute-defun)) "test-defun")
            ("T" (lambda () (interactive) (compile "go test .")) "test-package"))
           "Find"
           (("im" eglot-find-implementation "impls")
            ("rs" xref-find-references "refs"))
           "Refactor"
           (("ia" go-import-add "add-import")
            ("re" eglot-rename "rename")
            ("in" eglot-code-action-inline "inline")
            ("xt" eglot-code-action-extract "extract"))
           "Narrow"
           (("n" narrow-to-defun "narrow-to-defun")
            ("w" widen "widen")))))
  :hook
  (go-ts-mode . eglot-ensure)
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
  :bind (("C-x g" . magit-status)))

(use-package major-mode-hydra
  :bind ("C-c SPC" . major-mode-hydra))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

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

(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)))

(use-package org
  :config
  (setq org-startup-indented t)
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
                             ("~/my/tickler.org" :maxlevel . 2)))
  (advice-add 'org-capture
              :before
              (lambda (&optional goto keys)
                (set-frame-parameter nil 'unsplittable nil)))
  (advice-add 'org-capture-goto-target
              :after
              (lambda (&optional template-key)
                (set-frame-parameter nil 'unsplittable t)))
  (advice-add 'org-capture-goto-last-stored
              :after
              (lambda ()
                (set-frame-parameter nil 'unsplittable t)))
  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (set-frame-parameter nil 'unsplittable t))))

(use-package org-roam
  :after (major-mode-hydra org)
  :custom
  (org-roam-directory (file-truename "~/workshare/org-roam/"))
  :bind (("C-c r" . t0yv0/org-roam-hydra/body))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)
  ;; Setup org-roam-dailies
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org.gpg"
                              "#+title: %<%Y-%m-%d>\n"))))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (eval '(pretty-hydra-define
          t0yv0/org-roam-hydra
          (:color blue :quit-key "C-g")
          ("Node"
           (("c" org-roam-capture "capture")
            ("f" org-roam-node-find "find")
            ("i" org-roam-node-insert "insert"))
           "Relations"
           (("b" org-roam-buffer-toggle "buffer-toggle"))
           "Dailies"
           (("t" org-roam-dailies-capture-today "capture-today")
            ("d" org-roam-dailies-goto-today "goto-today")
            ("D" org-roam-dailies-goto-date "goto-date"))))))

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

(use-package recentf
  :config
  (setq recentf-max-menu-items 15
        recentf-max-saved-items 100)
  :init
  (recentf-mode 1))

(use-package t0yv0-treesit
  :bind (("<remap> <forward-list>" . t0yv0/forward-list)
         ("<remap> <backward-list>" . t0yv0/backward-list)
         ("<remap> <kill-sexp>" . t0yv0/kill-sexp)
         ("<remap> <mark-sexp>" . t0yv0/mark-sexp)
         ("<remap> <forward-word>" . t0yv0/forward-word)
         ("<remap> <backward-word>" . t0yv0/backward-word)
         ("<remap> <forward-sexp>" . t0yv0/forward-sexp)
         ("<remap> <backward-sexp>" . t0yv0/backward-sexp)
         ("<remap> <backward-up-list>" . t0yv0/backward-up-list)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (require 't0yv0-treesit)
  (t0yv0/ensure-tree-sitter-grammar-install)
  :hook
  (typescript-mode . (lambda ()
                       (eglot-ensure)
                       (treesit-parser-create 'typescript))))

(use-package vertico
  :init
  (vertico-mode))

(use-package vterm
  :bind (("C-x p v" . t0yv0/vterm-proj)
         ("C-c z"   . t0yv0/vterm-repeat)
         ("C-c v"   . t0yv0/vterm-hydra/body))
  :bind (:map vterm-mode-map
              ("M-c" . kill-ring-save)
              ("M-v" . yank)
              ("M-z" . undo)
              ("M-w" . kill-ring-save)
              ("M-/" . #'t0yv0/vterm-dabbrev-expand))
  :init (defhydra t0yv0/vterm-hydra (:color blue :idle 1.0)
          "vterms"
          ("v" t0yv0/vterm "vterm")
          ("p" t0yv0/vterm-proj "vterm-proj")
          ("d" t0yv0/vterm-dir "vterm-dir")
          ("r" t0yv0/vterm-repeat "vterm-repeat")))

(use-package windmove
  :bind (("C-c w" . t0yv0/windmove-hydra/body))
  :init
  (defhydra t0yv0/windmove-hydra (:hint nil)
    "windmove: use fbnp to navigate, FBNP to swap\n"
    ("C-j" nil "select")
    ("0" delete-window "delete")
    ("1" delete-other-windows "delete-other")
    ("2" split-window-vertically "v-split")
    ("3" split-window-horizontally "h-split")
    ("B" windmove-swap-states-left)
    ("F" windmove-swap-states-right)
    ("P" windmove-swap-states-up)
    ("N" windmove-swap-states-down)
    ("b" windmove-left)
    ("f" windmove-right)
    ("p" windmove-up)
    ("n" windmove-down)))

(use-package wgrep
  :config
  (require 'major-mode-hydra)
  (eval '(major-mode-hydra-define
          grep-mode nil
          ("wgrep"
           (("p" wgrep-change-to-wgrep-mode "wgrep-change-to-wgrep-mode [C-c C-p]")
            ("s" wgrep-save-all-buffers "wgrep-save-all-buffers"))))))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (list "~/my/snippets" t0yv0/yas-snippets))
  (yas-reload-all)
  (yas-global-mode))

(provide 'default)
;;; default.el ends here
