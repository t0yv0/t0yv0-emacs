;;; default --- Emacs setup
;;;
;;; Commentary:
;;; - nothing too interesting here
;;;
;;; Code:

(require 'hydra)
(require 't0yv0-basics)
(require 'use-package)

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package avy
  :bind (:map isearch-mode-map
              (("C-M-s" . avy-isearch))))

(use-package consult
  :after dash
  :bind (("M-y"       . consult-yank-pop)
         ("C-x b"     . consult-buffer)
         ("C-x C-b"   . consult-buffer)
         ("M-s d"     . t0yv0/consult-ripgrep-current-directory)
         ("M-s p"     . consult-ripgrep)
         ("M-g g"     . consult-goto-line)
         ("M-g M-g"   . consult-goto-line)
         ("M-g i"     . consult-imenu)
         ("M-g C-SPC" . consult-mark)
         ("M-g l"     . consult-line)
         ("M-g d"     . t0yv0/consult-changed-line)
         ("C-x p b"   . consult-project-buffer)
         ("C-x p g"   . consult-ripgrep)
         ("C-x r b"   . consult-bookmark)
         ("C-x r s"   . consult-register))
  :config
  (defun t0yv0/consult--multi--around (orig-fun &rest args)
    "Disable display-buffer custom placement when doing consult"
    (let ((c nil) (r nil))
      (setq c display-buffer-alist)
      (setq display-buffer-alist nil)
      (unwind-protect
          (apply orig-fun args)
        (setq display-buffer-alist c))))
  (advice-add 'consult--multi :around #'t0yv0/consult--multi--around)
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
  :bind (("C-c p" . t0yv0/copilot-hydra/body)
         ("C-c j" . copilot-accept-completion))
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
  (copilot-diagnose)
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
        ("C-SPC" . corfu-insert-separator)
        ("C-j" . corfu-insert)))

(use-package dape
  :bind (("C-c d" . t0yv0/dape-hydra/body))
  :config
  (dape-breakpoint-global-mode)

  (require 'major-mode-hydra)

  (eval '(pretty-hydra-define
          t0yv0/dape-hydra
          (:color blue :quit-key "C-g")
          ("Debug"
           (("n" dape-next     "next"     :color red)
            ("c" dape-continue "continue" :color red)
            ("o" dape-step-out "step-out" :color red)
            ("i" dape-step-in  "step-in"  :color red))
           "Breakpoint"
           (("b"           dape-breakpoint-toggle     "toggle"       :color red)
            ("<backspace>" dape-breakpoint-remove-all "remove-all"   :color red)
            ("e"           dape-breakpoint-expression "expression"   :color red)
            ("l"           dape-breakpoint-log        "log"          :color red))
           "Stack"
           (("<" dape-stack-select-up       "select-up"    :color red)
            (">" dape-stack-select-down     "select-down"  :color red)
            ("S" dape-select-stack          "select-stack" :color red))
           "Session"
           (("d" t0yv0/go-debug-current-test "debug-defun" :color red)
            ("r" dape-restart         "restart"         :color red)
            ("p" dape-pause           "pause"           :color red)
            ("q" dape-quit            "quit"            :color blue)
            ("D" dape-disconnect-quit "disconnect-quit" :color red))
           "Advanced"
           (("x" dape-evaluate-expression "evaluate-expression" :color red)
            ("w" dape-watch-dwim          "watch-dwim"          :color red)
            ("m" dape-read-memory         "read-memory"         :color red)
            ("t" dape-select-thread       "select-thread"       :color red))))))

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
  :bind* (("C-c q" . t0yv0/quit)
          ("M-`"   . other-frame)

          ("<remap> <dabbrev-expand>" . hippie-expand))

  :hook
  (before-save . delete-trailing-whitespace)

  :config
  (require-theme 'modus-themes)
  (load-theme 'modus-operandi)
  (define-key isearch-mode-map (kbd "C-j") 'isearch-exit)
  (global-hl-line-mode 1)

  :custom
  (display-buffer-alist
   `(((or "\\*Org Links"
          "\\*Org Select"
          "\\*Gofmt"
          "\\*Occur"
          "\\*Embark"
          "\\*test"
          "current-region.png"
          "\\*compilation"
          "\\*xref"
          "\\*Warnings")
      (display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-in-direction)
      (window . root)
      (window-height . 0.2356)
      (direction . bottom))

     ("\\*vterm"
      (display-buffer-reuse-window
       display-buffer-reuse-mode-window
       display-buffer-in-direction)
      (mode vterm-mode vterm-copy-mode)
      (window . root)
      (window-height . 0.2356)
      (direction . bottom))

     ((or (derived-mode . magit-mode)
          (derived-mode . org-mode)
          (mode . text-mode)
          (mode . go-dot-mod-mode))
      (display-buffer-reuse-mode-window
       display-buffer-in-direction)
      (mode magit-mode org-mode text-mode go-dot-mod-mode)
      (window . root)
      (window-width . 0.38)
      (direction . right))

     ((derived-mode . prog-mode)
      (display-buffer-reuse-window
       t0yv0/display-buffer-same-prog-mode-window
       display-buffer-reuse-mode-window
       display-buffer-pop-up-window)
      (mode prog-mode))))

  (bookmark-default-file "~/my/bookmarks")
  (bookmark-save-flag 1)
  (column-number-mode t)
  (completion-cycle-threshold 3)
  (display-fill-column-indicator-mode t)
  (fill-column 120)
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

(use-package forge
  :after magit
  :config
  (require 'auth-source)
  (auth-source-pass-enable))

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package gptel
  :bind (("C-c g" . t0yv0/gptel-hydra/body))
  :config
  (setq
   gptel-model "llama2:latest"
   gptel-backend (gptel-make-ollama "llama2"
                                    :host "localhost:11434"
                                    :stream t
                                    :models '("llama2:latest")))
  (eval '(pretty-hydra-define
          t0yv0/gptel-hydra
          (:color blue :quit-key "C-g")
          ("GPT"
           (("g" gptel-send "send")
            ("b" gptel "create-buffer")
            ("m" gptel-menu "menu"))
           "Org"
           (("t" gptel-set-topic "set-topic [org-mode]"))))))

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
           (("g" t0yv0/git-link "git-link")
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
  :bind (("C-c t" . t0yv0/testrun-hydra/body))
  :config
  (t0yv0/ensure-tree-sitter-grammar-install)
  (require 'major-mode-hydra)
  (require 'testrun)

  (eval '(pretty-hydra-define
          t0yv0/testrun-hydra
          (:color blue :quit-key "C-g" :idle 0.5)
          ("Test"
           (("SPC" testrun-at-point "at-point")
            ("t" recompile "retest")))))

  (eval '(major-mode-hydra-define
          go-ts-mode (:idle 0.5)
          ("Consult"
           (("e" consult-flymake "errors")
            ("E" (lambda () (interactive) (consult-flymake (project-current nil))) "project-errors")
            ("m" consult-imenu "imenu"))
           "Code"
           (("c" (lambda () (interactive) (compile "go build .")) "compile")
            ("t" (lambda () (interactive) (t0yv0/embark-execute-defun)) "test-defun")
            ("T" (lambda () (interactive) (compile "go test .")) "test-package")
            ("d" t0yv0/dape-hydra/body "debug"))
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

  (setq compilation-error-regexp-alist-alist
        (remove 'go-panic (remove 'go-test compilation-error-regexp-alist-alist)))

  (add-to-list
   'compilation-error-regexp-alist-alist
   (cons 'go-panic (list (rx (seq bol "\t") (group (* (not ":"))) (any ":") (group (+ digit))) 1 2)))

  :hook
  (go-ts-mode . (lambda ()
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
  (setq magit-display-buffer-function #'display-buffer)
  :bind (("C-x g" . magit-status)))

(use-package major-mode-hydra
  :bind ("C-c SPC" . major-mode-hydra))

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

(use-package multiple-cursors
  :bind (("C-c m" . mc/edit-lines)))

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (require 'major-mode-hydra)
  (eval '(major-mode-hydra-define
           org-mode (:idle 0.5)
           ("Walk"
            (("p" org-previous-visible-heading    "[C-c C-p] prev"     :color red)
             ("n" org-next-visible-heading        "[C-c C-n] next"     :color red)
             ("f" org-forward-heading-same-level  "[C-c C-f] forward"  :color red)
             ("b" org-backward-heading-same-level "[C-c C-b] backward" :color red)
             ("u" outline-up-heading              "[C-c C-u] up"       :color red))
            "Jump"
            (("j" consult-org-heading "consult-heading" :color blue))
            "Narrow"
            (("N" org-narrow-to-subtree "narrow" :color blue)
             ("W" widen :color blue))
            "Special"
            (("/" org-sparse-tree "sparse-tree [C-c /]" :color blue)
             ("x" org-columns "columns [C-c C-x C-c]" :color blue)))))
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
  (setq org-refile-targets '(("~/workshare/org/gtd.org.gpg" :maxlevel . 3)
                             ("~/workshare/org/someday.org.gpg" :level . 1)
                             ("~/workshare/org/tickler.org.gpg" :maxlevel . 2))))

(use-package org-present)

(use-package org-roam
  :bind (("C-c r" . t0yv0/org-roam-hydra/body))
  :custom
  (org-roam-directory (file-truename "~/workshare/org-roam/"))
  :config
  (require 'org)
  (require 'major-mode-hydra)
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

(use-package recentf
  :config
  (setq recentf-max-menu-items 15
        recentf-max-saved-items 100)
  :init
  (recentf-mode 1))

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
          ("r" t0yv0/vterm-repeat "vterm-repeat"))
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               (setq-local global-hl-line-mode nil))))

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
