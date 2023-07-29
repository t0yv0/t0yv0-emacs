;;; default --- Emacs setup
;;;
;;; Commentary:
;;; - nothing too interesting here
;;;
;;; Code:

(require 'use-package)
(require 't0yv0-ware)


;;; settings

(global-so-long-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(prefer-coding-system 'utf-8)
(winner-mode t)

(let ((font "Iosevka 16"))
  (set-frame-font font)

  (setq default-frame-alist
	`((menu-bar-lines . 0)
	  (tool-bar-lines . 0)
	  (font . ,font)
	  (vertical-scroll-bars . nil)
	  (horizontal-scroll-bars . nil))))

(setq bookmark-default-file "~/my/bookmarks")
(setq bookmark-save-flag 1)
(setq column-number-mode t)
(setq-default fill-column 120)
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "")
(setq major-mode 'text-mode)
(setq make-backup-files nil)
(setq sentence-end-double-space nil)
(setq suggest-key-bindings t)
(setq switch-to-buffer-obey-display-actions t)
(setq tab-width 4)
(setq visible-bell t)

(setq display-buffer-alist
      '(("\\*vterm"
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


;;; faces

(custom-set-faces
 '(region ((t (:extend t :background "khaki")))))


;;; hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; key bindings

(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "C-c d") 't0yv0/diary)
(global-set-key (kbd "C-c z") 't0yv0/vterm-repeat)
(global-set-key (kbd "C-c o") 'org-capture)
(global-set-key (kbd "C-c ?") 't0yv0/cheatsheet)
(global-set-key (kbd "C-c ,") 't0yv0/window-buffer-back)
(global-set-key (kbd "C-c .") 't0yv0/window-buffer-forward)

(global-set-key (kbd "RET") 'newline-and-indent)


;;; package configuration

(use-package company)

(use-package consult
  :after dash
  :bind (("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer))
  :config
  (setq consult-buffer-sources (t0yv0/consult-buffer-sources consult-buffer-sources))
  (setq consult-project-buffer-sources (t0yv0/consult-project-buffer-sources consult-project-buffer-sources)))


(use-package consult-flycheck)

(use-package copilot)

(use-package csharp-mode)

(use-package dash)

(use-package dap-mode
  :init (require 'dap-dlv-go))

(use-package edit-indirect)

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

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (("C-c e" . flycheck-next-error)))

(use-package go-mode
  :init (progn
	  (add-hook 'before-save-hook 'gofmt-before-save)
	  (add-hook 'go-mode-hook 'lsp-deferred)))

(use-package haskell-mode)

(use-package hydra
  :init

  (defhydra t0yv0/tab-hydra (:hint nil)
    "
^tabs^            ^nav^          ^edit^         ^other-tab
^^^^^^^^------------------------------------------------------------------
_0_: close        _O_:   prev    _m_: move      _t_:   prefix
_1_: close-other  _o_:   next    _M_: move-to   _b_:   buffer
_2_: new          _C-j_: done    _r_: rename    _f_:   find-file
_n_: duplicate    _RET_: switch  _u_: undo      _d_:   dired
_N_: new-to       ^ ^            _G_: group     _p_:   project-command
^ ^               ^ ^            ^ ^            _C-r_: find-file-read-only
"
    ("0" tab-close)
    ("1" tab-close-other :color blue)
    ("2" tab-new :color blue)
    ("n" tab-duplicate :color blue)
    ("N" tab-new-to :color blue)

    ("O" tab-previous)
    ("o" tab-next)
    ("C-j" nil)
    ("RET" tab-switch :color blue)

    ("m" tab-move)
    ("M" tab-move-to)
    ("r" tab-rename)
    ("u" tab-undo)
    ("G" tab-group)

    ("t" other-tab-prefix :color blue)
    ("b" switch-to-buffer-other-tab :color blue)
    ("f" find-file-other-tab :color blue)
    ("d" dired-other-tab :color blue)
    ("C-r" find-file-read-only-other-tab :color blue)
    ("p" project-other-tab-command :color blue))

  (defhydra t0yv0/frame-hydra (:hint nil)
    "
frame ^ ^    _O_: prev  _2_: make    _0_: delete        _5_: prefix
_C-j_: done  _o_: next  _r_: rename  _1_: delete-other  _m_: toggle-maximized"
    ("0" delete-frame)
    ("1" delete-other-frames :color blue)
    ("2" make-frame-command :color blue)
    ("O" (lambda () (interactive) (other-frame -1)))
    ("o" other-frame)
    ("C-j" nil)
    ("5" other-frame-prefix :color blue)
    ("r" set-frame-name)
    ("m" toggle-frame-maximized))

  (defhydra t0yv0/goto-hydra (:color blue :hint nil)
    "goto"
    ("l" consult-line "line")
    ("c" t0yv0/consult-changed-line "changed-line")
    ("m" consult-mark "mark")
    ("i" consult-imenu "imenu")
    ("e" consult-flycheck "flycheck-error"))

  (defhydra t0yv0/project-hydra (:hint nil)
    "
project ^ ^    _b_: buffer     _!_: shell-command        _k_: kill-buffers
_C-j_: select  _f_: find-file  _&_: async-shell-command  _r_: replace
_p_:   switch  _d_: find-dir   _c_: compile              _g_: ripgrep
^ ^            _D_: dired      _v_: vterm"
    ("b" consult-project-buffer :color blue)
    ("f" project-find-file :color blue)
    ("d" project-find-dir :color blue)
    ("D" project-dired :color blue)
    ("!" project-shell-command :color blue)
    ("&" project-async-shell-command :color blue)
    ("c" project-compile :color blue)
    ("p" project-switch-project :color blue)
    ("v" t0yv0/vterm-proj :color blue)
    ("k" project-kill-buffers :color blue)
    ("r" project-query-replace-regexp :color blue)
    ("g" consult-ripgrep :color blue)
    ("C-j" nil))

  (defhydra t0yv0/link-hydra (:color blue :hint nil)
    "links"
    ("g" t0yv0/kill-github-link-at-point "gh-kill")
    ("G" t0yv0/github-link-at-point-to-register "gh-register")
    ("o" t0yv0/kill-org-link "org-kill")
    ("O" t0yv0/org-link-to-register "org-register"))

  (defhydra t0yv0/search-hydra (:color blue :hint nil)
    "search"
    ("d" t0yv0/consult-ripgrep-current-directory "dir")
    ("p" consult-ripgrep "project")
    ("g" consult-git-grep "git")
    ("o" occur "occur"))

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

  (defhydra t0yv0/register-hydra (:color blue :hint nil)
    "register"
    ("SPC" point-to-register "point")
    ("c" consult-register "consult")
    ("j" t0yv0/jump-to-register "jump")
    ("s" copy-to-register "copy")
    ("i" insert-register "ins")
    ("r" copy-rectangle-to-register "copy-rect")
    ("w" window-configuration-to-register "win")
    ("+" increment-register "increment")
    ("m" bookmark-set "bookmark-set"))

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

  :bind (("C-c s" . t0yv0/search-hydra/body)
         ("C-c w" . t0yv0/windmove-hydra/body)
         ("C-c l" . t0yv0/link-hydra/body)
         ("C-c c" . t0yv0/compile-hydra/body)
         ("C-c v" . t0yv0/vterm-hydra/body)
         ("C-c g" . t0yv0/goto-hydra/body)
         ("C-c p" . t0yv0/project-hydra/body)
         ("C-c r" . t0yv0/register-hydra/body)
         ("C-c 5" . t0yv0/frame-hydra/body)
         ("C-c t" . t0yv0/tab-hydra/body)
         ("C-c /" . t0yv0/copilot-hydra/body)))

(use-package json-mode)

(use-package lsp-mode
  :defer t
  :hook (lsp-mode . lsp-lens-mode)
  :init (setq lsp-prefer-flymake nil)
  :config (define-key lsp-mode-map (kbd "C-c i") lsp-command-map))

(use-package lsp-ui :commands lsp-ui-mode)

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
	    #'enable-paredit-mode))

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
  :bind (:map vterm-mode-map
              ("M-/" . #'t0yv0/vterm-dabbrev-expand)))

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
     (consult-ripgrep "Ripgrep" ?g)
     (project-find-dir "Dir" nil)
     (t0yv0/vterm-proj "VTerm" ?v)
     (magit-project-status "Magit" nil))))


(provide 'default)
;;; default.el ends here
