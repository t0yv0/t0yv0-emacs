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
(tab-bar-mode 1)
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
      '((".go$"
         (display-buffer-reuse-window
          t0yv0/display-buffer-same-go-window
          display-buffer-reuse-mode-window)
         (inhibit-same-window . nil))
        ("\\*vterm"
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

(global-set-key (kbd "C-c 2") 't0yv0/open-shell-for-current-buffer)
(global-set-key (kbd "C-c h") 'vterm)
(global-set-key (kbd "C-c m") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-c n") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-c x") 'delete-frame)
(global-set-key (kbd "C-c z") 't0yv0/clear-and-repeat-last-command-in-project-shell)
(global-set-key (kbd "C-c o") 'org-capture)
(global-set-key (kbd "C-c ?") 't0yv0/cheatsheet)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "RET") 'newline-and-indent)


;;; package configuration

(use-package consult
  :bind (("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
         ("C-x B" . consult-project-buffer)
         ("C-h a" . consult-apropos)))

(use-package csharp-mode)

(use-package edit-indirect)

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (("C-c e" . flycheck-next-error)))

(use-package go-mode
  :init (progn
	  (add-hook 'before-save-hook 'gofmt-before-save)
	  (add-hook 'go-mode-hook 'lsp-deferred)))

(use-package hydra
  :bind (("C-c s" . t0yv0/search-hydra/body)
         ("C-c f" . t0yv0/find-hydra/body)
         ("C-c t" . t0yv0/tab-hydra/body)
         ("C-c w" . t0yv0/windmove-hydra/body)
         ("C-x r" . t0yv0/register-hydra/body)
         ("C-c l" . t0yv0/link-hydra/body)
         ("C-c c" . t0yv0/compile-hydra/body))
  :init
  (progn
    (defhydra t0yv0/link-hydra (:color blue :hint nil)
      "links"
      ("g" t0yv0/kill-github-link-at-point "gh-kill")
      ("G" t0yv0/github-link-at-point-to-register "gh-register")
      ("o" t0yv0/kill-org-link "org-kill")
      ("O" t0yv0/org-link-to-register "org-register"))

    (defhydra t0yv0/tab-hydra (:hint nil)
      "tabs"
      ("<return>" nil "select" :color blue)
      ("n" t0yv0/open-tab "new" :color blue)
      ("r" tab-bar-rename-tab "rename" :color blue)
      ("," tab-recent "recent" :color blue)
      ("<left>" tab-previous "left")
      ("<right>" tab-next "right")
      ("q" tab-close "close"))

    ;; this needs consult-grep, consult-ripgrep, consult-git-grep
    (defhydra t0yv0/search-hydra (:color blue :hint nil)
      "search"
      ("l" consult-line "line")
      ("m" consult-line-multi "line-multi")
      ("d" t0yv0/consult-ripgrep-current-directory "dir")
      ("p" consult-ripgrep "project")
      ("g" consult-git-grep "git"))

    (defhydra t0yv0/windmove-hydra (:hint nil)
      "windmove: use arrow keys to nav, add shift to swap\n"
      ("q" nil "quit")
      ("d" delete-window "delete")
      ("_" split-window-vertically "v-split")
      ("|" split-window-horizontally "h-split")
      ("S-<left>" windmove-swap-states-left)
      ("S-<right>" windmove-swap-states-right)
      ("S-<up>" windmove-swap-states-up)
      ("S-<down>" windmove-swap-states-down)
      ("<left>" windmove-left)
      ("<right>" windmove-right)
      ("<up>" windmove-up)
      ("<down>" windmove-down))

    (defhydra t0yv0/register-hydra (:color blue :hint nil)
      "register"
      ("SPC" point-to-register "point")
      ("j" t0yv0/jump-to-register "jump")
      ("s" copy-to-register "copy")
      ("i" insert-register "ins")
      ("r" copy-rectangle-to-register "copy-rect")
      ("w" window-configuration-to-register "win")
      ("+" increment-register "increment"))

    (defhydra t0yv0/find-hydra (:color blue :hint nil)
      "find-things"
      ("d" t0yv0/diary "diary")
      ("f" consult-find "file")
      ("i" consult-imenu "imenu")
      ("I" consult-imenu-multi "imenu*")
      ("k" consult-yank-from-kill-ring "kill")
      ("m" consult-mark "mark")
      ("M" consult-global-mark "mark*")
      ("r" consult-register "register"))

    (defhydra t0yv0/compile-hydra (:color blue)
      "compilation"
      ("c" compile "compile")
      ("m" t0yv0/mermaid-compile "mermaid"))))

(use-package lsp-mode
  :defer t
  :hook (lsp-mode . lsp-lens-mode)
  :init (setq lsp-prefer-flymake nil)
  :config (define-key lsp-mode-map (kbd "C-c i") lsp-command-map))

(use-package lsp-ui :commands lsp-ui-mode)

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
                                 '((shell . t)))
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

(use-package projectile
  :init
  (progn
    (projectile-mode +1))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

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
  (setq yas-snippet-dirs '("~/my/snippets"))
  (yas-reload-all)
  (yas-global-mode))

(provide 'default)
;;; default.el ends here
