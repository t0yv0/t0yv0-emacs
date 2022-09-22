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
(windmove-default-keybindings)

(let ((font "monospace 16"))
  (set-frame-font font)

  (setq default-frame-alist
	`((menu-bar-lines . 0)
	  (tool-bar-lines . 0)
	  (font . ,font)
	  (vertical-scroll-bars . nil)
	  (horizontal-scroll-bars . nil))))

(setq column-number-mode t)
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
      '(("\\*vterm" (display-buffer-reuse-mode-window
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
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c h") 'vterm)
(global-set-key (kbd "C-c l") 't0yv0/store-link)
(global-set-key (kbd "C-c m") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-c n") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c x") 'delete-frame)
(global-set-key (kbd "C-c z") 't0yv0/clear-and-repeat-last-command-in-project-shell)
(global-set-key (kbd "C-c o") 'org-capture)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "RET") 'newline-and-indent)


;;; package configuration

(use-package csharp-mode)

(use-package helm
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)))

(use-package helm-ls-git)

(use-package helm-ag)

(use-package hydra
  :bind (("C-c s" . t0yv0/search-hydra/body)
         ("C-c f" . t0yv0/find-hydra/body)
         ("C-c t" . t0yv0/tab-hydra/body))
  :init
  (progn
    (defhydra t0yv0/tab-hydra ()
      "tabs"
      ("t" nil                "select" :color blue)
      ("n" t0yv0/open-tab     "new"    :color blue)
      ("r" tab-bar-rename-tab "rename" :color blue)
      ("," tab-recent         "recent" :color blue)
      ("d" t0yv0/diary        "diary"  :color blue)
      ("[" tab-previous       "left")
      ("]" tab-next           "right")
      ("q" tab-close          "close"))

    (defhydra t0yv0/search-hydra ()
      "search"
      ("o" helm-occur           "occur"   :color blue)
      ("b" helm-ag-buffers      "buffers" :color blue)
      ("d" helm-ag              "dir"     :color blue)
      ("p" helm-ag-project-root "project" :color blue))

    (defhydra t0yv0/find-hydra ()
      "find-files"
      ("p" helm-ls-git  "project" :color blue)
      ("r" helm-recentf "recent"  :color blue))))


(use-package magit
  :bind (("C-x g" . magit-status)))


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
    (projectile-mode +1)
    (setq projectile-completion-system 'helm))
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package go-mode
  :init (progn
	  (add-hook 'before-save-hook 'gofmt-before-save)
	  (add-hook 'go-mode-hook 'lsp-deferred)))

(use-package lsp-mode
  :hook  (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (("C-c e" . flycheck-next-error)))

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

    ;; Make windmove work in Org mode:
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)

    (define-key org-mode-map (kbd "<S-left>") nil)
    (define-key org-mode-map (kbd "<S-right>") nil)
    (define-key org-mode-map (kbd "<C-S-left>") 'org-shiftleft)
    (define-key org-mode-map (kbd "<C-S-right>") 'org-shiftright)

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

(use-package typescript-mode)

(use-package tide
  :config (add-hook 'typescript-mode-hook #'(lambda ()
					      (company-mode +1)
					      (tide-setup))))

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
