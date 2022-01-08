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
(setq tab-width 4)
(setq visible-bell t)


;;; faces

(custom-set-faces
 '(region ((t (:extend t :background "khaki")))))


;;; hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; key bindings

(global-set-key (kbd "C-c 2") 't0yv0/open-shell-for-current-buffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c h") 'shell)
(global-set-key (kbd "C-c l") "λ")
(global-set-key (kbd "C-c m") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-c n") 'kmacro-end-or-call-macro)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c x") 'delete-frame)
(global-set-key (kbd "C-c z") 't0yv0/clear-and-repeat-last-command-in-project-shell)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "RET") 'newline-and-indent)


;;; side windows

(let ((bottom-pane
       '(display-buffer-in-side-window
	 (side . bottom)
	 (slot . 0)
	 (window-height . 0.382)     ;; frac of total
	 (preserve-size . (t . nil)) ;; preserve width
	 ))
      (right-pane
       '(display-buffer-in-side-window
	 (side . right)
	 (slot . 0)
	 (window-width . 0.382)      ;; frac of total
	 (preserve-size . (nil . t)) ;; preserve height
	 )))
  (setq
   display-buffer-alist
   `(("^\\*shell" ,@bottom-pane)
     ("^magit"    ,@right-pane)
     ("^\\*helm"  ,@bottom-pane))))


;;; package configuration

(use-package csharp-mode)

(use-package helm
  :bind (("C-x C-f" . helm-find-files)
	 ("C-x b"   . helm-buffers-list)))

(use-package helm-ag
  :bind (("M-x"   . helm-M-x)
	 ("C-c b" . helm-ag-buffers)
	 ("C-c d" . helm-ag)
	 ("C-c f" . helm-ag-this-file)
	 ("C-c j" . helm-ag-project-root)))

(use-package helm-ls-git
  :bind (("C-c g" . helm-ls-git-ls)))

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

(use-package typescript-mode)

(use-package tide
  :config (add-hook 'typescript-mode-hook #'(lambda ()
					      (company-mode +1)
					      (tide-setup))))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/my/snippets"))
  (yas-reload-all)
  (yas-global-mode))


(provide 'default)

;;; default ends here
