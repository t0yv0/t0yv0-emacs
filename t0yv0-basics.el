;;; t0yv0-basics.el --- helper code that is too small to have its own package  -*- lexical-binding:t -*-

;;; Commentary:

;; Utilities implemented here include:
;;
;;   1) vterm helpers and dabbrev support
;;   2) quick jumping to files and lines that are changed according to git
;;   3) go debugging and testing helpers
;;   4) orderless selection styles
;;   5) consult helpers for searching files and directories
;;   6) embark support for github references

;;; Code:


(require 'consult)
(require 'dabbrev)
(require 'dash)
(require 'project)
(require 'vterm)


(defvar dape-configs)
(defvar dape-history)


(declare-function dape "dape" (arg))
(declare-function dape--config-to-string "dape" (arg1 arg2))
(declare-function git-link "git-link" ())


(defvar-local t0yv0/vterm-dabbrev-state nil)


(defvar t0yv0/advice-around-delete-other-windows-state nil)


(defun t0yv0/advice-around-delete-other-windows (orig-fun &rest args)
  (if (and (frame-root-window-p (selected-window))
           (window-configuration-p t0yv0/advice-around-delete-other-windows-state))
      (set-window-configuration t0yv0/advice-around-delete-other-windows-state)
    (let ((ignore-window-parameters t)) ;; make it work even for side windows
      (setq t0yv0/advice-around-delete-other-windows-state (current-window-configuration))
      (apply orig-fun args))))


(defun t0yv0/consult-changed-line ()
  "Act like `consult-line' but only for lines changed according to git diff."
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer)))
         (diff (shell-command-to-string (format "git diff %s" fn)))
         (candidates (-map (lambda (pair) (t0yv0/consult-reformat-line-candidate pair))
                           (t0yv0/parse-git-diff (split-string diff "\n"))))
         (curr-line (line-number-at-pos (point))))
    (consult--read
     candidates
     :prompt "Go to changed line: "
     :annotate (consult--line-prefix curr-line)
     :category 'consult-location
     :sort nil
     :require-match t
     ;; Always add last `isearch-string' to future history
     :add-history (list (thing-at-point 'symbol) isearch-string)
     :history '(:input consult--line-history)
     :lookup #'consult--line-match
     :default (car candidates)
     ;; Add `isearch-string' as initial input if starting from Isearch
     :initial (and isearch-mode (prog1 isearch-string (isearch-done)))
     :state (consult--location-state candidates))))


(defun t0yv0/consult-flymake-project-errors ()
  "Consult all errors in the project."
  (interactive)
  (consult-flymake (project-current nil)))


(defun t0yv0/consult-reformat-line-candidate (pair)
  "Format a PAIR of line-num and line-text into a form suitable for `consult-line'."
  (let ((line-num (car pair))
        (line-text (cdr pair)))
    (consult--location-candidate line-text
                                 (cons (current-buffer) (t0yv0/point-at-line line-num))
                                 line-num
                                 line-num)))


(defun t0yv0/consult-ripgrep-current-directory ()
  "Search only current directory."
  (interactive)
  (consult-ripgrep default-directory))


(defvar t0yv0/consult-source-buffer-no-sorting
  `(:name     "Buffer"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :default  t
    :items
    ,(lambda () (consult--buffer-query :sort nil
                                       :as #'consult--buffer-pair)))
  "Buffer candidate source for `consult-buffer'.")


(defvar t0yv0/consult-source-git-status-file
  (list
   :name "Changed File"
   :narrow ?d
   :category 'file
   :face 'consult-file
   :history 'file-name-history
   :state 'consult--file-state
   :new 'consult--file-action
   :enabled (lambda () (not (null (project-current nil))))
   :items (lambda ()
            (let* ((cur-proj (project-current nil))
                   (default-directory (project-root cur-proj))
                   (changed-files
                    (-map (lambda (p) (string-join (list default-directory (substring p 3)) ""))
                          (-filter (lambda (p) (not (equal p "")))
                                   (split-string
                                    (shell-command-to-string "git status --short")
                                    "\n")))))
              changed-files)))
  "Changed file candidate source for `consult-buffer' powered by `git status`.")


(defvar t0yv0/consult-source-vterms
  `(:name     "Vterm"
    :narrow   ?v
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :items
    ,(lambda () (consult--buffer-query :sort nil
                                       :filter nil
                                       :mode 'vterm-mode
                                       :as #'consult--buffer-pair)))
  "Buffer candidate source for `consult-buffer' looking at all vterm-mode buffers.")


(defun t0yv0/embark-target-gh-ref ()
  "Target a link at point of the GitHub ref form like pulumi/pulumi#12117."
  (save-excursion
    (let* ((beg (progn (skip-chars-backward "[:alnum:]/#") (point)))
           (end (progn (skip-chars-forward "[:alnum:]/#") (point)))
           (str (buffer-substring-no-properties beg end))
           (rx (let* ((word "[[:alnum:]_]+")
                      (cap (lambda (x) (concat "\\(" x "\\)")))
                      (cap-word (funcall cap word)))
                 (concat cap-word "[/]" cap-word "[#]" cap-word))))
      (save-match-data
        (when (string-match rx str)
          `(url
            ,(format "https://github.com/%s/%s/issues/%s"
                     (match-string 1 str)
                     (match-string 2 str)
                     (match-string 3 str))
            ,beg . ,end))))))


(defun t0yv0/ensure-tree-sitter-grammar-install ()
  "Make sure ~/.emacs.d/tree-sitter symlink exists.
Ensures it is up-to-date with ./tree-sitter."
  (interactive)
  (let* ((real-dir (symbol-value 't0yv0/treesitter-dir))
         (target-dir (concat user-emacs-directory "tree-sitter")))
    (unless (equal (file-symlink-p target-dir) real-dir)
      (message (format "removing %s to reset treesit grammars" target-dir))
      (delete-file target-dir))
    (message (format "relinking %s to reset treesit grammars" target-dir))
    (make-symbolic-link real-dir target-dir t)))


(defun t0yv0/git-link (&optional arg)
  "Act like `git-link' but supports Go module cache.
Optional argument ARG is passed to `git-link'."
  (interactive "P")
  (if (t0yv0/git-link-file-name-to-github-remote (buffer-file-name))
      (t0yv0/git-link-go)
    (require 'git-link)
    (call-interactively #'git-link arg)))


(defun t0yv0/git-link-go ()
  "Infer a git link from current buffer file name in go mod cache."
  (interactive)
  (let ((url (t0yv0/git-link-file-name-to-github-remote (buffer-file-name))))
    (if url
        (progn
          (let ((link (format "%s#L%s" url (line-number-at-pos (point)))))
            (kill-new link)
            ;; prevent URL escapes from being interpreted as format strings
            (message (replace-regexp-in-string "%" "%%" link t t))
            (setq deactivate-mark t))))))


(defun t0yv0/git-link-file-name-to-github-remote (file-name)
  "Parse go mod cache FILE-NAME and reformat as git link."
  (let ((s file-name))
    (when (string-match
           (rx "go/pkg/mod/github.com/"
               (group (+ (not "/"))) "/"
               (group (+ (not (any "/@"))))
               "@"
               (group (+ (not "/")))
               "/"
               (group (+ any)))
           s)
      (let ((gh-org (match-string 1 s))
            (gh-repo (match-string 2 s))
            (gh-tag (match-string 3 s))
            (gh-rest (match-string 4 s)))
        (format "https://github.com/%s/%s/blob/%s/%s" gh-org gh-repo gh-tag gh-rest)))))


(defun t0yv0/go-defun-name (&optional defun-body)
  "Extract go func name from DEFUN-BODY source.

If DEFUN-BODY is not given, grab one from around point."
  (let* ((source (or defun-body (save-excursion
                                  (mark-defun)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end)))))
         (name (t0yv0/go-defun-name-from-body source)))
    (pop-mark)
    name))


(defun t0yv0/go-defun-name-from-body (defun-body)
  "Extract go func name from DEFUN-BODY source."
  (unless (null (string-match
                 (rx (seq "func") (* (any " ")) (group (* (not "("))) )
                 defun-body))
    (match-string-no-properties 1 defun-body)))


(defun t0yv0/go-debug-current-test ()
  "Run dape debugger for the Go test currently surrounding point."
  (interactive)
  (require 'dape)
  (let* ((name (t0yv0/go-defun-name))
         (cwd default-directory)
         (dape-config `(modes (go-mode go-ts-mode)
                              ensure dape-ensure-command
                              command "dlv"
                              command-args ("dap" "--listen" "127.0.0.1::autoport")
                              command-cwd ,cwd
                              port :autoport
                              :request "launch"
                              :type "test"
                              :mode "test"
                              :cwd "."
                              :program ,cwd
                              :args ["--test.run" ,name]))
         (dape-confs (dape--config-to-string 'go-last-test dape-config)))
    (setf (alist-get 'go-last-test dape-configs) dape-config)
    (when (or (null dape-history)
              (and dape-history (not (equal dape-confs (car dape-history)))))
      (setq dape-history (cons dape-confs dape-history)))
    (dape dape-config)))


(defun t0yv0/go-fixup-compilation-regexp ()
  "Improved compilation output parsing for Go."
  (let ((p nil)
        (entry nil))
    (setq p (rx (seq bol "\t")
                      (group (* (not ":")))
                      (any ":")
                      (group (+ digit))))
    (setq entry (cons 'go-panic-custom (list p 1 2 nil (list 2))))
    (setf (alist-get 'go-panic-custom compilation-error-regexp-alist-alist nil 'remove) nil)
    (setf (alist-get 'go-panic compilation-error-regexp-alist-alist nil 'remove) nil)
    (setf (alist-get 'go-test compilation-error-regexp-alist-alist nil 'remove) nil)
    (add-to-list 'compilation-error-regexp-alist-alist entry)
    (unless (memq 'go-panic-custom compilation-error-regexp-alist-alist)
      (add-to-list 'compilation-error-regexp-alist 'go-panic-custom))
    (message "Edited go-panic regexp for compilation-mode")))


(defun t0yv0/emacs-gptel-token ()
  "Extracts the Anthropic or OpenAI token from the secure store."
  (let ((token-store (getenv "EMACS_GPTEL_TOKEN_STORE")))
    (cond
     ((equal token-store "pass")
      (let ((tok (shell-command-to-string
                  (concat "pass "
                          (shell-quote-argument (getenv "EMACS_GPTEL_TOKEN_STORE_KEY"))
                          " 2>/dev/null"))))
        (setq tok (string-trim tok))
        (if (equal tok "") nil tok)))
     ((equal token-store "security")
      (let ((tok (shell-command-to-string
                  (concat "security find-generic-password -a "
                          (shell-quote-argument (getenv "EMACS_GPTEL_TOKEN_STORE_KEY"))
                          " -s "
                          (shell-quote-argument (getenv "EMACS_GPTEL_TOKEN_STORE_KEY2"))
                          " -w 2>/dev/null"))))
        (setq tok (string-trim tok))
        (if (equal tok "") nil tok))))))


(defun t0yv0/orderless-flex-if-twiddle (pattern _index _total)
  "See `t0yv0/orderless-style-dispatchers'.
PATTERN _INDEX _TOTAL as required by orderless."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))


(defun t0yv0/orderless-style-dispatchers ()
  "Compute style dispatchers for orderless 0.7."
  '(t0yv0/orderless-flex-if-twiddle
    t0yv0/orderless-without-if-bang))


(defun t0yv0/orderless-without-if-bang (pattern _index _total)
  "See `t0yv0/orderless-style-dispatchers'.
PATTERN _INDEX _TOTAL as required by orderless."
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))


(defun t0yv0/parse-git-diff (lines)
  "Parse LINES, the output of git diff, into a list with line numbers."
  (let ((m nil)
        (offset nil)
        (line-type nil)
        (emit (list)))
    (dolist (line lines)
      (setq line-type 'skipped)
      (setq m (string-match (rx "@@" (* space)
                                "-" (+ digit) "," (+ digit)
                                (* space)
                                "+" (group (+ digit)) "," (+ digit))
                            line))
      (unless (null m)
        (setq offset (string-to-number (match-string 1 line)))
        (setq line-type 'special))
      (when (equal (string-match (rx (any "+")) line) 0)
        (setq line-type 'added))
      (when (equal (string-match (rx (any "-")) line) 0)
        (setq line-type 'removed))
      (when (and (not (null offset)) (equal line-type 'added))
        (setq emit (cons (cons offset line) emit)))
      (when (and (not (null offset)) (or (equal line-type 'skipped)
                                         (equal line-type 'added)))
        (setq offset (+ offset 1))))
    (reverse emit)))


(defun t0yv0/point-at-line (n)
  "Convert a line number N to a point representing coordinates of that line."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- n))
    (point)))


(defun t0yv0/vterm-dabbrev-expand ()
  "Adaps `dabbrev-expand` to vterm."
  (interactive)
  (if t0yv0/vterm-dabbrev-state
      (let-alist t0yv0/vterm-dabbrev-state
        (let ((p (point)))
          (if (equal (- p .inserted) .point)
              (progn
                (vterm-delete-region (- p .inserted) p)
                (setq t0yv0/vterm-dabbrev-state (list (cons 'inserted 0)
                                                      (cons 'point p))))
            (dabbrev--reset-global-variables)
            (setq t0yv0/vterm-dabbrev-state nil))))
    (dabbrev--reset-global-variables))
  (let* ((current-dabbrev (dabbrev--abbrev-at-point))
         (expansion (dabbrev--find-expansion current-dabbrev 0 t)))
    (when expansion
      (let ((ins (substring expansion (length current-dabbrev) (length expansion)))
            (p (point)))
        (vterm-insert ins)
        (setq t0yv0/vterm-dabbrev-state (list (cons 'inserted (length ins))
                                              (cons 'point p)))))
    (unless expansion
      (dabbrev--reset-global-variables)
      (setq t0yv0/vterm-dabbrev-state nil))))


(provide 't0yv0-basics)
;;; t0yv0-basics.el ends here
