;;; t0yv0-basics --- definitions without heavy external dependencies
;;;
;;; Commentary:
;;;
;;; Code:


(require 'consult)
(require 'dabbrev)
(require 'dash)
(require 'project)
(require 'vterm)


(defvar-local t0yv0/vterm-dabbrev-state nil)


(defun t0yv0/consult-changed-line ()
  "Like `consult-line' but only for lines changed according to git diff."
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


(defun t0yv0/consult-reformat-line-candidate (pair)
  "Formats a PAIR of line-num and line-text into a form suitable for consult-line."
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


(defun t0yv0/display-buffer-same-prog-mode-window (buffer alist)
  (if (derived-mode-p 'prog-mode)
      (display-buffer-same-window buffer alist)
    nil))


(defun t0yv0/embark-execute-defun (&optional defun-body)
  (let ((name (t0yv0/go-defun-name defun-body)))
    (cond
     ((and (equal major-mode 'go-ts-mode)
           (string-prefix-p "Test" name))
      (t0yv0/embark-execute-identifier name))
     (t
      (message "Do not know how to execute defun: %s" defun-body)))))


(defun t0yv0/embark-execute-identifier (ident)
  (cond
   ((and (equal major-mode 'go-ts-mode)
         (string-prefix-p "Test" ident))
    (compile (format "go test -test.v -test.run '^%s'" ident)))
   (t
    (message "Do not know how to execute identifier: %s" ident))))


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
  "Like git-link but also understands files in Go package cache that have no Git repo."
  (interactive "P")
  (if (t0yv0/git-link-file-name-to-github-remote (buffer-file-name))
      (t0yv0/git-link-go)
    (call-interactively #'git-link arg)))


(defun t0yv0/git-link-go ()
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
  (let* ((source (or defun-body (save-excursion
                                  (mark-defun)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end)))))
         (name (t0yv0/go-defun-name-from-body source)))
    (pop-mark)
    name))


(defun t0yv0/go-defun-name-from-body (defun-body)
  (unless (null (string-match
                 (rx (seq "func") (* (any " ")) (group (* (not "("))) )
                 defun-body))
    (match-string-no-properties 1 defun-body)))


(defun t0yv0/go-debug-current-test ()
  "Run dape debugger for the Go test currently surrounding point."
  (interactive)
  (let ((name (t0yv0/go-defun-name))
        (cwd default-directory))
    (dape `(modes (go-mode go-ts-mode)
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
            :args ("--test.run" ,name)))
    (t0yv0/dape-hydra/body)))


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


(defun t0yv0/vterm ()
  "If a vterm is visible, switch to it, otherwise switch to the project vterm."
  (interactive)
  (let ((buf (t0yv0/vterm-find-visible-buffer)))
    (if buf
        (switch-to-buffer buf)
      (t0yv0/vterm-proj))))


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


(defun t0yv0/vterm-dir ()
  "Switch to the vterm buffer scoped at current directory."
  (interactive)
  (let* ((root (file-name-directory buffer-file-name))
         (buffer (concat "*vterm-"
                         (file-name-nondirectory
                          (substring root 0 (- (length root) 1)))
                         "*")))
    (t0yv0/vterm-impl root buffer)))


(defun t0yv0/vterm-find-visible-buffer ()
  "Find a visible vterm buffer.

This buffer must be associated with one of the windows in the
current frame. Returns nil if none is found."
  (let ((vterm-windows
         (-filter (lambda (it)
                    (equal 'vterm-mode (with-current-buffer
                                           (window-buffer it)
                                         major-mode)))
                  (window-list))))
    (if (null vterm-windows)
        nil
      (window-buffer (car vterm-windows)))))


(defun t0yv0/vterm-impl (root buffer)
  "Backend for varios t0yv0/vterm-* functions.

If there is already a buffer named BUFFER, switch to it.

Otherwise, start a new vterm in ROOT directory in a new buffer
named BUFFER, and then switch to BUFFER.

Also, enter `compilation-shell-minor-mode' in the new buffer."
  (unless (buffer-live-p (get-buffer buffer))
    (let ((default-directory root))
      (vterm buffer)
      (with-current-buffer (get-buffer buffer)
        (compilation-shell-minor-mode 1))))
  (switch-to-buffer buffer))


(defun t0yv0/vterm-proj ()
  "Switch to the vterm buffer scoped at current project."
  (interactive)
  (if (null (project-current))
      (t0yv0/vterm-impl (getenv "HOME") "*vterm*")
    (let* ((root (project-root (project-current)))
           (buffer (concat "*vterm-"
                           (file-name-nondirectory
                            (substring root 0 (- (length root) 1)))
                           "*")))
      (t0yv0/vterm-impl root buffer))))


(defun t0yv0/vterm-repeat ()
  "Clear project shell and re-submit last command to it."
  (interactive)
  (save-window-excursion
    (let ((buf (t0yv0/vterm-find-visible-buffer)))
      (when buf
        (with-current-buffer buf
          (vterm-clear)
          (vterm-send-key "<up>")
          (vterm-send-return))))))


(defun t0yv0/quit ()
  "Quit current window or buffer."
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
      (quit-window)
    (previous-buffer)))


(provide 't0yv0-basics)
;;; t0yv0-ware.el ends here
