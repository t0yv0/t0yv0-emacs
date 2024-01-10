;;; t0yv0-ware --- daily driver commands
;;;
;;; Commentary:
;;;
;;; Code:


(require 'comint)
(require 'consult)
(require 'dabbrev)
(require 'dash)
(require 'go-mode)
(require 'markdown-mode)
(require 'mermaid-mode)
(require 'org)
(require 'project)
(require 'vterm)
(require 'xref)


(defvar-local t0yv0/vterm-dabbrev-state nil)


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


(defun t0yv0/diary ()
  "Switch to diary entry for today in a dedicated tab."
  (interactive)
  (let ((diary-file (format-time-string "~/my/%Y/%m/%d.org" (current-time))))
    (cond ((null (buffer-file-name (current-buffer)))
           (find-file diary-file))
          ((equal
            (expand-file-name (buffer-file-name (current-buffer)))
            (expand-file-name diary-file))
           (previous-buffer))
          (t
           (find-file diary-file)))))


(defun t0yv0/org-follow-gh-link (path _)
   "Visit GitHub PR or issue link.
PATH should be something like pulumi/pulumi#123"

  (browse-url (concat "https://github.com/"
    (replace-regexp-in-string (regexp-quote "#")
      "/issues/" path nil 'literal))))


(defun t0yv0/shell (cmd &optional dir)
  "Execute a shell command and return the result as a trimmed string.

CMD shell command
DIR working directory"

  (with-temp-buffer
    (if (null dir)
        (shell-command cmd (current-buffer))
      (let ((default-directory dir))
        (shell-command cmd (current-buffer))))
    (replace-regexp-in-string "\n\\'" "" (buffer-string))))


(defun t0yv0/current-mode ()
  "Compute the mode of the current buffer."
  (with-current-buffer (window-buffer (selected-window)) major-mode))


(defun t0yv0/mermaid-compile ()
  "Compiles and displays a Mermaid diagram from current region."
  (interactive)
  (save-excursion
    (when (and (eq 'markdown-mode (t0yv0/current-mode))
               (not (use-region-p)))
      (markdown-mark-paragraph))
    (when (and (eq 'org-mode (t0yv0/current-mode))
               (not (use-region-p)))
      (org-babel-mark-block))
    (mermaid-compile-region)
    (deactivate-mark t)
    (with-current-buffer "current-region.png"
      (revert-buffer nil t))))


(defun t0yv0/consult-ripgrep-current-directory ()
  "Search only current dirrectory."
  (interactive)
  (consult-ripgrep default-directory))


(defun t0yv0/consult-ripgrep-current-project ()
  "Search current project."
  (interactive)
  (consult-ripgrep))


(defun t0yv0/flex-if-twiddle (pattern _index _total)
  "See `t0yv0/orderless-style-dispatchers'.
PATTERN _INDEX _TOTAL as required by orderless."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))


(defun t0yv0/without-if-bang (pattern _index _total)
  "See `t0yv0/orderless-style-dispatchers'.
PATTERN _INDEX _TOTAL as required by orderless."
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))


(defun t0yv0/orderless-style-dispatchers ()
  "Compute style dispatchers for orderless 0.7."
  '(t0yv0/flex-if-twiddle
    t0yv0/without-if-bang))


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


(defun t0yv0/find-visible-vterm-buffer ()
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


(defun t0yv0/vterm ()
  "If a vterm is visible, switch to it, otherwise switch to the project vterm."
  (interactive)
  (let ((buf (t0yv0/find-visible-vterm-buffer)))
    (if buf
        (switch-to-buffer buf)
      (t0yv0/vterm-proj))))


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


(defun t0yv0/vterm-dir ()
  "Switch to the vterm buffer scoped at current directory."
  (interactive)
  (let* ((root (file-name-directory buffer-file-name))
         (buffer (concat "*vterm-"
                         (file-name-nondirectory
                          (substring root 0 (- (length root) 1)))
                         "*")))
    (t0yv0/vterm-impl root buffer)))


(defun t0yv0/vterm-impl (root buffer)
  "Backend for varios t0yv0/vterm-* functions.

If there is already a buffer named BUFFER, switch to it.

Otherwise, start a new vterm in ROOT directory in a new buffer
named BUFFER, and then switch to BUFFER.

Also, enter `compilation-shell-minor-mode' in the new buffer."
  (t0yv0/with-splittable-frame
   (lambda ()
     (unless (buffer-live-p (get-buffer buffer))
       (let ((default-directory root))
         (vterm buffer)
         (with-current-buffer (get-buffer buffer)
           (compilation-shell-minor-mode 1))))
     (let ((result (switch-to-buffer buffer)))
       (set-window-dedicated-p (get-buffer-window buffer) t)
       result))))


(defun t0yv0/with-splittable-frame (f)
  (let ((prior (frame-parameter nil 'unsplittable)))
    (set-frame-parameter nil 'unsplittable nil)
    (let ((result (funcall f)))
      (set-frame-parameter nil 'unsplittable prior)
      result)))


(defun t0yv0/vterm-repeat ()
  "Clear project shell and re-submit last command to it."
  (interactive)
  (save-window-excursion
    (let ((buf (t0yv0/find-visible-vterm-buffer)))
      (when buf
        (with-current-buffer buf
          (vterm-clear)
          (vterm-send-key "<up>")
          (vterm-send-return))))))


(defvar t0yv0/consult-source-window-buffer
  (list
   :name "Recent Window Buffers"
   :narrow ?w
   :category 'buffer
   :history 'buffer-name-history
   :state #'consult--buffer-state
   :items (lambda ()
            (-map #'buffer-name
                  (-filter (lambda (b) (not (null (buffer-file-name b))))
                           (-map #'car
                                 (window-prev-buffers))))))
  "Ordered recent buffer candidate source for `consult-buffer' relying on `window-prev-buffers'.")


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


(defun t0yv0/reformat-line-candidate (pair)
  "Formats a PAIR of line-num and line-text into a form suitable for consult-line."
  (let ((line-num (car pair))
        (line-text (cdr pair)))
    (consult--location-candidate line-text
                                 (cons (current-buffer) (t0yv0/point-at-line line-num))
                                 line-num
                                 line-num)))


(defun t0yv0/consult-changed-line ()
  "Like `consult-line' but only for lines changed according to git diff."
  (interactive)
  (let* ((fn (buffer-file-name (current-buffer)))
         (diff (shell-command-to-string (format "git diff %s" fn)))
         (candidates (-map (lambda (pair) (t0yv0/reformat-line-candidate pair))
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


(defun t0yv0/ensure-tree-sitter-grammar-install ()
  "Make sure ~/.emacs.d/tree-sitter symlink exists.

Ensures it is up-to-date with ./tree-sitter."
  (let* ((real-dir (concat (file-name-directory load-file-name) "tree-sitter"))
         (target-dir (concat user-emacs-directory "tree-sitter")))
    (unless (equal (file-symlink-p target-dir) real-dir)
      (delete-file target-dir))
    (make-symbolic-link real-dir target-dir t)))


(defun t0yv0/gofmt-before-save ()
  "Like `gofmt-before-save' but extended for `go-ts-mode'."
  (interactive)
  (when (eq major-mode 'go-ts-mode)
    (gofmt)))


(defun t0yv0/consult-project-flymake ()
  (interactive)
  (consult-flymake (project-current nil)))


(defun t0yv0/disable-eglot-inlay-hints ()
  (eglot-inlay-hints-mode -1))


(defun t0yv0/reset-region ()
  (interactive)
  (er/contract-region 0))


(defun t0yv0/backspace (arg)
  (interactive "P")
  (if (eq major-mode 'vterm-mode)
      (vterm-send-backspace)
    (delete-backward-char (or arg 1))))


(defun t0yv0/quit ()
  "Quit current window or buffer."
  (interactive)
  (if (> (seq-length (window-list (selected-frame))) 1)
      (quit-window)
    (previous-buffer)))


(defun t0yv0/display-buffer-at-bottom (buffer alist)
  "Picks the bottom window to display the buffer in, splitting if needed."
  (let ((w (selected-window))
        (candidates (-difference (window-at-side-list nil 'bottom)
                                 (window-at-side-list nil 'top))))
    (window--display-buffer buffer
                            (if (null candidates)
                                (split-window-below
                                 (ceiling (* 0.62 (window-height w))) w)
                              (car candidates))
                            'reuse alist)))


;; motion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun t0yv0/forward-word (&optional arg)
  (interactive "P")
  "Moves point forward by tree-sitter nodes. If tree-sitter is not
available falls back to `forward-word'."
  (cond
   ((null (treesit-parser-list))
    (forward-word (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-forward-node (point))))))


(defun t0yv0/treesit-forward-node (pos)
  "Computes a position forward one node from original POS."
  (let* ((n (treesit-node-at pos))
         (ne (treesit-node-end n)))
    (if (> ne pos) ne
      (let ((nn (treesit-search-forward
                 n
                 (lambda (n) (> (treesit-node-end n) pos))
                 nil
                 t)))
        (if nn (treesit-node-end nn) pos)))))


(defun t0yv0/backward-word (&optional arg)
  (interactive "P")
  "Moves point backward by tree-sitter nodes. If tree-sitter is not
available falls back to `backward-word'."
  (cond
   ((null (treesit-parser-list))
    (backward-word (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-backward-node (point))))))


(defun t0yv0/treesit-backward-node (pos)
  "Computes a position backward one node from original POS."
  (let* ((n (treesit-node-at pos))
         (ns (treesit-node-start n)))
    (if (< ns pos) ns
      (let ((nn (treesit-search-forward
                 n
                 (lambda (n) (< (treesit-node-start n) pos))
                 t
                 t)))
        (if nn (treesit-node-start nn) pos)))))


(defun t0yv0/forward-sexp (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (forward-sexp (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-forward-sexp (point))))))


(defun t0yv0/backward-sexp (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (backward-sexp (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-backward-sexp (point))))))


(defun t0yv0/treesit-forward-sexp (pos)
  (let* ((x (t0yv0/treesit-topmost-node pos)))
    (while (and x (<= (treesit-node-start x) pos))
      (setq x (or (treesit-node-next-sibling x)
                  (treesit-node-parent x))))
    (if x (treesit-node-start x) pos)))


(defun t0yv0/treesit-backward-sexp (pos)
  (let* ((x (t0yv0/treesit-topmost-node pos)))
    (while (and x (>= (treesit-node-start x) pos))
      (setq x (or (treesit-node-prev-sibling x)
                  (treesit-node-parent x))))
    (if x (treesit-node-start x) pos)))


(defun t0yv0/backward-up-list (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (backward-up-list (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-backward-up-list (point))))))


(defun t0yv0/treesit-backward-up-list (pos)
  (let ((x (t0yv0/treesit-topmost-node pos)))
    (treesit-node-start (or (treesit-node-parent x) x))))


(defun t0yv0/treesit-topmost-node (pos)
  (let ((x (treesit-node-at pos)))
    (while (let ((p (treesit-node-parent x)))
             (and p (equal (treesit-node-start p)
                          (treesit-node-start x))))
      (setq x (treesit-node-parent x)))
    x))


(defun t0yv0/mark-sexp (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (mark-sexp (or arg 1)))
   ((region-active-p)
    (if (> (point) (mark))
        (t0yv0/forward-sexp arg)
      (t0yv0/backward-sexp arg)))
   (t
    (let ((n (t0yv0/treesit-topmost-node (point))))
      (set-mark (treesit-node-start n))
      (goto-char (treesit-node-end n))))))


(defun t0yv0/kill-sexp (&optional arg)
  (interactive "P")
  (let ((opoint (point)))
    (t0yv0/forward-sexp (or arg 1))
    (kill-region opoint (point))))


(defun t0yv0/forward-list (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (forard-list (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-search-large-node (point))))))


(defun t0yv0/backward-list (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (forard-list (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-search-large-node (point) 'backward)))))


(defun t0yv0/treesit-search-large-node (pos &optional backward)
  (let ((nn (treesit-search-forward
             (treesit-node-at pos)
             (lambda (n)
               (and (if backward
                        (t0yv0/line-gt pos (treesit-node-start n))
                      (t0yv0/line-gt (treesit-node-start n) pos))
                    (t0yv0/at-indentation-p (treesit-node-start n))))
             (if backward t nil)
             t)))
    (if nn (treesit-node-start nn) pos)))


(defun t0yv0/line-gt (pos1 pos2)
  (save-excursion
    (let ((p1 nil) (p2 nil))
      (goto-char pos1)
      (beginning-of-line)
      (setq p1 (point))
      (goto-char pos2)
      (beginning-of-line)
      (setq p2 (point))
      (> p1 p2))))


(defun t0yv0/at-indentation-p (pos)
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (equal (point) pos)))


(defun t0yv0/expand-region (arg)
  (interactive "P")
  (cond
   ((and (not (null (treesit-parser-list)))
         (region-active-p))
    (let* ((new-region (t0yv0/treesit-compute-expanded-region (region-beginning)
                                                              (region-end)))
           (b (car new-region))
           (e (cdr new-region)))
      (setq er/history (cons (cons (point) (mark)) er/history))
      (if (> (point) (mark))
          (progn
            (set-mark b)
            (goto-char e))
        (progn
          (goto-char b)
          (set-mark e)))))
   (t
    (er/expand-region (or arg 1)))))


(defun t0yv0/treesit-compute-expanded-region (b e)
  (let ((n (treesit-node-on b e)))
    (while (and n
                (treesit-node-parent n)
                (>= (treesit-node-start n) b)
                (<= (treesit-node-end n) e))
      (setq n (treesit-node-parent n)))
    (if n (cons (treesit-node-start n)
                (treesit-node-end n))
      (cons b e))))


(defun t0yv0/go-defun-name (defun-body)
  (unless (null (string-match
                 (rx bos (seq "func") (* (any " ")) (group (* (not "("))) )
                 defun-body))
    (match-string-no-properties 1 defun-body)))


(defun t0yv0/embark-execute-identifier (ident)
  (cond
   ((and (equal major-mode 'go-ts-mode)
         (string-prefix-p "Test" ident))
    (shell-command (format "go test -test.v -test.run '^%s'" ident) "*test*" "*test*"))
   (t
    (message "Do not know how to execute identifier: %s" ident))))


(defun t0yv0/embark-execute-defun (defun-body)
  (cond
   ((and (equal major-mode 'go-ts-mode)
         (string-prefix-p "Test" (t0yv0/go-defun-name defun-body)))
    (t0yv0/embark-execute-identifier (t0yv0/go-defun-name defun-body)))
   (t
    (message "Do not know how to execute defun: %s" defun-body))))


(provide 't0yv0-ware)
;;; t0yv0-ware.el ends here
