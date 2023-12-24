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


(defun t0yv0/display-buffer-same-vterm-window (buffer alist)
  "Like `display-buffer-same-window' if the selected window is in vterm-mode.

BUFFER is a buffer to display.

ALIST contains options such as `inhibit-same-window'."
  (when (eq 'vterm-mode (t0yv0/current-mode))
    (display-buffer-same-window buffer alist)))


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


(defun t0yv0/expand-region (arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (er/expand-region (or arg 1)))
   ((not (region-active-p))
    (setq t0yv0/region-history '())
    (t0yv0/treesit-expand-region-start))
   ((equal arg (list 4))
    (t0yv0/treesit-expand-region-to-node))
   ((<= (mark) (point))
    (t0yv0/treesit-expand-region-forward))
   (t ;; remaining case is backward direction
    (t0yv0/treesit-expand-region-backward))))


(defun t0yv0/widen-region (arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (er/expand-region (or arg 1)))
   ((not (region-active-p))
    (setq t0yv0/region-history '())
    (t0yv0/treesit-expand-region-start))
   (t
    (t0yv0/treesit-expand-region-to-node))))


(defun t0yv0/goto-region ()
  (interactive)
  (cond
   ((null (treesit-parser-list))
    (let ((p (point)))
      (er/contract-region 0)
      (deactivate-mark)
      (goto-char p)))
   (t
    (deactivate-mark)
    (setq t0yv0/region-history '()))))


(defun t0yv0/reset-region ()
  (interactive)
  (cond
   ((null (treesit-parser-list))
    (er/contract-region 0)
    (deactivate-mark))
   (t
    (when (not (null t0yv0/region-history))
      (goto-char (car (car (last t0yv0/region-history)))))
    (deactivate-mark)
    (setq t0yv0/region-history '()))))


(defun t0yv0/contract-region (arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (er/contract-region (or arg 1)))
   ((> (length t0yv0/region-history) 1)
    (set-mark (car (car t0yv0/region-history)))
    (goto-char (cdr (car t0yv0/region-history)))
    (setq t0yv0/region-history (cdr t0yv0/region-history)))
   (t
    (t0yv0/reset-region))))


(defvar t0yv0/region-history '()
  "A history of start and end points so we can contract after expanding.")

;; history is always local to a single buffer
(make-variable-buffer-local 't0yv0/region-history)


(defun t0yv0/remember-selection (start end)
  (push (cons start end) t0yv0/region-history)
  (set-mark start)
  (goto-char end))


(defun t0yv0/treesit-expand-region-start ()
  (let ((n (treesit-node-at (point))))
    (while (< (- (treesit-node-end n)
                 (treesit-node-start n))
              2)
      (setq n (treesit-node-parent n)))
    (t0yv0/remember-selection
     (treesit-node-start n)
     (treesit-node-end n))))


(defun t0yv0/treesit-expand-region-to-node ()
  (let* ((b (region-beginning))
         (e (region-end))
         (n (treesit-node-on b e)))
    (while (and (>= (treesit-node-start n) b)
                (<= (treesit-node-end n) e))
      (setq n (treesit-node-parent n)))
    (t0yv0/remember-selection
     (treesit-node-start n)
     (treesit-node-end n))))


(defun t0yv0/treesit-expand-region-backward ()
  (interactive)
  (if (not (region-active-p))
      (t0yv0/treesit-expand-region-start)
      (let* ((b (region-beginning))
             (e (region-end))
             (n (treesit-node-on b e))
             (c (treesit-filter-child
                 n
                 (lambda (c)
                   (and (>= b (treesit-node-start c))
                        (<= b (treesit-node-end c))))))
             (x (or (if c (treesit-node-prev-sibling (car c)) nil) n)))
        (while (and x
                    (>= (treesit-node-end x) b)
                    (>= (treesit-node-start x) b))
          (setq x (or (treesit-node-prev-sibling x)
                      (treesit-node-parent x))))
        (when x
          (t0yv0/remember-selection
           e
           (if (< (treesit-node-end x) b)
               (treesit-node-end x)
             (treesit-node-start x)))))))


(defun t0yv0/treesit-expand-region-forward ()
  (interactive)
  (if (not (region-active-p))
      (t0yv0/treesit-expand-region-start)
    (let* ((b (region-beginning))
           (e (region-end))
           (n (treesit-node-on b e))
           (c (treesit-filter-child
               n
               (lambda (c)
                 (and (>= e (treesit-node-start c))
                      (<= e (treesit-node-end c))))))
           (x (or (if c (treesit-node-next-sibling (car c)) nil) n)))
      (while (and x
                  (<= (treesit-node-end x) e)
                  (<= (treesit-node-start x) e))
        (setq x (or (treesit-node-next-sibling x)
                    (treesit-node-parent x))))
      (when x
        (t0yv0/remember-selection
         b
         (if (> (treesit-node-start x) e)
             (treesit-node-start x)
           (treesit-node-end x)))))))


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


(defun t0yv0/bottom-window ()
  "Returns the window closest to the bottom of current frame."
  (let ((win nil)
        (edge 0))
    (walk-window-tree
     (lambda (w)
       (let ((w-edge (cadddr (window-edges w))))
         (when (or (null win) (> w-edge edge))
           (setq edge w-edge)
           (setq win w)))))
    win))


(defun t0yv0/display-buffer-at-bottom (buffer alist)
  "Picks the bottom window to display the buffer in, splitting if needed."
  (when (equal (length (window-list)) 1)
    (split-window-below (ceiling (* 0.62 (window-height (selected-window))))))
  (window--display-buffer buffer (t0yv0/bottom-window) 'reuse alist))


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


(provide 't0yv0-ware)
;;; t0yv0-ware.el ends here
