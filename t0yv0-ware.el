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


(defun t0yv0/treesit-backward ()
  "Move point to the beginning of current or previous treesitter node."
  (interactive)
  (let* ((p (point))
         (n (treesit-node-at p))
         (ns (treesit-node-start n))
         (pn (t0yv0/treesit-prev-node n)))
    (cond
     ;; when not at start of current node, go there
     ((< ns p) (goto-char ns))
     ;; when there is a previous node, go to its start
     (pn (goto-char (treesit-node-start pn)))
     ;; otherwise step back one char
     ((> p 0) (goto-char (- p 1))))))


(defun t0yv0/treesit-forward ()
  "Move point to the beginning of next treesitter node."
  (interactive)
  (let* ((p (point))
         (n (treesit-node-at p))
         (nn (t0yv0/treesit-next-node n)))
    (cond
     ;; when there is a next node, go to its start
     (nn (goto-char (treesit-node-start nn)))
     ;; otherwise go to end of current
     (t (goto-char (treesit-node-end n))))))


(defun t0yv0/treesit-prev-node (n)
  (treesit-search-forward n (lambda (x) (< (treesit-node-start x)
                                           (treesit-node-start n)))
                          t t))


(defun t0yv0/treesit-next-node (n)
  (treesit-search-forward n (lambda (x) (> (treesit-node-start x)
                                           (treesit-node-start n)))
                          nil t))


(defun t0yv0/treesit-expand-region ()
  (interactive)
  (if (not (region-active-p))
      (t0yv0/treesit-expand-region-start)
    (t0yv0/treesit-expand-region-to-node)))


(defun t0yv0/treesit-expand-region-start ()
  (let ((n (treesit-node-at (point))))
    (while (< (- (treesit-node-end n)
                 (treesit-node-start n))
              2)
      (setq n (treesit-node-parent n)))
    (meow--select (meow--make-selection
                   '(tree . expand)
                   (treesit-node-start n)
                   (treesit-node-end n)))))


(defun t0yv0/treesit-expand-region-to-node ()
  (let* ((b (region-beginning))
         (e (region-end))
         (n (treesit-node-on b e)))
    (while (and (>= (treesit-node-start n) b)
                (<= (treesit-node-end n) e))
      (setq n (treesit-node-parent n)))
    (meow--select (meow--make-selection
                   '(tree . expand)
                   (treesit-node-start n)
                   (treesit-node-end n)))))


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
          (meow--select (meow--make-selection
                         '(tree . expand)
                         e
                         (if (< (treesit-node-end x) b)
                             (treesit-node-end x)
                           (treesit-node-start x))))))))


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
        (meow--select (meow--make-selection
                       '(tree . expand)
                       b
                       (if (> (treesit-node-start x) e)
                           (treesit-node-start x)
                         (treesit-node-end x))))))))


(defun t0yv0/wider-node (n b e)
  (cond
   ((< (treesit-node-start n) b)
    n)
   ((> (treesit-node-end n) e)
    n)
   ((and (treesit-node-parent n) (treesit-node-parent (treesit-node-parent n)))
    (t0yv0/wider-node (treesit-node-parent n) b e))
   (t
    n)))


(defvar t0yv0/treesit-notable-go-node-regex
  (rx (or "assignment_statement"
          "continue_statement"
          "default_case"
          "expression_switch_statement"
          "expression_case"
          "if_statement"
          "for_statement"
          "function_declaration"
          "keyed_element"
          "literal_element"
          "return_statement"
          "short_var_declaration"
          "type_case"
          "type_declaration"
          "type_switch_statement"
          "var_declaration")))


(defun t0yv0/treesit-notable-node (x)
  (if (string-match-p t0yv0/treesit-notable-go-node-regex (treesit-node-type x))
      t nil))


(defun t0yv0/search (start next found)
  (let ((x start))
    (while (and x (not (funcall found x)))
      (setq x (funcall next x)))
    (if (and x (funcall found x)) x nil)))


(defun t0yv0/treesit-kill ()
  (interactive)
  (let* ((p0 (point))
         (nh (t0yv0/treesit-topmost-starting-here-node))
         (n (t0yv0/search
             nh
             #'treesit-node-next-sibling
             (lambda (n)
               (and (t0yv0/treesit-notable-node n)
                    (> (treesit-node-start n) p0))))))
    (if n
      (kill-region p0 (treesit-node-start n))
      (when (t0yv0/treesit-notable-node nh)
        (kill-region (treesit-node-start nh)
                     (treesit-node-end nh))
        (when  (equal ","
                      (buffer-substring-no-properties (point) (+ 1 (point))))
          (append-next-kill)
          (kill-region (point) (+ 1 (point))))))))


(defun t0yv0/treesit-next ()
  (interactive)
  (let* ((p0 (point))
         (n (t0yv0/search
             (t0yv0/treesit-topmost-starting-here-node)
             #'treesit-node-next-sibling
             (lambda (n)
               (and (t0yv0/treesit-notable-node n)
                    (> (treesit-node-start n) p0))))))
    (when n
      (goto-char (treesit-node-start n)))))


(defun t0yv0/treesit-previous ()
  (interactive)
  (let* ((p0 (point))
         (n (t0yv0/search
             (t0yv0/treesit-topmost-starting-here-node)
             #'treesit-node-prev-sibling
             (lambda (n)
               (and (t0yv0/treesit-notable-node n)
                    (< (treesit-node-start n) p0))))))
    (when n
      (goto-char (treesit-node-start n)))))


(defun t0yv0/treesit-topmost-starting-here-node ()
  (t0yv0/treesit-topmost-starting-from-node
   (treesit-node-at (point))))


(defun t0yv0/treesit-topmost-starting-from-node (n)
  (let ((s (treesit-node-start n)))
    (while (and (treesit-node-parent n)
                (equal s (treesit-node-start (treesit-node-parent n))))
      (setq n (treesit-node-parent n)))
    n))


(defun t0yv0/treesit-down ()
  (interactive)
  (let* ((p0 (point))
         (n (treesit-search-subtree
             (t0yv0/treesit-topmost-starting-here-node)
             (lambda (n)
               (and (t0yv0/treesit-notable-node
                     (t0yv0/treesit-topmost-starting-from-node n))
                    (> (treesit-node-start n) p0))))))
    (when n
      (goto-char (treesit-node-start n)))))


(defun t0yv0/treesit-up ()
  (interactive)
  (let* ((p0 (point))
         (n (t0yv0/search
             (t0yv0/treesit-topmost-starting-here-node)
             #'treesit-node-parent
             (lambda (n)
               (and (t0yv0/treesit-notable-node n)
                    (< (treesit-node-start n) p0))))))
    (when n
      (goto-char (treesit-node-start n)))))


(defun t0yv0/meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("}" . t0yv0/treesit-expand-region-forward)
   '("{" . t0yv0/treesit-expand-region-backward)
   '("=" . t0yv0/treesit-expand-region)
   '("<escape>" . ignore)))

(provide 't0yv0-ware)
;;; t0yv0-ware.el ends here
