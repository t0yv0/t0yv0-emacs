;;; t0yv0-ware --- daily driver commands
;;;
;;; Commentary:
;;;
;;; Code:


(require 'comint)
(require 'dabbrev)
(require 'org)
(require 'xref)


(declare-function vterm "vterm" (x))
(declare-function vterm-clear "vterm" ())
(declare-function vterm-insert "vterm" (x))
(declare-function vterm-send-backspace "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-up "vterm" ())
(declare-function markdown-mark-paragraph "markdown-mode" ())
(declare-function mermaid-compile-region "mermaid-mode" ())
(declare-function consult-ripgrep "consult" (x))
(declare-function -filter "dash" (f xs))


(defun t0yv0/vterm-dabbrev-expand ()
  "Adaps `dabbrev-expand` to vterm."
  (interactive)
  (let* ((current-dabbrev (dabbrev--abbrev-at-point))
         (expansion (dabbrev--find-expansion current-dabbrev 0 t)))
    (if (eq expansion nil)
        (dabbrev--reset-global-variables)
      (progn
        (dotimes (i (length current-dabbrev))
          (vterm-send-backspace))
        (vterm-insert expansion)))))


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


(defun t0yv0/store-link ()
  "Store link to current location in the diary."
  (interactive)
  (save-window-excursion
    (let ((today-file-name
           (format-time-string "~/my/%Y/%m/%d.org" (current-time)))
          (the-link
           (org-store-link nil)))
      (find-file today-file-name)
      (insert (string-join (list the-link "\n")))
      (message the-link))))


(defun t0yv0/kill-org-link ()
  "Store an org-style link to current location in the kill ring."
  (interactive)
  (save-window-excursion
    (let* ((the-link (org-store-link nil))
           (the-link-string (string-join (list the-link "\n"))))
      (kill-new the-link-string)
      (message "Stored org link in the kill ring: %s" the-link-string))))


(defun t0yv0/org-link-to-register ()
  "Store an org-style link to current location in a register."
  (interactive)
  (let* ((lnk (org-store-link nil))
         (s   (string-join (list lnk "\n")))
         (reg (register-read-with-preview "Register for GitHub link:")))
    (message "Org link saved to register %s: %s" (string reg) s)
    (set-register reg s)))


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


(defun t0yv0/region-line-bounds ()
  "Compute active region line bounds or return current line."
  (interactive)

  (if (region-active-p)
      (let ((rb (region-bounds)))
        (if (= 1 (length rb))
            (let* ((bounds-pair (car rb))
                   (pos1 (car bounds-pair))
                   (pos2 (cdr bounds-pair))
                   (line1 (line-number-at-pos pos1 t))
                   (line2 (line-number-at-pos pos2 t)))
              (cons line1 line2))
          (cons (line-number-at-pos) (line-number-at-pos))))
    (cons (line-number-at-pos) (line-number-at-pos))))


(defun t0yv0/github-link-at-point ()
  "Builds a GitHub link to point."
  (let* ((fn (buffer-file-name))
         (dd (file-name-directory fn))
         (origin (t0yv0/shell "git config --get remote.origin.url" dd))
         (hash (t0yv0/shell "git rev-parse HEAD" dd))
         (toplevel (t0yv0/shell "git rev-parse --show-toplevel" dd))
         (repo (string-remove-suffix ".git" (string-remove-prefix "git@github.com:" origin)))
         (lines (t0yv0/region-line-bounds)))
    (concat (string-join (list "https://github.com"
                               repo
                               "blob"
                               hash
                               (file-relative-name fn toplevel))
                         "/")
            "#L"  (number-to-string (car lines))
            "-#L" (number-to-string (cdr lines)))))


(defun t0yv0/github-link-at-point-to-register ()
  "Store a GitHub link at point to a given register."
  (interactive)
  (let ((lnk (t0yv0/github-link-at-point))
        (reg (register-read-with-preview "Register for GitHub link:")))
    (message "GitHub link saved to register %s: %s" (string reg) lnk)
    (set-register reg lnk)))


(defun t0yv0/kill-github-link-at-point ()
  "Store a GitHub link at point to the kill ring."
  (interactive)
  (let ((lnk (t0yv0/github-link-at-point)))
    (message "GitHub link saved to the kill ring: %s" lnk)
    (kill-new lnk)))


(defun t0yv0/display-buffer-same-go-window (buffer alist)
  "Like `display-buffer-same-window' if the selected window is in go-mode.

BUFFER is a buffer to display.

ALIST contains options such as `inhibit-same-window'."
  (when (eq 'go-mode (t0yv0/current-mode))
    (display-buffer-same-window buffer alist)))


(defun t0yv0/display-buffer-same-vterm-window (buffer alist)
  "Like `display-buffer-same-window' if the selected window is in vterm-mode.

BUFFER is a buffer to display.

ALIST contains options such as `inhibit-same-window'."
  (when (eq 'vterm-mode (t0yv0/current-mode))
    (display-buffer-same-window buffer alist)))


(defun t0yv0/jump-to-register ()
  "Like `jump-to-register' but enables M-, to jump back."
  (interactive)
  (xref-push-marker-stack)
  (jump-to-register (register-read-with-preview "Jump to register:")))


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


(defun t0yv0/cheatsheet ()
  "Reminders on Emacs bindings."
  (interactive)
  (with-output-to-temp-buffer "*cheat*"
    (princ "Emacs Refcard

Motion               Fluency

char C-b    C-f      M-h  mark-paragraph
word M-b    M-f      M-m  back-to-indentation
line C-p    C-n      C-x <left>  previous-buffer
eol  C-a    C-e      C-x <right> next-buffer
sntc M-a    M-e      C-x C-SPC   pop-global-mark
para M-{    M-}      C-u C-SPC   set-mark-command (pop)
page C-x [  C-x ]    C-SPC C-SPC set-mark-command
sexp C-M-b  C-M-f    C-x f       consult-recent-file
func C-M-a  C-M-e    C-x C-b     consult-project-buffer
buf  M-<    M->"))
  (select-window (display-buffer "*cheat*")))


(defun t0yv0/consult-ripgrep-current-directory ()
  "Search only current dirrectory."
  (interactive)
  (consult-ripgrep default-directory))


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
  (unless (buffer-live-p (get-buffer buffer))
    (let ((default-directory root))
      (vterm buffer)
      (with-current-buffer (get-buffer buffer)
        (compilation-shell-minor-mode 1))))
  (switch-to-buffer buffer))


(defun t0yv0/vterm-repeat ()
  "Clear project shell and re-submit last command to it."
  (interactive)
  (save-window-excursion
    (let ((buf (t0yv0/find-visible-vterm-buffer)))
      (when buf
        (with-current-buffer buf
          (vterm-clear)
          (vterm-send-up)
          (vterm-send-return))))))


(provide 't0yv0-ware)
;;; t0yv0-ware.el ends here
