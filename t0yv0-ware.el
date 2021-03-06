;;; t0yv0-ware --- daily driver commands
;;;
;;; Commentary:
;;;
;;; Code:


(require 'comint)


(defun t0yv0/open-shell-for-current-buffer ()
  "Command to open a project shell."
  (interactive)
  (let ((new-shell-buf nil))
    (save-window-excursion
      (setq new-shell-buf (t0yv0/project-shell)))
    (when (string-match "^\\*vterm" (buffer-name (current-buffer)))
      (other-window 1))
    (delete-other-windows)
    (set-window-buffer (split-window-right) new-shell-buf)))


(defun t0yv0/clear-and-repeat-last-command-in-project-shell ()
  "Clear project shell and re-submit last command to it."
  (interactive)
  (save-window-excursion
    (let ((buf (t0yv0/project-shell)))
      (with-current-buffer buf
        (vterm-clear)
        (vterm-send-up)
        (vterm-send-return)))))


(defun t0yv0/project-shell ()
  "Like `projectile-run-shell` but with `compilation-shell-minor-mode`."
  (projectile-run-vterm nil))


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
  "Opens org diary entry for today."
  (interactive)
  (find-file (format-time-string "~/my/%Y/%m/%d.org" (current-time))))


(defun t0yv0/org-follow-gh-link (path _)
   "Visit GitHub PR or issue link.
PATH should be something like pulumi/pulumi#123"

  (browse-url (concat "https://github.com/"
    (replace-regexp-in-string (regexp-quote "#")
      "/issues/" path nil 'literal))))


(provide 't0yv0-ware)

;;; t0yv0-ware ends here
