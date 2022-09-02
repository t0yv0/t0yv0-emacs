;;; t0yv0-ware --- daily driver commands
;;;
;;; Commentary:
;;;
;;; Code:


(require 'comint)
(require 'dabbrev)
(require 'org)


(declare-function vterm-clear "dbus" ())
(declare-function vterm-insert "dbus" (x))
(declare-function vterm-send-backspace "dbus" ())
(declare-function vterm-send-return "dbus" ())
(declare-function vterm-send-up "dbus" ())
(declare-function projectile-run-vterm "projectile" (x))


(defun t0yv0/project-shell ()
  "Start a vterm/shell for a project."
  (projectile-run-vterm nil))


(defun t0yv0/open-shell-for-current-buffer ()
  "Command to open a project shell."
  (interactive)
  (t0yv0/project-shell))


(defun t0yv0/clear-and-repeat-last-command-in-project-shell ()
  "Clear project shell and re-submit last command to it."
  (interactive)
  (save-window-excursion
    (let ((buf (t0yv0/project-shell)))
      (with-current-buffer buf
        (vterm-clear)
        (vterm-send-up)
        (vterm-send-return)))))


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


(defvar t0yv0/-diary-saved-winconf nil)


(defun t0yv0/diary ()
  "Toggle between diary entry for today and current work."
  (interactive)
  (let ((today-file-name
         (format-time-string "~/my/%Y/%m/%d.org" (current-time))))

    (if (equal (expand-file-name today-file-name)
               (buffer-file-name (current-buffer)))
        (unless (null t0yv0/-diary-saved-winconf)
          (set-window-configuration t0yv0/-diary-saved-winconf)
          (setq t0yv0/-diary-saved-winconf nil))
      (progn
        (setq t0yv0/-diary-saved-winconf (current-window-configuration))
        (find-file today-file-name)))))


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


(provide 't0yv0-ware)
;;; t0yv0-ware.el ends here
