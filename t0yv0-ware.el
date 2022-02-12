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
    (when (string-match "^\\*shell" (buffer-name (current-buffer)))
      (other-window 1))
    (delete-other-windows)
    (set-window-buffer (split-window-right) new-shell-buf)))


(defun t0yv0/clear-and-repeat-last-command-in-project-shell ()
  "Clear project shell and re-submit last command to it."
  (interactive)
  (save-window-excursion
    (let ((buf (t0yv0/project-shell)))
      (with-current-buffer buf
        (let ((p (comint-previous-input-string 0)))
          (comint-clear-buffer)
          (comint-bol)
          (insert p)
          (comint-send-input))))))


(defun t0yv0/project-shell ()
  "Like `projectile-run-shell` but with `compilation-shell-minor-mode`."
  (let ((buf (projectile-run-shell nil)))
    (with-current-buffer buf
      (compilation-shell-minor-mode))
    buf))


(provide 't0yv0-ware)

;;; t0yv0-ware ends here
