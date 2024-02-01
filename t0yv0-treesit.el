;;; t0yv0-treesit --- extensions for treesitter
;;;
;;; Commentary:
;;;
;;; Code:

(require 'treesit)


(defun t0yv0/at-indentation-p (pos)
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (equal (point) pos)))


(defun t0yv0/backward-list (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (backward-list (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-search-large-node (point) 'backward)))))


(defun t0yv0/backward-sexp (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (backward-sexp (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-backward-sexp (point))))))


(defun t0yv0/backward-up-list (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (backward-up-list (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-backward-up-list (point))))))


(defun t0yv0/backward-word (&optional arg)
  "Moves point backward by tree-sitter nodes. If tree-sitter is not
available falls back to `backward-word'."
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (backward-word (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-backward-node (point))))))


(defun t0yv0/ensure-tree-sitter-grammar-install ()
  "Make sure ~/.emacs.d/tree-sitter symlink exists.

Ensures it is up-to-date with ./tree-sitter."
  (interactive)
  (let* ((real-dir (symbol-value 't0yv0/treesitter-dir))
         (target-dir (concat user-emacs-directory "tree-sitter")))
    (unless (equal (file-symlink-p target-dir) real-dir)
      (message (format "t0yv0-treesit.el: removing %s to reset treesit grammars" target-dir))
      (delete-file target-dir))
    (message (format "t0yv0-treesit.el: relinking %s to reset treesit grammars" target-dir))
    (make-symbolic-link real-dir target-dir t)))


(defun t0yv0/forward-list (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (forward-list (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-search-large-node (point))))))


(defun t0yv0/forward-sexp (&optional arg)
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (forward-sexp (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-forward-sexp (point))))))


(defun t0yv0/forward-word (&optional arg)
  "Moves point forward by tree-sitter nodes. If tree-sitter is not
available falls back to `forward-word'."
  (interactive "P")
  (cond
   ((null (treesit-parser-list))
    (forward-word (or arg 1)))
   (t
    (goto-char (t0yv0/treesit-forward-node (point))))))


(defun t0yv0/kill-sexp (&optional arg)
  (interactive "P")
  (let ((opoint (point)))
    (t0yv0/forward-sexp (or arg 1))
    (kill-region opoint (point))))


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


(defun t0yv0/treesit-backward-sexp (pos)
  (let* ((x (t0yv0/treesit-topmost-node pos)))
    (while (and x (>= (treesit-node-start x) pos))
      (setq x (or (treesit-node-prev-sibling x)
                  (treesit-node-parent x))))
    (if x (treesit-node-start x) pos)))


(defun t0yv0/treesit-backward-up-list (pos)
  (let ((x (t0yv0/treesit-topmost-node pos)))
    (treesit-node-start (or (treesit-node-parent x) x))))


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


(defun t0yv0/treesit-forward-sexp (pos)
  (let* ((x (t0yv0/treesit-topmost-node pos)))
    (while (and x (<= (treesit-node-start x) pos))
      (setq x (or (treesit-node-next-sibling x)
                  (treesit-node-parent x))))
    (if x (treesit-node-start x) pos)))


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


(defun t0yv0/treesit-topmost-node (pos)
  (let ((x (treesit-node-at pos)))
    (while (let ((p (treesit-node-parent x)))
             (and p (equal (treesit-node-start p)
                           (treesit-node-start x))))
      (setq x (treesit-node-parent x)))
    x))


(provide 't0yv0-treesit)
;;; t0yv0-treesit.el ends here
