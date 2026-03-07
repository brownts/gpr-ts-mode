;;; gpr-ts-paren.el --- Parenthesis Highlight support in GNAT project files  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Troy Brown

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'gpr-ts-common)
(require 'paren)
(eval-when-compile (require 'rx))
(require 'treesit)

;;;; `show-paren-mode' support

(defconst gpr-ts-paren--show-paren-info
  `(("case" :delimiter-type opener :matching-delimiter "end"
     :predicate ,(lambda (n)
                   (when-let* ((prev-node (gpr-ts-mode--prev-node n))
                               (prev-node-t (treesit-node-type prev-node)))
                     (not (string-equal prev-node-t "end")))))
    ("package" :delimiter-type opener :matching-delimiter "end")
    ("project" :delimiter-type opener :matching-delimiter "end"
     :predicate ,#'gpr-ts-mode--project-keyword-p)
    ("end" :delimiter-type closer :matching-delimiter ("case" "package" "project"))))

(defun gpr-ts-paren--show-paren-data-categorize (pos)
  "Return a list suitable for `show-paren-data-function'.

The delimiter must start at, end at, or contain position POS."
  (when-let* ((here-n (gpr-ts-mode--node-at pos 'or-ends-at-pos))
              (here-t (treesit-node-type here-n))
              (here-s (treesit-node-start here-n))
              (here-e (treesit-node-end here-n))
              (info (cdr (assoc-string here-t gpr-ts-paren--show-paren-info)))
              ((or (not (plist-get info :predicate))
                   (funcall (plist-get info :predicate) here-n)))
              (delimiter-type (plist-get info :delimiter-type))
              (matching-delimiters (ensure-list (plist-get info :matching-delimiter)))
              (parent-node (treesit-node-parent here-n)))
    (let* ((matched-delimiters
            (treesit-filter-child
             parent-node
             (lambda (there-n)
               (let* ((there-t (treesit-node-type there-n))
                      (there-s (treesit-node-start there-n)))
                 (and (member there-t matching-delimiters)
                      (cond  ((eq delimiter-type 'opener) (< here-s there-s))
                             ((eq delimiter-type 'closer) (< there-s here-s))
                             (t (error "Unknown delimiter type")))
                      (let* ((there-info (cdr (assoc-string there-t gpr-ts-paren--show-paren-info)))
                             (there-predicate (plist-get there-info :predicate)))
                        (or (not there-predicate)
                            (funcall there-predicate there-n))))))))
           (there-n (cond ((eq delimiter-type 'opener) (car matched-delimiters))
                          ((eq delimiter-type 'closer) (car (reverse matched-delimiters)))
                          (t (error "Unknown delimiter type"))))
           (there-s (treesit-node-start there-n))
           (there-e (treesit-node-end there-n))
           (error-n
            (car (treesit-filter-child
                  parent-node
                  (lambda (n)
                    (let ((n-t (treesit-node-type n)))
                      (or (string-equal n-t "ERROR")
                          (treesit-node-check n 'missing)
                          (treesit-node-check n 'has-error)))))))
           (error-s (treesit-node-start error-n)))
      (when (cond ((not (null show-paren-when-point-in-periphery)))
                  ((not (null show-paren-when-point-inside-paren))
                   (cond ((eq delimiter-type 'opener) (> pos here-s))
                         ((eq delimiter-type 'closer) (< pos here-e))))
                  (t
                   (cond ((eq delimiter-type 'opener) (< pos here-e))
                         ((eq delimiter-type 'closer) (> pos here-s)))))
        (if (and there-n
                 (or (not error-n)
                     ;; As long as the error occurs after the closing
                     ;; delimiter, still try to highlight both
                     ;; delimiters.
                     (and (> error-s there-s)
                          (> error-s here-s)))
                 (or error-n
                     (not (gpr-ts-mode--defun-p parent-node))
                     (gpr-ts-mode--matched-names-p parent-node)))
            (list here-s here-e there-s there-e)
          (list here-s here-e nil nil 'mismatch))))))

(defun gpr-ts-paren--show-paren-data ()
  "A function suitable for `show-paren-data-function'."
  (or (gpr-ts-paren--show-paren-data-categorize (point))
      (when show-paren-when-point-in-periphery
        (let* ((current-pos (point))
               (indent-pos (save-excursion
                             (back-to-indentation)
                             (point)))
               (eol-pos (save-excursion
                          (end-of-line)
                          (skip-chars-backward " \t" indent-pos)
                          (point))))
          (let ((show-paren-when-point-inside-paren nil))
            (cond ((<= current-pos indent-pos)
                   (gpr-ts-paren--show-paren-data-categorize indent-pos))
                  ((>= current-pos eol-pos)
                   (gpr-ts-paren--show-paren-data-categorize eol-pos))))))
      ;; Fall back for parenthesis matching.
      (show-paren--default)))

(defun gpr-ts-paren--show-paren-post-setup ()
  "Parenthesis matching setup performed after `treesit-major-mode-setup'."
  (setq-local show-paren-data-function #'gpr-ts-paren--show-paren-data))

(add-hook 'gpr-ts-mode--after-setup-hook #'gpr-ts-paren--show-paren-post-setup)

(provide 'gpr-ts-paren)

;;; gpr-ts-paren.el ends here
