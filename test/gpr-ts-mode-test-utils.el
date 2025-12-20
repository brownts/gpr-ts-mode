;;; gpr-ts-mode-test-utils.el --- Common utilities shared by tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Troy Brown

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

;;; Code:

(require 'cl-lib)
(require 'ert)

;;;; Utilities

(defun gpr-ts-mode-tests--modify-and-reindent ()
  "Modify each line and reindent buffer."
  ;; Modifying each line is used to make sure there is an indentation
  ;; rule that moves the line back to the initial location, rather
  ;; than no indentation rule that just leaves the line alone.  This
  ;; is useful for finding lines with missing indentation rules.
  (goto-char (point-min))
  (cl-flet ((line-length () (- (line-end-position)
                               (line-beginning-position))))
    (while (not (eobp))
      (when (> (line-length) 0)
        (if (= (following-char) ?\s)
            (while (and (> (line-length) 0)
                        (= (following-char) ?\s))
              (delete-char 1))
          (insert-char ?\s)))
      (forward-line 1)
      (beginning-of-line)))
  (indent-region (point-min) (point-max)))

(defun gpr-ts-mode-tests--check-indentation ()
  "Check buffer indentation is as expected."
  (let ((buffer (buffer-string))
        (point (point)))
    (gpr-ts-mode-tests--modify-and-reindent)
    (should (string-equal buffer (buffer-string)))
    (goto-char point)))

(defun gpr-ts-mode-tests--simulate-key-press (key)
  "Simulate interactively pressing KEY.

Simulates execution of the command associated with the key as well as
execution of pre and post command hooks."
  (let ((inhibit-message t))
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (execute-kbd-macro (kbd key)))))

(provide 'gpr-ts-mode-test-utils)

;;; gpr-ts-mode-test-utils.el ends here
