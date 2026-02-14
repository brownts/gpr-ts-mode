;;; gpr-ts-align.el --- Alignment support in GNAT project files  -*- lexical-binding: t; -*-

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

(require 'align)
(require 'gpr-ts-core)
(require 'rx)
(require 'treesit)

(defcustom gpr-ts-mode-align-region-separate #'gpr-ts-align--contains-separator-p
  "`gpr-ts-mode' specific value of `align-region-separate'.

If nil, then `align-region-separate' will not be set buffer locally."
  :type '(choice
          (const    :tag "Entire region is one section" entire)
          (const    :tag "Align by contiguous groups"   group)
          (regexp   :tag "Regexp defines section boundaries")
          (function :tag "Function defines section boundaries"))
  :group 'gpr-ts
  :risky t
  :link '(custom-manual :tag "Code Alignment" "(gpr-ts-mode)Code Alignment")
  :package-version '(gpr-ts-mode . "0.8.0"))

(defcustom gpr-ts-mode-align-rules
  `((gpr-variable-declaration-before-colon
     (regexp    . ,(rx (group (* (syntax whitespace)))
                       ":"
                       (or (not "=") eol)))
     (modes     . '(gpr-ts-mode))
     (valid     . gpr-ts-align--valid-p))
    (gpr-variable-declaration-after-colon
     (regexp    . ,(rx ":"
                       (group (* (syntax whitespace)))
                       (not (any whitespace "=" "\n"))))
     (modes     . '(gpr-ts-mode))
     (valid     . gpr-ts-align--valid-p))
    (gpr-variable-declaration-before-assign
     (regexp    . ,(rx (group (* (syntax whitespace))) ":="))
     (modes     . '(gpr-ts-mode))
     (valid     . gpr-ts-align--valid-p))
    (gpr-variable-declaration-after-assign
     (regexp    . ,(rx ":="
                       (group (* (syntax whitespace)))
                       (not (any whitespace "\n"))))
     (modes     . '(gpr-ts-mode))
     (valid     . gpr-ts-align--valid-p))
    (gpr-attribute-declaration-before-use
     (regexp    . ,(rx (group (* (syntax whitespace)))
                       symbol-start "use" symbol-end))
     (modes     . '(gpr-ts-mode))
     (case-fold . t)
     (valid     . gpr-ts-align--valid-p))
    (gpr-attribute-declaration-after-use
     (regexp    . ,(rx symbol-start "use" symbol-end
                       (group (* (syntax whitespace)))
                       (not (any whitespace "\n"))))
     (modes     . '(gpr-ts-mode))
     (case-fold . t)
     (valid     . gpr-ts-align--valid-p))
    (gpr-trailing-comment
     (regexp    . ,(rx (group (* (syntax whitespace))) "--"))
     (modes     . '(gpr-ts-mode))
     (valid     . ,(lambda ()
                     (and (save-excursion
                            (goto-char (match-beginning 1))
                            (not (bolp)))
                          (gpr-ts-align--valid-p))))))
  "Alignment rules specific to `gpr-ts-mode'.

See the variable `align-rules-list' for more details."
  :type align-rules-list-type
  :group 'gpr-ts
  :risky t
  :link '(custom-manual :tag "Code Alignment" "(gpr-ts-mode)Code Alignment")
  :package-version '(gpr-ts-mode . "0.8.0"))

(defcustom gpr-ts-mode-align-exclude-rules nil
  "Alignment exclusion rules specific to `gpr-ts-mode'.

See the variable `align-exclude-rules-list' for more details."
  :type align-exclude-rules-list-type
  :group 'gpr-ts
  :risky t
  :link '(custom-manual :tag "Code Alignment" "(gpr-ts-mode)Code Alignment")
  :package-version '(gpr-ts-mode . "0.8.0"))

(defcustom gpr-ts-mode-auto-align-keys '("RET")
  "Keys which trigger alignment when `gpr-ts-auto-align-mode' is enabled."
  :type '(repeat key)
  :group 'gpr-ts
  :risky t
  :link '(custom-manual :tag "Automatic Code Alignment"
                        "(gpr-ts-mode)Automatic Code Alignment")
  :package-version '(gpr-ts-mode . "0.8.0"))

;;; Alignment Predicates

(defconst gpr-ts-align--keyword-separators
  '("case" "end" "is" "package" "project" "type" "when"))

(defconst gpr-ts-align--keyword-separators-sans-project
  (remove "project" gpr-ts-align--keyword-separators))

(defconst gpr-ts-align--keyword-separators-regexp
  (rx-to-string `(seq symbol-start
                      (or ,@gpr-ts-align--keyword-separators)
                      symbol-end)))

(defun gpr-ts-align--contains-separator-p (beg end)
  "Check if section separator is found in range BEG to END."
  (let ((beg (or (and (markerp beg) (marker-position beg)) beg))
        (end (or (and (markerp end) (marker-position end)) end))
        (case-fold-search t))
    (if (and (null beg) (null end))
        t ; Use separate sections
      (or
       ;; keyword separator
       (save-excursion
         (goto-char beg)
         (condition-case _
             (prog1
                 t ; Separator found in range
               (search-forward-regexp gpr-ts-align--keyword-separators-regexp end)
               (while
                   (when-let* ((node (treesit-node-at (1- (point))))
                               (node-t (treesit-node-type node)))
                     (not
                      (or (member node-t gpr-ts-align--keyword-separators-sans-project)
                          (gpr-ts-mode--project-keyword-p node))))
                 (search-forward-regexp gpr-ts-align--keyword-separators-regexp end)))
           ;; No separator found in range
           (search-failed nil)))
       ;; empty line separator
       (save-excursion
         (goto-char beg)
         (search-forward-regexp
          (rx bol (* (syntax whitespace)) eol) end 'noerror))))))

(defun gpr-ts-align--valid-p ()
  "Determine if alignment location is valid."
  (when-let* ((beg (match-beginning 1))
              (end (match-end 1))
              (node (treesit-node-at beg))
              (node-s (treesit-node-start node))
              (node-e (treesit-node-end node))
              (node-t (treesit-node-type node)))
    (not
     (and (and (<= node-s beg) (>  node-e beg))
          (and (<  node-s end) (>= node-e end))
          (member node-t '("comment" "string_literal"))))))

;;; Commands

(defun gpr-ts-mode-align (&optional arg)
  "Align region or section.

This command dispatches to the following commands:
  - `align-entire': If region is marked and prefix ARG is non-nil,
    corresponding to \\[universal-argument] pressed.
  - `align': If region is marked and prefix ARG is nil.
  - `align-current': If region is not marked."
  (interactive "P" gpr-ts-mode)
  (cond ((and (region-active-p) arg)
         (align-entire (region-beginning) (region-end))
         (deactivate-mark 'force))
        ((region-active-p)
         (align (region-beginning) (region-end))
         (deactivate-mark 'force))
        (t (align-current))))

;;; Alignment Setup

(defun gpr-ts-align--setup ()
  "Setup align command for mode."
  (when gpr-ts-mode-align-region-separate
    (setq-local align-region-separate gpr-ts-mode-align-region-separate))
  (setq-local align-mode-rules-list gpr-ts-mode-align-rules)
  (setq-local align-mode-exclude-rules-list gpr-ts-mode-align-exclude-rules))

;;; Auto-Align Minor Mode

(defun gpr-ts-align--align-current-try (_)
  "Attempt to align the current section at point.

This function returns nil to allow the underlying command associated
with the key binding to be executed.  It is only used for side-effect
purposes to signal when the key is pressed, triggering alignment after
the command completes.  Additionally, a check is performed to make sure
the function is called from within the expected buffer and that
`this-command' is nil in order to prevent key look-ups from triggering
alignment."
  (prog1
      nil
    (when-let* (((null this-command))
                ((derived-mode-p 'gpr-ts-mode))
                ;; Don't bother aligning if point is on a section separator.
                (separate (or (if (and (symbolp align-region-separate)
                                       (boundp align-region-separate))
                                  (symbol-value align-region-separate)
                                align-region-separate)
                              'entire))
                ((save-match-data
                   (not (align-new-section-p (pos-bol) (pos-eol) separate)))))
      ;; Use the idle timer to perform alignment since the command
      ;; associated with the key binding has not yet been executed and
      ;; may cause modifications to the buffer (e.g., indentation).
      ;; The alignment should be performed after those modifications
      ;; have completed.
      ;;
      ;; If the entire alignment section was re-indented due to the
      ;; value of `align-indent-before-aligning', and the key press
      ;; created a separator (e.g., pressing "RET" at the end of a
      ;; line), the line containing point would not be part of the
      ;; alignment section and therefore not re-indented as part of
      ;; that section.  In such a scenario, and when the previous
      ;; line's indentation was determined using best-effort
      ;; indentation due to syntax errors, it's possible that the
      ;; previous line's indentation has changed.  As such, it's
      ;; possible that the current line's indentation is no longer
      ;; accurate (e.g., indentation for an empty line) and should be
      ;; re-indented.  When the current line contains a separator, and
      ;; `align-indent-before-aligning' is not nil, adjust the current
      ;; line's indentation using `indent-according-to-mode'.
      (run-with-idle-timer 0 nil
                           (lambda (buffer marker)
                             (with-current-buffer buffer
                               (save-excursion
                                 (goto-char marker)
                                 (align-current))
                               (set-marker marker nil)
                               (when (and align-indent-before-aligning
                                          (align-new-section-p
                                           (pos-bol) (pos-eol) separate))
                                 (indent-according-to-mode))))
                           (current-buffer)
                           (point-marker)))))

(defvar gpr-ts-auto-align-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key gpr-ts-mode-auto-align-keys)
      (keymap-set map key
                  '(menu-item "" ignore
                              :filter gpr-ts-align--align-current-try)))
    map))

(define-minor-mode gpr-ts-auto-align-mode
  "Minor mode for auto-aligning in GNAT Project buffers."
  :group 'gpr-ts
  :lighter " GPR/a"
  :interactive (gpr-ts-mode))

(provide 'gpr-ts-align)

;;; gpr-ts-align.el ends here
