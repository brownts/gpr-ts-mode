;;; gpr-ts-case.el --- Casing support in GNAT project files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Troy Brown

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

(require 'cl-generic)
(require 'gpr-ts-core)
(require 'rx)
(require 'treesit)

(declare-function treesit-node-end          "treesit.c" (node))
(declare-function treesit-node-eq           "treesit.c" (node1 node2))
(declare-function treesit-node-next-sibling "treesit.c" (node &optional named))
(declare-function treesit-node-start        "treesit.c" (node))
(declare-function treesit-node-type         "treesit.c" (node))

(defcustom gpr-ts-mode-case-formatting
  '((identifier :formatter capitalize
                :dictionary ("ALI" "CWE" "DSA" "GCC" "GNATstub" "GNATtest"
                             "HTML" "IDE" "PIC" "QGen" "URL" "VCS"))
    (keyword    :formatter downcase))
  "Case formatting rules for casing commands and modes.

Each rule should be of the form (CATEGORY . PROPS), where CATEGORY is
the category to which the formatting should be applied.  PROPS should
have the form:

   [KEYWORD VALUE]...

The following keywords are meaningful:

:formatter

   VALUE must be a function which takes a string and returns the
   formatted string.  This is a required property.

:dictionary

   VALUE must be a list of strings whose exact casing is applied to
   words and subwords.  Dictionary entries take precedence over the
   formatting function.  This is an optional property."
  :type '(alist
          :key-type (symbol :tag "Category")
          :value-type
          (plist
           :tag "Properties"
           :key-type symbol
           :options
           ((:formatter
             (choice
              :tag "Function"
              (function-item :tag "Mixed-Case (strict)" capitalize)
              (function-item :tag "Mixed-Case (loose)"  upcase-initials)
              (function-item :tag "Upper-Case"          upcase)
              (function-item :tag "Lower-Case"          downcase)
              (function      :tag "Custom")))
            (:dictionary
             (repeat :tag "Words" (string :tag "Word"))))))
  :group 'gpr-ts
  :link '(custom-manual :tag "Casing" "(gpr-ts-mode)Casing")
  :package-version '(gpr-ts-mode . "0.7.0"))

(defun gpr-ts-case--format-word (beg end formatter &optional dictionary)
  "Apply case formatting to word bounded by BEG and END using FORMATTER.

When words or subwords are found in the DICTIONARY, the formatting in
the DICTIONARY takes precedence over the FORMATTER."
  (let* ((point (point))
         (word (buffer-substring-no-properties beg end))
         (replacement
          (seq-find
           (lambda (item)
             (string-equal-ignore-case word item))
           dictionary)))
    ;; Don't modify the buffer unless necessary.  This allows running
    ;; formatting on an unmodified buffer and if there were no
    ;; formatting changes, the buffer won't show as modified.
    (if replacement
        (when-let* (((not (string-equal replacement word)))
                    (end-marker (set-marker (make-marker) end)))
          ;; apply word replacement
          (goto-char beg)
          (insert replacement)
          (delete-region (point) end-marker))
      (setq replacement (funcall formatter word))
      (when-let* (((not (string-equal replacement word)))
                  (end-marker (set-marker (make-marker) end)))
        ;; apply formatting change
        (goto-char beg)
        (insert replacement)
        (delete-region (point) end-marker))
      (when dictionary
        (setq word (buffer-substring-no-properties beg end))
        (let (subwords)
          (dolist (subword (split-string word "_"))
            (setq subword
                  (or
                   (seq-find
                    (lambda (item)
                      (string-equal-ignore-case subword item))
                    dictionary)
                   subword))
            (push subword subwords))
          (setq subwords (nreverse subwords))
          (setq replacement (string-join subwords "_"))
          (when-let* (((not (string-equal replacement word)))
                      (end-marker (set-marker (make-marker) end)))
            ;; apply subword replacements
            (goto-char beg)
            (insert replacement)
            (delete-region (point) end-marker)))))
    ;; Since we may be changing the content around point, we just
    ;; restore it when we're done.  Since the sum total of the
    ;; characters in the buffer hasn't changed (only the casing), the
    ;; saved position of point is still valid.
    (goto-char point)))

;;; Case Commands

(defun gpr-ts-mode-case-format-region (beg end)
  "Apply case formatting to region bounded by BEG and END."
  (interactive "r" gpr-ts-mode)
  (when-let* ((point
               (save-excursion
                 (goto-char beg)
                 (skip-chars-forward " \t\n" end)
                 (point)))
              (node (treesit-node-at point))
              (node-start (treesit-node-start node))
              (node-end (treesit-node-end node)))
    (while (and node (< node-start end))
      (when-let* ((entry
                   (seq-find
                    (lambda (entry)
                      (gpr-ts-case-category-p (car entry) node))
                    gpr-ts-mode-case-formatting)))
        (gpr-ts-case--format-word
         node-start
         node-end
         (plist-get (cdr entry) :formatter)
         (plist-get (cdr entry) :dictionary)))
      (setq point
            (save-excursion
              (goto-char node-end)
              (skip-chars-forward " \t\n" end)
              (point)))
      (setq node (treesit-node-at point))
      (when node
        (let ((new-start (treesit-node-start node)))
          (if (> new-start node-start)
              (progn
                (setq node-start new-start)
                (setq node-end (treesit-node-end node)))
            (setq node nil)))))))

(defun gpr-ts-mode-case-format-buffer ()
  "Apply case formatting to entire buffer."
  (interactive nil gpr-ts-mode)
  (without-restriction
    (gpr-ts-mode-case-format-region (point-min) (point-max))))

(defun gpr-ts-mode-case-format-at-point ()
  "Apply case formatting at point."
  (interactive nil gpr-ts-mode)
  (gpr-ts-mode-case-format-region (point) (min (1+ (point)) (point-max))))

(defun gpr-ts-mode-case-format-dwim ()
  "Apply case formatting intelligently."
  (interactive nil gpr-ts-mode)
  (if (region-active-p)
      (gpr-ts-mode-case-format-region (region-beginning) (region-end))
    (gpr-ts-mode-case-format-at-point)))

;;; Case Category Predicates

(cl-defgeneric gpr-ts-case-category-p
    (category _node &optional _last-input _pos)
  "Return non-nil if NODE is a member of CATEGORY.

LAST-INPUT is the auto-case triggering character, not yet inserted in
the buffer.  POS represents the buffer location where LAST-INPUT will be
inserted."
  (error "Unknown case category: %s" category))

(defconst gpr-ts-case--keyword-qualifier-regex
  (let* ((qualifiers '("aggregate" "configuration" "library" "standard")))
    (rx-to-string `(: bos (or ,@gpr-ts-mode--keywords ,@qualifiers) eos))))

(defconst gpr-ts-case--keyword-qualifier-project-regex
  (let* ((qualifiers '("aggregate" "configuration" "library" "standard")))
    (rx-to-string `(: bos (or ,@gpr-ts-mode--keywords ,@qualifiers "project") eos))))

(cl-defmethod gpr-ts-case-category-p
  ((_category (eql 'identifier)) node &optional last-input pos)
  "Return non-nil if NODE is a member of the \\='identifier\\=' CATEGORY.

LAST-INPUT is the auto-case triggering character, not yet inserted in
the buffer.  POS represents the buffer location where LAST-INPUT will be
inserted."
  (when-let* ((type (treesit-node-type node)))
    (if (null last-input)
        (or (string-equal type "identifier")
            ;; Consider "Project" prefix as identifier
            (and (string-equal type "project")
                 (when-let* ((next (treesit-node-next-sibling node))
                             (next-type (treesit-node-type next)))
                   (string-equal next-type "'"))))
      (or
       ;; Identifier staying an identifier
       (and (string-equal type "identifier")
            (or (eq last-input ?_)
                (eq last-input ?')
                ;; Check if by inserting the separator, we will be
                ;; creating a keyword.
                (not (string-match-p
                      gpr-ts-case--keyword-qualifier-project-regex
                      (downcase
                       (buffer-substring-no-properties
                        (treesit-node-start node)
                        (min pos (treesit-node-end node))))))
                ;; Looks like a keyword, but check if it's actually an
                ;; attribute name with the same name as a keyword
                ;; (e.g., "External").  Look to see if we follow a
                ;; "for" (attribute declaration) or "'" (attribute
                ;; reference).
                (let* ((prev node)
                       (prev-type (treesit-node-type prev)))
                  (while (and prev
                              (or (treesit-node-eq prev node)
                                  (string-equal prev-type "comment")))
                    (save-excursion
                      (goto-char (treesit-node-start prev))
                      (skip-chars-backward " \t\n")
                      (if (bobp)
                          (setq prev nil)
                        (setq prev (treesit-node-at (1- (point)))
                              prev-type (treesit-node-type prev)))))
                  (and prev
                       (or (string-equal prev-type "for")
                           (string-equal prev-type "'"))))))
       ;; Keyword becoming an identifier
       (and (string-match-p gpr-ts-case--keyword-qualifier-project-regex type)
            (or (eq last-input ?_)
                (eq last-input ?')))))))

(cl-defmethod gpr-ts-case-category-p
  ((_category (eql 'keyword)) node &optional last-input pos)
  "Return non-nil if NODE is a member of the \\='keyword\\=' CATEGORY.

LAST-INPUT is the auto-case triggering character, not yet inserted in
the buffer.  POS represents the buffer location where LAST-INPUT will be
inserted."
  (when-let* ((type (treesit-node-type node)))
    (if (null last-input)
        (or (string-match gpr-ts-case--keyword-qualifier-regex type)
            ;; Don't consider "Project" prefix as keyword
            (and (string-equal type "project")
                 (when-let* ((next (treesit-node-next-sibling node))
                             (next-type (treesit-node-type next)))
                   (not (string-equal next-type "'")))))
      (or
       ;; Keyword staying a keyword
       (and (string-match-p gpr-ts-case--keyword-qualifier-project-regex type)
            (not (eq last-input ?_))
            (not (eq last-input ?')))
       ;; Identifier becoming a keyword
       (and (string-equal type "identifier")
            (not (eq last-input ?_))
            (not (eq last-input ?'))
            ;; Check if by inserting the separator, a keyword will be
            ;; created.
            (string-match-p
             gpr-ts-case--keyword-qualifier-project-regex
             (downcase
              (buffer-substring-no-properties
               (treesit-node-start node)
               (min pos (treesit-node-end node)))))
            ;; Looks like a keyword, but check if it's actually an
            ;; attribute name with the same name as a keyword (e.g.,
            ;; "External").  Look to see if we follow a "for"
            ;; (attribute declaration) or "'" (attribute reference).
            (let* ((prev node)
                   (prev-type (treesit-node-type prev)))
              (while (and prev
                          (or (treesit-node-eq prev node)
                              (string-equal prev-type "comment")))
                (save-excursion
                  (goto-char (treesit-node-start prev))
                  (skip-chars-backward " \t\n")
                  (if (bobp)
                      (setq prev nil)
                    (setq prev (treesit-node-at (1- (point)))
                          prev-type (treesit-node-type prev)))))
              (or (null prev)
                  (and (not (string-equal prev-type "for"))
                       (not (string-equal prev-type "'"))))))))))

;;; Auto-Case Minor Mode

(defun gpr-ts-case--format-word-try (_)
  "Attempt to apply case formatting to word before point."
  (prog1
      nil ; return nil so overlaid keybinding triggers
    (when-let* ((last-input last-input-event)
                ;; Prevent key lookups from outside the buffer from
                ;; triggering case formatting
                ((derived-mode-p 'gpr-ts-mode))
                ((not (bobp)))
                (prev-point (1- (point)))
                (node (treesit-node-at prev-point))
                ;; Ensure not in whitespace
                ((and (<= (treesit-node-start node) prev-point)
                      (< prev-point (treesit-node-end node))))
                (entry
                 (seq-find
                  (lambda (entry)
                    (gpr-ts-case-category-p (car entry) node last-input (point)))
                  gpr-ts-mode-case-formatting)))
      ;; Point might be in the middle of a word and therefore about to
      ;; separate it into two words by the yet-to-be-inserted
      ;; key-press.  Only apply formatting before point.  The category
      ;; predicate already took this into consideration when
      ;; determining the category.
      (gpr-ts-case--format-word
       (treesit-node-start node)
       (min (point) (treesit-node-end node))
       (plist-get (cdr entry) :formatter)
       (plist-get (cdr entry) :dictionary)))))

(defvar gpr-ts-auto-case-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key '("RET" "SPC" "_" "&" "(" ")" "=" "|" ";" ":" "'" "\"" "," "." ">"))
      (define-key map (kbd key)
                  `(menu-item "" ignore
                              :filter gpr-ts-case--format-word-try)))
    map))

(define-minor-mode gpr-ts-auto-case-mode
  "Minor mode for auto-casing in GNAT Project buffers."
  :group 'gpr-ts
  :lighter " GPR/c"
  :interactive (gpr-ts-mode))

(provide 'gpr-ts-case)

;;; gpr-ts-case.el ends here
