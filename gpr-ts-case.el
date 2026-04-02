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
(require 'gpr-ts-common)
(require 'rx)
(require 'treesit)

(gpr-ts-mode--declare-treesit-functions)

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

   Dictionary entries take precedence over the formatting function.
   This is an optional property.

   VALUE may be a list of strings whose exact casing is applied to
   candidate words and subwords.

   VALUE may also be a property list, having the form:

      [KEYWORD VALUE]...

   The following keywords are meaningful:

   :words

      VALUE must be a list of strings whose exact casing is applied to
      candidate words and subwords.  This is an optional property.

   :files

      VALUE must be a list of files where the content of each file
      contains a word or subword per line whose exact casing is applied
      to candidate words and subwords.  This is an optional property."
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
             (choice
              :tag "Dictionary"
              (repeat :tag "Words" (string :tag "Word"))
              (plist
               :tag "Words/Files"
               :key-type symbol
               :options
               ((:words (repeat :tag "Words" (string :tag "Word")))
                (:files (repeat :tag "Files" (file :tag "File"))))))))))
  :group 'gpr-ts
  :link '(custom-manual :tag "Casing" "(gpr-ts-mode)Casing")
  :package-version '(gpr-ts-mode . "0.7.0"))

;;;###autoload
(put 'gpr-ts-mode-case-formatting
     'safe-local-variable
     (lambda (rules)
       (while (and (consp rules)
                   (consp (car rules))
                   (not (unsafep (list (plist-get (cdar rules) :formatter)))))
         (setq rules (cdr rules)))
       (null rules)))

;;;; Dictionary Files

(defvar gpr-ts-case--dictionary-file-alist nil)
(defvar gpr-ts-case--formatting nil)

(defun gpr-ts-case--dictionary-load (file)
  "Load dictionary FILE."
  (let (file-words)
    (with-temp-buffer
      (insert-file-contents file)
      (while (not (eobp))
        (if (looking-at (rx bol (* whitespace) eol) 'inhibit-modify)
            (forward-line 1) ; skip empty lines
          (skip-chars-forward " \t")
          (when-let* ((line-words
                       (string-split
                        (buffer-substring-no-properties (pos-bol) (pos-eol))
                        (rx "*") 'omit-nulls (rx (+ whitespace)))))
            (dolist (line-word line-words)
              (unless (assoc-string line-word file-words t)
                (push line-word file-words))))
          (forward-line 1))))
    (setq gpr-ts-case--dictionary-file-alist
          (assoc-delete-all file gpr-ts-case--dictionary-file-alist))
    (push (cons file `( :words ,(reverse file-words)
                        :modification-time ,(file-attribute-modification-time
                                             (file-attributes file))))
          gpr-ts-case--dictionary-file-alist)))

(defun gpr-ts-case--settings-process (symbol newval operation where)
  "Load/Reload dictionary files as needed and compute internal word list.

SYMBOL is expected to be `gpr-ts-mode-case-formatting', and OPERATION is
queried to check that it is a `set' operation (as defined by
`add-variable-watcher'), otherwise nothing is updated.  Either compute
the default or buffer-local value for `gpr-ts-mode-case-formatting'
based on NEWVAL for SYMBOL and any loaded/reloaded dictionaries."
  (when (and (eq symbol 'gpr-ts-mode-case-formatting)
             (eq operation 'set))
    (let (rules)
      (dolist (rule newval)
        (let (words)
          (when-let* ((dictionary (plist-get (cdr rule) :dictionary)))
            (if-let* ((files (plist-get dictionary :files)))
                (dolist (file files)
                  (let* ((file-path (substitute-in-file-name file)))
                    (if (file-name-absolute-p file-path)
                        (setq file-path (expand-file-name file-path))
                      (if-let* ((dir (locate-dominating-file (buffer-file-name) file-path)))
                          (setq file-path (expand-file-name file-path dir))
                        (setq file-path (expand-file-name file-path))))
                    (if (or (not (stringp file-path))
                            (not (file-readable-p file-path)))
                        (message "Cannot read %s, skipping dictionary file." file)
                      (let* ((dictionary-info
                              (cdr (assoc-string
                                    file-path
                                    gpr-ts-case--dictionary-file-alist))))
                        (when (or (not dictionary-info)
                                  (not (equal
                                        (plist-get dictionary-info :modification-time)
                                        (file-attribute-modification-time
                                         (file-attributes file-path)))))
                          (gpr-ts-case--dictionary-load file-path))))
                    (setq words
                          (append words
                                  (plist-get
                                   (cdr (assoc-string
                                         file-path
                                         gpr-ts-case--dictionary-file-alist))
                                   :words)
                                  (plist-get dictionary :words)))))
              (setq words (or (plist-get dictionary :words) dictionary))))
          (let ((new-rule (list (car rule)
                                :formatter (plist-get (cdr rule) :formatter))))
            (when words
              (setq new-rule
                    (append new-rule (list :dictionary words))))
            (push new-rule rules))))
      (setq rules (reverse rules))
      (if where
          (with-current-buffer where
            (setq-local gpr-ts-case--formatting rules))
        (setq-default gpr-ts-case--formatting rules)))))

(gpr-ts-case--settings-process
 'gpr-ts-mode-case-formatting
 (default-value 'gpr-ts-mode-case-formatting)
 'set nil)

(add-variable-watcher
 'gpr-ts-mode-case-formatting
 #'gpr-ts-case--settings-process)

;;;; Word Formatting

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
                    gpr-ts-case--formatting)))
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
  "Attempt to apply case formatting to word before point.

This function returns nil to allow the underlying command associated
with the key binding to be executed.  It is only used for side-effect
purposes to case format the word before point.  Additionally, a check is
performed to make sure the function is called from within the expected
buffer and that `this-command' is nil in order to prevent key look-ups
from triggering case formatting."
  (prog1
      nil
    (when-let* ((last-input last-input-event)
                ((null this-command))
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
                  gpr-ts-case--formatting)))
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
