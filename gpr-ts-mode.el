;;; gpr-ts-mode.el --- Major mode for GNAT project files using Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Troy Brown

;; Author: Troy Brown <brownts@troybrown.dev>
;; Created: February 2023
;; Version: 0.6.7
;; Keywords: gpr gnat ada languages tree-sitter
;; URL: https://github.com/brownts/gpr-ts-mode
;; Package-Requires: ((emacs "29.1"))

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

;; This package provides GNAT Project syntax highlighting, indentation
;; and navigation using Tree-Sitter.  To use the `gpr-ts-mode' major
;; mode you will need the appropriate grammar installed.  By default,
;; on mode startup if the grammar is not detected, you will be
;; prompted to automatically install it.

;;; Code:

(require 'cl-generic)
(require 'lisp-mnt)
(require 'treesit)
(eval-when-compile (require 'rx))

(declare-function treesit-available-p "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-language-available-p "treesit.c")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-check "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-child-count "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-eq "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defgroup gpr-ts nil
  "Major mode for GNAT Project files, using Tree-Sitter."
  :group 'languages
  :link '(emacs-library-link :tag "Source" "gpr-ts-mode.el")
  :link `(url-link :tag "Website"
                   ,(lm-website (locate-library "gpr-ts-mode.el")))
  :link '(custom-manual "(gpr-ts-mode)Top")
  :prefix "gpr-ts-mode-")

(defcustom gpr-ts-mode-imenu-categories
  '(attribute package type variable with-clause)
  "Configuration of Imenu categories."
  :type '(repeat :tag "Categories"
                 (choice :tag "Category"
                         (const :tag "Attribute Declaration" attribute)
                         (const :tag "Package Declaration" package)
                         (const :tag "Project Declaration" project)
                         (const :tag "Type Declaration" type)
                         (const :tag "Typed Variable Declaration" typed-variable)
                         (const :tag "Untyped Variable Declaration" untyped-variable)
                         (const :tag "Typed and Untyped Variable Declaration" variable)
                         (const :tag "With Clause" with-clause)))
  :group 'gpr-ts
  :link '(custom-manual :tag "Imenu" "(gpr-ts-mode)Imenu")
  :package-version "0.6.0")

(defcustom gpr-ts-mode-imenu-category-name-alist
  '((attribute        . "Attribute")
    (package          . "Package")
    (project          . "Project")
    (type             . "Type")
    (typed-variable   . "Typed Variable")
    (untyped-variable . "Untyped Variable")
    (variable         . "Variable")
    (with-clause      . "With Clause"))
  "Configuration of Imenu category names."
  :type '(alist :key-type   (symbol :tag "Category")
                :value-type (string :tag "Category Name"))
  :group 'gpr-ts
  :link '(custom-manual :tag "Imenu" "(gpr-ts-mode)Imenu")
  :package-version "0.6.0")

(defcustom gpr-ts-mode-indent-offset 3
  "Indentation of statements."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")
;;;###autoload(put 'gpr-ts-mode-indent-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-when-offset gpr-ts-mode-indent-offset
  "Indentation of `when' relative to `case'."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")
;;;###autoload(put 'gpr-ts-mode-indent-when-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-broken-offset (- gpr-ts-mode-indent-offset 1)
  "Indentation for the continuation of a broken line."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")
;;;###autoload(put 'gpr-ts-mode-indent-broken-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-exp-item-offset (- gpr-ts-mode-indent-offset 1)
  "Indentation for the continuation of an expression."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.5.0")
;;;###autoload(put 'gpr-ts-mode-indent-exp-item-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-strategy 'declaration
  "Indentation strategy to utilize."
  :type '(choice :tag "Indentation Strategy"
                 (const :tag "Declaration" declaration)
                 (const :tag "Line" line))
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version "0.6.0")

(defcustom gpr-ts-mode-grammar "https://github.com/brownts/tree-sitter-gpr"
  "Configuration for downloading and installing the tree-sitter language grammar.

Additional settings beyond the git repository can also be
specified.  See `treesit-language-source-alist' for full details."
  :type '(choice (string :tag "Git Repository")
                 (list :tag "All Options"
                       (string :tag "Git Repository")
                       (choice :tag "Revision" (const :tag "Default" nil) string)
                       (choice :tag "Source Directory" (const :tag "Default" nil) string)
                       (choice :tag "C Compiler" (const :tag "Default" nil) string)
                       (choice :tag "C++ Compiler" (const :tag "Default" nil) string)))
  :group 'gpr-ts
  :link '(custom-manual :tag "Grammar Installation" "(gpr-ts-mode)Grammar Installation")
  :package-version "0.5.0")

(defcustom gpr-ts-mode-grammar-install 'prompt
  "Configuration for installation of tree-sitter language grammar library."
  :type '(choice (const :tag "Automatically Install" auto)
                 (const :tag "Prompt to Install" prompt)
                 (const :tag "Do not install" nil))
  :group 'gpr-ts
  :link '(custom-manual :tag "Grammar Installation" "(gpr-ts-mode)Grammar Installation")
  :package-version "0.5.0")

(defcustom gpr-ts-mode-package-names
  '("binder" "builder" "check" "clean" "compiler" "cross_reference"
    "documentation" "eliminate" "finder" "format" "gnatls" "gnatstub"
    "ide" "install" "linker" "metrics" "naming" "pretty_printer"
    "remote" "stack" "synchronize")
  "List of known package names."
  :type '(repeat string)
  :group 'gpr-ts
  :link '(custom-manual :tag "Syntax Highlighting" "(gpr-ts-mode)Syntax Highlighting")
  :package-version "0.6.0")
;;;###autoload(put 'gpr-ts-mode-package-names 'safe-local-variable #'list-of-strings-p)

(defvar gpr-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?&  "."    table)
    (modify-syntax-entry ?\| "."    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?\' "."    table)
    (modify-syntax-entry ?\\ "."    table)
    (modify-syntax-entry ?\n ">"    table)
    table)
  "Syntax table for `gpr-ts-mode'.")

(defun gpr-ts-mode--indent-recompute (symbol newval operation where)
  "Recompute indentation variables when SYMBOL is changed.

SYMBOL is expected to be `gpr-ts-mode-indent-offset', and
OPERATION is queried to check that it is a `set' operation (as
defined by `add-variable-watcher'), otherwise nothing is updated.
Assuming the global value has not been updated by the user, the
indentation variables are updated using the NEWVAL of SYMBOL and
made buffer-local WHERE indicates a buffer-local modification of
SYMBOL, else the default value is updated instead."
  (when (and (eq symbol 'gpr-ts-mode-indent-offset)
             (eq operation 'set))
    (dolist (indent-symbol '(gpr-ts-mode-indent-when-offset
                             gpr-ts-mode-indent-broken-offset
                             gpr-ts-mode-indent-exp-item-offset))
      (let* ((valspec (or (custom-variable-theme-value indent-symbol)
                          (get indent-symbol 'standard-value)))
             (cur-custom-value (eval (car valspec)))
             ;; This routine is invoked before SYMBOL is updated to
             ;; NEWVAL so we need to bind it to the new value so the
             ;; other indentation variables are evaluated using the
             ;; updated value.
             (gpr-ts-mode-indent-offset newval)
             (new-custom-value (eval (car valspec))))
        ;; Only update if not globally modified by the user outside of
        ;; the customization system (e.g., via `set-default'), or the
        ;; symbol is already buffer local.
        (when (or (eql cur-custom-value (default-value indent-symbol))
                  (and where (buffer-local-boundp indent-symbol where)))
          (if where
              (with-current-buffer where
                (set (make-local-variable indent-symbol) new-custom-value))
            (set-default indent-symbol new-custom-value)))))))

(add-variable-watcher 'gpr-ts-mode-indent-offset #'gpr-ts-mode--indent-recompute)

(defvar gpr-ts-mode--keywords
  '("abstract" "all" "at"
    "case"
    "end" "extends" "external" "external_as_list"
    "for"
    "is"
    "limited"
    "null"
    "others"
    "package" ;"project"
    "renames"
    "type"
    "use"
    "when" "with")
  "GPR keywords for tree-sitter font-locking.")

(defun gpr-ts-mode--anchor-first-sibling-matching (type)
  "Position of first sibling of node whose type matches TYPE."
  (lambda (_n parent &rest _)
    (treesit-node-start
     (car
      (treesit-filter-child
       parent
       (lambda (n)
         (equal (treesit-node-type n) type)))))))

(defun gpr-ts-mode--after-first-sibling-p (sibling)
  "Determine if the location of node comes after SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let* ((sibling-node
               (gpr-ts-mode--first-child-matching parent sibling)))
        (< (treesit-node-start sibling-node) bol))))

(defun gpr-ts-mode--before-first-sibling-p (sibling)
  "Determine if the location of node comes before SIBLING."
  (lambda (_node parent bol &rest _)
    (if-let* ((sibling-node
               (gpr-ts-mode--first-child-matching parent sibling)))
        (> (treesit-node-start sibling-node) bol)
      ;; if it doesn't exist, node is before it
      t)))

(defun gpr-ts-mode--between-siblings-p (first-sibling last-sibling)
  "Determine if node is between FIRST-SIBLING and LAST-SIBLING."
  (lambda (node parent bol &rest _)
    (let ((after (gpr-ts-mode--after-first-sibling-p first-sibling))
          (before (gpr-ts-mode--before-first-sibling-p last-sibling)))
      (and (funcall after node parent bol)
           (funcall before node parent bol)))))

(defun gpr-ts-mode--sibling-exists-p (sibling)
  "Determine if SIBLING exists."
  (lambda (_node parent _bol &rest _)
    (gpr-ts-mode--first-child-matching parent sibling)))

(defun gpr-ts-mode--next-sibling (node parent bol &rest _)
  "Determine next sibling in PARENT after this NODE or BOL."
  (if node
      (treesit-node-next-sibling node)
    (car
     (treesit-filter-child
      parent
      (lambda (n)
        (> (treesit-node-start n) bol))))))

(defalias 'gpr-ts-mode--next-sibling-exists-p
  'gpr-ts-mode--next-sibling)

(defun gpr-ts-mode--next-sibling-not-matching (type)
  "Locate next sibling not matching TYPE."
  (lambda (node parent bol &rest _)
    (let ((sibling-node (gpr-ts-mode--next-sibling node parent bol)))
      (while (and sibling-node
                  (equal (treesit-node-type sibling-node) type))
        (setq sibling-node (treesit-node-next-sibling sibling-node)))
      sibling-node)))

(defalias 'gpr-ts-mode--next-sibling-not-matching-exists-p
  'gpr-ts-mode--next-sibling-not-matching)

(defun gpr-ts-mode--anchor-of-next-sibling-not-matching (type)
  "Determine indentation anchor of next sibling not matching TYPE."
  (lambda (node parent bol &rest _)
    (let ((sibling-node
           (funcall (gpr-ts-mode--next-sibling-not-matching type) node parent bol)))
      (car (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun gpr-ts-mode--prev-sibling (node parent bol &rest _)
  "Determine previous sibling in PARENT before this NODE or BOL."
  (if node
      (treesit-node-prev-sibling node)
    (car
     (reverse
      (treesit-filter-child
       parent
       (lambda (n)
         (< (treesit-node-start n) bol)))))))

(defun gpr-ts-mode--prev-sibling-matches-p (type)
  "Check if previous sibling matches TYPE."
  (lambda (node parent bol &rest _)
    (if-let* ((prev (gpr-ts-mode--prev-sibling node parent bol)))
        (string-equal (treesit-node-type prev) type))))

(defun gpr-ts-mode--prev-nonextra-sibling-matches-p (type)
  "Check if previous non-extra sibling matches TYPE."
  (lambda (node parent bol &rest _)
    (let ((prev (gpr-ts-mode--prev-sibling node parent bol)))
      (while (and prev (treesit-node-check prev 'extra))
        (setq prev (treesit-node-prev-sibling prev)))
      (when prev
        (string-equal (treesit-node-type prev) type)))))

(defun gpr-ts-mode--enclosing-compound (compound-info pos)
  "Find enclosing compound, specified in COMPOUND-INFO, at POS.

COMPOUND-INFO is a list of compound node information.  Each entry should
be of the form (FIRST-CHILD-NODE-TYPE . PROPS), where
FIRST-CHILD-NODE-TYPE is the type of the first child node of the
compound node.  PROPS should have the form:

   [KEYWORD VALUE]...

The following keywords are meaningful:

:compound-type

   VALUE must be the type of the compound node.  This is a required
   property.

:predicate

   VALUE must be a function which takes a candidate node (matching
   FIRST-CHILD-NODE-TYPE) as well as the node at POS, if one exists, and
   returns non-nil if the candidate is appropriate.  This is an optional
   property.

The returned node is the closest previous node matching
FIRST-CHILD-NODE-TYPE which is a child of the specified compound node
type and the compound node must enclose POS.

The returned node may also be the closest previous node matching
FIRST-CHILD-NODE-TYPE which is not a child of the specified compound
node.  This is a suspected incomplete compound.  When a predicate is
provided, it is used to disambiguate any usage of FIRST-CHILD-NODE-TYPE
elsewhere in the grammar and must return non-nil for the node to be
chosen."
  (let* ((matches nil)
         (node
          (save-excursion
            (goto-char pos)
            (if-let* ((node (treesit-node-at pos)))
                (and (<= (treesit-node-start node) pos)
                     (> (treesit-node-end node) pos)
                     node))))
         (parent
          (if node
              (treesit-node-parent node)
            (treesit-node-on pos pos))))
    (while (and parent (not matches))
      (setq matches
            (treesit-induce-sparse-tree
             parent
             (lambda (candidate)
               (when-let* (((< (treesit-node-start candidate) pos))
                           (type (treesit-node-type candidate))
                           (parent (treesit-node-parent candidate))
                           (parent-type (treesit-node-type parent))
                           (compound (cdr (assoc-string type compound-info)))
                           (compound-type (plist-get compound :compound-type)))
                 (if (string-equal parent-type compound-type)
                     (and (<= (treesit-node-start parent) pos)
                          (< pos (treesit-node-end parent)))
                   (let ((predicate (plist-get compound :predicate)))
                     (or (null predicate)
                         (funcall predicate candidate node))))))))
      (setq parent (treesit-node-parent parent)))
    (when matches
      (caar (reverse matches)))))

(defun gpr-ts-mode--indent-error-recovery (op)
  "Find indentation in the presence of syntax errors.

If OP is \\='anchor\\=', determine anchor.  If OP is \\='offset\\=',
determine offset."
  (lambda (node _parent bol &rest _)
    (let ((node-type
           (let ((type (treesit-node-type node)))
             (when (and type (string-equal type "ERROR"))
               ;; Replace ERROR node with leaf node.
               (setq node (treesit-node-at (treesit-node-start node)))
               (setq type (treesit-node-type node)))
             type))
          anchor-node anchor-type)
      ;; look for enclosing parenthesis or declaration.
      (let (force-compound prev-node prev-type)
        (save-excursion
          (goto-char bol)
          (while (and (not anchor-node)
                      (not force-compound)
                      (not (bobp)))
            (when prev-node
              (goto-char (treesit-node-start prev-node)))
            (skip-chars-backward " \t\n" (point-min))
            (setq prev-node (if (bobp) nil (treesit-node-at (1- (point))))
                  prev-type (treesit-node-type prev-node))
            (cond
             ;; parenthesis
             ((and prev-type (string-equal prev-type "("))
              (let ((end
                     (ignore-error scan-error
                       (scan-lists (treesit-node-start prev-node) 1 0))))
                (when (or (null end)
                          (> end bol))
                  ;; End parenthesis either not found (i.e.,
                  ;; unbalanced) or it was after bol, so it encloses
                  ;; bol.
                  (if (and node (member node-type '("project" "package" "case")))
                      (setq force-compound t) ; Recovery point
                    (setq anchor-node prev-node
                          anchor-type prev-type)))))
             ;; declaration
             ((and (or (null prev-type)
                       (member prev-type '(";" "=>"))
                       (and (string-equal prev-type "is")
                            ;; Only consider the "is" keyword as a
                            ;; declaration separator when it's part of a
                            ;; compound (e.g., project_declaration),
                            ;; otherwise ignore it when it isn't (e.g.,
                            ;; typed_string_declaration).
                            (let ((keywords (cons "project" gpr-ts-mode--keywords))
                                  (compound-keywords '("project" "package" "case"))
                                  (keyword-node prev-node)
                                  (keyword-type prev-type))
                              (save-excursion
                                (while (and keyword-node
                                            (or (treesit-node-eq keyword-node prev-node)
                                                (not (member keyword-type keywords))))
                                  (goto-char (treesit-node-start keyword-node))
                                  (skip-chars-backward " \t\n")
                                  (if (bobp)
                                      (setq keyword-node nil)
                                    (setq keyword-node (treesit-node-at (1- (point)))
                                          keyword-type (treesit-node-type keyword-node)))))
                              (and keyword-node
                                   (member keyword-type compound-keywords))))))
              (let (next-node next-type)
                (save-excursion
                  (skip-chars-forward " \t\n")
                  (setq next-node (if (eobp) nil (treesit-node-at (point)))
                        next-type (treesit-node-type next-node))
                  (while (and next-type
                              (string-equal next-type "comment"))
                    (goto-char (treesit-node-end next-node))
                    (skip-chars-forward " \t\n")
                    (setq next-node (if (eobp) nil (treesit-node-at (point)))
                          next-type (treesit-node-type next-node)))
                  (if next-node
                      (if (>= (treesit-node-start next-node) bol)
                          ;; bol is either a newline or a comment,
                          ;; so anchor to the compound.
                          (setq force-compound t)
                        (setq anchor-node next-node
                              anchor-type next-type))
                    (setq force-compound t)))))))))
      ;; look for enclosing compound
      (unless anchor-node
        (setq anchor-node
              (gpr-ts-mode--enclosing-compound
               '(("project" . ( :compound-type "project_declaration"
                                :predicate
                                ;; Don't anchor to a "project" keyword
                                ;; used in an attribute reference.
                                (lambda (anchor &optional _node)
                                  (let* ((next (treesit-node-next-sibling anchor))
                                         (next-type (treesit-node-type next)))
                                    (or (null next-type)
                                        (not (string-equal next-type "'")))))))
                 ("package" . ( :compound-type "package_declaration"
                                :predicate
                                ;; Don't allow a nested "package".
                                ;; This is an indication of an
                                ;; incomplete package declaration
                                ;; prior to the start of another
                                ;; package declaration.
                                (lambda (_anchor &optional node)
                                  (or (null node)
                                      (let ((type (treesit-node-type node)))
                                        (not (string-equal type "package")))))))
                 ("case"    . ( :compound-type "case_construction"))
                 ("when"    . ( :compound-type "case_item")))
               bol)
              anchor-type (treesit-node-type anchor-node)))
      (if (null anchor-node)
          (pcase op
            ((or (pred null) 'anchor)
             (when treesit--indent-verbose
               (message "Error recovery anchor: %s" anchor-node))
             (pos-bol))
            ('offset 0))
        (pcase op
          ((or (pred null) 'anchor)
           (when treesit--indent-verbose
             (message "Error recovery anchor: %s" anchor-node))
           (if (string-equal anchor-type "project")
               ;; Anchor at beginning of line to account for possible
               ;; qualifier prefixes (e.g., "abstract")
               (save-excursion
                 (goto-char (treesit-node-start anchor-node))
                 (forward-line 0)
                 (treesit-node-start (treesit-node-at (point))))
             (treesit-node-start anchor-node)))
          ('offset
           ;; Determine nodes after anchor and before bol
           (let ((zero-offset-types
                  (append '(";" "project")
                          (seq-difference
                           gpr-ts-mode--keywords
                           '("external" "external_as_list"))))
                 nodes-before)
             (save-excursion
               (goto-char (treesit-node-end anchor-node))
               (while (< (point) bol)
                 (skip-chars-forward " \t\n" bol)
                 (when (< (point) bol)
                   (let ((node (treesit-node-at (point))))
                     ;; Find largest complete node starting at point.
                     (setq node
                           (or
                            (treesit-parent-while
                             node
                             (lambda (n)
                               (and
                                (not (string-equal (treesit-node-type n) "ERROR"))
                                (= (treesit-node-start n) (point)))))
                            node))
                     (push (treesit-node-type node) nodes-before)
                     (goto-char (treesit-node-end node))))))
             (setq nodes-before (reverse nodes-before))
             (pcase anchor-type
               ("when"
                (cond
                 ((member "=>" nodes-before) gpr-ts-mode-indent-offset)
                 (t gpr-ts-mode-indent-broken-offset)))
               ("case"
                (cond
                 ((and (member "is" nodes-before)
                       (not (member "end" nodes-before))
                       (or (null node) (not (string-equal node-type "end"))))
                  gpr-ts-mode-indent-when-offset)
                 ((and node (member node-type zero-offset-types))
                  0)
                 (t gpr-ts-mode-indent-broken-offset)))
               ((or "package" "project")
                (cond
                 ((and (member "is" nodes-before)
                       (not (member "end" nodes-before))
                       (or (null node) (not (string-equal node-type "end"))))
                  gpr-ts-mode-indent-offset)
                 ((and node (member node-type zero-offset-types))
                  0)
                 (t gpr-ts-mode-indent-broken-offset)))
               ("("
                (if (and node-type (string-equal node-type ")"))
                    0
                  (let ((next-node anchor-node))
                    (while (and next-node
                                (or (treesit-node-eq next-node anchor-node)
                                    ;; skip comments
                                    (string-equal (treesit-node-type next-node) "comment")))
                      (save-excursion
                        (goto-char (treesit-node-end next-node))
                        (skip-chars-forward " \t\n" bol)
                        (if (>= (point) bol)
                            (setq next-node nil)
                          (setq next-node (treesit-node-at (point))))))
                    (if next-node
                        (let ((anchor-column
                               (save-excursion
                                 (goto-char (treesit-node-start anchor-node))
                                 (current-column))))
                          (save-excursion
                            (goto-char (treesit-node-start next-node))
                            (- (current-column) anchor-column)))
                      1))))
               (_ ; declaration
                (if (and node (member node-type zero-offset-types))
                    0
                  gpr-ts-mode-indent-broken-offset))))))))))

(defun gpr-ts-mode--anchor-of-indent-error-recovery ()
  "Determine indentation anchor of error recovery point."
  (gpr-ts-mode--indent-error-recovery 'anchor))

(defun gpr-ts-mode--offset-of-indent-error-recovery ()
  "Determine indentation offset of error recovery point."
  (gpr-ts-mode--indent-error-recovery 'offset))

(defun gpr-ts-mode--offset-of-next-sibling-not-matching (type)
  "Determine indentation offset of next sibling not matching TYPE."
  (lambda (node parent bol &rest _)
    (let ((sibling-node
           (funcall (gpr-ts-mode--next-sibling-not-matching type) node parent bol)))
      (cdr (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun gpr-ts-mode--anchor-next-sibling (node parent bol &rest _)
  "Determine location of next sibling after NODE in PARENT or BOL."
  (treesit-node-start
   (gpr-ts-mode--next-sibling node parent bol)))

(defun gpr-ts-mode--anchor-prev-sibling-matching (sibling-type)
  "Locate previous sibling matching SIBLING-TYPE."
  (lambda (_node parent bol &rest _)
    (treesit-node-start
     (car
      (last
       (treesit-filter-child
        parent
        (lambda (n)
          (and
           (equal (treesit-node-type n) sibling-type)
           (< (treesit-node-start n) bol)))))))))

(defun gpr-ts-mode--first-child-matching (parent type)
  "Find first child of PARENT matching TYPE.
Return nil if no child of that type is found."
  ;; NOTE: `treesit-filter-child' uses `treesit-node-next-sibling'
  ;; which doesn't traverse parser-inserted "missing" nodes (seems
  ;; like a bug), so filter the nodes manually.
  (seq-find
   (lambda (n)
     (string-equal (treesit-node-type n) type))
   (treesit-node-children parent)))

(cl-defgeneric gpr-ts-mode-indent (strategy)
  "Indent according to STRATEGY."
  (error "Unknown indentation strategy: %s" strategy))

(cl-defmethod gpr-ts-mode-indent ((_strategy (eql line)))
  "Indent according to line STRATEGY."
  (treesit-indent))

(cl-defmethod gpr-ts-mode-indent ((_strategy (eql declaration)))
  "Indent according to declaration STRATEGY."
  (let ((initial-point-column (current-column))
        (initial-indentation-column (current-indentation))
        (region
         (save-excursion
           (forward-line 0)
           (skip-chars-forward " \t")
           (unless (looking-at (rx (* whitespace) eol) t)
             (let* ((node (treesit-node-at (point)))
                    (root (treesit-buffer-root-node))
                    (candidate
                     (treesit-parent-until
                      node
                      (lambda (node)
                        (or (treesit-node-eq node root)
                            (string-equal (treesit-node-type node) "ERROR")
                            (gpr-ts-mode--declaration-p node)))
                      'include-node)))
               (when (and (gpr-ts-mode--declaration-p candidate)
                          (not (treesit-search-subtree candidate "ERROR")))
                 ;; Prevent interpreting a project declaration "end"
                 ;; next to an incomplete package declaration as a
                 ;; valid package declaration by checking if the names
                 ;; match.
                 (unless
                     (and (gpr-ts-mode--package-declaration-p candidate)
                          (not (gpr-ts-mode--package-declaration-names-match-p candidate)))
                   (cons (treesit-node-start candidate)
                         (treesit-node-end candidate)))))))))
    (if region
        (progn
          (treesit-indent-region (car region) (cdr region))
          ;; Move point if it was in the indentation.
          (when (<= initial-point-column
                    initial-indentation-column)
            (back-to-indentation)))
      (treesit-indent))))

(defun gpr-ts-mode--indent-line ()
  "Indent according to `gpr-ts-mode-indent-strategy'."
  (gpr-ts-mode-indent gpr-ts-mode-indent-strategy))

(defvar gpr-ts-mode--indent-rules
  `((gpr

     ;; Non-Parent-driven indentation

     ;; Indent empty lines immediately following a case_item as part
     ;; of the case_item.  This allows additional lines to keep being
     ;; added to the case_item without causing indentation to jump
     ;; after each newline.
     ((and no-node
           (gpr-ts-mode--prev-nonextra-sibling-matches-p "case_item"))
      (gpr-ts-mode--anchor-first-sibling-matching "case_item")
      gpr-ts-mode-indent-offset)

     ;; Error recovery rules.

     ((or (parent-is "ERROR")
          (node-is "ERROR")
          (gpr-ts-mode--prev-sibling-matches-p "ERROR"))
      (gpr-ts-mode--anchor-of-indent-error-recovery)
      (gpr-ts-mode--offset-of-indent-error-recovery))

     ;; Normal indentation rules.

     ((and (parent-is "case_construction")
           (gpr-ts-mode--between-siblings-p "is" "end"))
      parent
      gpr-ts-mode-indent-when-offset)
     ;; general indentation for package and project declaration bodies.
     ((and (or (parent-is "project_declaration")
               (parent-is "package_declaration"))
           (gpr-ts-mode--between-siblings-p "is" "end"))
      parent
      gpr-ts-mode-indent-offset)
     ;; general indentation for comment.
     ;;
     ;; NOTE: Indent to where next non-comment sibling would be
     ;; indented.  This may not be aligned to sibling if sibling isn't
     ;; properly indented, however it prevents a two-pass indentation
     ;; when region is indented, since comments won't have to be
     ;; reindented once sibling becomes properly aligned.
     ((and (node-is "comment")
           (gpr-ts-mode--next-sibling-not-matching-exists-p "comment"))
      (gpr-ts-mode--anchor-of-next-sibling-not-matching "comment")
      (gpr-ts-mode--offset-of-next-sibling-not-matching "comment"))
     ((and (node-is ")")
           (gpr-ts-mode--sibling-exists-p "("))
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      0)
     ;; top-level
     ((parent-is ,(rx bos "project" eos)) column-0 0)
     ;; with_declaration
     ((and (parent-is "with_declaration")
           (gpr-ts-mode--between-siblings-p "with" ";")
           (gpr-ts-mode--after-first-sibling-p "string_literal"))
      (gpr-ts-mode--anchor-first-sibling-matching "string_literal")
      0)
     ;; expression / expression_list
     ((and (parent-is "expression_list")
           (gpr-ts-mode--after-first-sibling-p "expression"))
      (gpr-ts-mode--anchor-first-sibling-matching "expression")
      0)
     ((parent-is "expression_list")
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      1)
     ((parent-is ,(rx bos "expression" eos))
      parent
      gpr-ts-mode-indent-exp-item-offset)
     ((or (parent-is ,(rx bos "project_reference" eos))
          (parent-is ,(rx bos "variable_reference" eos)))
      parent
      0)
     ((node-is "expression_list")
      parent
      gpr-ts-mode-indent-broken-offset)
     ((node-is ,(rx bos "expression" eos))
      parent
      gpr-ts-mode-indent-broken-offset)
     ;; typed_string_declaration
     ((and (parent-is "typed_string_declaration")
           (gpr-ts-mode--between-siblings-p "(" ")")
           (gpr-ts-mode--after-first-sibling-p "string_literal"))
      (gpr-ts-mode--anchor-first-sibling-matching "string_literal")
      0)
     ((and (parent-is "typed_string_declaration")
           (gpr-ts-mode--between-siblings-p "(" ")"))
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      1)
     ;; attribute_reference / attribute_declaration
     ((and (or (parent-is "attribute_reference")
               (parent-is "attribute_declaration"))
           (gpr-ts-mode--between-siblings-p "(" ")"))
      (gpr-ts-mode--anchor-first-sibling-matching "(")
      1)
     ;; attribute_declaration
     ((and (parent-is "attribute_declaration")
           (node-is "("))
      (gpr-ts-mode--anchor-first-sibling-matching "identifier")
      gpr-ts-mode-indent-broken-offset)
     ;; case_construction / case_item / discrete_choice_list
     ((parent-is "discrete_choice_list")
      parent
      0)
     ((node-is "discrete_choice_list")
      parent
      gpr-ts-mode-indent-broken-offset)
     ((parent-is "case_item")
      parent-bol
      gpr-ts-mode-indent-offset)
     ((match "variable_reference" "case_construction")
      parent-bol
      gpr-ts-mode-indent-broken-offset)
     ;; general indentation for newline.
     ;;
     ;; NOTE: Indent to where next non-comment sibling would be
     ;; indented.  This may not be aligned to sibling if sibling isn't
     ;; properly indented, however it prevents a two-pass indentation
     ;; when region is indented, since comments won't have to be
     ;; reindented once sibling becomes properly aligned.
     ((and no-node
           (gpr-ts-mode--next-sibling-not-matching-exists-p "comment"))
      (gpr-ts-mode--anchor-of-next-sibling-not-matching "comment")
      (gpr-ts-mode--offset-of-next-sibling-not-matching "comment"))
     ;; variable_declaration
     ((node-is ,(rx bos (or ":" ":=") eos))
      parent
      gpr-ts-mode-indent-broken-offset)
     ;; name (e.g., identifier . identifier)
     ((parent-is ,(rx bos "name" eos))
      parent
      0)
     ;; name, identifier, string_literal and string_literal_at.
     ((or (node-is ,(rx bos "name" eos))
          (node-is "identifier")
          (node-is "string_literal")
          (parent-is "string_literal_at"))
      parent
      gpr-ts-mode-indent-broken-offset)
     ;; keywords / semicolon / project / project qualifier
     ((or (node-is ,(eval `(rx bos (or ,@gpr-ts-mode--keywords ";") eos)))
          (match ,(rx bos "project" eos) "project_declaration")
          (parent-is "project_qualifier"))
      parent
      0)
     ;; non-expression opening parenthesis
     ((node-is "(") parent gpr-ts-mode-indent-broken-offset)
     ;; trivial recovery for unexpected broken line
     (catch-all prev-line 0)))
  "Tree-sitter indent rules for `gpr-ts-mode'.")

(defvar gpr-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; Attributes
   :language 'gpr
   :feature 'attribute
   '((attribute_reference (identifier) @font-lock-property-use-face))

   ;; Brackets
   :language 'gpr
   :feature 'bracket
   '((["(" ")"]) @font-lock-bracket-face)

   ;; Comments
   :language 'gpr
   :feature 'comment
   '((comment) @font-lock-comment-face)

   ;; Definition
   :language 'gpr
   :feature 'definition
   '((package_declaration name: (identifier) @font-lock-function-name-face)
     ((package_declaration endname: (identifier) @font-lock-function-name-face)
      @package-declaration
      (:pred gpr-ts-mode--package-declaration-names-match-p @package-declaration))
     (typed_string_declaration name: (identifier) @font-lock-type-face)
     (variable_declaration name: (identifier) @font-lock-variable-name-face)
     (attribute_declaration name: (identifier) @font-lock-property-name-face))

   ;; Delimiters
   :language 'gpr
   :feature 'delimiter
   '(["," "." ":" ";"] @font-lock-delimiter-face)

   ;; Functions
   :language 'gpr
   :feature 'function
   :override 'prepend
   '((builtin_function_call name: _ @font-lock-function-call-face))

   ;; Keywords
   :language 'gpr
   :feature 'keyword
   `(([,@gpr-ts-mode--keywords] @font-lock-keyword-face)
     (project_declaration "project" @font-lock-keyword-face)
     ((project_qualifier) @font-lock-keyword-face))

   ;; Numeric literals
   :language 'gpr
   :feature 'number
   '((numeric_literal) @font-lock-number-face)

   ;; Package
   :language 'gpr
   :feature 'package
   '((package_declaration
      [ origname: (name (identifier) @font-lock-function-call-face :anchor)
        basename: (name (identifier) @font-lock-function-call-face :anchor)])
     ((variable_reference (name (identifier) @font-lock-function-call-face))
      (:pred gpr-ts-mode--package-name-p @font-lock-function-call-face)))

   ;; String literals
   :language 'gpr
   :feature 'string
   '((string_literal) @font-lock-string-face)

   ;; Types
   :language 'gpr
   :feature 'type
   '((variable_declaration type: (name (identifier) @font-lock-type-face :anchor)))

   ;; Variables
   :language 'gpr
   :feature 'variable
   :override t
   '((variable_reference (name (identifier) @font-lock-variable-use-face :anchor) :anchor))

   ;; Operators
   :language 'gpr
   :feature 'operator
   '([":=" "&" "|" "=>"] @font-lock-operator-face)

   ;; Syntax errors
   :language 'gpr
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))

  "Font-lock settings for `gpr-ts-mode'.")

(defun gpr-ts-mode--package-name-p (node)
  "Check if NODE identifier matches a known package name."
  (let ((identifier (downcase (treesit-node-text node t)))
        (packages (mapcar #'downcase gpr-ts-mode-package-names)))
    (seq-find (apply-partially #'string-equal identifier) packages)))

(defun gpr-ts-mode--package-declaration-names-match-p (node)
  "Check if names match in package declaration NODE."
  (when (gpr-ts-mode--package-declaration-p node)
    (let ((name (treesit-node-child-by-field-name node "name"))
          (endname (treesit-node-child-by-field-name node "endname")))
      (string-equal-ignore-case (treesit-node-text name t)
                                (treesit-node-text endname t)))))


;;; Imenu

(defun gpr-ts-mode--tree-text
    (node &optional ignored-types region-beg-type region-end-type)
  "Extract text found within the tree of NODE.

IGNORED-TYPES specifies a list of node types within the tree to ignore.
REGION-BEG-TYPE specifies the type of the first child node of NODE to
begin text extraction (exclusive).  REGION-END-TYPE specifies the first
child node of NODE to end text extraction (exclusive).  When
REGION-BEG-TYPE or REGION-END-TYPE are nil, the region is expanded to
the beginning or end of NODE respectively.

The extracted text will retain its text properties and the text of the
nodes will be uniformly spaced with the exception of parenthesis and
periods."

  (let* ((beg (when region-beg-type
                (treesit-node-end
                 (gpr-ts-mode--first-child-matching node region-beg-type))))
         (end (when region-end-type
                (treesit-node-start
                 (gpr-ts-mode--first-child-matching node region-end-type))))
         (tree (treesit-induce-sparse-tree
                node
                (lambda (node)
                  (let ((start (treesit-node-start node)))
                    (and (or (null beg) (> start beg))
                         (or (null end) (< start end))
                         (not (member (treesit-node-type node) ignored-types))
                         (= (treesit-node-child-count node) 0))))))
         (terminals (flatten-list (cdr tree))))
    ;; Add spacing between terminals
    (let ((name nil)
          (prev-node-type nil)
          (node-type nil)
          (no-space-regexp (rx bos (or "(" ")" ".") eos)))
      (dolist (terminal terminals)
        (setq node-type (treesit-node-type terminal))
        (when prev-node-type
          (unless (or (string-match-p no-space-regexp node-type)
                      (string-match-p no-space-regexp prev-node-type))
            (push " " name)))
        (push (treesit-node-text terminal) name)
        (setq prev-node-type node-type))
      (string-join (reverse name)))))

(defun gpr-ts-mode--attribute-declaration-p (node)
  "Determine if NODE is an attribute declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "attribute_declaration"))

(defun gpr-ts-mode--attribute-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not an attribute declaration node."
  (when (gpr-ts-mode--attribute-declaration-p node)
    (gpr-ts-mode--tree-text node '("comment") "for" "use")))

(defun gpr-ts-mode--case_construction-p (node)
  "Determine if NODE is a case construction.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "case_construction"))

(defun gpr-ts-mode--empty-declaration-p (node)
  "Determine if NODE is an empty declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "empty_declaration"))

(defun gpr-ts-mode--package-declaration-p (node)
  "Determine if NODE is a package declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "package_declaration"))

(defun gpr-ts-mode--package-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a package declaration node."
  (when (gpr-ts-mode--package-declaration-p node)
    (treesit-node-text (treesit-node-child-by-field-name node "name"))))

(defun gpr-ts-mode--project-declaration-p (node)
  "Determine if NODE is a project declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "project_declaration"))

(defun gpr-ts-mode--project-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a project declaration node."
  (when (gpr-ts-mode--project-declaration-p node)
    (gpr-ts-mode--tree-text
     (treesit-node-child-by-field-name node "name")
     '("comment"))))

(defun gpr-ts-mode--defun-p (node)
  "Determine if NODE is a defun node.
Return non-nil to indicate it is."
  (or (gpr-ts-mode--project-declaration-p node)
      (gpr-ts-mode--package-declaration-p node)))

(defun gpr-ts-mode--defun-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a defun node."
  (or (gpr-ts-mode--project-declaration-name node)
      (gpr-ts-mode--package-declaration-name node)))

(defun gpr-ts-mode--type-declaration-p (node)
  "Determine if NODE is a type declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "typed_string_declaration"))

(defun gpr-ts-mode--type-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a type declaration node."
  (when (gpr-ts-mode--type-declaration-p node)
    (treesit-node-text (treesit-node-child-by-field-name node "name"))))

(defun gpr-ts-mode--variable-declaration-p (node)
  "Determine if NODE is a variable declaration.
Return non-nil to indicate that it is."
  (string-equal (treesit-node-type node) "variable_declaration"))

(defun gpr-ts-mode--variable-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a variable declaration node."
  (when (gpr-ts-mode--variable-declaration-p node)
    (treesit-node-text
     (treesit-node-child-by-field-name node "name"))))

(defun gpr-ts-mode--typed-variable-declaration-p (node)
  "Determine if NODE is a typed variable declaration.
Return non-nil to indicate that it is."
  (and (gpr-ts-mode--variable-declaration-p node)
       (treesit-node-child-by-field-name node "type")))

(defun gpr-ts-mode--untyped-variable-declaration-p (node)
  "Determine if NODE is an untyped variable declaration.
Return non-nil to indicate that it is."
  (and (gpr-ts-mode--variable-declaration-p node)
       (not (gpr-ts-mode--typed-variable-declaration-p node))))

(defun gpr-ts-mode--with-declaration-p (node)
  "Determine if NODE is a with declaration.
Return non-nil to indicate that it is."
  (string-equal (treesit-node-type node) "with_declaration"))

(defun gpr-ts-mode--declaration-p (node)
  "Determine if NODE is a declaration.
Return non-nil to indicate that it is."
  (or (gpr-ts-mode--attribute-declaration-p node)
      (gpr-ts-mode--case_construction-p node)
      (gpr-ts-mode--empty-declaration-p node)
      (gpr-ts-mode--package-declaration-p node)
      (gpr-ts-mode--project-declaration-p node)
      (gpr-ts-mode--type-declaration-p node)
      (gpr-ts-mode--variable-declaration-p node)
      (gpr-ts-mode--with-declaration-p node)))

(defun gpr-ts-mode--with-clause-name-p (node)
  "Determine if NODE is a string within a with clause."
  (and (gpr-ts-mode--with-declaration-p (treesit-node-parent node))
       (string-equal (treesit-node-type node) "string_literal")))

(defun gpr-ts-mode--imenu-index (tree item-p branch-p item-name-fn branch-name-fn)
  "Return Imenu index for a specific item category given TREE.

ITEM-P is a predicate for testing the item category's node.
ITEM-NAME-FN determines the name of the item given the item's node.
BRANCH-P is a predicate for determining if a node is a branch.  This is
used to identify higher level nesting structures (i.e., packages,
subprograms, etc.) which encompass the item.  BRANCH-NAME-FN determines
the name of the branch given the branch node."
  (let* ((node (car tree))
         (sort-fn
          (if imenu-sort-function
              (lambda (items) (sort items imenu-sort-function))
            #'identity))
         (subtrees
          (funcall sort-fn
                   (mapcan (lambda (tree)
                             (gpr-ts-mode--imenu-index tree
                                                       item-p
                                                       branch-p
                                                       item-name-fn
                                                       branch-name-fn))
                           (cdr tree))))
         (marker (set-marker (make-marker)
                             (treesit-node-start node)))
         (item (funcall item-p node))
         (item-name (when item (funcall item-name-fn node)))
         (branch (funcall branch-p node))
         (branch-name (when branch (funcall branch-name-fn node))))
    (cond ((and item (not subtrees))
           (list (cons item-name marker)))
          ((and item subtrees)
           (error "Unexpected nested item(s)"))
          ((and branch subtrees)
           (list (cons branch-name subtrees)))
          (t subtrees))))

(cl-defgeneric gpr-ts-mode-imenu-index (category)
  "Create Imenu index for CATEGORY."
  (error "Unknown category: %s" category))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql attribute)))
  "Create Imenu index for attributes."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--attribute-declaration-p node))))
   #'gpr-ts-mode--attribute-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--attribute-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql package)))
  "Create Imenu index for packages."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--package-declaration-p
    nil
    2)
   #'identity
   #'ignore
   #'gpr-ts-mode--package-declaration-name
   #'ignore))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql project)))
  "Create Imenu index for projects."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--project-declaration-p
    nil
    1)
   #'identity
   #'ignore
   #'gpr-ts-mode--project-declaration-name
   #'ignore))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql type)))
  "Create Imenu index for types."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--type-declaration-p
    nil
    2)
   #'identity
   #'ignore
   #'gpr-ts-mode--type-declaration-name
   #'ignore))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql typed-variable)))
  "Create Imenu index for typed variables."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--typed-variable-declaration-p node))))
   #'gpr-ts-mode--typed-variable-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--variable-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql untyped-variable)))
  "Create Imenu index for untyped variables."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--untyped-variable-declaration-p node))))
   #'gpr-ts-mode--untyped-variable-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--variable-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql variable)))
  "Create Imenu index for variables."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--variable-declaration-p node))))
   #'gpr-ts-mode--variable-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--variable-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-mode-imenu-index ((_category (eql with-clause)))
  "Create Imenu index for with clauses."
  (gpr-ts-mode--imenu-index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--with-clause-name-p
    nil
    2)
   #'identity
   #'ignore
   #'treesit-node-text
   #'ignore))

(defun gpr-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (font-lock-ensure)
  (seq-keep
   (lambda (category)
     (when-let* ((name (or (alist-get category gpr-ts-mode-imenu-category-name-alist)
                           (error "Unspecified category name for: %s" category)))
                 (index (gpr-ts-mode-imenu-index category)))
       (cons name index)))
   gpr-ts-mode-imenu-categories))

(require 'gpr-ts-completion)

;;;###autoload
(define-derived-mode gpr-ts-mode prog-mode "GNAT Project"
  "Major mode for editing GNAT Project files, powered by tree-sitter."
  :group 'gpr-ts

  ;; Grammar.
  (setq-local treesit-language-source-alist
              `((gpr . ,(ensure-list gpr-ts-mode-grammar))))

  (when (and (treesit-available-p)
             (not (treesit-language-available-p 'gpr))
             (pcase gpr-ts-mode-grammar-install
               ('auto t)
               ('prompt
                ;; Use `read-key' instead of `read-from-minibuffer' as
                ;; this is less intrusive.  The later will start
                ;; `minibuffer-mode' which impacts buffer local
                ;; variables, especially font lock, preventing proper
                ;; mode initialization and results in improper
                ;; fontification of the buffer immediately after
                ;; installing the grammar.
                (let ((y-or-n-p-use-read-key t))
                  (y-or-n-p
                   (format
                    (concat "Tree-sitter grammar for GPR is missing.  "
                            "Install it from %s? ")
                    (car (alist-get 'gpr treesit-language-source-alist))))))
               (_ nil)))
    (message "Installing the tree-sitter grammar for GPR")
    (treesit-install-language-grammar 'gpr))

  (unless (treesit-ready-p 'gpr)
    (error "Tree-sitter for GPR isn't available"))

  (treesit-parser-create 'gpr)

  ;; Comments.
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx "--" (* "-") (* (syntax whitespace))))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp (rx (or "project_declaration"
                                                "package_declaration")))
  (setq-local treesit-defun-name-function #'gpr-ts-mode--defun-name)

  ;; Things (Emacs 30+)
  (setq-local treesit-thing-settings
              `((gpr (sexp
                      (not ,(rx (or "(" ")" ","))))
                     (sentence
                      ,(rx (or "attribute_declaration"
                               "case_construction"
                               "case_item"
                               "empty_declaration"
                               "typed_string_declaration"
                               "variable_declaration"
                               "with_declaration")))
                     (text
                      ,(rx (or "comment"))))))

  ;; Imenu.
  (setq-local imenu-create-index-function #'gpr-ts-mode--imenu)

  ;; Indent.
  (setq-local treesit-simple-indent-rules gpr-ts-mode--indent-rules)
  (setq-local electric-indent-chars (append ";>," electric-indent-chars))

  ;; Outline minor mode (Emacs 30+)
  (setq-local treesit-outline-predicate #'gpr-ts-mode--defun-p)

  ;; EditorConfig (Emacs 30+)
  (setq-local editorconfig-indent-size-vars '(gpr-ts-mode-indent-offset))

  ;; Eglot (Emacs 29+)
  (setq-local eglot-server-programs
              '((gpr-ts-mode . ("ada_language_server" "--language-gpr"))))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings gpr-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (attribute function number operator package variable)
                (bracket delimiter error)))

  ;; Completion.
  (add-hook 'completion-at-point-functions #'gpr-ts-mode--completion-at-point nil t)

  (treesit-major-mode-setup)

  ;; Override `treesit-major-mode-setup' settings.
  (setq-local indent-line-function #'gpr-ts-mode--indent-line))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               `(,(rx (or ".gpr" ".cgpr") eos) . gpr-ts-mode))
  ;; Add gpr-mode as an "extra" parent so gpr-ts-mode can handle
  ;; directory local variables for gpr-mode, etc. (Emacs 30+)
  (when (fboundp 'derived-mode-add-parents)
    (derived-mode-add-parents 'gpr-ts-mode '(gpr-mode)))
  ;; Prefer `major-mode-remap-defaults' if available (Emacs 30+)
  (if (boundp 'major-mode-remap-defaults)
      (add-to-list 'major-mode-remap-defaults '(gpr-mode . gpr-ts-mode))
    (add-to-list 'major-mode-remap-alist '(gpr-mode . gpr-ts-mode))))

;; Lazily register mode's info lookup help.
(with-eval-after-load 'info-look
  (declare-function info-lookup-add-help "info-look" (&rest args))
  (info-lookup-add-help
   :topic 'symbol
   :mode '(emacs-lisp-mode . "gpr")
   :regexp "\\bgpr-ts-[^][()`'‘’,\" \t\n]+"
   :doc-spec '(("(gpr-ts-mode)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)"))))

(provide 'gpr-ts-mode)

;;; gpr-ts-mode.el ends here
