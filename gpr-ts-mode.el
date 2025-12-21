;;; gpr-ts-mode.el --- Major mode for GNAT project files using Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Troy Brown

;; Author: Troy Brown <brownts@troybrown.dev>
;; Created: February 2023
;; Version: 0.7.3
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

(require 'gpr-ts-core)
(require 'gpr-ts-indent)
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
  :package-version '(gpr-ts-mode . "0.6.0"))

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
  :package-version '(gpr-ts-mode . "0.6.0"))

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
  :package-version '(gpr-ts-mode . "0.5.0"))

(defcustom gpr-ts-mode-grammar-install 'prompt
  "Configuration for installation of tree-sitter language grammar library."
  :type '(choice (const :tag "Automatically Install" auto)
                 (const :tag "Prompt to Install" prompt)
                 (const :tag "Do not install" nil))
  :group 'gpr-ts
  :link '(custom-manual :tag "Grammar Installation" "(gpr-ts-mode)Grammar Installation")
  :package-version '(gpr-ts-mode . "0.5.0"))

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

;;; Imenu

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

(require 'gpr-ts-casing)
(require 'gpr-ts-completion)

;;;###autoload
(define-derived-mode gpr-ts-mode prog-mode "GNAT Project"
  "Major mode for editing GNAT Project files, powered by tree-sitter."
  :group 'gpr-ts

  ;; Grammar.
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
  (gpr-ts-indent--setup)

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
  (run-hooks 'gpr-ts-mode--after-setup-hook))

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

;; Register mode's default grammar
(add-to-list 'treesit-language-source-alist
             `(gpr . ,(ensure-list gpr-ts-mode-grammar))
             'append)

;; Lazily register mode's info lookup help.
(with-eval-after-load 'info-look
  (declare-function info-lookup-add-help "info-look" (&rest args))
  (info-lookup-add-help
   :topic 'symbol
   :mode '(emacs-lisp-mode . "gpr")
   :regexp "\\bgpr-ts-[^][()`'‘’,\" \t\n]+"
   :doc-spec '(("(gpr-ts-mode)Command Index" nil "^ -+ .*: " "\\( \\|$\\)")
               ("(gpr-ts-mode)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)"))))

(provide 'gpr-ts-mode)

;;; gpr-ts-mode.el ends here
