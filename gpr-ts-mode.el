;;; gpr-ts-mode.el --- Major mode for GNAT project files using Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Troy Brown

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

;; This package provides a major mode for editing GNAT Project source
;; code.  It supports syntax highlighting, indentation, navigation,
;; Imenu, outlining, casing, completion and code alignment.
;;
;; To use `gpr-ts-mode', you will need the appropriate tree-sitter
;; grammar installed.  By default, on mode startup if the grammar is
;; not detected, you will be prompted to automatically install it.

;;; Code:

(require 'gpr-ts-align)
(require 'gpr-ts-case)
(require 'gpr-ts-common)
(require 'gpr-ts-completion)
(require 'gpr-ts-imenu)
(require 'gpr-ts-indent)
(require 'gpr-ts-lspclient)
(require 'gpr-ts-paren)
(require 'cl-generic)
(require 'lisp-mnt)
(require 'treesit)
(eval-when-compile (require 'rx))

(gpr-ts-mode--declare-treesit-functions)

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

(defcustom gpr-ts-mode-package-names
  (sort
   (seq-filter
    (lambda (elt) (not (string-equal-ignore-case "Project" elt)))
    (seq-map #'car gpr-ts-mode-completion-definitions))
   #'string-lessp)
  "List of known package names."
  :type '(repeat string)
  :group 'gpr-ts
  :link '(custom-manual :tag "Syntax Highlighting" "(gpr-ts-mode)Syntax Highlighting")
  :package-version '(gpr-ts-mode . "0.6.0"))
;;;###autoload(put 'gpr-ts-mode-package-names 'safe-local-variable #'list-of-strings-p)

(defcustom gpr-ts-mode-keymap-prefix "C-c"
  "Keymap prefix for `gpr-ts-mode'."
  :type 'string
  :group 'gpr-ts
  :link '(custom-manual :tag "Miscellaneous" "(gpr-ts-mode)Miscellaneous")
  :package-version '(gpr-ts-mode . "0.8.0"))

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

(defun gpr-ts-mode--package-declaration-name-p (node)
  "Check if NODE is a package_declaration name."
  (when-let* ((node-t (treesit-node-type node))
              ((string-equal node-t "identifier"))
              (prev-node (gpr-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node)))
    (or (string-equal prev-node-t "package")
        (and (string-equal prev-node-t "end")
             (when-let*
                 ((parent-node (gpr-ts-mode--matching-prev-node
                                prev-node '("package" "project" "case")))
                  (parent-node-t (treesit-node-type parent-node))
                  ((string-equal parent-node-t "package"))
                  (name-node (gpr-ts-mode--next-node parent-node))
                  (name-node-t (treesit-node-type name-node))
                  ((string-equal name-node-t "identifier")))
               (string-equal-ignore-case
                (treesit-node-text node)
                (treesit-node-text name-node)))))))

(defun gpr-ts-mode--attribute-declaration-name-p (node)
  "Check if NODE is an attribute_declaration name."
  (when-let* ((node-t (treesit-node-type node))
              ((string-equal node-t "identifier"))
              (prev-node (gpr-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node)))
    (string-equal prev-node-t "for")))

(defun gpr-ts-mode--typed-string-declaration-name-p (node)
  "Check if NODE is a typed_string_declaration name."
  (when-let* ((node-t (treesit-node-type node))
              ((string-equal node-t "identifier"))
              (prev-node (gpr-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node)))
    (string-equal prev-node-t "type")))

(defun gpr-ts-mode--variable-declaration-name-p (node)
  "Check if NODE is a variable_declaraton name."
  (when-let* ((node-t (treesit-node-type node))
              ((string-equal node-t "identifier"))
              (next-node (gpr-ts-mode--next-node node))
              (next-node-t (treesit-node-type next-node)))
    (and (member next-node-t '(":" ":="))
         (let* ((prev-node (gpr-ts-mode--prev-node node))
                (prev-node-t (treesit-node-type prev-node)))
           (or (null prev-node)
               (not (string-equal prev-node-t ":")))))))

(defun gpr-ts-mode--variable-declaration-type-p (node)
  "Check if NODE is a variable_declaration type."
  (when-let* ((node-t (treesit-node-type node))
              ((string-equal node-t "identifier"))
              (prev-node (gpr-ts-mode--prev-node node))
              (prev-node-t (treesit-node-type prev-node)))
    (string-equal prev-node-t ":")))

(defun gpr-ts-mode--package-extends-or-renames-name-p (node)
  "Check if NODE is a package_declaration name.

The name must be for a package extension or package rename, and as such,
must reside immediately after an \\='extends\\=' or \\='renames\\='
keyword respectively.

Additionally, the \\='identifier\\=', if within a \\='name\\=' node,
must be the last segment of the name."
  (when-let* ((node-t (treesit-node-type node))
              ((string-equal node-t "identifier"))
              (parent-node (treesit-node-parent node))
              (parent-node-t (treesit-node-type parent-node)))
    (if (string-equal parent-node-t "name")
        (unless (null (treesit-node-next-sibling node))
          (setq parent-node nil))
      (setq parent-node node))
    (when parent-node
      (when-let* ((prev-node (gpr-ts-mode--prev-node parent-node))
                  (prev-node-t (treesit-node-type prev-node)))
        (member prev-node-t '("extends" "renames"))))))

(defun gpr-ts-mode--package-name-p (node)
  "Check if NODE identifier matches a known package name."
  (let ((identifier (treesit-node-text node t)))
    (seq-find
     (apply-partially #'string-equal-ignore-case identifier)
     gpr-ts-mode-package-names)))

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
   '(;; package_declaration
     ((identifier) @font-lock-function-name-face
      (:pred gpr-ts-mode--package-declaration-name-p @font-lock-function-name-face))
     ;; typed_string_declaration
     ((identifier) @font-lock-type-face
      (:pred gpr-ts-mode--typed-string-declaration-name-p @font-lock-type-face))
     ;; variable_declaration
     ((identifier) @font-lock-variable-name-face
      (:pred gpr-ts-mode--variable-declaration-name-p @font-lock-variable-name-face))
     ;; attribute_declaration
     ((identifier) @font-lock-property-name-face
      (:pred gpr-ts-mode--attribute-declaration-name-p @font-lock-property-name-face)))

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
     ("project" @font-lock-keyword-face
      (:pred gpr-ts-mode--project-keyword-p @font-lock-keyword-face))
     ((project_qualifier) @font-lock-keyword-face))

   ;; Numeric literals
   :language 'gpr
   :feature 'number
   '((numeric_literal) @font-lock-number-face)

   ;; Package
   :language 'gpr
   :feature 'package
   '(;; package_declaration extends/renames
     ((identifier) @font-lock-function-call-face
      (:pred gpr-ts-mode--package-extends-or-renames-name-p @font-lock-function-call-face))
     ;; Package Name in variable_reference
     ((variable_reference (name (identifier) @font-lock-function-call-face))
      (:pred gpr-ts-mode--package-name-p @font-lock-function-call-face)))

   ;; String literals
   :language 'gpr
   :feature 'string
   '((string_literal) @font-lock-string-face)

   ;; Types
   :language 'gpr
   :feature 'type
   '(;; variable_declaration
     ((name (identifier) @font-lock-type-face :anchor)
      (:pred gpr-ts-mode--variable-declaration-type-p @font-lock-type-face)))

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
   '((ERROR) @font-lock-warning-face))

  "Font-lock settings for `gpr-ts-mode'.")

(defvar gpr-ts-mode-map
  (let ((map (make-sparse-keymap))
        (key (if (fboundp 'prog-fill-reindent-defun)
                 "<remap> <prog-fill-reindent-defun>"
               "M-q")))
    (keymap-set map key #'gpr-ts-mode-fill-reindent-defun)
    (when gpr-ts-mode-keymap-prefix
      (keymap-set map
                  gpr-ts-mode-keymap-prefix
                  (define-keymap
                    "C-a" #'gpr-ts-mode-align
                    "C-]" #'gpr-ts-mode-close-block)))
    map)
  "Keymap for `gpr-ts-mode'.")

(defun gpr-ts-mode--browse-menu-url (item)
  "Browse URL for Menu ITEM."
  (let* ((current-repo
          (lm-website (locate-library "gpr-ts-mode.el")))
         (example-repo
          (replace-regexp-in-string "/[^/]+\\'" "/dotemacs-ada" current-repo))
         (repo-issues
          (concat current-repo "/issues"))
         (alist
          `((example . ,example-repo)
            (issues  . ,repo-issues)
            (guide   . "https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html"))))
    (browse-url (alist-get item alist))))

(easy-menu-define gpr-ts-mode-menu gpr-ts-mode-map
  "Menu keymap for `gpr-ts-mode'."
  `("GNAT Project"
    ["Toggle Auto-Casing"             gpr-ts-auto-case-mode                   t]
    ["Case Format Buffer"             gpr-ts-mode-case-format-buffer          t]
    ["Case Format Point/Region"       gpr-ts-mode-case-format-dwim            t]
    "-----"
    ["Toggle Auto-Alignment"          gpr-ts-auto-align-mode                  t]
    ["Align Region / Section"         gpr-ts-mode-align                       t]
    "-----"
    ["Re-Indent Defun / Fill Comment" gpr-ts-mode-fill-reindent-defun         t]
    ["Re-Indent Buffer"               gpr-ts-mode-reindent-buffer             t]
    "-----"
    ["Completion At Point"            completion-at-point
     :keys ,(when-let* ((key (where-is-internal 'completion-at-point nil 'first-only)))
              (key-description key))
     :active t]
    ["Close Block"                    gpr-ts-mode-close-block                 t]
    "-----"
    ["Beginning of Defun"             treesit-beginning-of-defun              t]
    ["End of Defun"                   treesit-end-of-defun                    t]
    "-----"
    ("Help"
     ["GPR Mode Manual"               (info "(gpr-ts-mode)Top")               t]
     ["Example Configuration"         (gpr-ts-mode--browse-menu-url 'example) t]
     ["Report An Issue"               (gpr-ts-mode--browse-menu-url 'issues)  t]
     ["GPRbuild User's Guide"         (gpr-ts-mode--browse-menu-url 'guide)   t])
    ["Customize"                      (customize-group 'gpr-ts)               t]))

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
                     (list
                      ,(rx (or "case_construction"
                               "expression_list"
                               "package_declaration"
                               "project_declaration"
                               "with_declaration")))
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
  (setq-local imenu-create-index-function #'gpr-ts-imenu)

  ;; Indent.
  (gpr-ts-indent--setup)

  ;; Outline minor mode (Emacs 30+)
  (setq-local treesit-outline-predicate #'gpr-ts-mode--defun-p)

  ;; EditorConfig (Emacs 30+)
  (setq-local editorconfig-indent-size-vars '(gpr-ts-mode-indent-offset))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings gpr-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string type)
                (attribute function number operator package variable)
                (bracket delimiter error)))

  ;; Completion.
  (add-hook 'completion-at-point-functions #'gpr-ts-mode--completion-at-point nil t)

  ;; Align.
  (gpr-ts-align--setup)

  ;; Parenthesis.
  (gpr-ts-paren--setup)

  ;; LSP Client.
  (run-hooks 'gpr-ts-lspclient-setup-hook)

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
   :doc-spec `(("(gpr-ts-mode)Command Index"
                nil
                ;; Prefix for command documentation in Info
                ,(rx (or
                      ;; Prefix for command without key binding
                      (seq bol space (+ "-") space (+ anychar) space)
                      ;; Prefix for command with key binding
                      (seq bol "‘" (+ anychar) "’" space "(‘")))
                ;; Suffix for command documentation in Info
                ,(rx (? "’)") (or space eol)))
               ("(gpr-ts-mode)Variable Index"
                nil
                ;; Prefix for variable documentation in Info
                ,(rx bol space (+ "-") space (+ anychar) ":" space)
                ;; Suffix for variable documentation in Info
                ,(rx (or space eol))))))

;; Lazily register mode with speedbar.
(with-eval-after-load 'speedbar
  (declare-function speedbar-add-supported-extension "speedbar" (extension))
  (defvar speedbar-use-imenu-flag)
  (when speedbar-use-imenu-flag
    (speedbar-add-supported-extension ".gpr")
    (speedbar-add-supported-extension ".cgpr")))

(provide 'gpr-ts-mode)

;;; gpr-ts-mode.el ends here
