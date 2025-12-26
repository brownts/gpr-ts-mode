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
(require 'gpr-ts-imenu)
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

(require 'gpr-ts-casing)
(require 'gpr-ts-completion)

(defvar gpr-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Backport `prog-fill-reindent-defun' to Emacs 29 and avoid the
    ;; Emacs 30 issue where `prog-fill-reindent-defun' would reindent
    ;; the wrong defun, as reported in https://debbugs.gnu.org/78703.
    (unless (boundp 'prog-fill-reindent-defun-function)
      (define-key map (kbd "M-q") #'gpr-ts-mode-fill-reindent-defun))
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
  '("GNAT Project"
    ["Toggle Auto-Casing"             gpr-ts-auto-case-mode                   t]
    ["Case Format Buffer"             gpr-ts-mode-case-format-buffer          t]
    ["Case Format Point/Region"       gpr-ts-mode-case-format-dwim            t]
    ["-----"                          nil                                     nil]
    ["Re-Indent Defun / Fill Comment" gpr-ts-mode-fill-reindent-defun
     :visible (not (boundp 'prog-fill-reindent-defun-function))]
    ["Re-Indent Defun / Fill Comment" prog-fill-reindent-defun
     :visible (boundp 'prog-fill-reindent-defun-function)]
    ["Re-Indent Buffer"               (indent-region (point-min) (point-max)) t]
    ["-----"                          nil                                     nil]
    ["Beginning of Defun"             treesit-beginning-of-defun              t]
    ["End of Defun"                   treesit-end-of-defun                    t]
    ["-----"                          nil                                     nil]
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
