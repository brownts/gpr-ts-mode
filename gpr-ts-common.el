;;; gpr-ts-common.el -- Common support for GPR Project files -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Troy Brown

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

(require 'lisp-mnt)
(require 'treesit)

(defgroup gpr-ts nil
  "Major mode for GNAT Project files, using Tree-Sitter."
  :group 'languages
  :link '(emacs-library-link :tag "Source" "gpr-ts-mode.el")
  :link `(url-link :tag "Website"
                   ,(lm-website (locate-library "gpr-ts-mode.el")))
  :link '(custom-manual "(gpr-ts-mode)Top")
  :prefix "gpr-ts-mode-")

;;; Private Hooks

(defvar gpr-ts-mode--after-setup-hook nil
  "Hook run after mode setup has completed.")

;;; Keywords

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

;;; Node Access

(defun gpr-ts-mode--prev-node (start &optional include-comments)
  "Find node before START, and possibly INCLUDE-COMMENTS.

START is either a node or a position."
  (let* ((prev-node-s
          (if (treesit-node-p start)
              (treesit-node-start start)
            start))
         (first-pass t)
         prev-node prev-node-e prev-node-t)
    (save-excursion
      (while (or first-pass
                 (and prev-node-t
                      (not include-comments)
                      (string-equal prev-node-t "comment")))
        (setq first-pass nil)
        (goto-char prev-node-s)
        (skip-chars-backward " \t\n" (point-min))
        (setq prev-node (if (bobp) nil (treesit-node-at (1- (point))))
              prev-node-e (treesit-node-end prev-node))
        (setq prev-node
              (treesit-parent-while
               prev-node
               (lambda (node)
                 (and
                  (not (string-equal (treesit-node-type node) "ERROR"))
                  (= (treesit-node-end node) prev-node-e))))
              prev-node-t (treesit-node-type prev-node)
              prev-node-s (treesit-node-start prev-node))))
    prev-node))

(defun gpr-ts-mode--next-node (start &optional include-comments)
  "Find node after START, and possibly INCLUDE-COMMENTS.

START is either a node or a position."
  (let* ((next-node-e
          (if (treesit-node-p start)
              (treesit-node-end start)
            (1+ start)))
         (first-pass t)
         next-node next-node-s next-node-t)
    (save-excursion
      (while (or first-pass
                 (and next-node-t
                      (not include-comments)
                      (string-equal next-node-t "comment")))
        (setq first-pass nil)
        (goto-char next-node-e)
        (skip-chars-forward " \t\n" (point-max))
        (setq next-node (if (eobp) nil (treesit-node-at (point)))
              next-node-s (treesit-node-start next-node))
        (setq next-node
              (treesit-parent-while
               next-node
               (lambda (node)
                 (and
                  (not (string-equal (treesit-node-type node) "ERROR"))
                  (= (treesit-node-start node) next-node-s))))
              next-node-t (treesit-node-type next-node)
              next-node-e (treesit-node-end next-node))))
    next-node))

(defun gpr-ts-mode--prev-leaf-node (start)
  "Find leaf node before START.

START is either a node or a position."
  (when-let* ((prev-node (gpr-ts-mode--prev-node start)))
    (treesit-node-at
     (1- (treesit-node-end prev-node)))))

(defun gpr-ts-mode--next-leaf-node (start)
  "Find leaf node after START.

START is either a node or a position."
  (when-let* ((next-node (gpr-ts-mode--next-node start)))
    (treesit-node-at (treesit-node-start next-node))))

(defun gpr-ts-mode--matching-prev-node (start matches)
  "Find a node before START where node type is contained in MATCHES.

  MATCHES is either a string representing a node type, a list of strings
  representing node types or a predicate function which takes a node as
  its sole parameter and returns non nil for a match."
  (let ((prev-node start)
        (predicate (if (functionp matches)
                       matches
                     (lambda (node)
                       (member (treesit-node-type node)
                               (ensure-list matches))))))
    (while (or (treesit-node-eq prev-node start)
               (and prev-node
                    (not (funcall predicate prev-node))))
      (setq prev-node (gpr-ts-mode--prev-node prev-node)))
    prev-node))

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

;;; Declaration Node Predicates

(defun gpr-ts-mode--attribute-declaration-p (node)
  "Determine if NODE is an attribute declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "attribute_declaration"))

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

(defun gpr-ts-mode--project-declaration-p (node)
  "Determine if NODE is a project declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "project_declaration"))

(defun gpr-ts-mode--type-declaration-p (node)
  "Determine if NODE is a type declaration.
Return non-nil to indicate it is."
  (string-equal (treesit-node-type node) "typed_string_declaration"))

(defun gpr-ts-mode--variable-declaration-p (node)
  "Determine if NODE is a variable declaration.
Return non-nil to indicate that it is."
  (string-equal (treesit-node-type node) "variable_declaration"))

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

;;; Miscellaneous Node Predicates

(defun gpr-ts-mode--defun-p (node)
  "Determine if NODE is a defun node.
Return non-nil to indicate it is."
  (or (gpr-ts-mode--project-declaration-p node)
      (gpr-ts-mode--package-declaration-p node)))

(defun gpr-ts-mode--with-clause-name-p (node)
  "Determine if NODE is a string within a with clause."
  (and (gpr-ts-mode--with-declaration-p (treesit-node-parent node))
       (string-equal (treesit-node-type node) "string_literal")))

(defun gpr-ts-mode--package-declaration-names-match-p (node)
  "Check if names match in package declaration NODE."
  (when (gpr-ts-mode--package-declaration-p node)
    (let ((name (treesit-node-child-by-field-name node "name"))
          (endname (treesit-node-child-by-field-name node "endname")))
      (string-equal-ignore-case (treesit-node-text name t)
                                (treesit-node-text endname t)))))

(defun gpr-ts-mode--project-keyword-p (node)
  "Check if NODE is a project keyword."
  (when-let* ((node-t (treesit-node-type node))
              ((string-equal node-t "project")))
    (let* ((prev-node (gpr-ts-mode--prev-node node))
           (prev-node-t (treesit-node-type prev-node)))
      (or (null prev-node)
          (member prev-node-t '("with_declaration"
                                "project_qualifier"))))))

;;; Node Name Utilities

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

;;; Declaration Names

(defun gpr-ts-mode--attribute-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not an attribute declaration node."
  (when (gpr-ts-mode--attribute-declaration-p node)
    (gpr-ts-mode--tree-text node '("comment") "for" "use")))

(defun gpr-ts-mode--package-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a package declaration node."
  (when (gpr-ts-mode--package-declaration-p node)
    (treesit-node-text (treesit-node-child-by-field-name node "name"))))

(defun gpr-ts-mode--project-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a project declaration node."
  (when (gpr-ts-mode--project-declaration-p node)
    (gpr-ts-mode--tree-text
     (treesit-node-child-by-field-name node "name")
     '("comment"))))

(defun gpr-ts-mode--type-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a type declaration node."
  (when (gpr-ts-mode--type-declaration-p node)
    (treesit-node-text (treesit-node-child-by-field-name node "name"))))

(defun gpr-ts-mode--variable-declaration-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a variable declaration node."
  (when (gpr-ts-mode--variable-declaration-p node)
    (treesit-node-text
     (treesit-node-child-by-field-name node "name"))))

;;; Defun Names

(defun gpr-ts-mode--defun-name (node)
  "Return the name associated with NODE.
Return nil if NODE is not a defun node."
  (or (gpr-ts-mode--project-declaration-name node)
      (gpr-ts-mode--package-declaration-name node)))

(provide 'gpr-ts-common)

;;; gpr-ts-common.el ends here
