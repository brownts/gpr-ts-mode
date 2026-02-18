;;; gpr-ts-imenu.el -- IMenu support in GPR Project files -*- lexical-binding: t; -*-

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

(require 'gpr-ts-common)
(require 'treesit)

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

(defun gpr-ts-imenu--index (tree item-p branch-p item-name-fn branch-name-fn)
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
                             (gpr-ts-imenu--index tree
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

(cl-defgeneric gpr-ts-imenu-index (category)
  "Create Imenu index for CATEGORY."
  (error "Unknown category: %s" category))

(cl-defmethod gpr-ts-imenu-index ((_category (eql attribute)))
  "Create Imenu index for attributes."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--attribute-declaration-p node))))
   #'gpr-ts-mode--attribute-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--attribute-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-imenu-index ((_category (eql package)))
  "Create Imenu index for packages."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--package-declaration-p
    nil
    2)
   #'identity
   #'ignore
   #'gpr-ts-mode--package-declaration-name
   #'ignore))

(cl-defmethod gpr-ts-imenu-index ((_category (eql project)))
  "Create Imenu index for projects."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--project-declaration-p
    nil
    1)
   #'identity
   #'ignore
   #'gpr-ts-mode--project-declaration-name
   #'ignore))

(cl-defmethod gpr-ts-imenu-index ((_category (eql type)))
  "Create Imenu index for types."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--type-declaration-p
    nil
    2)
   #'identity
   #'ignore
   #'gpr-ts-mode--type-declaration-name
   #'ignore))

(cl-defmethod gpr-ts-imenu-index ((_category (eql typed-variable)))
  "Create Imenu index for typed variables."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--typed-variable-declaration-p node))))
   #'gpr-ts-mode--typed-variable-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--variable-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-imenu-index ((_category (eql untyped-variable)))
  "Create Imenu index for untyped variables."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--untyped-variable-declaration-p node))))
   #'gpr-ts-mode--untyped-variable-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--variable-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-imenu-index ((_category (eql variable)))
  "Create Imenu index for variables."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    (lambda (node)
      (or (gpr-ts-mode--package-declaration-p node)
          (gpr-ts-mode--variable-declaration-p node))))
   #'gpr-ts-mode--variable-declaration-p
   #'gpr-ts-mode--package-declaration-p
   #'gpr-ts-mode--variable-declaration-name
   #'gpr-ts-mode--package-declaration-name))

(cl-defmethod gpr-ts-imenu-index ((_category (eql with-clause)))
  "Create Imenu index for with clauses."
  (gpr-ts-imenu--index
   (treesit-induce-sparse-tree
    (treesit-buffer-root-node)
    #'gpr-ts-mode--with-clause-name-p
    nil
    2)
   #'identity
   #'ignore
   #'treesit-node-text
   #'ignore))

(defun gpr-ts-imenu ()
  "Return Imenu alist for the current buffer."
  (font-lock-ensure)
  (seq-keep
   (lambda (category)
     (when-let* ((name (or (alist-get category gpr-ts-mode-imenu-category-name-alist)
                           (error "Unspecified category name for: %s" category)))
                 (index (gpr-ts-imenu-index category)))
       (cons name index)))
   gpr-ts-mode-imenu-categories))

(provide 'gpr-ts-imenu)

;;; gpr-ts-imenu.el ends here
