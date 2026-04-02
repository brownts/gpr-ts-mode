;;; gpr-ts-indent.el -- Indentation support in GPR Project files -*- lexical-binding: t; -*-

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

(gpr-ts-mode--declare-treesit-functions)

(defcustom gpr-ts-mode-indent-offset 3
  "Indentation of statements."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version '(gpr-ts-mode . "0.5.0"))
;;;###autoload(put 'gpr-ts-mode-indent-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-when-offset gpr-ts-mode-indent-offset
  "Indentation of `when' relative to `case'."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version '(gpr-ts-mode . "0.5.0"))
;;;###autoload(put 'gpr-ts-mode-indent-when-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-broken-offset (- gpr-ts-mode-indent-offset 1)
  "Indentation for the continuation of a broken line."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version '(gpr-ts-mode . "0.5.0"))
;;;###autoload(put 'gpr-ts-mode-indent-broken-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-exp-item-offset (- gpr-ts-mode-indent-offset 1)
  "Indentation for the continuation of an expression."
  :type 'integer
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version '(gpr-ts-mode . "0.5.0"))
;;;###autoload(put 'gpr-ts-mode-indent-exp-item-offset 'safe-local-variable #'integerp)

(defcustom gpr-ts-mode-indent-strategy 'declaration
  "Indentation strategy to utilize."
  :type '(choice :tag "Indentation Strategy"
                 (const :tag "Declaration" declaration)
                 (const :tag "Line" line))
  :group 'gpr-ts
  :link '(custom-manual :tag "Indentation" "(gpr-ts-mode)Indentation")
  :package-version '(gpr-ts-mode . "0.6.0"))

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

;;; Formal Indentation Rules

(defvar gpr-ts-indent--rules
  `((gpr

     ((or (query ((ERROR) @node))
          (query ((ERROR _ @node)))
          (query ((_ (ERROR) _ @node)))
          no-node) ; newline
      (anchor/best-effort)
      (offset/best-effort))

     ;; top-level
     ((query ((project _ @node))) column-0 0)

     ;; closing parenthesis
     ((query (")" @node)) (anchor/first-sibling "(") 0)

     ;; with_declaration
     ((query ((with_declaration (string_literal)
                                [(string_literal) ","] @node)))
      (anchor/first-sibling "string_literal")
      0)

     ;; discrete_choice_list
     ((query ((discrete_choice_list _ @node))) parent 0)
     ((query ((discrete_choice_list) @node))   parent gpr-ts-mode-indent-broken-offset)

     ;; case_construction / case_item
     ((query ((case_item "=>" _ @node)))               parent     gpr-ts-mode-indent-offset)
     ((query ((case_item _ @node)))                    parent-bol gpr-ts-mode-indent-broken-offset)
     ((query ((case_construction "is" _ @node "end"))) parent     gpr-ts-mode-indent-when-offset)
     ;; ((query ((case_construction _ @node)))            parent-bol gpr-ts-mode-indent-broken-offset)

     ;; project_declaration / project_qualifier / package declaration
     ((query ((project_declaration "is" _ @node "end"))) parent gpr-ts-mode-indent-offset)
     ((query ((project_declaration "project" @node)))    parent 0)
     ((query ((project_qualifier _ @node)))              parent 0)
     ((query ((package_declaration "is" _ @node "end"))) parent gpr-ts-mode-indent-offset)

     ;; expression_list
     ((query ((expression_list (expression) _ @node))) (anchor/first-sibling "expression") 0)
     ((query ((expression_list _ @node)))              (anchor/first-sibling "(")          1)
     ((query ((expression_list) @node))                parent gpr-ts-mode-indent-broken-offset)

     ;; expression
     ((query ((expression _ @node))) parent gpr-ts-mode-indent-exp-item-offset)
     ((query ((expression) @node))   parent gpr-ts-mode-indent-broken-offset)

     ;; project_reference / variable_reference
     ((query ((project_reference  _ @node))) parent 0)
     ((query ((variable_reference _ @node))) parent 0)
     ((query ((variable_reference) @node))   parent gpr-ts-mode-indent-broken-offset)

     ;; typed_string_declaration
     ((query ((typed_string_declaration "(" (string_literal) _ @node ")")))
      (anchor/first-sibling "string_literal")
      0)
     ((query ((typed_string_declaration "(" _ @node ")")))
      (anchor/first-sibling "(")
      1)

     ;; attribute_reference / attribute_declaration
     ((query ((attribute_reference   "(" _ @node ")"))) (anchor/first-sibling "(") 1)
     ((query ((attribute_declaration "(" _ @node ")"))) (anchor/first-sibling "(") 1)
     ((query ((attribute_declaration "(" @node)))
      (anchor/first-sibling "identifier")
      gpr-ts-mode-indent-broken-offset)

     ;; general indentation for comments.
     ;;
     ;; NOTE: Indent to where next non-comment sibling would be
     ;; indented.  This may not be aligned to sibling if sibling isn't
     ;; properly indented, however it prevents a two-pass indentation
     ;; when region is indented, since comments won't have to be
     ;; reindented once sibling becomes properly aligned.
     ((and (node-is "comment")
           (gpr-ts-indent--next-sibling-not-matching-exists-p "comment"))
      (anchor/next-sibling-not-matching "comment")
      (offset/next-sibling-not-matching "comment"))

     ;; name / identifier / string_literal / string_literal_at / etc.
     ((query ((name _ @node)))            parent 0)
     ((query ((name)              @node)) parent gpr-ts-mode-indent-broken-offset)
     ((query ((identifier)        @node)) parent gpr-ts-mode-indent-broken-offset)
     ((query ((string_literal)    @node)) parent gpr-ts-mode-indent-broken-offset)
     ((query ((string_literal_at) @node)) parent gpr-ts-mode-indent-broken-offset)
     ((query (":" @node))                 parent gpr-ts-mode-indent-broken-offset)
     ((query (":=" @node))                parent gpr-ts-mode-indent-broken-offset)
     ((query ("(" @node))                 parent gpr-ts-mode-indent-broken-offset)

     ;; keywords
     ((query ([,@gpr-ts-mode--keywords ";"] @node)) parent 0)

     ;; If rule set is complete, this rule should never be matched.
     (catch-all (anchor/catch-all) (offset/catch-all)))))

;;; Indentation Verbosity

(defvar gpr-ts-indent--verbose nil
  "If non-nil, log process when indenting.")

(defun advice/treesit--indent-rules-optimize (oldfun &rest r)
  "Advice to prevent compiling tree-sitter queries.

OLDFUN is the original `treesit--indent-rules-optimize' function and R
are its called arguments.

Preventing the compilation of tree-sitter queries is necessary so that
the queries can be properly displayed when `treesit--indent-verbose' is
enabled rather than displaying `treesit-compiled-query', which is
unhelpful when debugging indentation rules."
  (if (and treesit--indent-verbose
           (derived-mode-p 'gpr-ts-mode))
      (cl-letf (((symbol-function 'treesit-query-compile)
                 (lambda (_lang query &optional _eager)
                   (cond ((stringp query) query)
                         ((treesit-compiled-query-p query) query)
                         (t (treesit-query-expand query))))))
        (apply oldfun r))
    (apply oldfun r)))

(defun gpr-ts-indent--verbosity-config (symbol newval operation where)
  "Configure `gpr-ts-mode' indent verbosity.

SYMBOL is expected to be `gpr-ts-indent--verbose', OPERATION is queried
to check that it is a `set' operation (as defined by
`add-variable-watcher'), and WHERE is queried to check that it is
nil (i.e., not buffer local), otherwise nothing is updated.

When SYMBOL is `gpr-ts-indent--verbose' and NEWVAL is non-nil, rebuild
indentation rules with string queries for easier debugging, otherwise
rebuild rules with compiled queries for performance."
  (when (and (eq operation 'set)
             (null where)
             (eq symbol 'gpr-ts-indent--verbose))
    (let ((recompute-rules
           (lambda ()
             ;; Recompute indent rules to compile/not compile queries due to
             ;; the removal/addition of the `treesit--indent-rules-optimize'
             ;; advice.
             (dolist (buffer (buffer-list))
               (with-current-buffer buffer
                 (when (derived-mode-p 'gpr-ts-mode)
                   (message "Building %s indent queries for %s"
                            (if newval "uncompiled" "compiled")
                            (buffer-name))
                   (setq-local treesit-simple-indent-rules
                               (treesit--indent-rules-optimize
                                gpr-ts-indent--rules))))))))
      (cond (newval
             (setq treesit--indent-verbose t)
             (advice-add 'treesit--indent-rules-optimize
                         :around #'advice/treesit--indent-rules-optimize)
             (funcall recompute-rules))
            (gpr-ts-indent--verbose
             (setq treesit--indent-verbose nil)
             (advice-remove 'treesit--indent-rules-optimize
                            #'advice/treesit--indent-rules-optimize)
             (funcall recompute-rules))))))

(gpr-ts-indent--verbosity-config 'gpr-ts-indent--verbose gpr-ts-indent--verbose 'set nil)

(add-variable-watcher 'gpr-ts-indent--verbose #'gpr-ts-indent--verbosity-config)

;;; Best-Effort Indentation

(defun gpr-ts-indent--point-at-indentation (node)
  "Find position at indentation from start of NODE."
  (save-excursion
    (goto-char (treesit-node-start node))
    (back-to-indentation)
    (point)))

(defun gpr-ts-indent--is-keyword-anchor (node)
  "Find anchor node for \\='is\\=' keyword NODE."
  (when-let* ((anchor-node
               (gpr-ts-mode--matching-prev-node
                node
                '("case" "package" "project" "type")))
              (anchor-node-t (treesit-node-type anchor-node)))
    (treesit-node-at
     (gpr-ts-indent--point-at-indentation anchor-node))))

(defun gpr-ts-indent--best-effort (node _parent bol)
  "Attempt best effort to determine indentation of NODE at BOL."
  (when gpr-ts-indent--verbose
    (message "*** ORIG-NODE: %s" node))

  (setq node (treesit-node-at bol))
  (when (or (> (treesit-node-start node) bol)
            (<= (treesit-node-end node) bol))
    (setq node nil))

  (let* ((node-t (treesit-node-type node))
         (prev-node (gpr-ts-mode--prev-node bol))
         (prev-node-t (treesit-node-type prev-node))
         anchor offset scenario)
    (when gpr-ts-indent--verbose
      (message "*** NODE: %s" node)
      (message "*** BOL: %s" bol)
      (message "*** PREV-NODE: %s" prev-node))
    (unless prev-node
      (setq anchor (point-min)
            offset 0
            scenario "Scenario [No previous node]"))
    ;; Keyword: "is"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "is")))
                (anchor-node (gpr-ts-indent--is-keyword-anchor node)))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Keyword: 'is']"))
    ;; Keyword: "end"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "end")))
                (anchor-node (gpr-ts-mode--matching-prev-node
                              node
                              '("case" "package" "project")))
                (anchor-node-t (treesit-node-type anchor-node)))
      (setq anchor (gpr-ts-indent--point-at-indentation anchor-node)
            offset 0
            scenario "Scenario [Keyword: 'end']"))
    ;; Keyword: "when"
    (when-let* (((not anchor))
                ((and node-t (string-equal node-t "when")))
                (anchor-node (gpr-ts-mode--matching-prev-node node "case")))
      (setq anchor (treesit-node-start anchor-node)
            offset gpr-ts-mode-indent-when-offset
            scenario "Scenario [Keyword: 'when']"))
    ;; After Keyword: "is"
    (when-let* (((not anchor))
                ((string-equal prev-node-t "is"))
                (anchor-node (gpr-ts-indent--is-keyword-anchor prev-node))
                (anchor-node-s (treesit-node-start anchor-node))
                (anchor-node-t (treesit-node-type anchor-node)))
      (setq anchor anchor-node-s
            offset (cond ((string-equal anchor-node-t "case") gpr-ts-mode-indent-when-offset)
                         ((string-equal anchor-node-t "type") gpr-ts-mode-indent-broken-offset)
                         (t                                   gpr-ts-mode-indent-offset))
            scenario "Scenario [After Keyword: 'is']"))
    ;; After Punctuation: "=>"
    (when-let* (((not anchor))
                (prev-leaf-node (gpr-ts-mode--prev-leaf-node bol))
                (prev-leaf-node-t (treesit-node-type prev-leaf-node))
                ((string-equal prev-leaf-node-t "=>"))
                (anchor-node (gpr-ts-mode--matching-prev-node prev-leaf-node "when"))
                (anchor-node-t (treesit-node-type anchor-node)))
      (setq anchor (treesit-node-start anchor-node)
            offset gpr-ts-mode-indent-offset
            scenario "Scenario [After Punctuation: '=>']"))
    ;; Punctuation: "("
    (when-let* (((not anchor))
                ((and node (string-equal node-t "(")))
                (prev-leaf-node (gpr-ts-mode--prev-leaf-node node))
                (prev-leaf-node-t (treesit-node-type prev-leaf-node))
                ((string-equal prev-leaf-node-t "identifier"))
                (anchor-node (treesit-parent-while
                              prev-leaf-node
                              (lambda (node)
                                (member (treesit-node-type node) '("identifier" "name"))))))
      (setq anchor (gpr-ts-indent--point-at-indentation anchor-node)
            offset gpr-ts-mode-indent-broken-offset
            scenario "Scenario [Punctuation: '(']"))
    ;; Punctuation: ")"
    (when-let* (((not anchor))
                ((and node (string-equal node-t ")")))
                (anchor-node (gpr-ts-mode--matching-prev-node node "(")))
      (setq anchor (treesit-node-start anchor-node)
            offset 0
            scenario "Scenario [Punctuation: ')']"))
    ;; After ";", "," and "|"
    (when-let* (((not anchor))
                ((member prev-node-t '(";" "," "|")))
                (prev-prev-node (gpr-ts-mode--prev-node prev-node)))
      (setq anchor (treesit-node-start prev-prev-node)
            offset 0
            scenario "Scenario [After ';', ',', and '|']"))
    ;; Newline after identifer/name
    (when-let* (((not anchor))
                ((not node))
                (prev-leaf-node (gpr-ts-mode--prev-leaf-node (point)))
                (prev-leaf-node-t (treesit-node-type prev-leaf-node))
                ((string-equal prev-leaf-node-t "identifier"))
                (anchor-node (treesit-parent-while
                              prev-leaf-node
                              (lambda (node)
                                (member (treesit-node-type node) '("identifier" "name"))))))
      (setq anchor (gpr-ts-indent--point-at-indentation anchor-node)
            offset gpr-ts-mode-indent-broken-offset
            scenario "Scenario [Newline after identifier/name"))
    ;; Newline after case_item
    (when-let* (((not anchor))
                ((not node))
                ((string-equal prev-node-t "case_item")))
      (setq anchor (treesit-node-start prev-node)
            offset gpr-ts-mode-indent-offset
            scenario "Scenario [Newline after case_item]"))
    ;; After Punctuation: ":="
    (when-let* (((not anchor))
                ((string-equal prev-node-t ":="))
                (prev-anchor-node (gpr-ts-mode--matching-prev-node
                                   prev-node
                                   (lambda (node)
                                     (let ((node-t (treesit-node-type node)))
                                       (or (member node-t '("is" "(" ";"))
                                           (string-equal
                                            (treesit-node-type
                                             (treesit-node-at (1- (treesit-node-end node))))
                                            ";"))))))
                (anchor-node (gpr-ts-mode--next-node prev-anchor-node)))
      (setq anchor (treesit-node-start anchor-node)
            offset gpr-ts-mode-indent-broken-offset
            scenario "Scenario [After Punctuation: ':=']"))
    ;; After keywords
    (when-let* (((not anchor))
                ((member prev-node-t gpr-ts-mode--keywords)))
      (setq anchor (gpr-ts-indent--point-at-indentation prev-node)
            offset (if (and node
                            (member
                             (treesit-node-type (treesit-node-at (treesit-node-start node)))
                             gpr-ts-mode--keywords))
                       0 ;; Adjacent keywords (e.g., "aggregate library")
                     gpr-ts-mode-indent-broken-offset)
            scenario "Scenario [After keywords]"))
    ;; Previous Punctuation: "("
    (when-let* (((not anchor))
                ((string-equal prev-node-t "(")))
      (setq anchor (treesit-node-start prev-node)
            offset 1
            scenario "Scenario [Previous Punctuation: '(']"))
    ;; After Declaration
    (when-let* (((not anchor))
                ((gpr-ts-mode--declaration-p prev-node)))
      ;; Anchor to beginning of line to handle multiple items per line
      ;; (e.g., with "foo"; with "bar";)
      (setq anchor (gpr-ts-indent--point-at-indentation prev-node)
            offset 0
            scenario "Scenario [After Declaration]"))
    ;; Fallback
    (unless anchor
      (setq scenario "Scenario [fallback]")
      (if (string-equal
           (treesit-node-type
            (treesit-node-at (1- (treesit-node-end prev-node))))
           ";")
          (setq anchor (treesit-node-start prev-node)
                offset 0)
        (setq anchor (treesit-node-start prev-node)
              offset gpr-ts-mode-indent-broken-offset)))
    (when gpr-ts-indent--verbose
      (message scenario))
    (cons anchor (apply #'+ (ensure-list offset)))))

;;; Indentation Anchors and Offsets

(defun anchor/best-effort ()
  "Determine best-effort anchor."
  (lambda (node parent bol &rest _)
    (let ((anchor (car (gpr-ts-indent--best-effort node parent bol))))
      (when gpr-ts-indent--verbose
        (message "Anchor: %s" anchor))
      anchor)))

(defun offset/best-effort ()
  "Determine best-effort offset."
  (lambda (node parent bol &rest _)
    (let ((offset (cdr (gpr-ts-indent--best-effort node parent bol))))
      (when gpr-ts-indent--verbose
        (message "Offset: %s" offset))
      offset)))

;; NOTE: This function is overridden in the test harness to detect if
;; an indentation test attempts to use a "catch-all" rule, which is an
;; indication of a missing formal rule.
(defun anchor/catch-all ()
  "Determine catch-all anchor."
  (anchor/best-effort))

(defun offset/catch-all ()
  "Determine catch-all offset."
  (offset/best-effort))

(defun anchor/first-sibling (type &rest types)
  "Determine BOL anchor for first sibling matching TYPE.

If TYPES is provided, then match the first sibling whose type matches
any of the types in TYPE or TYPES."
  (let ((all-types (cons type types)))
    (lambda (_node parent &rest _)
      (when-let* ((sibling-node
                   (car
                    (treesit-filter-child
                     parent
                     (lambda (n)
                       (member (treesit-node-type n) all-types))))))
        (treesit-node-start sibling-node)))))

(defun gpr-ts-indent--next-sibling-not-matching (type &rest types)
  "Locate next sibling not matching TYPE or TYPES."
  (lambda (node _parent _bol &rest _)
    (let ((all-types (cons type types))
          (sibling-node (treesit-node-next-sibling node)))
      (while (and sibling-node
                  (seq-some (lambda (a-type)
                              (equal (treesit-node-type sibling-node) a-type))
                            all-types))
        (setq sibling-node (treesit-node-next-sibling sibling-node)))
      sibling-node)))

(defalias 'gpr-ts-indent--next-sibling-not-matching-exists-p
  'gpr-ts-indent--next-sibling-not-matching)

(defun anchor/next-sibling-not-matching (type &rest types)
  "Determine indentation anchor of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'gpr-ts-indent--next-sibling-not-matching all-types) node parent bol)))
      (car (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

(defun offset/next-sibling-not-matching (type &rest types)
  "Determine indentation offset of next sibling not matching TYPE or TYPES."
  (lambda (node parent bol &rest _)
    (let* ((all-types (cons type types))
           (sibling-node
            (funcall (apply #'gpr-ts-indent--next-sibling-not-matching all-types) node parent bol)))
      (cdr (treesit-simple-indent sibling-node parent (treesit-node-start sibling-node))))))

;;; Indent Line

(cl-defgeneric gpr-ts-indent--line (strategy)
  "Indent according to STRATEGY."
  (error "Unknown indentation strategy: %s" strategy))

(cl-defmethod gpr-ts-indent--line ((_strategy (eql line)))
  "Indent according to line STRATEGY."
  (treesit-indent))

(cl-defmethod gpr-ts-indent--line ((_strategy (eql declaration)))
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

(defvar-local gpr-ts-indent--last-indent-tick nil)

(defun gpr-ts-indent-line ()
  "Indent according to `gpr-ts-mode-indent-strategy'."
  (prog1
      (gpr-ts-indent--line gpr-ts-mode-indent-strategy)
    (setq gpr-ts-indent--last-indent-tick (buffer-chars-modified-tick))))

(defun gpr-ts-indent-region (beg end)
  "Perform region indentation between BEG and END."
  (prog1
      (treesit-indent-region beg end)
    (setq gpr-ts-indent--last-indent-tick (buffer-chars-modified-tick))))

;;; Electric Indentation

(defconst gpr-ts-indent--electric-punctuation
  '(";" ")" "=>", ",")
  "Punctuation which should trigger electric indentation.")

(defconst gpr-ts-indent--electric-keywords
  '("end" "when")
  "Keywords which should trigger electric indentation.")

(defun gpr-ts-indent--electric-indent-p (&optional _char)
  "Determine if electric indentation should be performed.

When triggered by `self-insert-command', CHAR will be the character
inserted, else nil."
  (when-let* (((not (bobp)))
              (node (treesit-node-at (1- (point))))
              (end (treesit-node-end node))
              ((= end (point)))
              (start (treesit-node-start node))
              (type (treesit-node-type node)))
    (or (member type gpr-ts-indent--electric-punctuation)
        (and (= start
                (save-excursion
                  (back-to-indentation)
                  (point)))
             (or (member type gpr-ts-indent--electric-keywords)
                 ;; Re-indent identifier that looked like a keyword
                 ;; (and was likely indented as a keyword) before last
                 ;; key press.
                 (and (string-equal type "identifier")
                      (let* ((text (treesit-node-text node 'no-property))
                             (text- (substring text 0 (1- (length text)))))
                        (member-ignore-case
                         text-
                         gpr-ts-indent--electric-keywords))))))))

(defvar-local gpr-ts-indent--electric-indent-check-needed nil)

(defun gpr-ts-indent--maybe-electric-indent ()
  "Maybe perform electric indentation."
  (when gpr-ts-indent--electric-indent-check-needed
    (when (and (or (null gpr-ts-indent--last-indent-tick)
                   (not (= gpr-ts-indent--last-indent-tick
                           (buffer-chars-modified-tick))))
               (gpr-ts-indent--electric-indent-p))
      (ignore-errors (indent-according-to-mode)))
    (setq gpr-ts-indent--electric-indent-check-needed nil)))

(defun gpr-ts-indent--after-change (beg end length)
  "Buffer local after-change function.

Only check indentation on text insertion (i.e., LENGTH = 0) or text
replacement (BEG /= END), but ignore changes that are deletions
only (BEG = END and LENGTH /= 0) as the user typically does not want to
cause electric indentation through deletion of characters immediately
following electric punctuation or electric keywords."
  (when (and (or (zerop length)
                 (/= beg end))
             (bound-and-true-p electric-indent-mode)
             (not (bound-and-true-p electric-indent-inhibit)))
    (setq gpr-ts-indent--electric-indent-check-needed t)))

;;; Indentation Setup

(defun gpr-ts-indent--setup ()
  "Setup indentation for buffer."
  (setq-local treesit-simple-indent-rules gpr-ts-indent--rules)

  ;; When `electric-indent-mode' is enabled, it only performs electric
  ;; condition checks after `self-insert-command'.  There are other
  ;; commands which can modify the buffer (e.g., `completion-at-point'),
  ;; but no check is performed by `electric-indent-mode'.
  ;;
  ;; As a workaround, hook into `after-change-functions' to know when
  ;; the buffer has changed.  Also hook into `post-command-hook' to
  ;; perform the electric condition check when `electric-indent-mode' is
  ;; enabled and we've detected a buffer change that wasn't due to
  ;; indentation.
  (add-hook 'after-change-functions #'gpr-ts-indent--after-change          nil 'local)
  (add-hook 'post-command-hook      #'gpr-ts-indent--maybe-electric-indent nil 'local))

(defun gpr-ts-indent--post-setup ()
  "Indentation setup performed after `treesit-major-mode-setup'."
  (setq-local indent-line-function   #'gpr-ts-indent-line)
  (setq-local indent-region-function #'gpr-ts-indent-region))

(add-hook 'gpr-ts-mode--after-setup-hook #'gpr-ts-indent--post-setup)

;;; Commands

;; Backport `prog-fill-reindent-defun' to Emacs 29 and avoid the
;; Emacs 30 issue where `prog-fill-reindent-defun' would reindent
;; the wrong defun, as reported in https://debbugs.gnu.org/78703.

(if (and (boundp 'prog-fill-reindent-defun-function)
         (fboundp 'prog-fill-reindent-defun))
    (defalias 'gpr-ts-mode-fill-reindent-defun #'prog-fill-reindent-defun)
  (defun gpr-ts-mode-fill-reindent-defun (&optional justify)
    "Refill or reindent the paragraph or defun containing point.

If the point is in a comment, fill the paragraph that contains point or
follows point.  Otherwise, re-indent the defun that contains point.

If JUSTIFY is non-nil (interactively, with prefix argument), and filling
a paragraph, justify as well."
    (interactive "P" gpr-ts-mode)
    (save-excursion
      (if-let* ((node (treesit-node-at (point)))
                (node-t (treesit-node-type node))
                ((string-equal node-t "comment")))
          (fill-paragraph justify (region-active-p))
        (when-let* ((node (treesit-defun-at-point))
                    (start (treesit-node-start node))
                    (end (treesit-node-end node)))
          (indent-region start end nil))))))

(defun gpr-ts-mode-reindent-buffer ()
  "Reindent buffer."
  (interactive nil gpr-ts-mode)
  (without-restriction
    (indent-region (point-min) (point-max))))

(provide 'gpr-ts-indent)

;;; gpr-ts-indent.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("advice/" . "gpr-ts-indent--advice-")
;;                          ("anchor/" . "gpr-ts-indent--anchor-")
;;                          ("offset/" . "gpr-ts-indent--offset-"))
;; End:
