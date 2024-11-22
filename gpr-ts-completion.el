;;; gpr-ts-completion.el --- Completion support in GNAT project files -*- lexical-binding:t -*-

;; Copyright (C) 2024 Troy Brown

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
(eval-when-compile (require 'rx))
(require 'treesit)

(declare-function treesit-induce-sparse-tree "treesit.c" (root predicate &optional process-fn depth))
(declare-function treesit-node-check         "treesit.c" (node property))
(declare-function treesit-node-end           "treesit.c" (node))
(declare-function treesit-node-next-sibling  "treesit.c" (node &optional named))
(declare-function treesit-node-parent        "treesit.c" (node))
(declare-function treesit-node-start         "treesit.c" (node))
(declare-function treesit-node-type          "treesit.c" (node))

(declare-function gpr-ts-mode--defun-name "gpr-ts-mode" (node))
(declare-function gpr-ts-mode--tree-text  "gpr-ts-mode" (node &optional ignored-types region-beg-type))
(declare-function gpr-ts-mode--enclosing-compound "gpr-ts-mode" (compound-info pos))

(defcustom gpr-ts-mode-completion-categories
  '(end-name package-name attribute-name)
  "Completion categories."
  :type '(repeat symbol)
  :group 'gpr-ts
  :link '(custom-manual :tag "Completion" "(gpr-ts-mode)Completion")
  :package-version "0.7.0")

(defcustom gpr-ts-mode-completion-definitions
  `(
    ;; Intrinsic
    ;;  - gpr2-project-registry-attribute.adb
    ;;  - gpr2-project-registry-attribute.ads
    ;;  - gpr2-project-registry-attribute-description.adb
    ;;  - gpr2-project-registry-pack.adb
    ;;  - gpr2-project-registry-pack.ads
    ;;  - gpr2-project-registry-pack-description.adb

    ("Project"
     :attributes
     ("Archive_Builder"
      "Archive_Builder_Append_Option"
      "Archive_Indexer"
      "Archive_Suffix"
      "Canonical_Target"
      "Create_Missing_Dirs"
      "Config_Prj_File"
      "Default_Language"
      "Excluded_Source_Dirs"
      "Excluded_Source_Files"
      "Excluded_Source_List_File"
      "Exec_Dir"
      "External"
      "Externally_Built"
      "Ignore_Source_Sub_Dirs"
      "Inherit_Source_Path"
      "Interfaces"
      "Languages"
      "Leading_Library_Options"
      "Library_ALI_Dir"
      "Library_Auto_Init"
      "Library_Auto_Init_Supported"
      "Library_Builder"
      "Library_Dir"
      "Library_Encapsulated_Options"
      "Library_Encapsulated_Supported"
      "Library_GCC"
      "Library_Install_Name_Option"
      "Library_Interface"
      "Library_Kind"
      "Library_Major_Minor_Id_Supported"
      "Library_Name"
      "Library_Options"
      "Library_Partial_Linker"
      "Library_Reference_Symbol_Name"
      "Library_Rpath_Options"
      "Library_Src_Dir"
      "Library_Standalone"
      "Library_Support"
      "Library_Symbol_File"
      "Library_Symbol_Policy"
      "Library_Version"
      "Library_Version_Switches"
      "Locally_Removed_Files"
      "Main"
      "Name"
      "Object_Dir"
      "Object_Generated"
      "Object_Linked"
      "Object_Lister"
      "Object_Lister_Matcher"
      "Origin_Project"
      "Only_Dirs_With_Sources"
      "Project_Dir"
      "Project_Files"
      "Project_Path"
      "Required_Toolchain_Version"
      "Roots"
      "Run_Path_Option"
      "Run_Path_Origin"
      "Runtime"
      "Runtime_Dir"
      "Runtime_Library_Dir"
      "Runtime_Source_Dir"
      "Runtime_Source_Dirs"
      "Separate_Run_Path_Options"
      "Shared_Library_Minimum_Switches"
      "Shared_Library_Prefix"
      "Shared_Library_Suffix"
      "Source_Dirs"
      "Source_Files"
      "Source_List_File"
      "Symbolic_Link_Supported"
      "Target"
      "Toolchain_Description"
      "Toolchain_Name"
      "Toolchain_Path"
      "Toolchain_Version"
      "Warning_Message"))
    ("Binder"
     :attributes
     ("Bindfile_Option_Substitution"
      "Default_Switches"
      "Driver"
      "Objects_Path"
      "Objects_Path_File"
      "Prefix"
      "Required_Switches"
      "Switches"))
    ("Builder"
     :attributes
     ("Default_Switches"
      "Executable"
      "Executable_Suffix"
      "Global_Compilation_Switches"
      "Global_Config_File"
      "Global_Configuration_Pragmas"
      "Switches"))
    ("Clean"
     :attributes
     ("Artifacts_In_Exec_Dir"
      "Artifacts_In_Object_Dir"
      "Object_Artifact_Extensions"
      "Source_Artifact_Extensions"
      "Switches"))
    ("Compiler"
     :attributes
     ("Config_Body_File_Name"
      "Config_Body_File_Name_Index"
      "Config_Body_File_Name_Pattern"
      "Config_File_Switches"
      "Config_File_Unique"
      "Config_Spec_File_Name"
      "Config_Spec_File_Name_Index"
      "Config_Spec_File_Name_Pattern"
      "Default_Switches"
      "Dependency_Driver"
      "Dependency_Kind"
      "Dependency_Switches"
      "Driver"
      "Include_Path"
      "Include_Path_File"
      "Include_Switches"
      "Include_Switches_Via_Spec"
      "Language_Kind"
      "Leading_Required_Switches"
      "Local_Config_File"
      "Local_Configuration_Pragmas"
      "Mapping_Body_Suffix"
      "Mapping_File_Switches"
      "Mapping_Spec_Suffix"
      "Max_Command_Line_Length"
      "Multi_Unit_Object_Separator"
      "Multi_Unit_Switches"
      "Object_File_Suffix"
      "Object_File_Switches"
      "Object_Path_Switches"
      "Path_Syntax"
      "PIC_Option"
      "Required_Switches"
      "Response_File_Format"
      "Response_File_Switches"
      "Source_File_Switches"
      "Switches"
      "Trailing_Required_Switches"))
    ("Gnatls"
     :attributes
     ("Switches"))
    ("Install"
     :attributes
     ("ALI_Subdir"
      "Active"
      "Artifacts"
      "Exec_Subdir"
      "Install_Name"
      "Install_Project"
      "Lib_Subdir"
      "Link_Lib_Subdir"
      "Mode"
      "Prefix"
      "Project_Subdir"
      "Required_Artifacts"
      "Side_Debug"
      "Sources_Subdir"))
    ("Linker"
     :attributes
     ("Default_Switches"
      "Driver"
      "Export_File_Format"
      "Export_File_Switch"
      "Leading_Switches"
      "Linker_Options"
      "Map_File_Option"
      "Max_Command_Line_Length"
      "Required_Switches"
      "Response_File_Format"
      "Response_File_Switches"
      "Switches"
      "Trailing_Switches"))
    ("Naming"
     :attributes
     ("Body"
      "Body_Suffix"
      "Casing"
      "Dot_Replacement"
      "Implementation"
      "Implementation_Exceptions"
      "Implementation_Suffix"
      "Separate_Suffix"
      "Spec"
      "Spec_Suffix"
      "Specification"
      "Specification_Exceptions"
      "Specification_Suffix"))
    ("Remote"
     :attributes
     ("Excluded_Patterns"
      "Included_Patterns"
      "Included_Artifact_Patterns"
      "Root_Dir"))

    ;; External tools
    ;;  - lsp-gpr_external_tools.adb

    ("IDE"
     :attributes
     ("Artifacts_Dir"
      "Communication_Protocol"
      "Compiler_Command"
      "Connection_Tool"
      "Connection_Config_File"
      "Debugger_Command"
      "Gnat"
      "Gnatlist"
      "Program_Host"
      "VCS_File_Check"
      "VCS_Kind"
      "VCS_Log_Check"
      "VCS_Patch_Root"
      "VCS_Repository_Root"
      "Xref_Database"))
    ("Ant"
     :attributes
     ("Ant"
      "Antfile"
      "Switches"))
    ("Make"
     :attributes
     ("Makefile"
      "Switches"))

    ;; Extra tools

    ;; GNATsas
    ("Analyzer"
     :attributes
     ("Additional_Patterns"
      "Bug_Status"
      "Excluded_Source_Files"
      "Message_Patterns"
      "Not_A_Bug_Status"
      "Output_Dir"
      "Pending_Status"
      "Review_File"
      "Subdirs"))
    ;; GNATcheck
    ("Check"
     :attributes
     ("Default_Switches"
      "Switches"))
    ("Codepeer"
     :attributes
     ("Additional_Patterns"
      "Bug_Status"
      "CWE"
      "Database_Directory"
      "Excluded_Source_Files"
      "File_Patterns"
      "Message_Patterns"
      "Not_A_Bug_Status"
      "Output_Directory"
      "Pending_Status"
      "Server_URL"))
    ;; GNATcov
    ("Coverage"
     :attributes
     ("Excluded_Routines"
      "Excluded_Routines_List"
      "Excluded_Units"
      "Excluded_Units_List"
      "Ignored_Units"
      "Ignored_Units_List"
      "Routines"
      "Routines_List"
      "Switches"
      "Units"
      "Units_List"))
    ;; GNATdist
    ("DSA"
     :attributes
     ("Configuration_File"))
    ;; GNATdoc
    ("Documentation"
     :attributes
     ("Custom_Tags_Definition"
      "Doc_Pattern"
      "Documentation_Dir"
      "HTML_Custom_Dir"
      "Ignored_Subprojects"
      "Image_Dir"
      "Output_Dir"))
    ;; GNATemulator
    ("Emulator"
     :attributes
     ("Board"
      "Debug_Port"
      "Switches"))
    ;; GNATpp
    ("Pretty_Printer"
     :attributes
     ("Default_Switches"
      "Switches"))
    ;; GNATprove
    ("Prove"
     :attributes
     ("Proof_Dir"
      "Proof_Switches"
      "Switches"))
    ("QGen"
     :attributes
     ("Debug_Args"
      "Output_Dir"
      "Target"))
    ;; GNATstack
    ("Stack"
     :attributes
     ("Switches"))
    ;; GNATstub
    ("Stub"
     :attributes
     ("Default_Switches"
      "Switches"))
    ("GNATtest"
     :attributes
     ("Additional_Tests"
      "Default_Stub_Exclusion_List"
      "GNATtest_Mapping_File"
      "GNATtest_Switches"
      "Harness_Dir"
      "Skeletons_Default"
      "Stub_Exclusion_List"
      "Stubs_Dir"
      "Subdir"
      "Tests_Dir"
      "Tests_Root"))

    ;; Self-configuring tools

    ;; GNATformat (gnatformat-configuration.adb)
    ("Format"
     :attributes
     (("Charset"
       :doc "Charset to use for source decoding")
      ("End_Of_Line"
       :doc "End of line sequence: lf | crlf")
      ("Indentation"
       :doc "Indentation size")
      ("Indentation_Continuation"
       :doc "Continuation Line Indentation size")
      ("Indentation_Kind"
       :doc "Indentation kind: spaces | tabs")
      ("Width"
       :doc "Max line width"))))
  "Completion definitions.

Each definition should be of the form (PACKAGE-NAME . PROPS), where
PACKAGE-NAME is the GNAT Project package name to which the definition
corresponds.  \"Project\" is a special PACKAGE-NAME referring to the
top-level project.  PROPS should have the form:

   [KEYWORD VALUE]...

The following keywords are meaningful:

:doc

   VALUE must be a string which is a description of the package.  This
   is an optional property.

:attributes

   VALUE is a list of GNAT Project attributes applicable to the
   package (or project).  This is a required property.  Each attribute
   is a string representing the GNAT Project attribute name.  Each
   attribute may also be of the form (ATTRIBUTE-NAME . PROPS), where
   ATTRIBUTE-NAME is the GNAT Project attribute name to which the
   corresponding properties apply.  PROPS should have the form:

      [KEYWORD VALUE]...

   The following keywords are meaningful:

   :doc

      VALUE must be a string which is a description of the attribute.
      This is an optional property."
  :type '(alist :key-type string
                :value-type plist)
  :group 'gpr-ts
  :link '(custom-manual :tag "Completion" "(gpr-ts-mode)Completion")
  :package-version "0.7.0")

(defun gpr-ts-mode--compound-name (node)
  "Find name associated with compound NODE or node text if no name exists."
  (pcase (treesit-node-type node)
    ((or "package_declaration" "project_declaration")
     (gpr-ts-mode--defun-name node))
    ((or "package" "project") ; incomplete compound
     (let ((next (treesit-node-next-sibling node)))
       (while (and next (treesit-node-check next 'extra))
         (setq next (treesit-node-next-sibling next)))
       (let ((type (treesit-node-type next)))
         (when (and type
                    (or (string-equal type "identifier")
                        (string-equal type "name")))
           (substring-no-properties
            (gpr-ts-mode--tree-text next '("comment")))))))
    (_ (treesit-node-text node 'no-property))))

;;;; Completion Documentation Buffer

(defun gpr-ts-mode--doc-buffer (doc)
  "Retrieve and populate documentation buffer with DOC."
  (with-current-buffer (get-buffer-create "*gpr-ts-mode-doc*")
    (erase-buffer)
    (insert doc)
    (current-buffer)))

(defun gpr-ts-mode--package-doc-buffer (package-name)
  "Retrieve and populate PACKAGE-NAME documentation buffer."
  (when-let*
      ((package-definition
        (cdr (assoc-string package-name
                           gpr-ts-mode-completion-definitions
                           'case-fold)))
       (package-documentation
        (plist-get package-definition :doc)))
    (gpr-ts-mode--doc-buffer package-documentation)))

(defun gpr-ts-mode--attribute-doc-buffer (package-attributes attribute-name)
  "Retrieve and populate ATTRIBUTE-NAME documentation buffer.

PACKAGE-ATTRIBUTES is a list of attributes in the same package as
ATTRIBUTE-NAME."
  (when-let*
      ((attribute-definition
        (cdr-safe
         (seq-find
          (lambda (item)
            (cond ((stringp item)
                   (string-equal-ignore-case attribute-name item))
                  ((consp item)
                   (string-equal-ignore-case attribute-name (car item)))))
          package-attributes)))
       (attribute-documentation (plist-get attribute-definition :doc)))
    (gpr-ts-mode--doc-buffer attribute-documentation)))

;;;; Completion at Point

(defun gpr-ts-mode--completion-at-point ()
  "Return completion table for the current buffer."
  (seq-some
   (lambda (category)
     (gpr-ts-mode-completion-at-point category))
   gpr-ts-mode-completion-categories))

(cl-defgeneric gpr-ts-mode-completion-at-point (category)
  "Find completion table for CATEGORY."
  (error "Unknown completion category: %s" category))

(cl-defmethod gpr-ts-mode-completion-at-point ((_category (eql attribute-name)))
  "Find completion table for attribute name."
  (when-let* ((status (syntax-ppss))
              ((not (nth 3 status))) ;; Not in a string
              ((not (nth 4 status))) ;; Not in a comment
              (case-fold-search t))
    (cond
     ;; Attribute declaration
     ((looking-back (rx (or bol (+ whitespace))
                        "for"
                        (+ whitespace)
                        (* (or alnum "_")))
                    (pos-bol))
      (when-let* ((bounds
                   (let ((start nil)
                         (end nil))
                     (save-excursion
                       ;; Get to first non-whitespace character near point
                       (re-search-backward (rx whitespace))
                       (re-search-forward (rx point (1+ whitespace)) (pos-eol))
                       (unless (eolp)
                         (setq start (point))
                         (if (re-search-forward (rx point (+ (or alnum "_"))) (pos-eol) 'no-error)
                             ;; Found name
                             (setq end (point))
                           ;; No name found, but something else on line
                           (setq start nil))))
                     (if (null start)
                         (cons (point) (point))
                       (cons start end)))))
        (unless (<= (car bounds) (point) (cdr bounds))
          (if (string-equal-ignore-case
               (buffer-substring-no-properties (car bounds) (cdr bounds))
               "use")
              ;; Don't consider "use" keyword as attribute name
              (setq bounds (cons (point) (point)))
            (setq bounds nil)))
        (when bounds
          (let* ((for-node
                  (save-excursion
                    (re-search-backward (rx (or bol whitespace) "for" whitespace) (pos-bol))
                    (treesit-node-at (point))))
                 ;; Find enclosing package
                 (package-node
                  (gpr-ts-mode--enclosing-compound
                   '(("package" . (:compound-type "package_declaration")))
                   (treesit-node-start for-node)))
                 ;; Find compound name
                 (compound-name
                  (if package-node
                      (gpr-ts-mode--compound-name package-node)
                    "Project")))
            (when-let* ((compound-name)
                        ;; Lookup attributes
                        (compound-definition
                         (cdr (assoc-string compound-name
                                            gpr-ts-mode-completion-definitions
                                            'case-fold)))
                        (compound-attributes
                         (plist-get compound-definition :attributes))
                        (attribute-names
                         (seq-map
                          (lambda (item)
                            (cond ((stringp item) item)
                                  ((consp item) (car item))))
                          compound-attributes)))
              (list
               (car bounds)
               (cdr bounds)
               (completion-table-case-fold attribute-names)
               :annotation-function (lambda (_) " Attribute")
               :company-kind (lambda (_) 'property)
               :company-doc-buffer
               (apply-partially
                #'gpr-ts-mode--attribute-doc-buffer compound-attributes)
               :exclusive 'no))))))
     ;; Attribute Reference
     ((looking-back (rx (+ (or alnum "_"))
                        "'"
                        (* (or alnum "_")))
                    (pos-bol))

      (let (bounds compound-name)
        (save-excursion
          ;; Move to end of attribute name
          (re-search-forward (rx (+ (or alnum "_"))) (pos-eol) 'no-error)
          ;; Extract package/project name and attribute name
          (re-search-backward
           (rx (or bol (not (any alnum "_")))
               (group (+ (or alnum "_"))) ; Package (or Project)
               "'"
               (group (* (or alnum "_"))))) ; Attribute
          (setq bounds
                (cons (match-beginning 2)
                      (match-end 2)))
          (setq compound-name
                (buffer-substring-no-properties
                 (match-beginning 1)
                 (match-end 1)))
          (unless
              (assoc-string compound-name
                            gpr-ts-mode-completion-definitions
                            'case-fold)
            (setq compound-name "Project")))
        (when-let* ((compound-definition
                     (cdr (assoc-string compound-name
                                        gpr-ts-mode-completion-definitions
                                        'case-fold)))
                    (compound-attributes
                     (plist-get compound-definition :attributes))
                    (attribute-names
                     (seq-map
                      (lambda (item)
                        (cond ((stringp item) item)
                              ((consp item) (car item))))
                      compound-attributes)))
          (list
           (car bounds)
           (cdr bounds)
           (completion-table-case-fold attribute-names)
           :annotation-function (lambda (_) " Attribute")
           :company-kind (lambda (_) 'property)
           :company-doc-buffer
           (apply-partially
            #'gpr-ts-mode--attribute-doc-buffer compound-attributes)
           :exclusive 'no)))))))

(cl-defmethod gpr-ts-mode-completion-at-point ((_category (eql package-name)))
  "Find completion table for package name."
  (when-let* ((status (syntax-ppss))
              ((not (nth 3 status))) ;; Not in a string
              ((not (nth 4 status))) ;; Not in a comment
              (case-fold-search t)
              ((looking-back (rx (or bol (+ whitespace))
                                 "package"
                                 (+ whitespace)
                                 (* (or alnum "_")))
                             (pos-bol)))
              (bounds
               (let ((start nil)
                     (end nil))
                 (save-excursion
                   ;; Get to first non-whitespace character near point
                   (re-search-backward (rx whitespace))
                   (re-search-forward (rx point (1+ whitespace)) (pos-eol))
                   (unless (eolp)
                     (setq start (point))
                     (if (re-search-forward (rx point (+ (or alnum "_"))) (pos-eol) 'no-error)
                         ;; Found name
                         (setq end (point))
                       ;; No name found, but something else on line
                       (setq start nil))))
                 (if (null start)
                     (cons (point) (point))
                   (cons start end)))))
    (unless (<= (car bounds) (point) (cdr bounds))
      (if (string-equal-ignore-case
           (buffer-substring-no-properties (car bounds) (cdr bounds))
           "is")
          ;; Don't consider "is" keyword as package name
          (setq bounds (cons (point) (point)))
        (setq bounds nil)))
    (when bounds
      (list
       (car bounds)
       (cdr bounds)
       (completion-table-case-fold
        (seq-difference
         (seq-map #'car gpr-ts-mode-completion-definitions)
         '("Project")
         #'string-equal-ignore-case))
       :annotation-function (lambda (_) " Package")
       :company-kind (lambda (_) 'module)
       :company-doc-buffer #'gpr-ts-mode--package-doc-buffer
       :exclusive 'no))))

(cl-defmethod gpr-ts-mode-completion-at-point ((_category (eql end-name)))
  "Find completion table for end name."
  (when-let* ((status (syntax-ppss))
              ((not (nth 3 status)))  ;; Not in a string
              ((not (nth 4 status)))  ;; Not in a comment
              (case-fold-search t)
              ((looking-back (rx (or bol (+ whitespace))
                                 "end"
                                 (+ whitespace)
                                 (* (or alnum "_" ".")))
                             (pos-bol)))
              (bounds
               (let ((start nil)
                     (end nil))
                 (save-excursion
                   ;; Get to first non-whitespace character near point
                   (re-search-backward (rx whitespace))
                   (re-search-forward (rx point (1+ whitespace)) (pos-eol))
                   (unless (eolp)
                     (setq start (point))
                     (if (re-search-forward (rx point (+ (or alnum "_" "."))) (pos-eol) 'no-error)
                         ;; Found name
                         (setq end (point))
                       ;; No name found, but something else on line
                       (setq start nil))))
                 (if (null start)
                     (cons (point) (point))
                   (cons start end))))
              (enclosing-node
               (gpr-ts-mode--enclosing-compound
                '(("case"    . ( :compound-type "case_construction"))
                  ("package" . ( :compound-type "package_declaration"))
                  ("project" . ( :compound-type "project_declaration"
                                 :predicate
                                 ;; Don't anchor to a "project" keyword
                                 ;; used in an attribute reference.
                                 (lambda (anchor &optional _node)
                                   (let* ((next (treesit-node-next-sibling anchor))
                                          (next-type (treesit-node-type next)))
                                     (or (null next-type)
                                         (not (string-equal next-type "'"))))))))
                (car bounds)))
              (compound-name (gpr-ts-mode--compound-name enclosing-node))
              (match-info
               (pcase (treesit-node-type enclosing-node)
                 ("case"
                  `( :text       ,compound-name
                     :kind       keyword
                     :annotation "Keyword"))
                 ("package"
                  `( :text       ,compound-name
                     :kind       module
                     :annotation "Package"))
                 ("project"
                  `( :text       ,compound-name
                     :kind       module
                     :annotation "Project")))))
    (unless (<= (car bounds) (point) (cdr bounds))
      ;; Point is not near bounds, so it's possible the bounds we have
      ;; are from invalid syntax and thus not really part of the name.
      ;; Check to see if its a prefix match for the candidate.  If
      ;; not, we assume the bounds don't represent a partial name and
      ;; instead we should insert the candidate at point.  Otherwise,
      ;; we don't complete since point is not near the candidate.
      (if (string-prefix-p
           (buffer-substring-no-properties (car bounds) (cdr bounds))
           (plist-get match-info :text)
           'ignore-case)
          (setq bounds nil)
        (setq bounds (cons (point) (point)))))
    (when bounds
      (list
       (min (point) (car bounds))
       (max (point) (cdr bounds))
       (completion-table-case-fold (list (plist-get match-info :text)))
       :annotation-function (lambda (_) (concat " " (plist-get match-info :annotation)))
       :company-kind (lambda (_) (plist-get match-info :kind))
       :exclusive 'no))))

(provide 'gpr-ts-completion)

;;; gpr-ts-completion.el ends here
