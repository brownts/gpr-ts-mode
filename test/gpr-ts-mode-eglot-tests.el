;;; gpr-ts-mode-eglot-tests.el --- Tests specific to Eglot and LSP -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Troy Brown

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

;;; Code:

(require 'gpr-ts-mode)
(require 'gpr-ts-mode-test-utils)
(require 'eglot)
(require 'ert)
(require 'ert-x)

(ert-deftest gpr-ts-mode-test-eglot-config-exists ()
  "Tests that Eglot contains a server configuration for `gpr-ts-mode'."
  (should (gpr-ts-lspclient-eglot--find-mode-config 'gpr-ts-mode)))

(ert-deftest gpr-ts-mode-test-eglot-config-language ()
  "Tests that Eglot correctly determines language for `gpr-ts-mode'."
  (skip-unless (and (gpr-ts-lspclient-eglot--find-mode-config 'gpr-ts-mode)
                    (executable-find "ada_language_server")))
  (with-file-in-project
      "hello_world.gpr"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server eglot
      (let ((language
             (cond ((functionp 'eglot--languageId)
                    (eglot--languageId (eglot-current-server)))
                   ((functionp 'eglot--language-id)
                    (eglot--language-id (eglot-current-server)))
                   (t (ert-fail "Unknown language query API")))))
        (should (stringp language))
        (should (string-equal language "gpr"))))))

(ert-deftest gpr-ts-mode-test-eglot-setup ()
  "Tests that Eglot is setup correctly for `gpr-ts-mode'."
  (skip-unless (and (gpr-ts-lspclient-eglot--find-mode-config 'gpr-ts-mode)
                    (executable-find "ada_language_server")))
  (with-file-in-project
      "hello_world.gpr"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server eglot
      (should (local-variable-p 'eglot-stay-out-of))
      (should (equal (symbol-value 'eglot-stay-out-of)
                     '(imenu)))
      (should (local-variable-p 'eglot-ignored-server-capabilites))
      (should (equal (symbol-value 'eglot-ignored-server-capabilites)
                     '(:documentOnTypeFormattingProvider
                       :semanticTokensProvider))))))

(provide 'gpr-ts-mode-eglot-tests)

;;; gpr-ts-mode-eglot-tests.el ends here
