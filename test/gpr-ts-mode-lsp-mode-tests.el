;;; gpr-ts-mode-lsp-mode-tests.el --- Tests specific to `lsp-mode' and LSP -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Troy Brown

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
(require 'lsp-mode nil 'noerror)
(require 'ert)
(require 'ert-x)

(ert-deftest gpr-ts-mode-test-lsp-mode-config-exists ()
  "Tests that Eglot contains a server configuration for `gpr-ts-mode'."
  (skip-unless (featurep 'lsp-mode))
  (require 'lsp-ada)
  (let ((client (gethash 'gpr-ls lsp-clients)))
    (should (lsp--client-p client))
    (should (memq 'gpr-ts-mode (lsp--client-major-modes client)))))

(ert-deftest gpr-ts-mode-test-lsp-mode-config-language ()
  "Tests that Eglot correctly determines language for `gpr-ts-mode'."
  (skip-unless (and (executable-find "ada_language_server")
                    (featurep 'lsp-mode)))
  (with-file-in-project
      "hello_world.gpr"
      (ert-resource-file "hello_world")
      "hello_world.gpr"
    (with-language-server lsp-mode
      (let ((language (lsp-buffer-language)))
        (should (stringp language))
        (should (string-equal language "gpr"))))))

(provide 'gpr-ts-mode-lsp-mode-tests)

;;; gpr-ts-mode-lsp-mode-tests.el ends here
