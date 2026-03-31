;;; gpr-ts-lspclient-lsp-mode.el -- LSP client interface for lsp-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Troy Brown

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

(require 'gpr-ts-lspclient)

;;;; Customization

(defcustom gpr-ts-lspclient-lsp-mode-settings-alist
  '(;; Let major mode control Imenu
    (lsp-enable-imenu . nil)
    ;; Let major mode control indentation
    (lsp-enable-indentation . nil)
    ;; Interferes with Emacs indenting
    ;; See: https://github.com/AdaCore/ada_language_server/issues/1197
    (lsp-enable-on-type-formatting . nil)
    ;; No benefit for major mode
    (lsp-semantic-tokens-enable . nil))
  "Mode specific settings for `lsp-mode'."
  :type '(alist :key-type symbol :value-type boolean)
  :group 'gpr-ts-lspclient
  :risky t
  :link '(custom-manual :tag "LSP Client Support" "(gpr-ts-mode)LSP Client Support")
  :package-version '(gpr-ts-mode . "0.8.0"))

;;;; Setup

(defun gpr-ts-lspclient-lsp-mode--setup ()
  "Mode specific settings for `lsp-mode'."
  (dolist (setting gpr-ts-lspclient-lsp-mode-settings-alist)
    (let ((name (car setting))
          (value (cdr setting)))
      (set (make-local-variable name) value))))

(add-hook 'gpr-ts-lspclient-setup-hook #'gpr-ts-lspclient-lsp-mode--setup)

(when (derived-mode-p 'gpr-ts-mode)
  (gpr-ts-lspclient-lsp-mode--setup))

(provide 'gpr-ts-lspclient-lsp-mode)

;;;###autoload
(with-eval-after-load 'gpr-ts-mode
  (with-eval-after-load 'lsp-mode
    (require 'gpr-ts-lspclient-lsp-mode)))

;;; gpr-ts-lspclient-lsp-mode.el ends here
