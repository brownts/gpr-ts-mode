;;; gpr-ts-lspclient-eglot.el -- LSP client interface for Eglot -*- lexical-binding: t; -*-

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

;;; Commentary:

;;; Code:

(require 'gpr-ts-lspclient)
(require 'eglot)

;;;; Customization

(defcustom gpr-ts-lspclient-eglot-stay-out-of
  '(;; Let major mode control Imenu
    imenu)
  "Mode specific settings for Eglot's `eglot-stay-out-of'."
  :type '(repeat symbol)
  :group 'gpr-ts-lspclient
  :link '(custom-manual :tag "LSP Client Support" "(gpr-ts-mode)LSP Client Support")
  :package-version '(gpr-ts-mode . "0.8.0"))

(defcustom gpr-ts-lspclient-eglot-ignored-server-capabilities
  '(;; Interferes with Emacs indenting
    ;; See: https://github.com/AdaCore/ada_language_server/issues/1197
    :documentOnTypeFormattingProvider
    ;; No benefit for major mode
    :semanticTokensProvider)
  "Mode specific settings for Eglot's `eglot-ignored-server-capabilities'."
  :type '(repeat symbol)
  :group 'gpr-ts-lspclient
  :link '(custom-manual :tag "LSP Client Support" "(gpr-ts-mode)LSP Client Support")
  :package-version '(gpr-ts-mode . "0.8.0"))

;;;; Configuration

(defun gpr-ts-lspclient-eglot--find-mode-config (mode-to-find)
  "Find Eglot server configuration for MODE-TO-FIND."
  (seq-find
   (pcase-lambda (`(,modes . ,contact))
     (when (or (and (symbolp modes)
                    (eq mode-to-find modes))
               (and (listp modes)
                    (keywordp (cadr modes))
                    (eq mode-to-find (car modes)))
               (and (listp modes)
                    (seq-find
                     (lambda (mode)
                       (or (and (symbolp mode)
                                (eq mode-to-find mode))
                           (and (listp mode)
                                (keywordp (cadr mode))
                                (eq mode-to-find (car mode)))))
                     modes)))
       (cons modes contact)))
   (progn (require 'eglot)
          eglot-server-programs)))

(defun gpr-ts-lspclient-eglot--config ()
  "Configure Eglot for mode.

No configuration was provided for `gpr-ts-mode' in the version of Eglot
as shipped with Emacs 29, so it is added if it cannot be found.

The language id was not properly inferred for tree-sitter major modes in
the version of Eglot shipped with Emacs 29, so the language id is
included if the mode configuration must be added."
  (unless (gpr-ts-lspclient-eglot--find-mode-config 'gpr-ts-mode)
    (if-let* ((config '(gpr-ts-mode :language-id "gpr"))
              (entry (gpr-ts-lspclient-eglot--find-mode-config 'gpr-mode))
              (modes (car entry))
              (contact (cdr entry))
              (new-modes
               (cond ((symbolp modes)
                      (list modes config))
                     ((and (listp modes)
                           (keywordp (cadr modes)))
                      (list modes config))
                     ((listp modes)
                      (append modes (list config)))
                     (t nil))))
        ;; Update existing GPR configuration
        (add-to-list 'eglot-server-programs (cons new-modes contact))
      ;; Add GPR configuration
      (add-to-list 'eglot-server-programs
                   (list `((gpr-mode :language-id "gpr") ,config) "ada_language_server" "--language-gpr")))))

(gpr-ts-lspclient-eglot--config)

;;;; Setup

(defun gpr-ts-lspclient-eglot--setup ()
  "Mode specific settings for Eglot."
  (when gpr-ts-lspclient-eglot-stay-out-of
    (setq-local eglot-stay-out-of
                (seq-union (default-value 'eglot-stay-out-of)
                           gpr-ts-lspclient-eglot-stay-out-of)))
  (when gpr-ts-lspclient-eglot-ignored-server-capabilities
    (setq-local eglot-ignored-server-capabilities
                (seq-union (default-value 'eglot-ignored-server-capabilities)
                           gpr-ts-lspclient-eglot-ignored-server-capabilities))))

(add-hook 'gpr-ts-lspclient-setup-hook #'gpr-ts-lspclient-eglot--setup)

(when (derived-mode-p 'gpr-ts-mode)
  (gpr-ts-lspclient-eglot--setup))

(provide 'gpr-ts-lspclient-eglot)

;;;###autoload
(with-eval-after-load 'gpr-ts-mode
  (with-eval-after-load 'eglot
    (require 'gpr-ts-lspclient-eglot)))

;;; gpr-ts-lspclient-eglot.el ends here
