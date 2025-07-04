;;; ligo-mode.el --- A major mode for editing LIGO source code  -*- lexical-binding: t -*-

;; Version: 0.2.0
;; Author: mattiasdrp (originally LigoLang SASU)
;; URL: https://github.com/mattiasdrp/ligo-mode
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

;; This file is distributed under the terms of the MIT license.

;;; Commentary:

;; This provides font lock and other support for the three dialects of
;; the Ligo smart contract language for the Tezos blockchain.

;; For users of `lsp-mode', setup can be performed automatically by
;; calling the command `ligo-setup-lsp', or with the following snippet
;; in an init file:

;;   (with-eval-after-load 'lsp-mode
;;     (with-eval-after-load 'ligo-mode
;;       (ligo-setup-lsp)))

;;; Code:

(defface ligo-font-lock-attribute-face
	'((t (:inherit font-lock-preprocessor-face )))
	"Face description for todos."
	:group 'ligo)

(defvar ligo-font-lock-attribute-face
	'ligo-font-lock-attribute-face)

(defface ligo-font-lock-character-face
	'((t (:inherit font-lock-string-face )))
	"Face description for characters."
	:group 'ligo)

(defvar ligo-font-lock-character-face
	'ligo-font-lock-character-face)

(defface ligo-font-lock-number-face
	'((t (:inherit default )))
	"Face description for numbers."
	:group 'ligo)

(defvar ligo-font-lock-number-face
	'ligo-font-lock-number-face)

(defface ligo-font-lock-float-face
	'((t (:inherit default )))
	"Face description for floats."
	:group 'ligo)

(defvar ligo-font-lock-float-face
	'ligo-font-lock-float-face)

(defface ligo-font-lock-builtin-function-face
	'((t (:inherit font-lock-function-name-face )))
	"Face description for builtin functions."
	:group 'ligo)

(defvar ligo-font-lock-builtin-function-face
	'ligo-font-lock-builtin-function-face)

(defface ligo-font-lock-statement-face
	'((t (:inherit font-lock-keyword-face )))
	"Face description for statements."
	:group 'ligo)

(defvar ligo-font-lock-statement-face
	'ligo-font-lock-statement-face)

(defface ligo-font-lock-conditional-face
	'((t (:inherit font-lock-keyword-face )))
	"Face description for conditionals."
	:group 'ligo)

(defvar ligo-font-lock-conditional-face
	'ligo-font-lock-conditional-face)

(defface ligo-font-lock-repeat-face
	'((t (:inherit font-lock-keyword-face )))
	"Face description for repeat keywords."
	:group 'ligo)

(defvar ligo-font-lock-repeat-face
	'ligo-font-lock-repeat-face)

(defface ligo-font-lock-label-face
	'(
		(((background dark)) (:foreground "#eedd82"))
		(t (:inherit font-lock-function-name-face )))
	"Face description for labels."
	:group 'ligo)

(defvar ligo-font-lock-label-face
	'ligo-font-lock-label-face)

(defface ligo-font-lock-operator-face
	'((t (:inherit default )))
	"Face description for operators."
	:group 'ligo)

(defvar ligo-font-lock-operator-face
	'ligo-font-lock-operator-face)

(defface ligo-font-lock-exception-face
	'(
		(((background light)) (:foreground "dark orange"))
		(((background dark)) (:foreground "orange")))
	"Face description for exceptions."
	:group 'ligo)

(defvar ligo-font-lock-exception-face
	'ligo-font-lock-exception-face)

(defface ligo-font-lock-builtin-type-face
	'((t (:inherit font-lock-type-face )))
	"Face description for builtin types."
	:group 'ligo)

(defvar ligo-font-lock-builtin-type-face
	'ligo-font-lock-builtin-type-face)

(defface ligo-font-lock-storage-class-face
	'((t (:inherit font-lock-keyword-face )))
	"Face description for storage classes."
	:group 'ligo)

(defvar ligo-font-lock-storage-class-face
	'ligo-font-lock-storage-class-face)

(defface ligo-font-lock-builtin-module-face
	'((t (:inherit font-lock-function-name-face )))
	"Face description for builtin modules."
	:group 'ligo)

(defvar ligo-font-lock-builtin-module-face
	'ligo-font-lock-builtin-module-face)

(defface ligo-font-lock-structure-face
	'((t (:inherit font-lock-constant-face )))
	"Face description for structures."
	:group 'ligo)

(defvar ligo-font-lock-structure-face
	'ligo-font-lock-structure-face)

(defface ligo-font-lock-type-def-face
	'((t (:inherit font-lock-type-face )))
	"Face description for type definitions."
	:group 'ligo)

(defvar ligo-font-lock-type-def-face
	'ligo-font-lock-type-def-face)

(defface ligo-font-lock-special-char-face
	'((t (:inherit font-lock-string-face )))
	"Face description for special characters."
	:group 'ligo)

(defvar ligo-font-lock-special-char-face
	'ligo-font-lock-special-char-face)

(defface ligo-font-lock-special-comment-face
	'((t (:inherit font-lock-comment-face )))
	"Face description for special comments."
	:group 'ligo)

(defvar ligo-font-lock-special-comment-face
	'ligo-font-lock-special-comment-face)

(defface ligo-font-lock-error-face
	'((t (:inherit error )))
	"Face description for errors."
	:group 'ligo)

(defvar ligo-font-lock-error-face
	'ligo-font-lock-error-face)

(defface ligo-font-lock-todo-face
	'((t (:inherit highlight )))
	"Face description for todos."
	:group 'ligo)

(defvar ligo-font-lock-todo-face
	'ligo-font-lock-todo-face)

(defgroup ligo nil
  "Support for LIGO code.";  :link '(url-link "https://www.ligolang.org/")
  :group 'languages)

(defcustom ligo-bin "ligo"
  "Path to LIGO executable."
  :type 'string
  :group 'ligo)

;; Forward declarations for byte compiler
(defvar lsp-language-id-configuration)
(declare-function lsp-register-client 'lsp-mode)
(declare-function make-lsp-client 'lsp-mode)
(declare-function lsp-stdio-connection 'lsp-mode)

;;;###autoload
(defun ligo-setup-lsp ()
  "Set up an LSP backend for ligo that will use `ligo-bin'."
  (interactive)
  (add-to-list 'lsp-language-id-configuration '(ligo-caml-mode . "ligo"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection `(,ligo-bin "lsp" "all-capabilities"))
    :major-modes '(ligo-caml-mode)
    :server-id 'ligo)))

(defun ligo-reload ()
	"Reload the ligo-mode code and re-apply the default major mode in the current buffer."
	(interactive)
	(unload-feature 'ligo-mode)
	(require 'ligo-mode)
	(normal-mode))

(require 'ligo-caml-mode)
(require 'jsligo-mode)

(provide 'ligo-mode)

;;; ligo-mode.el ends here
