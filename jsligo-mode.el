;;; jsligo-mode.el --- A major mode for editing JSLIGO source code -*- lexical-binding: t -*-

;; Version: 0.2.0
;; Author: mattiasdrp (originally LigoLang SASU)
;; URL: https://github.com/mattiasdrp/ligo-mode
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

;; This file is distributed under the terms of the MIT license.

;;; Commentary:

;; This provides font lock and other support for the three dialects of
;; the JSLigo smart contract language for the Tezos blockchain.

;;; Code:

(defun jsligo-syntax-table ()
	"Syntax table"
	(let ((st (make-syntax-table)))
	  (modify-syntax-entry ?_ "w" st)
	  (modify-syntax-entry ?' "_" st)
	  (modify-syntax-entry ?. "'" st)
	  (modify-syntax-entry ?= "." st)
	  (modify-syntax-entry ?# "." st)
	  (modify-syntax-entry ?< "." st)
	  (modify-syntax-entry ?> "." st)
	  (modify-syntax-entry ?/ "." st)
	  (modify-syntax-entry ?* "." st)
	  (modify-syntax-entry ?- "." st)
	  (modify-syntax-entry ?+ "." st)
	  (modify-syntax-entry ?% "." st)
	  (modify-syntax-entry ?! "." st)
	  (modify-syntax-entry ?\" "\"" st)
	  (modify-syntax-entry ?` "\"" st)
	  (modify-syntax-entry ?* ". 23" st)
	  (modify-syntax-entry ?
                         "> b" st)
	  (modify-syntax-entry ?/ ". 124b" st)
	  st))

(defvar jsligo-font-lock-defaults
	`(
		(,"\\(@[a-zA-Z][a-zA-Z0-9_:.@%]*\\)"
		 . ligo-font-lock-attribute-face)
		(,"^\\(#[a-zA-Z]+\\)"
		 . font-lock-preprocessor-face)
		(,"\\b\\(let\\|const\\)\\b"
		 (1 font-lock-keyword-face))
		(,"\\b\\(export\\|import\\|from\\|contract_of\\|parameter_of\\|function\\|do\\|namespace\\|interface\\|implements\\|extends\\|false\\|true\\)\\b"
		 (1 font-lock-keyword-face))
		(,"\\b\\(switch\\|if\\|else\\|for\\|of\\|while\\|return\\|break\\|continue\\|match\\)\\b"
		 . ligo-font-lock-conditional-face)
		(,"\\b[-+]?\\([0-9]+\\)\\(n\\|\\tz\\|tez\\|mutez\\|\\)\\b"
		 . ligo-font-lock-number-face)
		(,"\\b\\(-\\|+\\|%\\|&&\\||\\||==\\|!=\\|<=\\|>=\\|<\\|>\\|\\*\\|/\\|=\\|!\\|\\*=\\|/=\\|%=\\|+=\\|-=\\)\\b"
		 . ligo-font-lock-operator-face)
		(,";")
		(,",")
		(,"?" ( 1 ligo-font-lock-operator-face))
		(,"\\bwhen\\b" ( 1 ligo-font-lock-conditional-face))
		(,"\\b\\([A-Z][a-zA-Z0-9_$]*\\)\\b"
		 (1 ligo-font-lock-structure-face))
		(,"\\b\\([a-z$_][a-zA-Z0-9$_]*\\)\\b"
		 (1 font-lock-variable-name-face))
		(,"\\b\\([A-Z][a-zA-Z0-9_$]*\\)\\.[:space:]*\\b\\([a-zA-Z0-9_$]*\\)\\b"
		 (1 ligo-font-lock-structure-face)
		 (2 font-lock-variable-name-face))
		(,"\\b\\(import\\)\\b[:space:]*\\b\\([A-Z][a-zA-Z0-9_$]*\\)\\b"
		 (1 ligo-font-lock-conditional-face)
		 (2 ligo-font-lock-structure-face))
		(,"{" ())
		(,"\\(" ())
		(,"\\b\\(case\\|default\\)\\b" ( 1 ligo-font-lock-conditional-face))
		(,"[:space:]*:" ( 1 ligo-font-lock-label-face 2 ligo-font-lock-operator-face))
		(,"[:space:]*:" ( 1 ligo-font-lock-number-face 2 ligo-font-lock-operator-face))
		(,"[:space:]*:" ( 1 font-lock-string-face 2 ligo-font-lock-operator-face))
		(,"[:space:]*:" ( 1 font-lock-variable-name-face 2 ligo-font-lock-operator-face))
		(,"<" ())
		(,"\\btype\\b" ( 1 font-lock-keyword-face))
		(,":" ( 1 ligo-font-lock-operator-face))
		(,":" ( 1 ligo-font-lock-operator-face))
		(,"\\bas\\b" ( 1 font-lock-keyword-face))
		(,"\\(=>\\|\\.\\||\\)"
		 . ligo-font-lock-operator-face)
		(,"<" ())
		(,"\\b\\([a-zA-Z$_][a-zA-Z0-9$_]*\\)\\b[:space:]*:"
		 (1 font-lock-variable-name-face))
		(,"\\b[a-zA-Z_][a-zA-Z0-9]\\*\\b"
		 . font-lock-type-face)
		(,"\\(" ())
		(,"\\b\\([0-9]+\\)\\b"
		 . ligo-font-lock-number-face)
		(,"\\[" ())
		(,"{" ()))
	"Syntax highlighting rules for jsligo")

(defun jsligo-reload ()
	"Reload the jsligo-mode code and re-apply the default major mode in the current buffer."
	(interactive)
	(unload-feature 'jsligo-mode)
	(require 'jsligo-mode)
	(normal-mode))

(define-derived-mode ligo-javascript-mode prog-mode "jsligo"
	"Major mode for writing jsligo code."
	(setq font-lock-defaults '(jsligo-font-lock-defaults))
	(set-syntax-table (jsligo-syntax-table)))

(add-to-list 'auto-mode-alist '("\\.jsligo\\'" . ligo-javascript-mode))

(provide 'jsligo-mode)

;;; jsligo-mode.el ends here
