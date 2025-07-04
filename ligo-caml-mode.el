;;; ligo-caml-mode.el --- A major mode for editing MLIGO source code -*- lexical-binding: t -*-

;; Version: 0.2.0
;; Author: mattiasdrp (originally LigoLang SASU)
;; URL: https://github.com/mattiasdrp/ligo-mode
;; Keywords: languages
;; Package-Requires: ((emacs "27.1"))

;; This file is distributed under the terms of the MIT license.

;;; Commentary:

;; This provides font lock and other support for the three dialects of
;; the MLigo smart contract language for the Tezos blockchain.

;;; Code:

(defun ligo-caml-syntax-table ()
	"Syntax table."
	(let ((st (make-syntax-table)))
	  (modify-syntax-entry ?_ "w" st)
	  (modify-syntax-entry ?' "_" st)
	  (modify-syntax-entry ?. "'" st)
	  (modify-syntax-entry ?^ "." st)
	  (modify-syntax-entry ?# "." st)
	  (modify-syntax-entry ?< "." st)
	  (modify-syntax-entry ?> "." st)
	  (modify-syntax-entry ?/ "." st)
	  (modify-syntax-entry ?* "." st)
	  (modify-syntax-entry ?- "." st)
	  (modify-syntax-entry ?+ "." st)
	  (modify-syntax-entry ?\" "\"" st)
	  (modify-syntax-entry ?
                         "> b" st)
	  (modify-syntax-entry ?/ ". 12b" st)
	  (modify-syntax-entry ?* ". 23" st)
	  (modify-syntax-entry ?\( "()1n" st)
	  (modify-syntax-entry ?\) ")(4n" st)
	  st))

(defvar ligo-caml-font-lock-defaults
	`(
		(,"\\[@[^\\]]*\\]"
		 . ligo-font-lock-attribute-face)
		(,"^\\(#[a-zA-Z]+\\)"
		 . font-lock-preprocessor-face)
		(,"\\b\\(match\\|with\\|if\\|then\\|else\\|assert\\|failwith\\|begin\\|for\\|upto\\|downto\\|do\\|while\\|done\\)\\b"
		 . ligo-font-lock-conditional-face)
		(,"\\b\\(struct\\|end\\|let\\|in\\|mut\\|rec\\contract_of|parameter_of\\|module\\|sig\\|val\\|include\\|false\\|true\\)\\b"
		 . font-lock-keyword-face)
		(,"\\b[-+]?\\([0-9]+\\)\\(n\\|\\tz\\|tez\\|mutez\\|\\)\\b"
		 . ligo-font-lock-number-face)
		(,"::\\|-\\|+\\|/\\|\\b\\(mod\\|land\\|lor\\|lxor\\|lsl\\|lsr\\)\\b\\|&&\\|||\\|<\\|>\\|<>\\|<=\\|>=\\||>\\|->\\|:=\\|\\^\\|*\\|+=\\|-=\\|*=\\|/=\\||="
		 . ligo-font-lock-operator-face)
		(,";")
		(,"\\b\\(of\\)\\b"
		 (1 font-lock-keyword-face))
		(,"\\b\\(fun\\)\\b" ( 1 ligo-font-lock-statement-face))
		(,"\\b\\([A-Z][a-zA-Z0-9_$]*\\)\\b"
		 (1 ligo-font-lock-structure-face))
		(,"\\b\\([a-z$_][a-zA-Z0-9$_]*\\)\\b"
		 (1 font-lock-variable-name-face))
		(,"\\btype\\b" ( 1 font-lock-keyword-face))
		(,":" ( 1 ligo-font-lock-operator-face))
		(,":" ( 1 ligo-font-lock-operator-face))
		(,"\\(->\\|\\.\\|\\*\\||\\)"
		 . ligo-font-lock-operator-face)
		(,"\\b[a-z_][a-zA-Z0-9]\\*\\b"
		 . font-lock-type-face)
		(,"'\\b[a-z_][a-zA-Z0-9]\\*\\b"
		 . font-lock-type-face)
		(,"\\(" ())
		(,"\\b\\([0-9]+\\)\\b"
		 . ligo-font-lock-number-face)
		(,"{" ()))
	"Syntax highlighting rules for ligo-caml.")

(define-derived-mode ligo-caml-mode prog-mode "ligo-caml"
	"Major mode for writing ligo-caml code."
	(setq font-lock-defaults '(ligo-caml-font-lock-defaults))
	(set-syntax-table (ligo-caml-syntax-table)))

(add-to-list 'auto-mode-alist '("\\.mligo\\'" . ligo-caml-mode))

(provide 'ligo-caml-mode)

;;; ligo-caml-mode.el ends here
