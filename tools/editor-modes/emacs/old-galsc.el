;;; galsc.el --- galsC mode

;; This file is derived from nesc.el.  It is thus
;;     Copyright (C) 1985-2002 by Free Software Foundation, Inc.
;;     Copyright (C) 2004 Intel Corporation
;; Changes for galsC are
;;     Copyright (C) 2004 Elaine Cheong
;; Author: Elaine Cheong <celaine @ users.sourceforge.net>

;;-------------------------------------------------------------------------
;; nesc.el information:
;; Author: Dennis Haney <davh@diku.dk>
;;         David Gay <dgay@intel-research.net>
;; Maintainer: David Gay <dgay@intel-research.net>
;; Keywords: c, languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License Version 2
;; as published by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;-------------------------------------------------------------------------

;;; Commentary:

;;

;;; Code:

(require 'cc-mode)

(if (not (string-match "^5.2[0-9]" c-version))
    (error "cc-mode 5.30 and later not supported by this file"))

(require 'font-lock)

(defconst galsc-keywords
  (eval-when-compile
    (regexp-opt
     '("actor" "actorControl" "application" "appstart"
       "in" "out" "parameter" "port"
       "abstract" "as" "atomic" "async"
       "call" "command" "components" "configuration" 
       "event" "implementation" "interface" "includes" 
       "module" "norace" "post" "provides"
       "signal" "task" "uses" ) t)))

(setq galsc-font-lock-keywords-1
      (list
       `(eval .
	      (cons (concat "\\<" (,@ galsc-keywords) "\\>") 'font-lock-keyword-face))))

(defconst galsc-font-lock-keywords
  (append galsc-font-lock-keywords-1
   c++-font-lock-keywords-2))

(defvar galsc-mode-abbrev-table nil
  "Abbreviation table used in galsc-mode buffers.")
(define-abbrev-table 'galsc-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar galsc-mode-map ()
  "Keymap used in galsc-mode buffers.")
(if galsc-mode-map
    nil
  (setq galsc-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for galsC
  )

(easy-menu-define c-galsc-menu galsc-mode-map "galsC Mode Commands"
		  (c-mode-menu "galsC"))

(defvar galsc-mode-syntax-table nil
  "Syntax table used in galsc-mode buffers.")
(if galsc-mode-syntax-table
    ()
  (setq galsc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table galsc-mode-syntax-table))

(defconst c-galsC-comment-start-regexp c-C++-comment-start-regexp)
(defconst c-galsC-class-kwds "struct\\|union\\|implementation")
(defconst c-galsC-class-key (c-paren-re c-galsC-class-kwds))

(defvar cc-imenu-galsc-generic-expression
  cc-imenu-c-generic-expression
  "Imenu generic expression for galsC mode.  See `imenu-generic-expression'.")

(defun galsc-mode ()
  "Major mode for editing galsC code."
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table galsc-mode-syntax-table)
  (setq major-mode 'galsc-mode
 	mode-name "galsC"
	local-abbrev-table galsc-mode-abbrev-table
	abbrev-mode t
	; we have javadoc-style comments
	c-append-paragraph-start c-Java-javadoc-paragraph-start)
  (use-local-map galsc-mode-map)
  (c-common-init)
  (setq comment-start "// "
 	comment-end   ""
        c-keywords (c-identifier-re (concat c-C-keywords "\\|" galsc-keywords))
 	c-conditional-key c-C-conditional-key
 	c-comment-start-regexp c-galsC-comment-start-regexp
  	c-class-key c-galsC-class-key
	c-method-key nil
 	c-baseclass-key nil
	c-recognize-knr-p nil
	c-inexpr-class-key nil
	;defun-prompt-regexp c-galsC-defun-prompt-regexp
	)
  (cc-imenu-init cc-imenu-galsc-generic-expression)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
        '(galsc-font-lock-keywords nil t))
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'galsc-mode-hook)
  (c-update-modeline))

(provide 'galsc-mode)

;;; galsc.el ends here
