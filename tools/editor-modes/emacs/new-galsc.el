;; galsc.el --- galsC mode

;; This file is derived from nesc.el.  It is thus
;;     Copyright (C) 2004 Intel Corporation
;; Changes for galsC are
;;     Copyright (C) 2004 Elaine Cheong
;; Author: Elaine Cheong <celaine @ users.sourceforge.net>

;;-------------------------------------------------------------------------
;; nesc.el information:
;; Author:     2002 Martin Stjernholm
;;	       2004 David Gay
;; Maintainer: David Gay <dgay@intel-research.net>
;; Created:    March 2004

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License Version 2
;; as published by the Free Software Foundation.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;-------------------------------------------------------------------------

;;; Code:

(require 'cc-mode)

(if (string-match "^5.2[0-9]" c-version)
    (error "cc-mode 5.30 or later required by this file"))

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use C
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'galsc-mode 'c-mode)
  ;; cc-mode 5.30.8 is buggy:
  (if (not (get 'galsc-mode 'c-fallback-mode))
      (put 'galsc-mode 'c-fallback-mode 'c-mode)))

(c-lang-defconst c-class-decl-kwds
  galsc (append '("interface" "implementation")
	       (c-lang-const c-class-decl-kwds)))

(c-lang-defconst c-brace-list-decl-kwds
  galsc (append '("module" "configuration" "provides" "uses")
	       (c-lang-const c-brace-list-decl-kwds)))

(c-lang-defconst c-typeless-decl-kwds
  galsc (append '("as" "components" "interface")
	       (c-lang-const c-typeless-decl-kwds)))

(c-lang-defconst c-modifier-kwds
  galsc (append '("command" "event" "task" "norace" "async")
	       (c-lang-const c-modifier-kwds)))

(c-lang-defconst c-other-decl-kwds
  galsc (append '("includes") (c-lang-const c-other-decl-kwds)))

(c-lang-defconst c-block-stmt-1-kwds
  galsc (append '("atomic") (c-lang-const c-block-stmt-1-kwds)))

;; This gives post, call, signal a slightly incorrect priority
(c-lang-defconst c-operators
  galsc (append '((prefix "post" "call" "signal"))
	       ;; Note: need to ask specifically for the C operators
	       ;; as there are explicit tests for the C name in the 
	       ;; c-operators constant specification...
	       (c-lang-const c-operators c)))

(c-lang-defconst c-other-kwds
  galsc (cons "abstract" (c-lang-const c-other-kwds)))


(defcustom galsc-font-lock-extra-types '("result_t" "bool"
					"int8_t" "uint8_t"
					"int16_t" "uint16_t"
					"int32_t" "uint32_t")
  "*List of extra types (aside from the type keywords) to recognize in galsC mode.
Each list item should be a regexp matching a single identifier.")

(defconst galsc-font-lock-keywords-1 (c-lang-const c-matchers-1 galsc)
  "Minimal highlighting for galsC mode.")

(defconst galsc-font-lock-keywords-2 (c-lang-const c-matchers-2 galsc)
  "Fast normal highlighting for galsC mode.")

(defconst galsc-font-lock-keywords-3 (c-lang-const c-matchers-3 galsc)
  "Accurate normal highlighting for galsC mode.")

(defvar galsc-font-lock-keywords galsc-font-lock-keywords-3
  "Default expressions to highlight in galsC mode.")

(defvar galsc-mode-syntax-table nil
  "Syntax table used in galsc-mode buffers.")
(or galsc-mode-syntax-table
    (setq galsc-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table galsc))))

(defvar galsc-mode-abbrev-table nil
  "Abbreviation table used in galsc-mode buffers.")
(c-define-abbrev-table 'galsc-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar galsc-mode-map (let ((map (c-make-inherited-keymap)))
		      ;; Add bindings which are only useful for galsC
		      map)
  "Keymap used in galsc-mode buffers.")

(easy-menu-define galsc-menu galsc-mode-map "galsC Mode Commands"
		  ;; Can use `galsc' as the language for `c-mode-menu'
		  ;; since its definition covers any language.  In
		  ;; this case the language is used to adapt to the
		  ;; nonexistence of a cpp pass and thus removing some
		  ;; irrelevant menu alternatives.
		  (cons "galsC" (c-lang-const c-mode-menu galsc)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nc\\'" . galsc-mode))

;;;###autoload
(defun galsc-mode ()
  "Major mode for editing galsC (pronounced \"nes-see\") code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `galsc-mode-hook'.

Key bindings:
\\{galsc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table galsc-mode-syntax-table)
  (setq major-mode 'galsc-mode
	mode-name "galsC"
	local-abbrev-table galsc-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars galsc-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'galsc-mode)
  (easy-menu-add galsc-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'galsc-mode-hook)
  (c-update-modeline))


(provide 'galsc-mode)

;;; new-galsc.el ends here
