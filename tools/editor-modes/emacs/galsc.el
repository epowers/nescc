;;; galsc.el --- galsC mode

;; This file is derived from nesc.el.
;; 
;; Changes for galsC are
;;     Copyright (C) 2004 Elaine Cheong
;; Author: Elaine Cheong <celaine @ users.sourceforge.net>

;;-------------------------------------------------------------------------
;; nesc.el information:
;; Author: David Gay <dgay@intel-research.net>
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

(require 'cc-mode)

;; Load appropriate version of galsC emacs mode
(if (string-match "^5.2[0-9]" c-version)
    (load-library "old-galsc.el")
  (load-library "new-galsc.el"))
