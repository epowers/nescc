/* This file is part of the galsC compiler.

This file is derived from the nesC compiler, which is derived from the
RC Compiler. It is thus
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
   Copyright (C) 2002 Intel Corporation
Changes for galsC are
   Copyright (C) 2003-2004 Palo Alto Research Center

The attached "galsC" software is provided to you under the terms and
conditions of the GNU General Public License Version 2 as published by the
Free Software Foundation.

galsC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with galsC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA. */

#ifndef EDIT_H
#define EDIT_H

/* Declare a new variable that can be assigned a value of type t.
   Place the declaration at the start of block. 
   XXX: See discussion in types.c:tag2ast about the (lack of) correctness of
   this approach.
   Return it's declaration */
data_decl build_declaration(region r, type t, const char *name);

word build_word(region r, const char *cword);

#endif
