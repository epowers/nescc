/* This file is part of the galsC compiler.

This file is derived from nesC and the GNU C Compiler. It is thus
   Copyright (C) 1987, 1988, 1991, 1992, 1993, 1994, 1995, 1996, 1997,
   1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
Changes for nesC are
   Copyright (C) 2002, 2003 Intel Corporation
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

/* Initialiser handling. */

#ifndef INIT_H
#define INIT_H

void start_init(declaration decl);
void finish_init(void);
void simple_init(expression expr);
void really_start_incremental_init(type t);
void push_init_level(int implicit);
designator set_init_index(location loc, expression first, expression last);
designator set_init_label(location loc, cstring fieldname);
void process_init_element(expression value);

expression make_init_specific(designator dlist, expression initval);
expression make_init_list(location loc, expression elist);
expression make_cast_list(location loc, asttype t, expression init);

#endif
