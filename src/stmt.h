/* This file is part of the galsC compiler.

This file is derived from the nesC compiler and RC and the GNU C Compiler.
It is thus
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.
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

#ifndef STMT_H
#define STMT_H

void check_condition(const char *context, expression e);
void check_switch(expression e);
void check_void_return(void);
void check_return(expression e);
void check_computed_goto(expression e);

void lookup_label(id_label label);
void use_label(id_label label);
void define_label(id_label label);
void declare_label(id_label label);

void check_labels(void);

void check_case(label case_label);
void check_default(label default_label);
void check_break(statement break_statement);
void check_continue(statement continue_statement);
void fail_in_atomic(const char *context);

void push_loop(statement loop_statement);
void pop_loop(void);

label_declaration new_label_declaration(region r, const char *name, id_label firstuse);

#endif

