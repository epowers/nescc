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

#ifndef INPUT_H
#define INPUT_H

#include "c-lex.h"

struct file_stack
  {
    struct file_stack *next;
    struct location l;
    struct lex_state lex; /* internal info for lexer only */
  };

/* Stack of currently pending input files. */
extern struct file_stack *input_file_stack;

/* Incremented on each change to input_file_stack.  */
extern int input_file_stack_tick;

void set_input(FILE *f, const char *filename);
void end_input(void);
void push_input(void);
void pop_input(void);

#endif
