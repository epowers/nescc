/* This file is part of the galsC compiler.

This file is derived from the nesC compiler.  It is thus
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

#ifndef NESC_USES_H
#define NESC_USES_H

typedef enum {
  c_atomic = 1,
  c_executable = 2,
  c_read = 4,
  c_write = 8,
  c_fncall = 16,
  c_addressed = 32,
  c_deref = 64,
  c_constant = 128
} context;

typedef struct use
{
  location l;
  data_declaration fn;		/* function containing use */
  context c;
} *use;

typedef struct iduse
{
  data_declaration id;
  use u;
} *iduse;

extern dd_list nglobal_uses;

void collect_uses(declaration decls);

context exe_context(context c);

void init_uses(void);

use new_use(location l, context c);

#endif
