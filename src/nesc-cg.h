/* This file is part of the nesC compiler.
   Copyright (C) 2002 Intel Corporation

The attached "nesC" software is provided to you under the terms and
conditions of the GNU General Public License Version 2 as published by the
Free Software Foundation.

nesC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with nesC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef NESC_CG_H
#define NESC_CG_H

/* A connection graph */

struct endp
{
  data_declaration component, interface, function;
  expression args; /* args for the interface if not NULL.
		      If no errors have been reported, then
		      constant_integral(e->cst) for all expressions e in args
		   */
  int instance;
  int MDW_hack_count;
};

typedef struct endp *endp;

typedef struct cgraph *cgraph;

cgraph new_cgraph(region r);

gnode endpoint_lookup(cgraph ch, endp ep);
gnode fn_lookup(cgraph cg, data_declaration fndecl, int instance);

ggraph cgraph_graph(cgraph cg);

void print_endp(char *head, endp ep);

#endif
