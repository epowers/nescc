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

#ifndef NESC_CG_H
#define NESC_CG_H

/* A connection graph */

struct endp
{
    
  data_declaration 
#ifdef GALSC
      parameter, // parameter (TinyGUYS) for a node in the connection graph.
      actor,     // actor for a node in the connection graph.
      port,      // port for a node in the connection graph.
#endif
      component, interface, function;

  expression args; /* args for the interface if not NULL.
		      If no errors have been reported, then
		      constant_integral(e->cst) for all expressions e in args
		   */
};

typedef struct endp *endp;

#ifdef GALSC
void init_endp(endp ep);
bool endp_compare(endp p1, endp p2);
#endif

typedef struct cgraph *cgraph;

cgraph new_cgraph(region r);

gnode endpoint_lookup(cgraph ch, endp ep);
gnode fn_lookup(cgraph cg, data_declaration fndecl);

#ifdef GALSC
// Find the TinyGUYS (declared in fndecl) in the connection graph and
// fill in ep with the appropriate pointers.
//
// See fn_lookup() in nesc-cg.c
gnode parameter_lookup(cgraph cg, data_declaration fndecl);

// Find the port (declared in fndecl) in the connection graph and fill
// in ep with the appropriate pointers.
//
// See fn_lookup() in nesc-cg.c
gnode port_lookup(cgraph cg, data_declaration fndecl);
#endif

ggraph cgraph_graph(cgraph cg);

#ifdef GALSC
// For debugging purposes.  Prints endp information.
// Usage in gdb:
//
// p dbg_graph(c->cg->g, 0)
// p dbg_graph(c->cg->g, pnode_galsc)
// <see graph.c>
void pnode_galsc(gnode n);
#endif

#endif
