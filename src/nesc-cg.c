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

/* A connection graph */
#include "parser.h"
#include "nesc-cg.h"
#include "utils.h"

#ifdef GALSC
void init_endp(endp ep) {
    ep->parameter =
        ep->actor =
        ep->port =
        ep->component =
        ep->interface =
        ep->function = NULL;
    ep->args = NULL;
}

bool endp_compare(endp p1, endp p2) {
    return p1->function == p2->function &&
        p1->parameter == p2->parameter &&
        p1->port == p2->port &&
        p1->args == p2->args;
}
#endif

struct cgraph
{
  ggraph sameregion g;
  dhash_table sameregion ep_table;
};

typedef struct ep_table_entry
{
  struct endp ep; /* ep.function is the key */
#ifdef GALSC
    // ep.parameter is also part of the key
    // ep.port (and ep.args) is also part of the key.
#endif
  gnode n;
} *ep_table_entry;

static int ep_compare(void *e1, void *e2)
{
  ep_table_entry ep1 = e1, ep2 = e2;

  return ep1->ep.function == ep2->ep.function &&
#ifdef GALSC
      ep1->ep.parameter == ep2->ep.parameter &&
      ep1->ep.port == ep2->ep.port &&
#endif
    ep1->ep.args == ep2->ep.args;
}

static unsigned long ep_hash(void *e)
{
  ep_table_entry ep = e;

  return hash_ptr(ep->ep.function) ^ hash_ptr(ep->ep.args)
#ifdef GALSC
      ^ hash_ptr(ep->ep.port)
      ^ hash_ptr(ep->ep.parameter)
#endif
      ;
}

cgraph new_cgraph(region r)
{
  cgraph cg = ralloc(r, struct cgraph);

  cg->g = new_graph(r);
  cg->ep_table = new_dhash_table(r, 64, ep_compare, ep_hash);

  return cg;
}

gnode endpoint_lookup(cgraph cg, endp ep)
{
  ep_table_entry gep;

  gep = dhlookup(cg->ep_table, ep);

  if (gep)
    return gep->n;

  gep = ralloc(regionof(cg), struct ep_table_entry);
  gep->ep = *ep;

  dhadd(cg->ep_table, gep);
  return gep->n = graph_add_node(cg->g, &gep->ep);
}

gnode fn_lookup(cgraph cg, data_declaration fndecl)
{
  struct endp ep;

#ifdef GALSC
  ep.parameter = NULL;
  ep.actor = NULL;
  ep.port = NULL;
#endif
  ep.component = ep.interface = NULL;
  ep.function = fndecl;
  ep.args = NULL;
  return endpoint_lookup(cg, &ep);
}

#ifdef GALSC
// Find the TinyGUYS (declared in fndecl) in the connection graph and
// fill in ep with the appropriate pointers.
//
// See fn_lookup() in nesc-cg.c
gnode parameter_lookup(cgraph cg, data_declaration fndecl)
{
  struct endp ep;

  ep.parameter = fndecl;
  ep.actor = NULL;
  ep.port = NULL;

  ep.component = ep.interface = NULL;
  ep.function = NULL;
  ep.args = NULL;
  return endpoint_lookup(cg, &ep);
}

// Find the port (declared in fndecl) in the connection graph and fill
// in ep with the appropriate pointers.
//
// See fn_lookup() in nesc-cg.c
gnode port_lookup(cgraph cg, data_declaration fndecl)
{
  struct endp ep;

  ep.parameter = NULL;
  ep.actor = fndecl->actor;
  ep.port = fndecl;
  
  ep.component = ep.interface = NULL;
  ep.function = NULL;
  ep.args = NULL;
  return endpoint_lookup(cg, &ep);
}
#endif

ggraph cgraph_graph(cgraph cg)
{
  return cg->g;
}

#ifdef GALSC
// For debugging purposes.  Prints endp information.
// Usage in gdb:
//
// p dbg_graph(c->cg->g, 0)
// p dbg_graph(c->cg->g, pnode_galsc)
// <see graph.c>
void pnode_galsc(gnode n) {
    endp ep = NODE_GET(endp, n);

    fprintf(stderr, "\t");

    if (!(ep->parameter))
        fprintf(stderr, "null");
    else
        fprintf(stderr, "%s", ep->parameter->name);
    
    if (!(ep->actor))
        fprintf(stderr, "null");
    else
        fprintf(stderr, "%s", ep->actor->name);

    if (!(ep->port))
        fprintf(stderr, ".null");
    else
        fprintf(stderr, ".%s", ep->port->name);    

    if (!(ep->component))
        fprintf(stderr, ".null");
    else
        fprintf(stderr, ".%s", ep->component->name);
    
    if (!(ep->interface))
        fprintf(stderr, ".null");
    else
        fprintf(stderr, ".%s", ep->interface->name);

    if (!(ep->function))
        fprintf(stderr, ".null");
    else
        fprintf(stderr, ".%s", ep->function->name);
}
#endif
