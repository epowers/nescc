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

/* A connection graph */
#include "parser.h"
#include "nesc-cg.h"
#include "nesc-semantics.h"

struct cgraph
{
  ggraph sameregion g;
  dhash_table sameregion ep_table;
};

typedef struct ep_table_entry
{
  struct endp ep; /* ep.function is the key */
  gnode n;
} *ep_table_entry;

/* The number of non-significant low-order bits in pointer addresses
   (typically log2 of the alignment of allocations) */
#define ALIGNMENT_BITS 3

void print_endp(char *head, endp ep) {
  fprintf(stderr,head);
  // Use of "> 1000" used to catch cases of bad pointers
  fprintf(stderr,"[%s.%s.%s addr 0x%lx INSTANCE %d(%d) c:0x%lx i:0x%lx f:0x%lx a:0x%lx]\n", 
      ((ep->component)?ep->component->name:"null"),
      ((ep->interface)?ep->interface->name:"null"),
      ((ep->function)?ep->function->name:"null"),
      (unsigned long)ep, 
      ep->instance,
      ((ep->component)?ep->component->instance_number:-2),
      (unsigned long)ep->component, 
      (unsigned long)ep->interface, 
      (unsigned long)ep->function, 
      (unsigned long)ep->args);
}

static int ep_compare(void *e1, void *e2)
{
  ep_table_entry ep1 = e1, ep2 = e2;

  // XXX MDW: Clean this up
  // Tricky: And endpoint matches if the function and args match,
  // and (either one component is NULL or the components are the same).
  // This is necessary to match endpoints in different instances of 
  // the same abstract component correctly.
  if (ep1->ep.function == ep2->ep.function &&
      ep1->ep.args == ep2->ep.args &&
      ep1->ep.instance == ep2->ep.instance) {
//      (ep1->ep.component == ep2->ep.component ||
//      ((ep1->ep.component == NULL) ^ (ep2->ep.component == NULL)))) {
    //print_endp("MDW: ep_compare MATCH ep1 ", &ep1->ep);
    //print_endp("MDW: ep_compare MATCH ep2 ", &ep2->ep);
    return TRUE;
  } else {
    return FALSE;
  }

}

static unsigned long hashPtr(void *p) 
{
  return (unsigned long)p >> ALIGNMENT_BITS;
}

static unsigned long ep_hash(void *e)
{
  ep_table_entry ep = e;

  return hashPtr(ep->ep.function) ^ hashPtr(ep->ep.args);
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

  //print_endp("MDW: endpoint_lookup for ", ep);

  gep = dhlookup(cg->ep_table, ep);

  if (gep)
    return gep->n;

  gep = ralloc(regionof(cg), struct ep_table_entry);
  gep->ep = *ep;

  dhadd(cg->ep_table, gep);
  return gep->n = graph_add_node(cg->g, &gep->ep);
}

gnode fn_lookup(cgraph cg, data_declaration fndecl, int instance_num)
{
  struct endp ep;

  ep.component = ep.interface = NULL;
  ep.function = fndecl;
  ep.args = NULL;

  // Override 'instance_num' if this is in fact not an abstract component,
  // or is part of a static interface
  if (!fndecl->container || !fndecl->container->is_abstract ||
      (fndecl->interface && fndecl->interface->static_interface)) {
    ep.instance = -1;
  } else {
    ep.instance = instance_num;
  }

  return endpoint_lookup(cg, &ep);
}

ggraph cgraph_graph(cgraph cg)
{
  return cg->g;
}

