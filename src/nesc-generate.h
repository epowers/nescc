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

#ifndef NESC_GENERATE_H
#define NESC_GENERATE_H

#include "nesc-cg.h"

void generate_c_code(nesc_declaration program, const char *target_name,
		     cgraph cg, dd_list modules);

#ifdef GALSC
/* A description of the target functions a used function is connected to */
typedef struct full_connection
{
  endp ep;
  expression cond;
  expression args;
#ifdef GALSC
    dd_list parameters;
#endif
} *full_connection;


struct connections
{
  /* Connection being sought */
  region r;
  cgraph cg;
  data_declaration called;

  /* the list of targets which are called generically (with generic arguments
     passed through unchanged). NULL if 'called' is not generic.
     Both 'cond' and 'args' are NULL for generic_calls */
  dd_list/*<full_connection>*/ generic_calls; 

  /* normal_calls is the list of all other targets.

     If 'called' is generic, 'cond' is the expression list that must match
     the generic parameters of 'called' for the call to 'ep' to
     take place. 'args' is the expression list to add to the arguments
     if 'ep' is generic. */
  dd_list/*<full_connection>*/ normal_calls;

  /* The combiner function used, if any */
  data_declaration combiner;
};

void find_connections(cgraph cg, nesc_declaration mod);
cgraph mark_reachable_code(void);
void prt_ncf_conditional_calls(struct connections *c, bool first_call, type return_type);
void prt_ncf_default_call(struct connections *c, type return_type,
        function_declarator called_fd);
bool prt_ncf_direct_calls(struct connections *c,
			  dd_list/*<full_connection>*/ calls,
        type return_type);
void suppress_function(const char *name);
void prt_nesc_function_declarations(nesc_declaration mod);
void prt_nesc_module(cgraph cg, nesc_declaration mod);
void prt_inline_functions(cgraph callgraph);
void prt_noninline_functions(cgraph callgraph);
bool prt_arguments(declaration parms, bool first);
#endif

#endif
