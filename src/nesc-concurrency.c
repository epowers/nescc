/* This file is part of the nesC compiler.
   Copyright (C) 2002 UC Berkeley

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

/*
  Author:   J. Robert von Behren <jrvb@cs.berkeley.edu>
  Created:  9/29/2002

  This file contains routines for performing compile-time checks of
  concurrency restrictions.  In the future, this file may also include
  routines to generate support code for runtime concurrency management
  tools (eg, a scheduler).
  
*/


#include "dhash.h"
#include "parser.h"
#include "c-parse.h"
#include "nesc-doc.h"
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "nesc-c.h"
#include "unparse.h"
#include "AST_utils.h"
#include "edit.h"
#include "semantics.h"
#include "constants.h"
#include "sd_list.h"
#include "nesc-cg.h"
#include "graph.h"
#include "nesc-env.h"
#include "regions.h"
#include "unparse.h"
#include "errors.h"


#include "parser.h"
#include "nesc-concurrency.h"
#include "constants.h"
#include "unparse.h"
#include "nesc-findvars.h"


static FILE *outfile = NULL;
static region conc_region = NULL;

static void print_ddecl(data_declaration ddecl)
{
  psd_options opts = 0;

  if (ddecl->definition && !ddecl->suppress_definition)
    opts |= psd_print_default;

  if (is_function_decl(ddecl->ast))
    {
      function_decl d = CAST(function_decl, ddecl->ast);

      prt_declarator(d->declarator, d->modifiers, d->attributes, ddecl, opts);
    }
  else 
    {
      variable_decl d = CAST(variable_decl, ddecl->ast);

      prt_declarator(d->declarator, NULL, d->attributes, ddecl, opts);
    }
}





typedef enum {
  ENTRY_TASK,                 // non-interrupting, interruptable
  ENTRY_REENTRANT_INTERRUPT,  // interrupting, interruptable
  ENTRY_ATOMIC_INTERRUPT,     // interrupting, non-interuptable
  ENTRY_MAX                   // for array size
} entry_point_type;





/**
 * recurse down the call tree from a particular function, marking
 * called functions with the given type.q
 **/
static void mark_functions(gnode parent, entry_point_type type, int indent,
			   bool iscall)
{
  int subtotal;
  endp ep = NODE_GET(endp, parent);
  data_declaration fn = ep->function;
  gedge edge;
  gnode child;

  // ignore gcc builtins 
  //
  // FIXME: we'll need to be careful about these if we want to mark
  // long-running / blocking calls
  if( builtin_declaration(fn) )
    return;
  

  // debugging output
  fprintf(outfile, "%*s",indent,"");
  print_ddecl(fn);
  if( !iscall ) {
    fprintf(outfile, "  (ref only)");
    if( fn->definition ) {
      fprintf(outfile, " def not null\n");
      fprintf(outfile, "LOCATION: %s:%ld\n",fn->ast->location->filename, fn->ast->location->lineno);
      fprintf(outfile, "DEFINITION: %s:%ld\n",fn->definition->location->filename, fn->definition->location->lineno);
      return;
    }
  }

  fprintf(outfile,"\n");
  

  // error, if this node has already been seen
  if( graph_node_markedp(parent) ) {
    if( type == ENTRY_TASK ) {
      fprintf(outfile,"LOOP d' LOOP (task context)\n");
    } else {
      // FIXME: need to give info about which function, and where the cycle lies 
      //error("loop the loop");
      fprintf(outfile,"LOOP d' LOOP (interrupt context)\n");
    }
    return;
  }

  // set the flags for the current function
  if(type == ENTRY_TASK) {
    if(fn->task_context) return;  // bottom out, already counted
    fn->task_context = TRUE;
    if( fn->uninterruptable ) 
      warning("uninterruptable function `%s' called from task context",fn->name);
  } 
  else if(type == ENTRY_REENTRANT_INTERRUPT) {
    if( fn->reentrant_interrupt_context ) return; // bottom out, already counted
    fn->reentrant_interrupt_context = TRUE;
    if( fn->uninterruptable ) 
      warning("uninterruptable function `%s' called from reentrant interrupt context",fn->name);
    if( fn->task_only ) 
      warning("task_only function `%s' called from reentrant interrupt context",fn->name);
  }
  else if(type == ENTRY_ATOMIC_INTERRUPT) {
    if( fn->atomic_interrupt_context ) return; // bottom out, already counted
    fn->atomic_interrupt_context = TRUE;
    if( fn->task_only ) 
      warning("task_only function `%s' called from atomic interrupt context",fn->name);
  }


  // mark that this function has been seen, before we recurse
  graph_mark_node(parent);

  // recurse
  subtotal = 1; // current function
  graph_scan_out(edge,parent) {
    child = graph_edge_to(edge);
    mark_functions(child, type, indent+2, EDGE_GET(void *, edge) != NULL);
  }
  
  // clear the seen flag
  graph_unmark_node(parent);
} 



/**
 * scan the call graph, and record the contexts in which various
 * functions are called.
 * 
 * 
 **/
static void mark_entry_points(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;

  entry_point_type type;
  int totals[ENTRY_MAX];
  bzero(totals, sizeof(totals));
  
  // find entry points
  graph_clear_all_marks(cg);
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;

      // skip things that aren't entry points
      if( !fn->spontaneous  &&  graph_first_edge_in(n) != NULL && !type_task(fn->type))
        continue;

      
      // choose the entry point type
      type = ENTRY_TASK;
      {
        function_decl fd = CAST(function_decl, fn->ast);
        attribute a;
        scan_attribute(a,fd->attributes) {
          //fprintf(outfile,"%s\n",a->word1->cstring.data);
          if( strcmp(a->word1->cstring.data,"interrupt") == 0)
            type = ENTRY_REENTRANT_INTERRUPT;
          else if( strcmp(a->word1->cstring.data,"signal") == 0)
            type = ENTRY_ATOMIC_INTERRUPT;
        }
        //prt_type_elements(CAST(type_element, fd->attributes), 0);
      }
      

      // recursively mark all called functions, according to type
      fprintf(outfile,"\n\n------------------------\n");
      mark_functions(n,type,0, TRUE);
      fprintf(outfile,"------------------------\n");
    }

  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;

      if (fn->reentrant_interrupt_context)
	totals[ENTRY_REENTRANT_INTERRUPT]++;
      else if (fn->task_context && !fn->atomic_interrupt_context)
	totals[ENTRY_TASK]++;
      else if (!fn->task_context && fn->atomic_interrupt_context)
	totals[ENTRY_ATOMIC_INTERRUPT]++;
      else
	/* This is task & atomic interrupt. The reentrant interrupt is a
	   slight misnomer */
	totals[ENTRY_REENTRANT_INTERRUPT]++;
    }

  fprintf(outfile,"function type totals:\n");
  fprintf(outfile,"    task           %d\n",totals[ENTRY_TASK]);
  fprintf(outfile,"    atomic int     %d\n",totals[ENTRY_ATOMIC_INTERRUPT]);
  fprintf(outfile,"    reentrant int  %d\n",totals[ENTRY_REENTRANT_INTERRUPT]);
}



/**
 * check the variable refs in the given function.  Errors are
 * generated if any of the refs might create a race condition.
 **/
static void check_variable_refs(cgraph callgraph) //data_declaration fn, entry_point_type type) 
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  node temp;

  fv_init();

  // first pass - collect the list of all vars
  graph_scan_nodes (n, cg) {
    data_declaration fn = NODE_GET(endp, n)->function;

#if 0
    printf("\n\n\n++++++++++++++++++++++++++++++++++++++++\n");
    temp = fn->ast->next;
    fn->ast->next = NULL;
    AST_print( CAST(node,fn->ast) );
    fn->ast->next = temp;
    printf("----------------------------------------\n");
#endif    

    // FIXME: do I need to deal w/ the variable_decls in the graph??
    if( is_function_decl(fn->ast) )
      find_function_vars(fn);
  }


  // now go throught the variable list, and look for conflicts
  check_for_conflicts();


  fv_cleanup();
}






/**
 * External entry point to these routines
 **/
void perform_concurrency_checks(cgraph callgraph)
{
  outfile = fopen("/tmp/funclist","w");
  assert(outfile);

  conc_region = newregion();

  unparse_start(outfile);
  set_function_separator(".");
  disable_line_directives();

  mark_entry_points(callgraph);
  check_variable_refs(callgraph);


  unparse_end();
  fprintf(outfile, "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");

  fclose(outfile);

  deleteregion( conc_region );
}
