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


#include "parser.h"
#include "unparse.h"
#include "semantics.h"
#include "graph.h"
#include "nesc-concurrency.h"
#include "nesc-findvars.h"




//////////////////////////////////////////////////////////////////////
// print utility functions
//////////////////////////////////////////////////////////////////////

// print the name & args of the function described by the data_declaration
static void print_ddecl(FILE *out, data_declaration ddecl)
{
  psd_options opts = 0;
  FILE *temp;

  // set the output
  temp = set_unparse_outfile(out);

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

  set_unparse_outfile(temp);
}




//////////////////////////////////////////////////////////////////////
// debug functions, for printing out the call graph, etc.
//////////////////////////////////////////////////////////////////////

static FILE *outfile = NULL;
static region conc_region = NULL;
static bool print_call_graph = 1;

#define conc_debug(format, args...) if(print_call_graph) fprintf(outfile, format, ## args)

static void conc_debug_start(const char *filename) 
{
  if( print_call_graph ) {
    outfile = fopen(filename,"w");
    assert(outfile);
  }
}

static void conc_debug_end()
{
  if( print_call_graph ) {
    conc_debug("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");

    fclose(outfile);
    outfile = NULL;
  }  
}






//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
//
//   routines for marking the call graph
//
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////


// manage a stack of calls through the graph - used for printing out
// errors messages.

#define MAX_CALL_DEPTH 1024
static data_declaration func_stack[MAX_CALL_DEPTH];
static int func_stack_idx = 0;

static inline void push_f(data_declaration f)
{
  assert(func_stack_idx < MAX_CALL_DEPTH);
  func_stack[ func_stack_idx++ ] = f;
}

static inline void pop_f()
{
  assert(func_stack_idx > 0);
  func_stack_idx--;
}

static void print_current_call_trace(int indent)
{
  int i;

  for(i=0; i<func_stack_idx; i++) {
    fprintf(stderr,"%*s",indent,"");
    print_ddecl(stderr,func_stack[i]);
    fprintf(stderr,"\n"); 
  }
  fprintf(stderr,"\n"); 
}


static void print_current_call_loop(data_declaration f, int indent)
{
  int i=0;

  while( func_stack[i] != f ) 
    i++;

  while( i < func_stack_idx ) {
    fprintf(stderr,"%*s",indent,"");
    print_ddecl(stderr,func_stack[i]);
    fprintf(stderr,"\n"); 
    i++;
  }

  fprintf(stderr,"\n"); 
}


                         // 
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

  // push the current function onto the stack
  push_f(fn);

  // ignore gcc builtins 
  //
  // FIXME: we'll need to be careful about these if we want to mark
  // long-running / blocking calls
  if( builtin_declaration(fn) ) {
    pop_f();
    return;
  }
  

  // debugging output
  conc_debug("%*s",indent,"");
  if(print_call_graph) 
    print_ddecl(outfile, fn);
  if( !iscall ) {
    conc_debug("  (ref only)");
    if( fn->definition ) {
      conc_debug(" def not null\n");
      conc_debug("LOCATION: %s:%ld\n",fn->ast->location->filename, fn->ast->location->lineno);
      conc_debug("DEFINITION: %s:%ld\n",fn->definition->location->filename, fn->definition->location->lineno);
      pop_f();
      return;
    }
  }

  conc_debug("\n");
  

  // error, if this node has already been seen
  if( graph_node_markedp(parent) ) {
    conc_debug("LOOP d' LOOP\n");

    warning("Found a loop in the call graph:");
    print_current_call_loop(fn,4);
    pop_f();
    return;
  }

  // set the flags for the current function
  if(type == ENTRY_TASK) {
    // bottom out, already counted
    if(fn->task_context) {
      pop_f();
      return;  
    }
    fn->task_context = TRUE;
    if( fn->uninterruptable ) {
      error("uninterruptable function `%s' called from task context.  Call trace follows.",fn->name);
      print_current_call_trace(4);
    }
  } 
  else if(type == ENTRY_REENTRANT_INTERRUPT) {
    // bottom out, already counted
    if( fn->reentrant_interrupt_context ) {
      pop_f();
      return; 
    }
    fn->reentrant_interrupt_context = TRUE;
    if( fn->uninterruptable ) {
      error("uninterruptable function `%s' called from reentrant interrupt context.  Call trace follows.",fn->name);
      print_current_call_trace(4);
    }
    if( fn->task_only ) {
      error("task_only function `%s' called from reentrant interrupt context.  Call trace follows.",fn->name);
      print_current_call_trace(4);
    }
  }
  else if(type == ENTRY_ATOMIC_INTERRUPT) {
    // bottom out, already counted
    if( fn->atomic_interrupt_context ) {
      pop_f();
      return; 
    }
    fn->atomic_interrupt_context = TRUE;
    if( fn->task_only ) {
      error("task_only function `%s' called from atomic interrupt context.  Call trace follows.",fn->name);
      print_current_call_trace(4);
    }
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

  // pop the function off the stack
  pop_f();
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
          if( strcmp(a->word1->cstring.data,"interrupt") == 0)
            type = ENTRY_REENTRANT_INTERRUPT;
          else if( strcmp(a->word1->cstring.data,"signal") == 0)
            type = ENTRY_ATOMIC_INTERRUPT;
        }
      }
      

      // recursively mark all called functions, according to type
      conc_debug("\n\n------------------------\n");
      mark_functions(n,type,0, TRUE);
      conc_debug("------------------------\n");
    }

  if( print_call_graph ) {
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

    conc_debug("function type totals:\n");
    conc_debug("    task           %d\n",totals[ENTRY_TASK]);
    conc_debug("    atomic int     %d\n",totals[ENTRY_ATOMIC_INTERRUPT]);
    conc_debug("    reentrant int  %d\n",totals[ENTRY_REENTRANT_INTERRUPT]);
  }
}



/**
 * check the variable refs in the given function.  Errors are
 * generated if any of the refs might create a race condition.
 **/
static void check_variable_refs(cgraph callgraph) //data_declaration fn, entry_point_type type) 
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  bool iscall;

  fv_init();

  // first pass - collect the list of all vars
  graph_scan_nodes (n, cg) {
    data_declaration fn = NODE_GET(endp, n)->function;

    if( iscall && !builtin_declaration(fn) )
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
  conc_region = newregion();

  // for good measure - make sure output_loc is set correctly
  unparse_start(NULL);
  set_function_separator(".");
  disable_line_directives();

  conc_debug_start("/tmp/funclist");
  mark_entry_points(callgraph);
  conc_debug_end();


  check_variable_refs(callgraph);

  unparse_end();
  
  deleteregion( conc_region );
}
