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
#include "nesc-generate.h"



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

  if( ddecl->ftype == function_event )
    fprintf(out, "event ");
  if( ddecl->ftype == function_command )
    fprintf(out, "command ");

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

#define conc_debug(format, args...) if (print_call_graph) fprintf(outfile, format, ## args)

static void conc_debug_start(const char *filename) 
{
  if (print_call_graph) {
    outfile = fopen(filename,"w");
    assert(outfile);
  }
}

static void conc_debug_end()
{
  if (print_call_graph) {
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

  while( func_stack[i] != f) 
    i++;

  while( i < func_stack_idx) {
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
 * return a string, showing the function call contexts
 **/
static char* context_str(enum contexts context) 
{
  static char str[100];
  int len;

  str[0] = '\0';
  

  if(context & c_atomic_task)
    strcat(str, "a_task ");
  if(context & c_task)
    strcat(str, "task ");
  if(context & c_atomic_int)
    strcat(str, "a_int ");
  if(context & c_int)
    strcat(str, "int ");
  if(context & c_reentrant_atomic_int)
    strcat(str, "r_a_int ");
  if(context & c_reentrant_int)
    strcat(str, "r_int ");

  len = strlen(str);
  if(len <= 0)
    strcat(str,"none");
  else
    str[len-1] = '\0';

  return str;
}



/**
 * recurse down the call tree from a particular function, printing the
 * calls
 **/
static void print_called_functions(enum contexts context,
				   gnode parent, int indent, bool iscall)
{
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
  if (builtin_declaration(fn))
    {
      pop_f();
      return;
    }
  

  // debugging output
  conc_debug("%*s", indent, "");
  if (print_call_graph) print_ddecl(outfile, fn);
  conc_debug("[%s] %s %s %s", 
             context_str(fn->contexts), 
             fn->container ? fn->container->name : "null", 
             fn->ast->location->filename, 
             fn->definition ? fn->definition->location->filename : "null");

  if (!iscall)
    {
      conc_debug("  (ref only)");
      if (fn->definition)
	{
	  conc_debug(" def not null\n");
	  conc_debug("LOCATION: %s:%ld\n",fn->ast->location->filename, fn->ast->location->lineno);
	  conc_debug("DEFINITION: %s:%ld\n",fn->definition->location->filename, fn->definition->location->lineno);
	}
      pop_f();
      return;
    }

  // error, if this node has already been seen
  if (graph_node_markedp(parent))
    {
      conc_debug("LOOP d' LOOP\n");
      warning("Found a loop in the call graph:");
      print_current_call_loop(fn, 4);
      pop_f();
      return;
    }

  if (fn->checked_contexts & context)
    {
      /* We've already seen this node in this context. Ignore it.
	 (If not, we risk the infamous context-sensitive exponential
	 blowup) */
      conc_debug("--already seen in context %s\n", context_str(context));
      pop_f();
      return;
    }
  fn->checked_contexts |= context;

  conc_debug("\n");
  
  if (fn->task_only && context != c_task)
    {
      error("task_only function `%s' called from interrupt context.  Call trace follows.", fn->name);
      print_current_call_trace(4);
    }

  if (fn->uninterruptable &&
      fn->contexts & (c_task | c_int | c_reentrant_int))
    {
      error("uninterruptable function `%s' has non-atomic call.  Call trace follows.",fn->name);
      print_current_call_trace(4);
    }

  // mark that this function has been seen, before we recurse
  graph_mark_node(parent);

  // recurse
  graph_scan_out(edge,parent) {
    child = graph_edge_to(edge);
    print_called_functions(context, child, indent+2,
			   EDGE_GET(void *, edge) != NULL);
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
static void print_call_errors(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  graph_clear_all_marks(cg);
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;

      // skip things that aren't entry points
      if (!fn->entrypoint)
        continue;

      // print called functions
      conc_debug("\n\n------------------------\n");
      conc_debug("entry point: %s\n", context_str(fn->entrypoint));
      print_called_functions(fn->entrypoint, n, 0, TRUE);
      conc_debug("------------------------\n");
    }

#if 0
  int totals[ENTRY_MAX];
  bzero(totals, sizeof(totals)); 

 /* I'll resurrect this if we care */
  if (print_call_graph) {
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
#endif
}



/**
 * check the variable refs in the given function.  Errors are
 * generated if any of the refs might create a race condition.
 **/
static void check_variable_refs(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  //bool iscall;

  fv_init();

  // first pass - collect the list of all vars
  graph_scan_nodes (n, cg) {
    data_declaration fn = NODE_GET(endp, n)->function;

    //if (iscall && !builtin_declaration(fn))
    if ( !builtin_declaration(fn) )
      find_function_vars(fn);
  }


  // now go throught the variable list, and look for conflicts
  check_for_conflicts();


  fv_cleanup();
}

static void mark_entry_points(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;
      function_decl fd;
      attribute a;
      enum contexts context;
      bool nonreentrant;

      // skip things that aren't entry points
      if (!fn->spontaneous && graph_first_edge_in(n) != NULL &&
	 !type_task(fn->type))
        continue;
      
      // skip unreachable functions
      // FIXME: add this back
      //if( !fn->isused )
      //  continue;

      // FIXME: trap errors when ALLCODE is set.  I believe the
      // previous check should take care of this.
      //assert( getenv("ALLCODE") || is_function_decl(fn->ast) )
      
      // choose the entry point type
      context = c_task;
      nonreentrant = FALSE;

      //if( !is_function_decl(fn->ast) ) {
      //  AST_print(fn->ast);
      //  printf("\n\n");
      //  exit(0);
      //}

      fd = CAST(function_decl, fn->ast);
      scan_attribute (a, fd->attributes)
	{
	  const char *name = a->word1->cstring.data;

          if (strcmp(name, "interrupt") == 0)
	    context = c_reentrant_int;
          else if (strcmp(name, "signal") == 0)
	    context = c_reentrant_atomic_int;
	  else if (strcmp(name, "nonreentrant") == 0)
	    nonreentrant = TRUE;
        }
      if (nonreentrant && context == c_reentrant_int)
	context = c_int;
      else if (nonreentrant && context == c_reentrant_atomic_int)
	context = c_atomic_int;
      fn->entrypoint = context;
    }
}

static void rec_find_nonreentrant_functions(data_declaration entry, gnode n)
{
  gedge edge;
  data_declaration fn = NODE_GET(endp, n)->function;

  if (fn->nonreentrant_caller == entry || fn->multiple_nonreentrant_callers)
    return;

  if (fn->nonreentrant_caller != NULL)
    fn->multiple_nonreentrant_callers = TRUE;
  fn->nonreentrant_caller = entry;

  graph_scan_out (edge, n) 
    if (EDGE_GET(void *, edge)) /* a call */
      rec_find_nonreentrant_functions(entry, graph_edge_to(edge));
}

/* Find which functions are called from two different non-reentrant
   interrupt entry points (these functions cannot be treated as
   non-reentrant). */
static void find_nonreentrant_functions(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;

      if (!(fn->entrypoint == c_atomic_int ||
	    fn->entrypoint == c_int))
	continue;

      rec_find_nonreentrant_functions(fn, n);
    }
}

/* fix context for multiple reentrant callers of fn */
enum contexts fix_contexts_for_multiple_callers(data_declaration fn,
					       enum contexts contexts)
{
  if (fn->multiple_nonreentrant_callers)
    {
      if (contexts & c_int)
	contexts = (contexts & !c_int) | c_reentrant_int;
      if (contexts & c_atomic_int)
	contexts = (contexts & !c_atomic_int) | c_reentrant_atomic_int;
    }

  return contexts;
}

static void rec_close_context(gnode n, enum contexts new_contexts)
{
  gedge edge;
  data_declaration fn = NODE_GET(endp, n)->function;
  enum contexts nc;

  nc = fn->contexts | new_contexts | fn->entrypoint;
  nc = fix_contexts_for_multiple_callers(fn, nc);
  if (nc == fn->contexts)
    return;
  fn->contexts = nc;

  graph_scan_out (edge, n) 
    {
      void *callkind = EDGE_GET(void *, edge);

      if (callkind) /* a call */
	{
	  if (callkind == &atomic_call_edge)
	    {
	      if (nc & c_task)
		nc = (nc & ~c_task) | c_atomic_task;
	      if (nc & c_int)
		nc = (nc & ~c_int) | c_atomic_int;
	      if (nc & c_reentrant_int)
		nc = (nc & ~c_reentrant_int) | c_reentrant_atomic_int;
	    }

	  rec_close_context(graph_edge_to(edge), nc);
	}
    }
}

static void close_contexts(cgraph callgraph)
{
  ggraph cg = cgraph_graph(callgraph);
  gnode n;
  
  /* Find least fixed point w/ recursive graph walk */
  graph_scan_nodes (n, cg)
    rec_close_context(n, 0);
}

/* Sets the contexts fields of all data_declarations of functions 
   in the callgraph to the appropriate set of contexts.
*/
static void set_function_contexts(cgraph callgraph)
{
  mark_entry_points(callgraph);
  /* find nonreentrant functions which are called from different
     nonreentrant entrypoints. These cannot be considered non-reentrant.
     This has to be done before closing contexts, otherwise contexts
     would not be monotically increasing. */
  find_nonreentrant_functions(callgraph);
  close_contexts(callgraph);
}


/**
 * External entry point to these routines
 **/
void perform_concurrency_checks(cgraph callgraph)
{
  conc_region = newregion();

  // for good measure - make sure output_loc is set correctly
  unparse_start(NULL);
  enable_documentation_mode();
  set_function_separator(".");
  disable_line_directives();

  conc_debug_start("/tmp/funclist");
  set_function_contexts(callgraph);
  print_call_errors(callgraph);
  conc_debug_end();


  check_variable_refs(callgraph);

  unparse_end();
  
  deleteregion_ptr(&conc_region);
}
