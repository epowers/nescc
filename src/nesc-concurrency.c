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


static FILE *outfile = NULL;
static region conc_region = NULL;

static inline void check_print_func_args(function_decl fd, data_decl dd, variable_decl vd,
                                         function_declarator *fdr, data_declaration *ddecl) {
  if( fd ) {
    assert(dd==NULL);
    assert(vd==NULL);
    if(fdr) *fdr = CAST(function_declarator, fd->fdeclarator);
    if(ddecl) *ddecl = fd->ddecl;
  }
  else {
    assert(fd==NULL);
    //assert(dd);    
    assert(vd);
    if (is_function_declarator((ast_generic)vd->declarator)) {
      if(fdr) *fdr = CAST(function_declarator, vd->declarator);
      if(ddecl) *ddecl = vd->ddecl;
    }
    // Chase down pointers if necessary.
    else if (is_pointer_declarator((ast_generic)vd->declarator)) {
      function_declarator fdcl = get_fdeclarator(vd->declarator);
      if(fdr) *fdr = fdcl;
      if(ddecl) *ddecl = vd->ddecl;
    }
    else {
      if(ddecl) *ddecl = vd->ddecl;
    }
  }
}

/**
 * print function return type & modifiers
 **/
static void print_func_return(function_decl fd, data_decl dd, variable_decl vd) 
{
  check_print_func_args(fd, dd, vd, NULL, NULL);
  if(fd) {
    prt_declarator(NULL, fd->modifiers, fd->attributes, fd->ddecl, 0);//psd_skip_container);
  } else {
    // FIXME: reinstate this?
    //prt_type_elements(dd->modifiers, pte_skip_command_event); 
  }
}

/**
 * print function name
 **/
static void print_func_name(function_decl fd, data_decl dd, variable_decl vd) 
{
  function_declarator fdr;
  data_declaration ddecl; 

  check_print_func_args(fd, dd, vd, &fdr, &ddecl);
  prt_simple_declarator(fdr->declarator, ddecl, /*psd_skip_container |*/ psd_need_paren_for_star | psd_need_paren_for_qual);
  
}

/**
 * print function arguments
 **/
// FIXME: this should allow an option to print only arg types, and not
// names - necessary to canonicalize anchors and hrefs.
static void print_func_args(function_decl fd, data_decl dd, variable_decl vd) 
{
  function_declarator fdr;
  data_declaration ddecl; 

  check_print_func_args(fd, dd, vd, &fdr, &ddecl);
  prt_parameters(fdr->gparms ? fdr->gparms :
                 ddecl ? ddecl_get_gparms(ddecl) : NULL,
                 fdr->parms,
                 0);//psd_skip_container);
  
}


/**
 * print the entire header for a function - return name(args)
 **/
static void print_function_header(function_decl fd, data_decl dd, variable_decl vd) 
{
  print_func_return(fd, dd, vd);
  print_func_name(fd, dd, vd);
  print_func_args(fd, dd, vd);
}








typedef enum {
  ENTRY_TASK,                 // non-interrupting, interruptable
  ENTRY_REENTRANT_INTERRUPT,  // interrupting, interruptable
  ENTRY_ATOMIC_INTERRUPT,     // interrupting, non-interuptable
  ENTRY_MAX                   // for array size
} entry_point_type;


/**
 * check the variable refs in the given function.  Errors are
 * generated if any of the refs might create a race condition.
 **/
static void check_variable_refs(data_declaration fn, entry_point_type type) 
{
  dhash_table vars;
  dhash_scan scanner;
  node n;

  printf("\n\n\n++++++++++++++++++++++++++++++++++++++++\n");
  n = fn->ast->next;
  fn->ast->next = NULL;
  AST_print( CAST(node,fn->ast) );
  fn->ast->next = n;
  printf("----------------------------------------\n");

  if( !is_function_decl(fn->ast) )
    return;

  
  vars = AST_find_vars(conc_region, CAST(node, CAST(function_decl,fn->ast)->stmt));
  scanner = dhscan(vars);
  for(n=(node)dhnext(&scanner); n; n=(node)dhnext(&scanner)) {
    AST_print( n );
    printf("------------------------------\n");
  }

  exit(1);
}



/**
 * recurse down the call tree from a particular function, marking
 * called functions with the given type.q
 **/
static int mark_functions(gnode parent, entry_point_type type, int indent)
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
    return 0;
  

  // debugging output
  fprintf(outfile, "%*s",indent,"");
  if( is_function_decl(fn->ast) )
    print_function_header(CAST(function_decl, fn->ast),NULL,NULL);
  else if( is_variable_decl(fn->ast) )
    print_function_header(NULL, NULL, CAST(variable_decl, fn->ast));
  else {
    fprintf(outfile,"************** UNKNOWN AST\n");
    AST_print(CAST(node,fn->ast));
    fprintf(outfile,"**************\n");
    return 0;
  }
  fprintf(outfile," --- %s", fn->container ? fn->container->name : "null");
  if( !fn->is_function_call ) {
    fprintf(outfile, "  (ref only)");
    if( fn->definition ) {
      fprintf(outfile, " def not null\n");
      fprintf(outfile, "LOCATION: %s:%ld\n",fn->ast->location->filename, fn->ast->location->lineno);
      fprintf(outfile, "DEFINITION: %s:%ld\n",fn->definition->location->filename, fn->definition->location->lineno);
      return 0;
    }
  }

  fprintf(outfile,"\n");
  

  // error, if this node has already been seen
  if( fn->already_seen ) {
    if( type == ENTRY_TASK ) {
      fprintf(outfile,"LOOP d' LOOP (task context)\n");
    } else {
      // FIXME: need to give info about which function, and where the cycle lies 
      //error("loop the loop");
      fprintf(outfile,"LOOP d' LOOP (interrupt context)\n");
    }
    return 0;
  }

  // set the flags for the current function
  if(type == ENTRY_TASK) {
    if(fn->task_context) return 0;  // bottom out, already counted
    fn->task_context = TRUE;
    if( fn->uninterruptable ) 
      warning("uninterruptable function `%s' called from task context",fn->name);
  } 
  else if(type == ENTRY_REENTRANT_INTERRUPT) {
    if( fn->reentrant_interrupt_context ) return 0; // bottom out, already counted
    fn->reentrant_interrupt_context = TRUE;
    if( fn->uninterruptable ) 
      warning("uninterruptable function `%s' called from reentrant interrupt context",fn->name);
    if( fn->task_only ) 
      warning("task_only function `%s' called from reentrant interrupt context",fn->name);
  }
  else if(type == ENTRY_ATOMIC_INTERRUPT) {
    if( fn->atomic_interrupt_context ) return 0; // bottom out, already counted
    fn->atomic_interrupt_context = TRUE;
    if( fn->task_only ) 
      warning("task_only function `%s' called from atomic interrupt context",fn->name);
  }


  // check the variable refs in the current function
  // 
  // FIXME: add this back
  //  check_variable_refs(fn,type);


  // mark that this function has been seen, before we recurse
  fn->already_seen = TRUE;

  // recurse
  subtotal = 1; // current function
  graph_scan_out(edge,parent) {
    child = graph_edge_to(edge);
    subtotal += mark_functions(child, type, indent+2);
  }
  

  // clear the seen flag
  fn->already_seen = FALSE;

  
  return subtotal;
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
  graph_scan_nodes (n, cg)
    {
      data_declaration fn = NODE_GET(endp, n)->function;

      // skip things that aren't entry points
      if( !fn->spontaneous  &&  graph_first_edge_in(n) != NULL )
        continue;

      
      // FIXME: will this cause false loops?
      fn->is_function_call = TRUE;

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
      totals[type] += mark_functions(n,type,0);
      fprintf(outfile,"------------------------\n");
    }


  fprintf(outfile,"function type totals:\n");
  fprintf(outfile,"    task           %d\n",totals[ENTRY_TASK]);
  fprintf(outfile,"    atomic int     %d\n",totals[ENTRY_ATOMIC_INTERRUPT]);
  fprintf(outfile,"    reentrant int  %d\n",totals[ENTRY_REENTRANT_INTERRUPT]);
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
  unparse_end();
  fprintf(outfile, "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n\n");

  fclose(outfile);

  deleteregion( conc_region );
}
