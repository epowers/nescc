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

#include "parser.h"
#include "c-parse.h"
#include "nesc-generate.h"
#include "nesc-inline.h"
#include "nesc-concurrency.h"
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "nesc-c.h"
#include "unparse.h"
#include "AST_utils.h"
#include "edit.h"
#include "semantics.h"
#include "constants.h"

static void prt_nesc_function_hdr(data_declaration fn_decl,
				  psd_options options)
/* Effects: prints the C function declaration for fn_decl
*/
{
  /* We print the declaration from the interface rather than that of fn_decl
     itself, as this latter may use not-yet-defined typedefs.
     prt_declarator will use the name from fn_decl in its output. */
  variable_decl ifn_vd = CAST(variable_decl, fn_decl->ast);
  data_decl fn_dd = CAST(data_decl, ifn_vd->parent);

  output("static ");
  prt_type_elements(fn_dd->modifiers, FALSE);

  prt_declarator(ifn_vd->declarator, NULL, ifn_vd->attributes, fn_decl, 
		 psd_rename_parameters | options);
}
void prt_nesc_function_declaration(data_declaration fndecl, void *data)
{
  if (fndecl->definition && fndecl->isused && !fndecl->suppress_definition)
    {
      prt_nesc_function_hdr(fndecl, psd_print_default);
      outputln(";");
    }
}

void prt_nesc_function_declarations(nesc_declaration mod)
{
  component_functions_iterate(mod, prt_nesc_function_declaration, NULL);
}

/* A description of the target functions a used function is connected to */
typedef struct full_connection
{
  endp ep;
  expression cond;
  expression args;
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
};

static full_connection new_full_connection(region r, endp ep, expression cond,
				 expression args)
{
  full_connection c = ralloc(r, struct full_connection);

  c->ep = ep;
  c->cond = cond;
  c->args = args;

  return c;
}

static type function_return_type(data_declaration fndecl)
{
  return type_function_return_type(get_actual_function_type(fndecl->type));
}

void prt_ncf_header(struct connections *c, type return_type)
{
  if (c->called->makeinline)
    output("inline ");
  prt_nesc_function_hdr(c->called, 0);
  outputln("{");
  indent();
  if (!type_void(return_type))
    {
      prt_data_decl(build_declaration(parse_region, return_type, "result"));
      newline();
    }
}

void prt_ncf_trailer(type return_type)
{
  if (!type_void(return_type))
    {
      newline();
      outputln("return result;");
    }
  unindent();
  outputln("}");
}

static bool prt_arguments(declaration parms, bool first)
/* Effects: prints argument list composed of the variables declared in 'parms'.
     'first' must be TRUE iff no arguments have yet been printed.
   Returns: TRUE iff 'first' and no arguments printed
*/
{
  declaration parm;

  scan_declaration (parm, parms)
    {
      /* Not supporting ... here for now. Fix requires different approach */
      data_decl dd = CAST(data_decl, parm);
      variable_decl vd = CAST(variable_decl, dd->decls);
	  
      if (!first)
	output(", ");
      first = FALSE;

      output("arg_%p", vd->ddecl);
    }
  return first;
}

void prt_ncf_direct_call(struct connections *c,
			 full_connection ccall,
			 bool last_call,
			 psd_options options,
			 type return_type,
			 function_declarator called_fd)
/* Effects: prints call to 'calls' in a connection function.
     Assigns result if last_call is TRUE.
*/
{
  bool first = TRUE;

  /* set result for last call */
  if (last_call && !type_void(return_type))
    output("result = ");

  prt_ddecl_full_name(ccall->ep->function, options);
  output("(");
  if (ccall->ep->function->gparms)
    {
      if (ccall->args)
	{
	  /* Non-generic calling generic, add arguments */
	  prt_expressions(ccall->args, first);
	  first = FALSE;
	}
      else
	{
	  /* Generic calling generic, pass arguments through */
	  first = prt_arguments(ddecl_get_gparms(c->called), first);
	}
    }
  else
    assert(!ccall->args);

  prt_arguments(called_fd->parms, first);
  outputln(");");
}

void prt_ncf_default_call(struct connections *c,
			  type return_type,
			  function_declarator called_fd)
{
  struct full_connection default_target;
  struct endp default_ep;

  default_target.ep = &default_ep;
  default_target.cond = default_target.args = NULL;
  default_ep.function = c->called;

  prt_ncf_direct_call(c, &default_target, TRUE, psd_print_default,
		      return_type, called_fd);
}

void prt_ncf_direct_calls(struct connections *c,
			  dd_list/*<full_connection>*/ calls,
			  type return_type)
/* Effects: prints calls to 'calls' in a connection function.
*/
{
  dd_list_pos call;
  function_declarator called_fd = ddecl_get_fdeclarator(c->called);

  dd_scan (call, calls)
    {
      full_connection ccall = DD_GET(full_connection, call);

      assert(!ccall->cond);

      prt_ncf_direct_call(c, ccall, dd_is_end(dd_next(call)), 0,
			  return_type, called_fd);
    }
}

static int constant_expression_list_compare(expression arg1, expression arg2)
{
  while (arg1)
    {
      largest_int uval1, uval2;

      uval1 = cval_sint_value(arg1->cst->cval);
      uval2 = cval_sint_value(arg2->cst->cval);

      /* Can't use - as might overflow and mod down to 0 */
      if (uval1 < uval2)
	return -1;
      else if (uval1 > uval2)
	return 1;

      arg1 = CAST(expression, arg1->next);
      arg2 = CAST(expression, arg2->next);
    }
  assert(!arg2);

  return 0;
}

static int condition_compare(const void *p1, const void *p2)
{
 struct full_connection *const *c1 = p1, *const *c2 = p2;

  return constant_expression_list_compare((*c1)->cond, (*c2)->cond);
}

static void prt_ncf_condition(struct connections *c, expression cond)
{
  declaration gparm;
  bool first = TRUE;

  scan_declaration (gparm, ddecl_get_gparms(c->called))
    {
      data_decl dd = CAST(data_decl, gparm);
      variable_decl vd = CAST(variable_decl, dd->decls);
	  
      if (first)
	output("if (");
      else
	output(" && ");
      first = FALSE;

      output("arg_%p == ", vd->ddecl);
      prt_expression(cond, P_REL);

      cond = CAST(expression, cond->next);
    }
  output(") ");
}

static void prt_ncf_conditional_calls(struct connections *c, type return_type)
{
  dd_list_pos call;
  int i, j, ncalls = dd_length(c->normal_calls);
  full_connection *cond_eps =
    rarrayalloc(c->r, ncalls, full_connection);
  function_declarator called_fd = ddecl_get_fdeclarator(c->called);
  bool one_arg = FALSE;

  /* No work to do */
  if (ncalls == 0 && !dd_is_empty(c->generic_calls))
    return;

  /* Sort calls so we can find connections with the same conditions */
  i = 0;
  dd_scan (call, c->normal_calls)
    cond_eps[i++] = DD_GET(full_connection, call);
  qsort(cond_eps, ncalls, sizeof(full_connection), condition_compare);

  if (ncalls > 0 && !cond_eps[0]->cond->next)
    {
      /* use switch rather than cascaded ifs (gcc generate better code) */
      one_arg = TRUE;
      output("switch (");
      prt_arguments(ddecl_get_gparms(c->called), TRUE);
      outputln(") {");
      indent();
    }

  /* output the calls */
  i = 0;
  while (i < ncalls)
    {
      expression cond = cond_eps[i]->cond;

      /* output latest condition */
      if (one_arg)
	{
	  output("case ");
	  prt_expression(cond, P_ASSIGN);
	  outputln(":");
	}
      else
	{
	  if (i != 0)
	    output("else ");
	  prt_ncf_condition(c, cond);
	  outputln("{");
	}
      indent();

      /* find last target with same condition */
      j = i;
      while (++j < ncalls && condition_compare(&cond_eps[i], &cond_eps[j]) == 0)
	;

      /* print them, setting result for the last one */
      while (i < j)
	{
	  prt_ncf_direct_call(c, cond_eps[i], i == j - 1, 0,
			      return_type, called_fd);
	  i++;
	}
	
      if (one_arg)
	outputln("break;");
      unindent();
      if (!one_arg)
	outputln("}");
    }
  /* output call to default if there are no generic calls */
  if (dd_is_empty(c->generic_calls))
    {
      if (ncalls > 0)
	{
	  if (one_arg)
	    outputln("default:");
	  else
	    outputln("else");
	}
      indent();
      prt_ncf_default_call(c, return_type, called_fd);
      unindent();
      if (ncalls > 0 && one_arg)
	{
	  outputln("}");
	  unindent();
	}
    }
  else if (one_arg)
    {
      unindent();
      outputln("}");
    }
}

static void prt_nesc_connection_function(struct connections *c)
{
  type return_type = function_return_type(c->called);

  prt_ncf_header(c, return_type);

  if (c->called->gparms)
    {
      prt_ncf_direct_calls(c, c->generic_calls, return_type);
      prt_ncf_conditional_calls(c, return_type);
    }
  else
    {
      if (dd_is_empty(c->normal_calls))
	prt_ncf_default_call(c, return_type,
			     ddecl_get_fdeclarator(c->called));
      else
	prt_ncf_direct_calls(c, c->normal_calls, return_type);
    }

  prt_ncf_trailer(return_type);
}

void prt_nesc_called_function_hdr(data_declaration fndecl, void *data)
{
  if (!(fndecl->defined || fndecl->uncallable) && fndecl->isused)
    {
      prt_nesc_function_hdr(fndecl, 0);
      outputln(";");
    }
}

void prt_nesc_called_function_headers(cgraph cg, nesc_declaration mod)
{
  component_functions_iterate(mod, prt_nesc_called_function_hdr, NULL);
}

void prt_nesc_module(cgraph cg, nesc_declaration mod)
{
  prt_nesc_called_function_headers(cg, mod);
  prt_toplevel_declarations(CAST(module, mod->impl)->decls);
}

static bool find_reachable_functions(struct connections *c, gnode n,
				     expression gcond, expression gargs)
{
  endp ep = NODE_GET(endp, n);

  if (ep->args)
    {
      /* First set of arguments is a condition if 'called' is generic */
      if (c->called->gparms && !gcond)
	gcond = ep->args;
      else if (gargs)
	{
	  /* We already have some arguments, so this is a condition again.
	     If the condition doesn't match gargs, then the call is
	     filtered out. If they do match, we set gargs to null (we're
	     back to a non-parameterised call) */
	  if (constant_expression_list_compare(gargs, ep->args) != 0)
	    return FALSE;
	  gargs = NULL;
	}
      else
	{
	  assert(!gargs);
	  gargs = ep->args;
	}
    }
  if (graph_node_markedp(n))
    return TRUE;
  else if (!ep->args && ep->function->defined &&
	   is_module(((nesc_declaration)ep->function->container)->impl))
    {
      full_connection target = new_full_connection(c->r, ep, gcond, gargs);


      assert(!graph_first_edge_out(n));

      dd_add_last(c->r, 
		  c->called->gparms && !gcond ?
		    c->generic_calls : c->normal_calls, 
		  target);
    }
  else
    {
      gedge out;

      graph_mark_node(n);
      graph_scan_out (out, n)
	if (find_reachable_functions(c, graph_edge_to(out), gcond, gargs))
	  return TRUE;
      graph_unmark_node(n);
    }
  return FALSE;
}

static void find_connected_functions(struct connections *c)
{
  gnode called_fn_node;

  graph_clear_all_marks(cgraph_graph(c->cg));
  called_fn_node = fn_lookup(c->cg, c->called);
  assert(!graph_first_edge_in(called_fn_node));
  if (find_reachable_functions(c, called_fn_node, NULL, NULL))
    error_with_location(c->called->ast->location,
			"cycle in configuration (for %s%s%s.%s)",
			c->called->container->name,
			c->called->interface ? "." : "",
			c->called->interface ? c->called->interface->name : "",
			c->called->name);
}

void find_function_connections(data_declaration fndecl, void *data)
{
  cgraph cg = data;

  if (!fndecl->defined)
    {
      region r = parse_region;
      struct connections *connections;

      fndecl->connections = connections = ralloc(r, struct connections);
      connections->r = r;
      connections->cg = cg;
      connections->called = fndecl;

      connections->generic_calls = dd_new_list(r);
      connections->normal_calls = dd_new_list(r);

      find_connected_functions(connections);

      /* a function is uncallable if it has no default definition and
	   non-generic: no connections
	   generic: no generic connections
      */
      if (!(fndecl->definition ||
	    !dd_is_empty(connections->generic_calls) ||
	    (!fndecl->gparms && !dd_is_empty(connections->normal_calls))))
	fndecl->uncallable = TRUE;
      else
	fndecl->suppress_definition =
	  !dd_is_empty(fndecl->gparms ? connections->generic_calls :
		       connections->normal_calls);
    }
}

void find_connections(cgraph cg, nesc_declaration mod)
{
  component_functions_iterate(mod, find_function_connections, cg);
}

static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl,
				    bool force_call);

static void mark_connected_function_list(cgraph cg,
					 data_declaration caller,
					 dd_list/*full_connection*/ calls)
{
  dd_list_pos connected;

  dd_scan (connected, calls)
    {
      full_connection conn = DD_GET(full_connection, connected);

      mark_reachable_function(cg, caller, conn->ep->function, TRUE);
    }
}

static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl,
				    bool force_call)
{
  dd_list_pos use;

  if (caller && ddecl->kind == decl_function)
    {
      static int notnull;
      void *iscall = NULL;

      /* FIXME: gratuitous n^2 behaviour */
      if (force_call || dd_find(caller->calls, ddecl))
	iscall = &notnull;

      graph_add_edge(fn_lookup(cg, caller), fn_lookup(cg, ddecl), iscall);
    }

  if (ddecl->isused)
    return;
  ddecl->isused = TRUE;

  if (ddecl->kind != decl_function ||
      (ddecl->container && 
       !(ddecl->container->kind == l_component &&
	 is_module(CAST(component, ddecl->container->ast)->implementation))))
    return;

  if ((ddecl->ftype == function_command || ddecl->ftype == function_event) &&
      !ddecl->defined)
    {
      /* Call to a command or event not defined in this module.
	 Mark all connected functions */
      mark_connected_function_list(cg, ddecl, ddecl->connections->generic_calls);
      mark_connected_function_list(cg, ddecl, ddecl->connections->normal_calls);

      /* Don't process body of suppressed default defs */
      if (ddecl->suppress_definition)
	return;
    }

  /* Make sure ddecl gets a node in the graph even if it doesn't call
     anything */
  fn_lookup(cg, ddecl);

  dd_scan (use, ddecl->uses)
    mark_reachable_function(cg, ddecl, DD_GET(data_declaration, use), FALSE);
}

static cgraph mark_reachable_code(void)
{
  dd_list_pos used;
  cgraph cg = new_cgraph(parse_region);

  /* We use the connection graph type to represent our call graph */

  dd_scan (used, spontaneous_calls)
    mark_reachable_function(cg, NULL, DD_GET(data_declaration, used), FALSE);
  dd_scan (used, global_uses)
    mark_reachable_function(cg, NULL, DD_GET(data_declaration, used), FALSE);

  return cg;
}

static void prt_nesc_function(data_declaration fn)
{
  assert(fn->kind == decl_function);

  if (fn->definition && !fn->suppress_definition)
    prt_function_body(CAST(function_decl, fn->definition));

  /* if this is a connection function, print it now */
  if ((fn->ftype == function_command || fn->ftype == function_event) &&
      !fn->defined && !fn->uncallable)
    prt_nesc_connection_function(fn->connections);
}

static bool isinlined(data_declaration fn)
{
  return fn->isinline || fn->makeinline;
}

static void topological_prt(gnode gep, bool force)
{
  gedge out;
  data_declaration fn;

  fn = NODE_GET(endp, gep)->function;
  if (isinlined(fn) || force)
    {
      if (graph_node_markedp(gep))
	return;

      graph_mark_node(gep);

      graph_scan_out (out, gep)
	topological_prt(graph_edge_to(out), FALSE);

      prt_nesc_function(fn);
    }
}
static void prt_inline_functions(cgraph callgraph)
{
  gnode fns;

  graph_clear_all_marks(cgraph_graph(callgraph));
  graph_scan_nodes (fns, cgraph_graph(callgraph))
    {
      data_declaration fn = NODE_GET(endp, fns)->function;
      if (isinlined(fn))
	{
	  gedge callers;
	  bool inlinecallers = FALSE;

	  graph_scan_in (callers, fns)
	    {
	      data_declaration caller =
		NODE_GET(endp, graph_edge_from(callers))->function;

	      if (isinlined(caller))
		{
		  inlinecallers = TRUE;
		  break;
		}
	    }

	  if (!inlinecallers)
	    topological_prt(fns, FALSE);
	}
    }
}

static void prt_noninline_functions(cgraph callgraph)
{
  gnode fns;
  graph_scan_nodes (fns, cgraph_graph(callgraph))
    {
      data_declaration fn = NODE_GET(endp, fns)->function;

      if (!isinlined(fn))
	{
	  /* There may be some inlined functions which were not printed
	     earlier, because they were:
	     a) recursive (possibly indirectly), with explicit inline
	        keywords
	     b) not called from another inline function
	     So we use topological_prt here to ensure they are printed
	     before any calls to them from non-inlined functions
	  */
	  topological_prt(fns, TRUE);
	}
    }
}

static void suppress_function(const char *name)
{
  data_declaration d = lookup_global_id(name);

  if (d && d->kind == decl_function && d->definition)
    d->suppress_definition = TRUE;
}

void generate_c_code(nesc_declaration program, const char *target_name,
		     cgraph cg, dd_list modules)
{
  dd_list_pos mod;
  cgraph callgraph;
  FILE *output = NULL;

  if (target_name)
    {
      output = fopen(target_name, "w");
      if (!output)
	{
	  perror("couldn't create output file");
	  exit(2);
	}
    }
  
  /* We start by finding each module's connections and marking uncallable
     functions */
  dd_scan (mod, modules)
    find_connections(cg, DD_GET(nesc_declaration, mod));

  /* mark reachable code, and perform some concurrency analysis */
  callgraph =  mark_reachable_code();
  perform_concurrency_checks(callgraph);

  unparse_start(output ? output : stdout);
  disable_line_directives();

  /* suppress debug functions if necessary */
  if (flag_no_debug)
    {
      suppress_function("dbg");
      suppress_function("dbg_clear");
      suppress_function("dbg_active");
      outputln("#define dbg(mode, format, ...) ((void)0)");
      outputln("#define dbg_clear(mode, format, ...) ((void)0)");
      outputln("#define dbg_active(mode) 0");
    }

  /* We start by finding each module's connections and marking uncallable
     functions */
  //  dd_scan (mod, modules)
    //    find_connections(cg, DD_GET(nesc_declaration, mod));

  /* Then we set the 'isused' bit on all functions that are reachable
     from spontaneous_calls or global_uses */
  inline_functions(callgraph);

  /* Then we print the code. */
  /* The C declarations first */
  enable_line_directives();
  prt_toplevel_declarations(all_cdecls);
  disable_line_directives();

  dd_scan (mod, modules)
    prt_nesc_function_declarations(DD_GET(nesc_declaration, mod));

  enable_line_directives();

  dd_scan (mod, modules)
    prt_nesc_module(cg, DD_GET(nesc_declaration, mod));

  prt_inline_functions(callgraph);
  prt_noninline_functions(callgraph);

  unparse_end();

  if (output)
    fclose(output);
}
