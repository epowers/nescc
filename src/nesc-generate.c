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

void prt_nesc_abstract_declarations(nesc_declaration mod)
{
  declaration dlist = CAST(module, mod->impl)->decls;
  declaration aplist = CAST(component, mod->ast)->abs_param_list;
  declaration d;
  int i;

  if (mod->is_abstract) {

    /* Print instance variable structure */
    output_instancetype(mod);
    output(" {\n");

    /* Instance parameters and magic variables need to go first, so 
     * we can initialize them */
    scan_declaration(d, aplist) {
      if (d->kind == kind_data_decl) {
	data_decl dd = CAST(data_decl, d);
	declaration vd;
	scan_declaration (vd, dd->decls) {
	  output("  ");
       	  prt_data_decl(dd);
	  output("\n");
	}
      }
    }

    /* Now add instance variables */
    scan_declaration (d, dlist) {
      if (d->kind == kind_data_decl) {
	data_decl dd = CAST(data_decl, d);
	declaration vd;
	scan_declaration (vd, dd->decls) {
	  variable_decl vdd = CAST(variable_decl, vd);
	  data_declaration vdecl = vdd->ddecl;
	  if (is_instance_variable(vdecl)) {
	    output("  ");
	    prt_data_decl(dd);
	    output("\n");
	  }
	}
      }
    }

    output(" } ");
    output_stripped_string_dollar(mod->name);
    output(NESC_INSTANCEARR_LITERAL);
    output("[%d]", mod->abstract_instance_count);

//#define MDW_BROKEN_NEED_INITIALIZERS
#ifdef MDW_BROKEN_NEED_INITIALIZERS
    /* Initializers */
    output(" = {\n");
    for (i = 0; i < mod->abstract_instance_count; i++) {
      output("{ ");

      scan_declaration(d, aplist) {
	if (d->kind == kind_data_decl) {
	  data_decl dd = CAST(data_decl, d);
	  declaration vd;
	  scan_declaration (vd, dd->decls) {
	    variable_decl vdd = CAST(variable_decl, vd);
	    data_declaration vdecl = vdd->ddecl;
	    if (!strcmp(vdecl->name, NESC_INSTANCENUM_LITERAL)) {
	      output("  %d /* %s */, \n", i, NESC_INSTANCENUM_LITERAL);
	    } else {
	      /* XXX Need to determine constant initializers */
	      output(" 0 /* XXX PLACEHOLDER */, ");
	    }
	  }
	}
      }
      output("},\n");
    }
    output("\n}");
#endif

    output(";\n");
  }

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
  int instance;

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

  print_endp("MDW: new_full_connection: ep: ", ep);
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

  /* Emit instance pointer if this is a call to an abstract component */
  if (ccall->ep->component && 
      ccall->ep->component->kind == decl_component_ref &&
      ccall->ep->component->instance_number != -1) {

    fprintf(stderr,"MDW: prt_ncf_direct_call: function %s\n", ccall->ep->function->name);
    print_endp("MDW: prt_ncf_direct_call: ep is ", ccall->ep);
    fprintf(stderr,"MDW: prt_ncf_direct_call: instance %d\n", ccall->ep->component->instance_number);

    output("&(");
    output_stripped_string_dollar(ccall->ep->component->ctype->name);
    output(NESC_INSTANCEARR_LITERAL);
    output("[%d])", ccall->ep->component->instance_number);
    first = FALSE;
  }

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
  default_ep.component = default_ep.interface = NULL;
  default_ep.instance = -1;
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

  output("/* MDW: prt_nesc_connection_function on %s */\n", c->called->name);

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


void prt_toplevel_module_declarations(nesc_declaration mod) {
  /* Print toplevel decls that are not in the instance structure */
  declaration dlist = CAST(module, mod->impl)->decls;
  declaration d;

  if (mod->is_abstract) {
    scan_declaration (d, dlist) {
      if (d->kind == kind_data_decl) {
	data_decl dd = CAST(data_decl, d);
	declaration vd;
	scan_declaration (vd, dd->decls) {
	  variable_decl vdd = CAST(variable_decl, vd);
	  data_declaration vdecl = vdd->ddecl;
	  if (!is_instance_variable(vdecl)) { 
	    prt_data_decl(dd);
	  }
	}
      } else {        
	prt_toplevel_declaration(d);
      }
    }

  } else {
    scan_declaration (d, dlist)
      prt_toplevel_declaration(d);
  }
}

void prt_nesc_module(cgraph cg, nesc_declaration mod)
{
  output("/* MDW: begin: prt_nesc_module(%s) */\n", mod->name);
  prt_nesc_called_function_headers(cg, mod);
  prt_toplevel_module_declarations(mod);
  output("/* MDW: end: prt_nesc_module(%s) */\n", mod->name);
}

static bool find_reachable_functions(struct connections *c, gnode n,
				     expression gcond, expression gargs)
{
  endp ep = NODE_GET(endp, n);
  print_endp("MDW: find_reachable_functions: ep ", ep);

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

      fprintf(stderr,"  MDW: Recursing into graph_scan_out (ep=0x%lx)\n", ep);
      graph_mark_node(n);
      graph_scan_out (out, n)
	if (find_reachable_functions(c, graph_edge_to(out), gcond, gargs))
	  return TRUE;
      graph_unmark_node(n);
      fprintf(stderr,"  MDW: Done with graph_scan_out (ep=0x%lx)\n", ep);

    }
  return FALSE;
}

static void find_connected_functions(struct connections *c)
{
  gnode called_fn_node;
  endp ep;

  graph_clear_all_marks(cgraph_graph(c->cg));
  called_fn_node = fn_lookup(c->cg, c->called, c->instance);

  ep = NODE_GET(endp, called_fn_node);
  print_endp("MDW: find_connected_functions: got ep: ", ep);

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
  int num_instances, instance;

  fprintf(stderr,"\nMDW: find_function_connections: fndecl %s (type %d)\n", fndecl->name, fndecl->ftype);

  if (!fndecl->defined)
    {
      region r = parse_region;
      struct connections *connections;

      fprintf(stderr,"MDW: find_function_connections: NOT DEFINED\n");
      if (fndecl->container) {
        fprintf(stderr,"MDW: container is %s (0x%lx) is_abstract %d aic %d\n", 
	    fndecl->container->name,
	    fndecl->container,
	    fndecl->container->is_abstract,
	    fndecl->container->abstract_instance_count);
      }

      num_instances = num_abstract_instances(fndecl);
      if (!num_instances) num_instances = 1;

      for (instance = 0; instance < num_instances; instance++) {
	fndecl->connections = connections = ralloc(r, struct connections);
	connections->r = r;
	connections->cg = cg;
	connections->called = fndecl;
	if (fndecl->container && fndecl->container->is_abstract) {
	  connections->instance = instance;
	} else {
	  connections->instance = -1; // No instance
	}

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
}

void find_connections(cgraph cg, nesc_declaration mod)
{
  fprintf(stderr,"\nMDW: find_connections for module %s\n\n", mod->name);
  component_functions_iterate(mod, find_function_connections, cg);
  fprintf(stderr,"\nMDW: DONE with find_connections for module %s\n\n", mod->name);
}

static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl);

static void mark_connected_function_list(cgraph cg,
					 data_declaration caller,
					 dd_list/*full_connection*/ calls)
{
  dd_list_pos connected;

  dd_scan (connected, calls)
    {
      full_connection conn = DD_GET(full_connection, connected);

      mark_reachable_function(cg, caller, conn->ep->function);
    }
}
static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl)
{
  dd_list_pos use;
  int num_caller_instances, caller_instance, num_ddecl_instances, 
    ddecl_instance;

  if (caller && ddecl->kind == decl_function) {
    num_caller_instances = num_abstract_instances(caller);
    if (!num_caller_instances) num_caller_instances = 1;
    num_ddecl_instances = num_abstract_instances(ddecl);
    if (!num_ddecl_instances) num_ddecl_instances = 1;
    for (caller_instance = 0; caller_instance < num_caller_instances; caller_instance++) {
      for (ddecl_instance = 0; ddecl_instance < num_ddecl_instances; ddecl_instance++) {
	graph_add_edge(fn_lookup(cg, caller, caller_instance), fn_lookup(cg, ddecl, ddecl_instance), NULL);
      }
    }
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
  num_ddecl_instances = num_abstract_instances(ddecl);
  if (!num_ddecl_instances) num_ddecl_instances = 1;
  for (ddecl_instance = 0; ddecl_instance < num_ddecl_instances; ddecl_instance++) {
    fn_lookup(cg, ddecl, ddecl_instance);
  }

  dd_scan (use, ddecl->uses)
    mark_reachable_function(cg, ddecl, DD_GET(data_declaration, use));
}

static cgraph mark_reachable_code(void)
{
  dd_list_pos used;
  cgraph cg = new_cgraph(parse_region);

  /* We use the connection graph type to represent our call graph */

  dd_scan (used, spontaneous_calls)
    mark_reachable_function(cg, NULL, DD_GET(data_declaration, used));
  dd_scan (used, global_uses)
    mark_reachable_function(cg, NULL, DD_GET(data_declaration, used));

  return cg;
}

static void prt_nesc_function(data_declaration fn, endp ep)
{
  assert(fn->kind == decl_function);

  fprintf(stderr, "   MDW: prt_nesc_function %s (%d instances) (inum %d)\n", fn->name, num_abstract_instances(fn), ep->instance);

  if (fn->definition && !fn->suppress_definition) {
    // Only emit one instance 
    if (fn->container && fn->container->is_abstract && 
	(ep->instance == 0) || (ep->instance == -1)) {
      fprintf(stderr, "   MDW: prt_function_body\n");
      prt_function_body(CAST(function_decl, fn->definition));
    }
  }

  /* if this is a connection function, print it now */
  if ((fn->ftype == function_command || fn->ftype == function_event) &&
      !fn->defined && !fn->uncallable) {
    fprintf(stderr, "   MDW: prt_nesc_connection_function\n");
    prt_nesc_connection_function(fn, ep);
  }

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

  print_endp("MDW: topological_prt: ", NODE_GET(endp, gep));

  if (isinlined(fn) || force)
    {
      if (graph_node_markedp(gep))
	return;

      graph_mark_node(gep);

      graph_scan_out (out, gep)
	topological_prt(graph_edge_to(out), FALSE);

      prt_nesc_function(fn, NODE_GET(endp, gep));
    }
}
static void prt_inline_functions(cgraph callgraph)
{
  gnode fns;

  graph_clear_all_marks(cgraph_graph(callgraph));
  graph_scan_nodes (fns, cgraph_graph(callgraph))
    {
      endp ep = NODE_GET(endp, fns);
      data_declaration fn = ep->function;
      print_endp("\nMDW: prt_inline_functions: ep ", ep);

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

	  if (!inlinecallers) {
	    topological_prt(fns, FALSE);
	  }
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

  fprintf(stderr,"\n\nMDW: generate_c_code starting\n\n\n");

  /* We start by finding each module's connections and marking uncallable
     functions */
  dd_scan (mod, modules)
    find_connections(cg, DD_GET(nesc_declaration, mod));

  fprintf(stderr,"MDW: done with find_connections\n");

  /* Then we set the 'isused' bit on all functions that are reachable
     from spontaneous_calls or global_uses */
  callgraph =  mark_reachable_code();
  inline_functions(callgraph);

  fprintf(stderr,"MDW: done with inline_functions\n");
  outputln("\n/* MDW: prt_toplevel_declarations */");

  /* Then we print the code. */
  /* The C declarations first */
  enable_line_directives();
  prt_toplevel_declarations(all_cdecls);
  disable_line_directives();

  fprintf(stderr,"MDW: done with prt_toplevel_declarations\n");

  dd_scan (mod, modules) {
    outputln("\n/* MDW: prt_abstract_declarations */");
    prt_nesc_abstract_declarations(DD_GET(nesc_declaration, mod));
    outputln("\n/* MDW: prt_function_declarations */");
    prt_nesc_function_declarations(DD_GET(nesc_declaration, mod));
  }

  enable_line_directives();

  fprintf(stderr,"MDW: Printing modules\n");

  dd_scan (mod, modules)
    prt_nesc_module(cg, DD_GET(nesc_declaration, mod));

  fprintf(stderr,"MDW: Printing inline functions\n");
  outputln("\n/* MDW: prt_inline_functions */");
  prt_inline_functions(callgraph);

  fprintf(stderr,"MDW: Printing noninline functions\n");
  outputln("\n/* MDW: prt_noninline_functions */");
  prt_noninline_functions(callgraph);

  unparse_end();

  if (output)
    fclose(output);

  fprintf(stderr,"MDW: Done with generate_c_code\n");
}
