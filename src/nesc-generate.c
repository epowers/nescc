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
#include "nesc-concurrency.h"
#include "nesc-uses.h"

#ifdef GALSC
#include "galsc-generate.h"
#include "galsc-a.h"

// The string to use to separate substrings in the generated
// port-related functions/variables/names.
static char *galsc_separator = "$";
#endif

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

#ifdef GALSC
#else
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

  /* The combiner function used, if any */
  data_declaration combiner;
};
#endif

#ifdef GALSC
static full_connection new_full_connection(region r, endp ep, expression cond,
				 expression args,
        dd_list parameters)
#else
static full_connection new_full_connection(region r, endp ep, expression cond,
				 expression args)
#endif
{
  full_connection c = ralloc(r, struct full_connection);

  c->ep = ep;
  c->cond = cond;
  c->args = args;

#ifdef GALSC
  c->parameters = parameters;
#endif

  return c;
}

#ifdef GALSC
// Initialize 'fconn' to NULL.
void init_full_connection(full_connection fconn) {
    fconn->ep = NULL;
    fconn->cond = NULL;
    fconn->args = NULL;
    fconn->parameters = NULL;
}
#endif

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

#ifdef GALSC
bool prt_arguments(declaration parms, bool first)
#else
static bool prt_arguments(declaration parms, bool first)
#endif
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

#ifdef GALSC
// FIXME comment
/* Effects: prints call to 'calls' in a connection function.
     Assigns result if last_call is TRUE.
*/
void prt_ncf_direct_call(struct connections *c,
			 full_connection ccall,
			 bool first_call,
			 psd_options options,
			 type return_type,
			 function_declarator called_fd) {
    bool first_arg = TRUE;
    data_declaration combiner = type_combiner(return_type);
    bool calling_combiner = FALSE;
    
    if ( !(options & psd_galsc_print_port_get_call) ) {   // f ? -> x
        if (!type_void(return_type)) {
            output("result = ");
            
            // Combine w/ the combiner on subsequent calls.
            if (!first_call && combiner) {
                output("%s(result, ", combiner->name);
                calling_combiner = TRUE;
            }
        }
    }

    // ccall->ep is the target of the connection.
    if (ccall->ep->port && ccall->ep->port->in) {         // f -> p
        prt_ddecl_full_name(ccall->ep->port, options | psd_galsc_print_port_put_call);
    } else if (ccall->ep->port && !ccall->ep->port->in) { // p -> p
        output("GALSC__");
        prt_ddecl_full_name(ccall->ep->port, options);
        output_string(galsc_separator);
        output("put");
    } else if (ccall->ep->parameter) {                    // f -> l
        output("GALSC_PARAM__");
        prt_ddecl_full_name(ccall->ep->parameter, options);
        output_string(galsc_separator);
        output("put");
    } else {                                              // f -> f
        prt_ddecl_full_name(ccall->ep->function, options);
    }
    
    output("(");

    // f -> f <parameterised>
    if (!ccall->ep->port && !ccall->ep->parameter &&
            ccall->ep->function->gparms) {
        if (ccall->args) {
            // Non-generic calling generic, add arguments.
            prt_expressions(ccall->args, first_arg);
            first_arg = FALSE;
        } else {
            //Generic calling generic, pass arguments through.
            first_arg = prt_arguments(ddecl_get_gparms(c->called), first_arg);
        }
    } else {
        assert(!ccall->args);
    }
    
    if ( (options & psd_galsc_print_port_get_call) ) {    // p ? -> x
        if (!ccall->parameters) {                         // p -> x
          // Port GET: [p -> f], [p -> p]
          // See prt_galsc_ports() in galsc-generate.c
          declarator tdeclarator;
          type_element tmodifiers;
          type2ast(c->r, dummy_location, c->called->type, NULL,
                  &tdeclarator, &tmodifiers);
          prt_simple_declarator(tdeclarator, c->called,
                  psd_galsc_print_port_get_call);
        } else {                                          // (p, l) -> x
            // Port GET + TinyGUYS GET:
            // [(p, l) -> p], [(p, l) -> f], [(p, l) -> l] 
            first_arg = galsc_prt_parameter_get_call(c, ccall, options, called_fd, first_arg);
        } 
    } else {                                              // f ? -> x
        if (!ccall->parameters) {                         // f -> x
            // [f -> p], [f -> f]
            prt_arguments(called_fd->parms, first_arg);
        } else {                                          // (f, l) -> x
            // function + TinyGUYS GET:
            // [(f, l) -> p], [(f, l) -> f], [(f, l) -> f]
            first_arg = galsc_prt_parameter_get_call(c, ccall, options, called_fd, first_arg);
        }
    }
    
    if (calling_combiner)
        output(")");
    
    outputln(");");
}

#else
void prt_ncf_direct_call(struct connections *c,
			 full_connection ccall,
			 bool first_call,
			 psd_options options,
			 type return_type,
			 function_declarator called_fd)
/* Effects: prints call to 'calls' in a connection function.
     Assigns result if last_call is TRUE.
*/
{
  bool first_arg = TRUE;
  data_declaration combiner = type_combiner(return_type);
  bool calling_combiner = FALSE;

  if (!type_void(return_type))
    {
      output("result = ");

      /* Combine w/ the combiner on subsequent calls */
      if (!first_call && combiner)
	{
	  output("%s(result, ", combiner->name);
	  calling_combiner = TRUE;
	}
    }

  prt_ddecl_full_name(ccall->ep->function, options);
  output("(");
  if (ccall->ep->function->gparms)
    {
      if (ccall->args)
	{
	  /* Non-generic calling generic, add arguments */
	  prt_expressions(ccall->args, first_arg);
	  first_arg = FALSE;
	}
      else
	{
	  /* Generic calling generic, pass arguments through */
	  first_arg = prt_arguments(ddecl_get_gparms(c->called), first_arg);
	}
    }
  else
    assert(!ccall->args);

  prt_arguments(called_fd->parms, first_arg);

  if (calling_combiner)
    output(")");

  outputln(");");
}
#endif

void prt_ncf_default_call(struct connections *c,
			  type return_type,
			  function_declarator called_fd)
{
  struct full_connection default_target;
  struct endp default_ep;
#ifdef GALSC
  // FIXME comment
  init_full_connection(&default_target);
  init_endp(&default_ep);
#endif

  default_target.ep = &default_ep;
  default_target.cond = default_target.args = NULL;
  default_ep.function = c->called;

  prt_ncf_direct_call(c, &default_target, TRUE, psd_print_default,
		      return_type, called_fd);
}

bool prt_ncf_direct_calls(struct connections *c,
			  dd_list/*<full_connection>*/ calls,
			  type return_type)
/* Effects: prints calls to 'calls' in a connection function.
*/
{
  dd_list_pos call;
  bool first_call = TRUE;
#ifdef GALSC
  // FIXME comment
  function_declarator called_fd = NULL;
  if (!(c->called->kind == decl_port_ref))
      called_fd = ddecl_get_fdeclarator(c->called);
#else
  function_declarator called_fd = ddecl_get_fdeclarator(c->called);
#endif

  dd_scan (call, calls)
    {
      full_connection ccall = DD_GET(full_connection, call);

      assert(!ccall->cond);
#ifdef GALSC
      // FIXME comment
      if (c->called->kind == decl_port_ref && c->called->in) {
          // Print the call to the get() function.
          prt_ncf_direct_call(c, ccall, first_call, psd_galsc_print_port_get_call, return_type, called_fd);
      } else if (c->called->kind == decl_port_ref && !c->called->in) {
          assert(0);
      } else
#endif
      prt_ncf_direct_call(c, ccall, first_call, 0, return_type, called_fd);

      first_call = FALSE;
    }

  return first_call;
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

#ifdef GALSC
void prt_ncf_conditional_calls(struct connections *c, bool first_call, type return_type)
#else
static void prt_ncf_conditional_calls(struct connections *c, bool first_call, type return_type)
#endif
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
      bool first_cond_call = first_call;

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
	  prt_ncf_direct_call(c, cond_eps[i], first_cond_call, 0,
			      return_type, called_fd);
	  first_cond_call = FALSE;
	  i++;
	}
	
      if (one_arg)
	outputln("break;");
      unindent();
      if (!one_arg)
	outputln("}");
    }
  /* output call to default if there are no non-conditional calls */
  if (first_call)
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

  set_fixed_location(c->called->ast->location);

  prt_ncf_header(c, return_type);

  if (c->called->gparms)
    {
      bool first_call;

      first_call = prt_ncf_direct_calls(c, c->generic_calls, return_type);
      prt_ncf_conditional_calls(c, first_call, return_type);
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

  clear_fixed_location();
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

  /* Make local static variables gloabal when nido is used.
     Note: this raises several issues (aka problems):
     - local static variables are named mod$fn$x rather than just x
       (mildly confusing when debugging)
     - the generated code will have errors (or worse, incorrect 
       behaviour) if the local static declaration relied on local
       declarations inside fn (e.g., typedefs, enums, structs).
       There's no really nice fix to this except renaming and
       extracting all such entities (maybe this can be done when
       we have our own version of gdb and preserving symbol names
       is less important)
   */
  if (use_nido)
    {
      dd_list_pos scan;

      dd_scan (scan, mod->local_statics)
	{
	  data_declaration localsvar = DD_GET(data_declaration, scan);
	  variable_decl localsvd;
	  data_decl localsdd;

	  if (!localsvar->isused)
	    continue;

	  localsvd = CAST(variable_decl, localsvar->ast);
	  localsdd = CAST(data_decl, localsvd->parent);
	  /* Note: we don't print the elements with pte_duplicate as we
	     don't (easily) know here if the elements will be printed
	     several times. If the type elements define a new type we most
	     likely have a problem anyway (see discussion above) */
	  prt_type_elements(localsdd->modifiers, 0);
	  prt_variable_decl(localsvd);
	  outputln(";");
	}
    }
}

#ifdef GALSC
// FIXME fix comment
// This function is first called from galsc_find_connected_functions()
// in galsc-generate.c with "gnode n" set to the node in the connection
// graph corresponding to the called function.
// galsc_find_reachable_functions() follows the call chain of
// functions until it reaches an implementation for the function
// (i.e., the function is in a module).  At this point, it adds the
// implementation ("target") to either the c->generic_calls or
// c->normal_calls list.  When this routine encounters an input port
// as part of the call chain of functions, it adds the connection
// information to c->ports and also adds c to the "dd_list
// connections" list.
#endif

#ifdef GALSC
static bool find_reachable_functions(struct connections *c, gnode n,
				     expression gcond, expression gargs,
        dd_list parameters) // parameters whose target is n
#else
static bool find_reachable_functions(struct connections *c, gnode n,
				     expression gcond, expression gargs)
#endif
{
  endp ep = NODE_GET(endp, n);

#ifdef GALSC
  {
      if (!parameters && c->called->parameters) {
          dd_list_pos pos;
          dd_scan (pos, c->called->parameters) {
              galsc_parameter_connection pconn = DD_GET(galsc_parameter_connection, pos);
              struct endp compare;
              if (lookup_endpoint(pconn->configuration_env, pconn->conn->ep1, &compare)) {
                  if (endp_compare(&compare, ep)) {
                      if (!parameters)
                          parameters = dd_new_list(c->r);
                      dd_add_last(regionof(parameters), parameters, pconn);
                  }
              }
          }
      }
  }
#endif

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
#ifdef GALSC
  else if ((!ep->args && ep->function && ep->function->defined &&
            ep->function->container &&
            is_module(((nesc_declaration)ep->function->container)->impl)) ||
          (ep->parameter && ep->parameter->container &&
          is_application_implementation(((nesc_declaration)ep->parameter->container)->impl))) {
#else
  else if (!ep->args && ep->function->defined &&
	   is_module(((nesc_declaration)ep->function->container)->impl))
#endif
    {
        
#ifdef GALSC
        full_connection target = new_full_connection(c->r, ep, gcond, gargs, parameters);
#else
      full_connection target = new_full_connection(c->r, ep, gcond, gargs);
#endif


      assert(!graph_first_edge_out(n));

      dd_add_last(c->r, 
		  c->called->gparms && !gcond ?
		    c->generic_calls : c->normal_calls, 
		  target);
    }
#ifdef GALSC
  } else if ((ep->actor && ep->port && ep->port->in)) {
            // There is a port connected after c->called.
            full_connection target = new_full_connection(c->r, ep, gcond, gargs, parameters);
            // Add connection information to c->ports.
            //dd_add_last(c->r, c->ports, target);
            // Add c to the global connections list (contains call
            // chains that have ports).
            // FIXME: what if already added the port?
            //dd_add_last(regionof(connections), connections, c);

            dd_add_last(c->r, 
                c->called->gparms && !gcond ?
                c->generic_calls : c->normal_calls, 
                target);
            
            // from galsc_find_function_connections()
            data_declaration decl = ep->port;
            struct connections *connections;
            decl->connections = connections = ralloc(c->r, struct connections);
            connections->r = c->r;
            connections->cg = c->cg;
            connections->called = decl;
            connections->generic_calls = dd_new_list(c->r);
            connections->normal_calls = dd_new_list(c->r);

            // FIXME save parameters
            
            // from below
            gedge out;
            graph_mark_node(n);
            graph_scan_out (out, n) {
                // FIXME null parameters
                if (find_reachable_functions(connections, graph_edge_to(out), NULL, NULL, NULL)) { // FIXME is NULL correct? or should pass gcond, gargs?
                    return TRUE;
                }
            }
            graph_unmark_node(n);
  }
#endif
  else
    {
      gedge out;

      graph_mark_node(n);
      graph_scan_out (out, n)
#ifdef GALSC
          if (find_reachable_functions(c, graph_edge_to(out), gcond, gargs, parameters))
#else
          if (find_reachable_functions(c, graph_edge_to(out), gcond, gargs))
#endif
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
#ifdef GALSC
  if (find_reachable_functions(c, called_fn_node, NULL, NULL, NULL))
#else
  if (find_reachable_functions(c, called_fn_node, NULL, NULL))
#endif
    error_with_location(c->called->ast->location,
			"cycle in configuration (for %s%s%s.%s)",
			c->called->container->name,
			c->called->interface ? "." : "",
			c->called->interface ? c->called->interface->name : "",
			c->called->name);
}

static void combine_warning(struct connections *c)
{
  if (warn_no_combiner)
    {
      /* Warnings to be enabled when result_t gets defined correctly */
      if (c->called->interface)
	nesc_warning("calls to %s.%s in %s are uncombined",
		     c->called->interface->name,
		     c->called->name,
		     c->called->container->name);
      else
	nesc_warning("calls to %s in %s are uncombined",
		     c->called->name, c->called->container->name);
    }
}

static bool combiner_used;

static bool cicn_direct_calls(dd_list/*<full_connection>*/ calls)
{
  dd_list_pos first = dd_first(calls);

  if (dd_is_end(first))
    return TRUE;

  if (!dd_is_end(dd_next(first)))
    combiner_used = TRUE;

  return FALSE;
}

static void cicn_conditional_calls(struct connections *c, bool first_call)
{
  dd_list_pos call;
  int i, j, ncalls = dd_length(c->normal_calls);
  full_connection *cond_eps =
    rarrayalloc(c->r, ncalls, full_connection);

  /* Sort calls so we can find connections with the same conditions */
  i = 0;
  dd_scan (call, c->normal_calls)
    cond_eps[i++] = DD_GET(full_connection, call);
  qsort(cond_eps, ncalls, sizeof(full_connection), condition_compare);

  /* output the calls */
  i = 0;
  while (i < ncalls)
    {
      /* find last target with same condition */
      j = i;
      while (++j < ncalls && condition_compare(&cond_eps[i], &cond_eps[j]) == 0)
	;

      if (i + first_call < j)
	combiner_used = TRUE;
      i = j;
    }
}

static void check_if_combiner_needed(struct connections *c)
{
  /* To see if a combiner is needed, we follow (a simplified form of) the
     logic used in printing the connection function (see
     prt_nesc_connection_function) */
  type return_type = function_return_type(c->called);

  if (type_void(return_type)) /* No combiner needed */
    return;

  combiner_used = FALSE;

  if (c->called->gparms)
    {
      bool first_call;

      first_call = cicn_direct_calls(c->generic_calls);
      cicn_conditional_calls(c, first_call);
    }
  else
    cicn_direct_calls(c->normal_calls);

  if (combiner_used)
    {
      c->combiner = type_combiner(return_type);
      if (!c->combiner)
	combine_warning(c);
    }
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

      check_if_combiner_needed(connections);
    }
}

void find_connections(cgraph cg, nesc_declaration mod)
{
  component_functions_iterate(mod, find_function_connections, cg);
}

static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl,
				    use caller_use);

static void mark_connected_function_list(cgraph cg,
					 data_declaration caller,
					 dd_list/*full_connection*/ calls)
{
  dd_list_pos connected;

  dd_scan (connected, calls)
    {
      full_connection conn = DD_GET(full_connection, connected);

#ifdef GALSC
      data_declaration ddecl = (conn->ep->function) ? (conn->ep->function) :
          ((conn->ep->port) ? (conn->ep->port) : (conn->ep->parameter));
      assert(ddecl);
      mark_reachable_function(cg, caller, ddecl,
              new_use(dummy_location, c_executable | c_fncall));
#else
      mark_reachable_function(cg, caller, conn->ep->function,
			      new_use(dummy_location, c_executable | c_fncall));
#endif
    }
}
static void mark_reachable_function(cgraph cg,
				    data_declaration caller,
				    data_declaration ddecl,
				    use caller_use)
{
  dd_list_pos use;

#ifdef GALSC
  // FIXME
  assert(ddecl);
  if (caller && ((ddecl->kind == decl_function) || (ddecl->kind == decl_port_ref) || (ddecl->kind == decl_variable && ddecl->container && ddecl->container->kind == l_application))) {
      gnode gcaller = (caller->kind == decl_function) ? fn_lookup(cg, caller) : (caller->kind == decl_port_ref ? port_lookup(cg, caller) : parameter_lookup(cg, caller));
      assert(gcaller);
      gnode gddecl = (ddecl->kind == decl_function) ? fn_lookup(cg, ddecl) : ((ddecl->kind == decl_port_ref) ? port_lookup(cg, ddecl) : parameter_lookup(cg, ddecl));
      assert(gddecl);
      graph_add_edge(gcaller, gddecl, caller_use);

  }
#else
  if (caller && ddecl->kind == decl_function)
    graph_add_edge(fn_lookup(cg, caller), fn_lookup(cg, ddecl), caller_use);
#endif

  if (ddecl->isused)
    return;
  ddecl->isused = TRUE;
#ifdef GALSC
  if ((ddecl->kind != decl_port_ref) || (ddecl->kind != decl_variable && ddecl->container && ddecl->container->kind == l_application))
#endif
  if (ddecl->kind != decl_function ||
      (ddecl->container && 
       !(ddecl->container->kind == l_component &&
	 is_module(CAST(component, ddecl->container->ast)->implementation))))
    return;

#ifdef GALSC
  if (((ddecl->ftype == function_command || ddecl->ftype == function_event) &&
      !ddecl->defined) ||
          ((ddecl->kind == decl_port_ref) && (ddecl->ftype == function_implicit))
          || ((ddecl->kind == decl_variable) && (ddecl->ftype == function_implicit) && (ddecl->container && ddecl->container->kind == l_application)))
#else
  if ((ddecl->ftype == function_command || ddecl->ftype == function_event) &&
      !ddecl->defined)
#endif
    {
      struct connections *conn = ddecl->connections;

      /* Call to a command or event not defined in this module.
	 Mark all connected functions */
      mark_connected_function_list(cg, ddecl, conn->generic_calls);
      mark_connected_function_list(cg, ddecl, conn->normal_calls);
      if (conn->combiner)
	mark_reachable_function(cg, ddecl, conn->combiner,
				new_use(dummy_location, c_executable | c_fncall));

      /* Don't process body of suppressed default defs */
      if (ddecl->suppress_definition)
	return;
    }

  /* Make sure ddecl gets a node in the graph even if it doesn't call
     anything */
#ifdef GALSC
  if (ddecl->kind == decl_port_ref) {
      port_lookup(cg, ddecl);
  } else if (ddecl->kind == decl_variable && ddecl->container && ddecl->container->kind == l_application) {
      parameter_lookup(cg, ddecl);
  } else
#endif
  fn_lookup(cg, ddecl);

  if (ddecl->fn_uses)
    dd_scan (use, ddecl->fn_uses)
      {
	iduse i = DD_GET(iduse, use);

	mark_reachable_function(cg, ddecl, i->id, i->u);
      }
}

#ifdef GALSC
cgraph mark_reachable_code(void)
#else
static cgraph mark_reachable_code(void)
#endif
{
  dd_list_pos used;
  cgraph cg = new_cgraph(parse_region);

  /* We use the connection graph type to represent our call graph */

  dd_scan (used, spontaneous_calls)
    mark_reachable_function(cg, NULL, DD_GET(data_declaration, used), NULL);
  dd_scan (used, nglobal_uses)
    mark_reachable_function(cg, NULL, DD_GET(iduse, used)->id, NULL);

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
#ifdef GALSC
  // FIXME comment
  endp ep = NODE_GET(endp, gep);
  fn = (ep->function) ? ep->function : (ep->port ? ep->port : ep->parameter);
  assert(fn);
#else
  
  fn = NODE_GET(endp, gep)->function;
#endif
  
  if (isinlined(fn) || force)
    {
      if (graph_node_markedp(gep))
	return;

      graph_mark_node(gep);

      graph_scan_out (out, gep)
	topological_prt(graph_edge_to(out), FALSE);
#ifdef GALSC
      // FIXME comment
      if (ep->function) {
          prt_nesc_function(fn);
      }
#else
      prt_nesc_function(fn);
#endif
    }
}

#ifdef GALSC
void prt_inline_functions(cgraph callgraph)
#else
static void prt_inline_functions(cgraph callgraph)
#endif
{
  gnode fns;

  graph_clear_all_marks(cgraph_graph(callgraph));
  graph_scan_nodes (fns, cgraph_graph(callgraph))
    {
#ifdef GALSC
        // FIXME comment
        endp ep = NODE_GET(endp, fns);
        data_declaration fn = (ep->function) ? ep->function : (ep->port ? ep->port : ep->parameter);
        assert(fn);
#else
        data_declaration fn = NODE_GET(endp, fns)->function;
#endif
      if (isinlined(fn))
	{
	  gedge callers;
	  bool inlinecallers = FALSE;

	  graph_scan_in (callers, fns)
	    {
#ifdef GALSC
                endp caller_ep = NODE_GET(endp, graph_edge_from(callers));
                data_declaration caller = (caller_ep->function) ? caller_ep->function : (caller_ep->port ? caller_ep->port : caller_ep->parameter);
#else
	      data_declaration caller =
		NODE_GET(endp, graph_edge_from(callers))->function;
#endif

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

#ifdef GALSC
void prt_noninline_functions(cgraph callgraph)
#else
static void prt_noninline_functions(cgraph callgraph)
#endif
{
  gnode fns;
  graph_scan_nodes (fns, cgraph_graph(callgraph))
    {
#ifdef GALSC
        // FIXME comment
        endp ep = NODE_GET(endp, fns);
        data_declaration fn = (ep->function) ? ep->function : (ep->port ? ep->port : ep->parameter);
        assert(fn);
#else
      data_declaration fn = NODE_GET(endp, fns)->function;
#endif

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

#ifdef GALSC
void suppress_function(const char *name)
#else
static void suppress_function(const char *name)
#endif
{
  data_declaration d = lookup_global_id(name);

  if (d && d->kind == decl_function && d->definition)
    d->suppress_definition = TRUE;
}

static void prt_ddecl_for_init(data_declaration ddecl)
{
  prt_plain_ddecl(ddecl, 0);
  output("[__nesc_mote]");
}

static void prt_nido_initializer(variable_decl vd)
{
  data_declaration ddecl = vd->ddecl;
  expression init;

  if (!ddecl || !ddecl->isused || ddecl->kind != decl_variable)
    return; /* Don't print if not referenced */

  init = vd->arg1;

  if (!init)
    {
      output("memset(&");
      prt_ddecl_for_init(ddecl);
      output(", 0, sizeof ");
      prt_ddecl_for_init(ddecl);
      output(")");
    }
  else if (is_init_list(init))
    {
      declarator vtype;
      type_element vmods;

      output("memcpy(&");
      prt_ddecl_for_init(ddecl);

      output(", &");
      type2ast(parse_region, dummy_location, ddecl->type, NULL,
	       &vtype, &vmods);
      output("(");
      prt_declarator(vtype, vmods, NULL, NULL, 0);
      output(")");
      prt_expression(init, P_ASSIGN);

      output(", sizeof ");
      prt_ddecl_for_init(ddecl);
      output(")");
    }
  else 
    {
      prt_ddecl_for_init(ddecl);
      output(" = ");
      prt_expression(init, P_ASSIGN);
    }
  outputln(";");
}

static void prt_nido_initializations(nesc_declaration mod) 
{
  declaration dlist = CAST(module, mod->impl)->decls;
  declaration d;
  dd_list_pos lscan;

  outputln("/* Module %s */", mod->name);

  /* Static variables */
  scan_declaration (d, dlist)
    {
      declaration reald = ignore_extensions(d);
      variable_decl vd;

      if (reald->kind != kind_data_decl)
	continue;

      scan_variable_decl (vd, CAST(variable_decl, CAST(data_decl, d)->decls))
	prt_nido_initializer(vd);
    }

  /* Local static variables */
  dd_scan (lscan, mod->local_statics)
    {
      data_declaration localsd = DD_GET(data_declaration, lscan);

      prt_nido_initializer(CAST(variable_decl, localsd->ast));
    }
  newline();
}

static void prt_nido_initialize(dd_list modules) 
{
  dd_list_pos mod;

  nido_mote_number = "__nesc_mote";
  outputln("/* Invoke static initialisers for mote '__nesc_mote' */\n");
  outputln("static void __nesc_nido_initialise(int __nesc_mote)");
  outputln("{");
  indent();

  dd_scan (mod, modules) 
    prt_nido_initializations(DD_GET(nesc_declaration, mod));
 
  unindent();
  outputln("}");
}

static void prt_nido_resolver(variable_decl vd)
{
  data_declaration ddecl = vd->ddecl;
  expression init;

  if (!ddecl || !ddecl->isused || ddecl->kind != decl_variable)
    return; /* Don't print if not referenced */

  init = vd->arg1;

  output("if (!strcmp(varname, \"");
  prt_plain_ddecl(ddecl, 0);
  outputln("\"))");
  outputln("{");
  indent();
  output("*addr = (uintptr_t)&");
  prt_ddecl_for_init(ddecl);
  outputln(";");
  output("*size = sizeof(");
  prt_ddecl_for_init(ddecl);
  outputln(");");
  outputln("return 0;");
  unindent();
  outputln("}");
}

static void prt_nido_resolvers(nesc_declaration mod) 
{
  declaration dlist = CAST(module, mod->impl)->decls;
  declaration d;

  outputln("/* Module %s */", mod->name);

  /* Static variables */
  scan_declaration (d, dlist)
    {
      declaration reald = ignore_extensions(d);
      variable_decl vd;

      if (reald->kind != kind_data_decl)
	continue;
      
      scan_variable_decl (vd, CAST(variable_decl, CAST(data_decl, d)->decls))
	prt_nido_resolver(vd);
    }

  newline();
}

static void prt_nido_resolver_function(dd_list modules)
{
  dd_list_pos mod;

  outputln("/* Nido variable resolver function */\n");
  outputln("static int __nesc_nido_resolve(int __nesc_mote,");
  outputln("                               char* varname,");
  outputln("                               uintptr_t* addr, size_t* size)");
  outputln("{");
  indent();
  
  dd_scan (mod, modules) 
    prt_nido_resolvers(DD_GET(nesc_declaration, mod));

  outputln("return -1;");
  
  unindent();
  outputln("}");
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

  /* We start by finding each module's identifier uses and connections
     and marking uncallable functions */
  collect_uses(all_cdecls);
  dd_scan (mod, modules)
    {
      nesc_declaration m = DD_GET(nesc_declaration, mod);

      collect_uses(CAST(module, m->impl)->decls);
      find_connections(cg, m);
    }

  /* Then we set the 'isused' bit on all functions that are reachable
     from spontaneous_calls or global_uses */
  callgraph = mark_reachable_code();

  check_async(callgraph);
  check_races(callgraph);

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

  if (use_nido)
    {
      prt_nido_resolver_function(modules);
      disable_line_directives();
      prt_nido_initialize(modules); 
    }

  unparse_end();

  if (output)
    fclose(output);
}

