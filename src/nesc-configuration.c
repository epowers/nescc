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
#include "nesc-configuration.h"
#include "nesc-component.h"
#include "nesc-env.h"
#include "nesc-cg.h"
#include "semantics.h"
#include "nesc-semantics.h"
#include "constants.h"
#include "nesc-generate.h"
#include "c-parse.h"
#include "AST_utils.h"

/* define this to forbid linking a single function from an interface
   independently of the whole interface */
#define NO_FUNCTION_INTERFACE_MATCHING

/* define this to forbid the implicit multiple matches from 
   component -> component connections */
#define NO_COMPONENT_MATCHING

void init_configuration_instance(nesc_configuration_instance cinst, configuration conf) {
  cinst->configuration = conf;
  //cinst->ienv = new_environment(parse_region, conf->ienv, TRUE, FALSE);
  cinst->ienv = new_environment(parse_region, conf->ienv->parent, TRUE, FALSE);
  cinst->instance_number = 
    conf->cdecl->is_abstract?(conf->cdecl->abstract_instance_count++):(-1);
  cinst->crefs = dd_new_list(parse_region);
  current.env = cinst->ienv;
  dd_add_last(parse_region, conf->cdecl->conf_instances, cinst);
}

void component_scan(data_declaration cref, env_scanner *scan)
{
  env_scan(cref->ctype->env->id_env, scan);
}

static void connect_function(cgraph cg, struct endp from, struct endp to)
{
  gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

  fprintf(stderr,"\nMDW: connect_function:\n");
  print_endp("    MDW: from: ", &from);
  print_endp("    MDW: to:   ", &to);

  assert(from.function && to.function);

  graph_add_edge(gfrom, gto, NULL);
  /* If an endpoint has args, we must also connect the node w/o args */
  if (from.args)
    graph_add_edge(fn_lookup(cg, from.function, from.instance), gfrom, NULL);
  if (to.args)
    graph_add_edge(gto, fn_lookup(cg, to.function, to.instance), NULL);
}

static type endpoint_type(endp p)
{
  type t = NULL;

  if (p->args)
    {
      if (p->function)
	t = type_function_return_type(p->function->type);
      else if (p->interface)
	t = p->interface->type;
    }
  else
    {
      if (p->function)
	t = p->function->type;
      else if (p->interface)
	{
	  t = p->interface->type;

	  /* We don't normally include the generic parameters in the 
	     interface's type, but we do here to allow correct matching */
	  if (p->interface->gparms)
	    t = make_generic_type(t, p->interface->gparms);
	}
    }
  return t;
}

static void connect_interface(cgraph cg, struct endp from, struct endp to,
			      bool reverse, bool match_instances)
{
  env_scanner scanfns;
  const char *fnname;
  void *fnentry;

  assert(!from.function && !to.function &&
	 from.interface->itype == to.interface->itype);

  /* All functions */
  interface_scan(to.interface, &scanfns);
  while (env_next(&scanfns, &fnname, &fnentry))
    {
      data_declaration fndecl = fnentry;

      assert(fndecl->kind == decl_function);
      to.function = fndecl;
      from.function = env_lookup(from.interface->functions->id_env, fndecl->name, TRUE);

      if (fndecl->defined ^ reverse)
	connect_function(cg, from, to);
      else
	connect_function(cg, to, from);
    }
}


int match_endpoints(endp p1, endp p2, endp amatch)
{
  /* Should this be type_equal ? unclear 
     (only real diff, given that we will forbid old style parameter lists,
     is transparent union handling) */
  if (type_compatible(endpoint_type(p1), endpoint_type(p2)))
    {
      if (amatch)
	*amatch = *p2;
      return 1;
    }
  else
    return 0;
}

int match_function_interface(bool eqconnection,
			     struct endp f, struct endp i, endp amatch)
{
#ifdef NO_FUNCTION_INTERFACE_MATCHING
  return 0;
#else
  env_scanner scanfns;
  const char *fnname;
  void *fnentry;
  int matched = 0;
  bool want_defined;

  assert(f.function && !i.function);

  want_defined = f.function->defined ^ !eqconnection;

  /* Check all functions */
  interface_scan(i.interface, &scanfns);
  while (env_next(&scanfns, &fnname, &fnentry))
    {
      i.function = fnentry;
      if (i.function->defined == want_defined)
	matched += match_endpoints(&f, &i, amatch); 
    }

  return matched;
#endif
}

int match_interface_component(bool eqconnection,
			      struct endp i, struct endp c, endp amatch)
{
  const char *ifname;
  void *ifentry;
  int matched = 0;
  env_scanner scanifs;
  bool want_required;

  assert(i.interface && !c.interface);

  want_required = i.interface->required ^ !eqconnection;

  component_scan(c.component, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;

      if (idecl->kind == decl_interface_ref)
	{
	  c.interface = idecl;
	  if (c.interface->required == want_required)
	    matched += match_endpoints(&i, &c, amatch);
	}
    }
  return matched;
}

int match_function_component(bool eqconnection,
			     struct endp f, struct endp c, endp amatch)
{
  const char *ifname;
  void *ifentry;
  int matched = 0;
  env_scanner scanifs;
  bool want_defined;

  assert(f.function && !c.interface && !c.function);

  want_defined = f.function->defined ^ !eqconnection;

  component_scan(c.component, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;

      c.function = c.interface = NULL;
      if (idecl->kind == decl_interface_ref)
	{
	  c.interface = idecl;
	  matched += match_function_interface(want_defined ^ idecl->required,
					      f, c, amatch);
	}
      else
	{
	  c.function = idecl;
	  if (c.function->defined == want_defined)
	    matched += match_endpoints(&f, &c, amatch);
	}
    }
  return matched;
}


static void check_generic_arguments(expression args, typelist gparms)
{
  expression arg;
  typelist_scanner scan_gparms;

  typelist_scan(gparms, &scan_gparms);
  scan_expression (arg, args)
    {
      location l = arg->location;
      type gparm_type = typelist_next(&scan_gparms);

      if (!gparm_type)
	{
	  error_with_location(l, "too many arguments");
	  return;
	}
      else 
	{
	  if (!arg->cst || !constant_integral(arg->cst))
	    error_with_location(l, "constant expression expected");
	  else
	    {
	      if (!cval_inrange(arg->cst->cval, gparm_type))
		error_with_location(l, "constant out of range for argument type");
	    }
	}
    }
  if (typelist_next(&scan_gparms))
    error_with_location(args->location, "too few arguments");
}

/* Recursively evaluate the given expression, setting expr->cst to the
 * appropriate constant value.
 */
static void eval_const_expr(declaration parent_aparms, expression expr) {
  fprintf(stderr, "MDW: eval_const_expr expr 0x%lx kind %d %s\n",
      (unsigned long)expr, expr->kind, expr->cst?"CONST":"");

  // Note that 'expr' is shared across all instances of is associated
  // component_ref. Rather than copy 'component_ref->args' for each
  // instance, we just clear out any initializers and overwrite for
  // each instance of 'expr' processed. 
  if (is_lexical_cst(expr)) return;
  expr->cst = NULL; 

  if (is_binary(expr)) {
    binary bin = CAST(binary, expr);
    eval_const_expr(parent_aparms, bin->arg1);
    eval_const_expr(parent_aparms, bin->arg2);
    bin->cst = fold_binary(bin->type, CAST(expression, bin));

  } else if (is_identifier(expr)) {
    /* Only allowed identifiers are in parent_aparms */
    identifier id = CAST(identifier, expr);
    declaration d;
    variable_decl found_vd = NULL;
    scan_declaration(d, parent_aparms) {
      data_decl dd; 
      variable_decl vd;
      assert(d->kind == kind_data_decl);
      dd = CAST(data_decl, d);
      assert(dd->decls->kind == kind_variable_decl);
      vd = CAST(variable_decl, dd->decls);
      if (!strcmp(vd->ddecl->name, id->cstring.data)) {
	found_vd = vd;
	break;
      }
    }
    if (!found_vd) {
      error("cannot find `%s' in abstract parameters");
      return;
    }
    fprintf(stderr,"MDW: eval_const_expr: identifier assigned from vd 0x%lx ('%s')\n", (unsigned long)found_vd, found_vd->ddecl->name);
    expr->cst = found_vd->arg1->cst; 

  } else {
    error_with_location(expr->location, "XXX MDW XXX: eval_const_expr: Cannot handle expr kind %d\n", expr->kind);
    return;
  }

  if (!expr->cst) {
    error("cannot resolve abstract parameter initialization to constant value");
  }
}

static declaration copy_declaration_list(region r, declaration dl) {
  declaration newl = NULL, tail, d;
  data_decl dd, newdd;
  variable_decl vd, newvd;

  scan_declaration(d, dl) {
    assert(d->kind == kind_data_decl);
    dd = CAST(data_decl, d);
    assert(dd->decls->kind == kind_variable_decl);
    vd = CAST(variable_decl, dd->decls);

    newvd = new_variable_decl(r, vd->location, vd->declarator, vd->attributes, vd->arg1, vd->asm_stmt, vd->ddecl);
    newdd = new_data_decl(r, dd->location, dd->modifiers, CAST(declaration, newvd));
    newdd->next = NULL;

    if (newl == NULL) {
      newl = CAST(declaration, newdd);
      tail = newl;
    } else {
      tail->next = CAST(node, newdd);
      tail = CAST(declaration, tail->next);
    }
  }
  return newl;
}

static bool resolve_abstract_parameters(declaration parent_aparms, nesc_configuration_cref cref) {
  expression args = cref->comp->args;
  expression arg;
  declaration aparms = DD_GET(declaration, dd_getindex(cref->comp->cdecl->abs_parms, cref->instance_number));
  declaration aparm;
  data_decl dd;
  variable_decl vd;
  type aparm_type;
  region r = parse_region;

  fprintf(stderr,"MDW: resolve_abstract_parameters: abs_parms length %ld\n", dd_length(cref->comp->cdecl->abs_parms));
  fprintf(stderr,"\nMDW: resolve_abstract_parameters: comp %s instance %d\n", cref->comp->cdecl->name, cref->instance_number);
  fprintf(stderr,"\nMDW: aparms ptr is 0x%lx\n", (unsigned long)aparms);

  // Start head of aparms list, skipping _INSTANCENUM and _NUMINSTANCES
  assert(aparms != NULL && aparms->next != NULL);
  aparm = CAST(declaration, aparms->next->next);
  if (aparm == NULL) {
    error_with_location(args->location, "too few initialization parameters for abstract component");
    return FALSE;
  }

  assert(is_data_decl(aparm));
  dd = CAST(data_decl, aparm);
  vd = CAST(variable_decl, dd->decls);
  aparm_type = vd->ddecl->type;
  fprintf(stderr,"MDW: resolve_abstract_parameters: aparm '%s'\n", vd->ddecl->name);
  if (!aparm_type) {
    error("abstract component does not have _INSTANCENUM variable? This is a bug - contact mdw@cs.berkeley.edu");
    return FALSE;
  }

  scan_expression(arg, args) {
    fprintf(stderr, "MDW: resolve_abstract_parameters: vd '%s'\n", vd->ddecl->name);

    eval_const_expr(parent_aparms, arg);
    if (!arg->cst) {
      error_with_location(arg->location, "constant expression expected");
      return FALSE;
    } 

    if (constant_integral(arg->cst) && !cval_inrange(arg->cst->cval, aparm_type)) {
      error_with_location(arg->location, "constant out of range for argument type");
      return FALSE;
    }

    fprintf(stderr,"MDW: Setting cst for vd 0x%lx ('%s')\n", (unsigned long)vd, vd->ddecl->name);

    if (arg->cst->type == int_type) {
      vd->arg1 = build_int_constant(r, arg->location, 
	  arg->cst->type, cval_sint_value(arg->cst->cval));
    } else if (arg->cst->type == unsigned_int_type) {
      vd->arg1 = build_int_constant(r, arg->location, arg->cst->type, 
	  cval_uint_value(arg->cst->cval));
    } else if (arg->cst->type == float_type ||
	arg->cst->type == double_type) {
      vd->arg1 = build_float_constant(r, arg->location, arg->cst->type, 
	  cval_float_value(arg->cst->cval));
    } else {
      error("MDW: Unable to handle constant type %d\n", arg->cst->type);
    }

    // Next aparms entry
    aparm = CAST(declaration, aparm->next);
    if (aparm == NULL) {
      if (arg->next != NULL) {
	error_with_location(args->location, "too few initialization parameters for abstract component");
	return FALSE;
      }
    } else {
      assert(is_data_decl(aparm));
      dd = CAST(data_decl, aparm);
      vd = CAST(variable_decl, dd->decls);
      aparm_type = vd->ddecl->type;
      fprintf(stderr,"MDW: resolve_abstract_parameters: aparm '%s'\n", vd->ddecl->name);
      if (!aparm_type) {
	error_with_location(arg->location, "too many arguments");
	return FALSE;
      }
    }
  }

  if (aparm) {
    error_with_location(cref->comp->args->location, "too few initialization parameters for abstract component");
    return FALSE;
  }

  fprintf(stderr,"MDW: done with resolve_abstract_parameters: comp %s instance %d\n", cref->comp->cdecl->name, cref->instance_number);
  return TRUE;


}

static bool lookup_endpoint(nesc_configuration_instance cinst, endpoint ep, endp lep)
{
  parameterised_identifier pid;
  environment lookup_env = cinst->ienv;

  lep->component = lep->interface = lep->function = NULL;
  // The default instance is the instance number of the enclosing configuration 
  lep->instance = cinst->instance_number;
  lep->args = NULL;

  scan_parameterised_identifier (pid, ep->ids)
    {
      const char *idname = pid->word1->cstring.data;
      location l = pid->location;

      fprintf(stderr,"MDW: lookup_endpoint %s\n", idname);

      if (!lookup_env)
	error_with_location(l, "unexpected identifier `%s'", idname);
      else
	{
	  expression args = pid->args;
	  data_declaration d = env_lookup(lookup_env->id_env, idname, TRUE);

	  if (!d)
	    {
	      /* This is a bit hacky: lookup in parent env, but not if
		 it's the global env. We want to check a configuration's
		 env, and it's parent component's env, but not the global
		 env. */
	      if (lookup_env->parent && lookup_env->parent != global_env) {
                //fprintf(stderr,"MDW: lookup_endpoint %s in parent\n", idname);
		d = env_lookup(lookup_env->parent->id_env, idname, TRUE);
	      }
	      if (!d)
		{
		  error_with_location(l, "cannot find `%s' (MDW:lookup_endpoint 1)", idname);
		  return FALSE; /* prevent cascading error messages */
		}
	    } 

	  fprintf(stderr,"MDW: lookup_endpoint got decl %s kind %d\n", 
	      d->name, d->kind);

	  if (args)
	    {
	      if (pid->next)
		error_with_location(l, "arguments must be specified last");
	      lep->args = args;
	    }

	  switch (d->kind)
	    {
	    default:
	      error_with_location(l, "cannot find `%s' (MDW:lookup_endpoint 2)", idname);
	      return FALSE; /* prevent cascading error messages */

	    case decl_component_ref:
	      //fprintf(stderr,"MDW: lookup_endpoint decl is component_ref\n");
	      assert(!lep->component);
	      lep->component = d;
	      lep->instance = lep->component->instance_number;
	      lookup_env = d->ctype->env;
	      break;
	    case decl_interface_ref:
	      //fprintf(stderr,"MDW: lookup_endpoint decl is interface_ref\n");
	      assert(!lep->interface);
	      lep->interface = d;

	      // If wiring to a static interface, instance is -1
	      if (d->static_interface) {
		fprintf(stderr,"MDW: lookup_endpoint: interface is static (0x%lx)\n", (unsigned long)d->interface);
		lep->instance = -1;
	      }

#ifdef NO_FUNCTION_INTERFACE_MATCHING
	      /* Can't lookup a function inside an interface (no partial interface
		 connections) */
	      lookup_env = NULL;
#else
	      /* Get next environment */
	      lookup_env = d->itype->decls;
#endif
	      break;
	    case decl_function:
	      //fprintf(stderr,"MDW: lookup_endpoint decl is function\n");
	      lep->function = d;
	      lookup_env = NULL;
	      break;
	    }
	}
    }

  /* Check generic arguments */
  if (lep->args)
    {
      typelist gparms = NULL;

      if (lep->function)
	{
	  type t = lep->function->type;

	  if (type_generic(t))
	    gparms = type_function_arguments(t);
	}
      else if (lep->interface)
	gparms = lep->interface->gparms;

      if (gparms)
	check_generic_arguments(lep->args, gparms);
      else
	error_with_location(ep->location, "endpoint is not generic");
    }

  return TRUE;
}


static void process_interface_connection(cgraph cg, connection conn,
					 struct endp p1, struct endp p2)
{
  location l = conn->location;

  if (is_eq_connection(conn)) /* p1 = p2 */
    {
      if (!p1.component && !p2.component)
	{
	  if (p1.interface->required == p2.interface->required)
	    error_with_location(l, "external to external connections must be between provided and used interfaces");
	  else
	    connect_interface(cg, p1, p2, TRUE, TRUE);
	}
      else
	{
	  if (p1.interface->required != p2.interface->required)
	    error_with_location(l, "external to internal connections must be both provided or both used");
	  else if (!p1.component)
	    connect_interface(cg, p1, p2, FALSE, TRUE);
	  else
	    connect_interface(cg, p2, p1, FALSE, TRUE);
	  /* Note: connect_interface takes care of choosing the right edge
	     direction. There are two cases:
	     - the interface is provided: then we want edges from outside in,
	     so from = the outside interface
	     - the interface is required: then we want edges from inside out,
	     but connect_interface will reverse them because the interface
	     is required. So we also pick from = the outside interface.
	  */
	}
    }
  else /* p1 <- p2 */
    {
      if (p1.interface->required)
	error_with_location(l, "target of '<-' interface must be provided");
      else if (!p2.interface->required)
	error_with_location(l, "source of '<-' interface must be required");
      else connect_interface(cg, p2, p1, FALSE, FALSE);
    }
}

static void process_function_connection(cgraph cg, connection conn,
					struct endp p1, struct endp p2)
{
  location l = conn->location;
  bool p1def = (p1.interface && !p1.interface->required) ^ p1.function->defined;
  bool p2def = (p2.interface && !p2.interface->required) ^ p2.function->defined;

  //print_endp("MDW: process_function_connection: p1: ", &p1);
  //print_endp("MDW: process_function_connection: p2: ", &p2);

  if (is_eq_connection(conn)) /* p1 = p2 */
    {
      if (!p1.component && !p2.component)
	{
	  if (p1def == p2def)
	    error_with_location(l, "external to external connections must be between provided and used functions");
	  else if (p1def)
	    connect_function(cg, p1, p2); /* from provided to used */
	  else
	    connect_function(cg, p2, p1);
	}
      else 
	{
	  if (p1def != p2def)
	    error_with_location(l, "external to internal connections must be both provided or both used");
	  else if ((!p1.component && !p1def) || (p1.component && p1def))
	    connect_function(cg, p2, p1);
	  else
	    connect_function(cg, p1, p2);
	}
    }
  else /* p1 <- p2 */
    {
      if (!p1def)
	error_with_location(l, "target of '<-' function must be defined");
      else if (p2def)
	error_with_location(l, "source of '<-' function must be used");
      else connect_function(cg, p2, p1);
    }
}

static void process_actual_connection(cgraph cg, connection conn,
				      struct endp p1, struct endp p2)
{
  location l = conn->location;

  //print_endp("MDW: process_actual_connection: p1: ", &p1);
  //print_endp("MDW: process_actual_connection: p2: ", &p2);

  if (is_eq_connection(conn)) /* p1 = p2 */
    {
      if (p1.component && p2.component)
	error_with_location(l, "there must be at least one external interface in an '=' connection");
    }
  else /* p1 <- p2 */
    {
      if (!p1.component || !p2.component)
	error_with_location(l, "external interfaces cannot be connected with `<-' or `->'");
    }

  if (p1.function)
    process_function_connection(cg, conn, p1, p2);
  else
    process_interface_connection(cg, conn, p1, p2);
}

static void process_connection(cgraph cg, connection conn,
			       struct endp p1, struct endp p2)
{
  int matches;
  bool eqconnection = is_eq_connection(conn);

  //print_endp("MDW: process_connection: p1: ", &p1);
  //print_endp("MDW: process_connection: p2: ", &p2);

  if (p1.function) /* f X ... */
    {
      if (p2.function) /* f X f */
	matches = match_endpoints(&p1, &p2, NULL);
      else if (p2.interface) /* f X i */
	matches = match_function_interface(eqconnection, p1, p2, &p2);
      else /* f X c */
	matches = match_function_component(eqconnection, p1, p2, &p2);
    }
  else if (p1.interface) /* i X ... */
    {
      if (p2.function) /* i X f */
	matches = match_function_interface(eqconnection, p2, p1, &p1);
      else if (p2.interface) /* i X i */
	matches = match_endpoints(&p1, &p2, NULL);
      else /* i X c */
	matches = match_interface_component(eqconnection, p1, p2, &p2);
    }
  else /* c X ... */
    {
      if (p2.function) /* c X f */
	matches = match_function_component(eqconnection, p2, p1, &p1);
      else /* c X i */
	matches = match_interface_component(eqconnection, p2, p1, &p1);
    }

  if (matches == 0)
    error_with_location(conn->location, "no match");
  else if (matches > 1)
    error_with_location(conn->location, "ambiguous match");
  else 
    process_actual_connection(cg, conn, p1, p2);
}

static void process_component_connection(cgraph cg, connection conn,
					 struct endp p1, struct endp p2)
{
#ifndef NO_COMPONENT_MATCHING
  /* c X c, the only list case */
  const char *ifname;
  void *ifentry;
  int total_matches = 0;
  env_scanner scanifs;
  bool eqconnection = is_eq_connection(conn);

  component_scan(p1.component, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;
      int matches;

      p1.interface = p1.function = p2.interface = p2.function = NULL;
      if (idecl->kind == decl_interface_ref)
	{
	  p1.interface = idecl;
	  matches = match_interface_component(eqconnection, p1, p2, &p2);
	}
      else
	{
	  p1.function = idecl;
	  matches = match_function_component(eqconnection, p1, p2, &p2);
	}

      total_matches += matches;
      if (matches > 1)
	{
	  error_with_location(conn->location, "ambiguous match");
	  break;
	}
      else if (matches == 1)
	process_actual_connection(cg, conn, p1, p2);
    }
  if (total_matches == 0)
#endif
    error_with_location(conn->location, "no match");
}

static void process_connections(configuration c, nesc_configuration_instance cinst)
{
  connection conn;
  struct endp p1, p2;
  cgraph cg = c->cdecl->connections;

  fprintf(stderr, "\nMDW: process_connections for configuration %s\n", c->cdecl->name);

  scan_connection (conn, c->connections)
    if (lookup_endpoint(cinst, conn->ep1, &p1) &&
	lookup_endpoint(cinst, conn->ep2, &p2))
      {
	/* There are a lot of kinds of connections here.
	   lookup_endpoint has already resolved pseudo-interfaces to functions
	   (c is component, i is interface, f is function, X is = or <-)
	   c X c, c X i, i X c, c X f, f X c, i X i, i X f, f X i, f X f

	   We first resolve the c X c case, which can lead to multiple
	   connections, then handle all remaining cases in process_connection
	*/
	//fprintf(stderr,"MDW: Matching\n");
       	print_endp("MDW: process_connections: p1: ", &p1);
	print_endp("MDW: process_connections: p2: ", &p2);

	if (!p1.interface && !p2.interface && !p1.function && !p2.function)
	  process_component_connection(cg, conn, p1, p2);
	else
	  process_connection(cg, conn, p1, p2);
      }
}

static void require_components(region r, configuration c, nesc_configuration_instance cinst)
{
  component_ref comp;
  nesc_declaration cdecl = c->cdecl;
  bool new_conf = (cinst->instance_number < 1);

  if (new_conf) cdecl->connections = new_cgraph(r);

  scan_component_ref (comp, c->components)
    {
      struct data_declaration tempdecl;
      data_declaration old_decl;
      int instance_number;
      const char *cname = comp->word1->cstring.data;
      const char *asname =
	(comp->word2 ? comp->word2 : comp->word1)->cstring.data;
      nesc_configuration_cref cref;

      fprintf(stderr,"MDW: Component %s requires %s\n",
	  cdecl->name, cname);
      comp->cdecl = require(l_component, comp->location, cname);

      if (comp->cdecl->is_abstract) {
	if (is_module(comp->cdecl->impl)) {
	  // Only increment instance count for modules; for configurations
	  // this is done before process_configuration
	  instance_number = comp->cdecl->abstract_instance_count;
	  comp->cdecl->abstract_instance_count++;
	  fprintf(stderr,"MDW: AIC for %s incremented to %d\n",
	      comp->cdecl->name, comp->cdecl->abstract_instance_count);
	} else {
	  // For configurations, the instance number has already been incremented
	  instance_number = comp->cdecl->abstract_instance_count - 1;
	}

	// Append per-instance copy of abs_parms to cdecl->abs_parms
	dd_add_last(r, comp->cdecl->abs_parms, 
	    copy_declaration_list(r, CAST(component, comp->cdecl->ast)->abs_param_list));

      } else {
	instance_number = -1; // Indicates not an abstract component
      }

      init_data_declaration(&tempdecl, CAST(declaration, comp), asname,
			    void_type);
      tempdecl.kind = decl_component_ref;
      tempdecl.ctype = comp->cdecl;
      tempdecl.instance_number = instance_number;
      fprintf(stderr,"MDW: require_components: %s has instance num %d\n",
	  tempdecl.ctype->name, tempdecl.instance_number);

      current.env = cinst->ienv;
      old_decl = lookup_id(asname, TRUE);
      if (!old_decl)
	{
	  current.env = c->ienv->parent;
	  old_decl = lookup_id(asname, TRUE);
	}
      if (old_decl)
	error_with_location(comp->location, "redefinition of `%s'", asname);
      declare(cinst->ienv, &tempdecl, FALSE);

      cref = ralloc(r, struct nesc_configuration_cref);
      cref->comp = comp;
      cref->instance_number = instance_number;
      dd_add_last(r, cinst->crefs, cref);
    }
}

struct cfc_data
{
  location loc;
  cgraph cg;
  data_declaration intf_last_error;
};

/* Check that function fndecl (from the configuration's external interface)
   is connected, i.e.:
   - if defined there is an outgoing edge
   - if used there is an incoming edge
*/
static void check_function_connected(data_declaration fndecl, void *data)
{
  struct cfc_data *d = data;
  gnode epnode;
  data_declaration idecl = fndecl->interface;
  int instance;

  assert(fndecl->kind == decl_function);

  fprintf(stderr,"MDW: check_function_connected: %s interface %s\n",
      fndecl->name, (idecl?idecl->name:"null"));

#ifdef NO_FUNCTION_INTERFACE_MATCHING
  /* Avoid duplicate error messages: if one function not connected in
     an interface, then none are */
  if (idecl == d->intf_last_error)
    return;
#endif

  // Note: container->abstract_instance_count is only incremented by the 
  // configuration *requiring* this configuration, yet we need to ensure 
  // that everything is connected here. So, if we are abstract, we look 
  // for connections to instance 0 (the only instance in existence when 
  // this configuration is being built), assuming that all (future) 
  // instances will be wired identically (which is true as long as
  // we don't introduce conditional wirings).

  if (fndecl->container && fndecl->container->is_abstract) instance = 0;
  else instance = -1;

  epnode = fn_lookup(d->cg, fndecl, instance);

  print_endp("MDW: check_function_connected: ep ", NODE_GET(endp, epnode));

  if ((fndecl->defined && !graph_first_edge_out(epnode)) ||
      (!fndecl->defined && !graph_first_edge_in(epnode)))
  {
    d->intf_last_error = idecl;

    if (idecl)
#ifdef NO_FUNCTION_INTERFACE_MATCHING
      error_with_location(d->loc, "`%s' not connected (MDW:configuration 1)", idecl->name);
#else
    error_with_location(d->loc, "`%s.%s' not connected (MDW:configuration 2)",
	idecl->name, fndecl->name);
#endif
    else
      error_with_location(d->loc, "`%s' not connected (MDW:configuration 3)", fndecl->name);
  }
}

/* Checks that all external interfaces/functions of the configuration
   are connected somewhere in cg */
static void check_complete_connection(configuration c)
{
  struct cfc_data d;

  d.intf_last_error = NULL;
  d.loc = c->location;
  d.cg = c->cdecl->connections;
  component_functions_iterate(c->cdecl, check_function_connected, &d);
}

void process_configuration(configuration c, nesc_configuration_instance cinst)
{
  int old_errorcount = errorcount;

  fprintf(stderr,"MDW: process_configuration for %s instance %d\n", c->cdecl->name, cinst->instance_number);

  fprintf(stderr,"MDW: require_components for %s instance %d\n", c->cdecl->name, cinst->instance_number);

  require_components(parse_region, c, cinst);

  fprintf(stderr,"MDW: process_connections for %s instance %d\n", c->cdecl->name, cinst->instance_number);

  process_connections(c, cinst);

  fprintf(stderr,"MDW: check_complete_connection for %s instance %d\n", c->cdecl->name, cinst->instance_number);

  /* Don't give error messages for missing connections if we found
     errors in the connections (to avoid duplicate errors) */
  if (old_errorcount == errorcount)
    check_complete_connection(c);
}

/* Recurse through component graph and resolve abstract parameters to 
 * constants.
 */
bool process_abstract_params(nesc_configuration_instance cinst) {
  dd_list_pos cr;

  fprintf(stderr,"\nMDW: process_abstract_params for configuration: %s parent_instance %d\n", cinst->configuration->cdecl->name, cinst->instance_number);

  assert(cinst->crefs != NULL);
  dd_scan(cr, cinst->crefs) {
    nesc_configuration_cref cref = DD_GET(nesc_configuration_cref, cr);
    if (cref->comp->cdecl->is_abstract) {
      // If the parent is abstract, get a handle to its abs params
      declaration parent_aparms = NULL;
      if (cinst->configuration->cdecl->is_abstract) {
	parent_aparms = DD_GET(declaration, 
	    dd_getindex(cinst->configuration->cdecl->abs_parms, cinst->instance_number));
	assert(parent_aparms != NULL);
      }

      /* Set _NUMINSTANCES initializer */
      if (cref->instance_number == 0 && is_module(cref->comp->cdecl->impl)) {
	module mod = CAST(module, cref->comp->cdecl->impl);
	declaration dlist = mod->decls, d;
	scan_declaration(d, dlist) {
	  data_decl dd;
	  variable_decl vd;
	  if (d->kind != kind_data_decl) continue;
	  dd = CAST(data_decl, d);
	  vd = CAST(variable_decl, dd->decls);
	  fprintf(stderr,"MDW: process_abstract_params: scanning '%s'\n", vd->ddecl->name);
	  if (!strcmp(vd->ddecl->name, NESC_NUMINSTANCES_LITERAL)) {
	    fprintf(stderr,"MDW: process_abstract_params: Setting _NUMINSTANCES to %d\n", mod->cdecl->abstract_instance_count);
	    vd->arg1 = build_int_constant(parse_region, dd->location,
		int_type, mod->cdecl->abstract_instance_count);
	    break;
	  }
	}
      }

      if (cref->comp->args) {
	if (!resolve_abstract_parameters(parent_aparms, cref)) {
	  return FALSE;
	}
      }

      if (is_configuration(cref->comp->cdecl->impl)) {
   	if (!process_abstract_params(
	    DD_GET(nesc_configuration_instance,
	      dd_getindex(cref->comp->cdecl->conf_instances, cref->instance_number)))) {
	  return FALSE;
	}
      }
    }
  }

  fprintf(stderr,"MDW: process_abstract_params DONE for configuration: %s parent_instance %d\n", cinst->configuration->cdecl->name, cinst->instance_number);
  return TRUE;
}

