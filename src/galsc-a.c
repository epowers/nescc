/* This file is part of the galsC compiler.
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
#include "nesc-cg.h"
#include "semantics.h"
#include "nesc-configuration.h"
#include "galsc-a.h"

/* define this to forbid linking a single function from an interface
   independently of the whole interface */
//#define NO_FUNCTION_INTERFACE_MATCHING

// Fill in a "endp" data structure with the correct pointers when
// given an "endpoint", which is a basically list of names (strings).
//
// Based on lookup_endpoint() in nesc-configuration.c.  Added cases
// for decl_port_ref and decl_actor_ref.  Also fixed the
// decl_interface_ref case for NO_FUNCTION_INTERFACE_MATCHING.  We use
// this separate implementation so that we can leave
// NO_FUNCTION_INTERFACE_MATCHING undefined.
// FIXME comment
bool lookup_endpoint(environment configuration_env, endpoint ep,
			    endp lep)
{
  parameterised_identifier pid;
  environment lookup_env = configuration_env;

  init_endp(lep);

  scan_parameterised_identifier (pid, ep->ids)
    {
      const char *idname = pid->word1->cstring.data;
      location l = pid->location;

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
	      if (lookup_env->parent && lookup_env->parent != global_env)
		d = env_lookup(lookup_env->parent->id_env, idname, TRUE);
	      if (!d)
		{
		  error_with_location(l, "cannot find `%s'", idname);
		  return FALSE; /* prevent cascading error messages */
		}
	    }

	  if (args)
	    {
	      if (pid->next)
		error_with_location(l, "arguments must be specified last");
	      lep->args = args;
	    }

	  switch (d->kind)
	    {
	    default:
	      error_with_location(l, "cannot find `%s'", idname);
	      return FALSE; /* prevent cascading error messages */
            case decl_variable:
                // TinyGUYS
                assert(!lep->parameter);
                lep->parameter = d;
                lookup_env = NULL;
                break;
            case decl_actor_ref:
              assert(!lep->actor);
	      lep->actor = d;
	      lookup_env = d->ctype->env;
              break;
            case decl_port_ref:
              assert(!lep->port);
              lep->port = d;
              lookup_env = NULL;
              break;
	    case decl_component_ref:
	      assert(!lep->component);
	      lep->component = d;
	      lookup_env = d->ctype->env;
	      break;
	    case decl_interface_ref:
	      assert(!lep->interface);
	      lep->interface = d;

#ifdef NO_FUNCTION_INTERFACE_MATCHING
	      /* Can't lookup a function inside an interface (no partial interface
		 connections) */
	      lookup_env = NULL;
#else
	      /* Get next environment */
              lookup_env = d->functions;
#endif
	      break;
	    case decl_function:
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
	error_with_location(ep->location, "endpoint is not a parameterised interface");
    }

  return TRUE;
}

// Create a new galsc_parameter_connection (for use with
// {data_declaration}->parameters)
galsc_parameter_connection new_galsc_parameter_connnection(region r) {
    galsc_parameter_connection conn = ralloc(r, struct galsc_parameter_connection);
    conn->conn = NULL;
    conn->configuration_env = NULL;

    return conn;
}

// Find the galsc_parameter_connection in the list "pconns" that
// contains "target".
galsc_parameter_connection get_galsc_parameter_connection(dd_list pconns, endp target, endp source) {
    if (pconns) {
        dd_list_pos pos;
        dd_scan (pos, pconns) {
            galsc_parameter_connection pconn = DD_GET(galsc_parameter_connection, pos);
            struct endp compare;
            if (target) {
                if (lookup_endpoint(pconn->configuration_env, pconn->conn->ep1, &compare)) {
                    if (endp_compare(&compare, target))
                        return pconn;
                }
            } else if (source) {
                if (lookup_endpoint(pconn->configuration_env, pconn->conn->ep2, &compare)) {
                    if (endp_compare(&compare, source))
                              return pconn;
                }
            }
        }
    }
    return NULL;
}
