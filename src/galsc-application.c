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

/*
   This file is based on:
   - nesc-component.c
   - nesc-configuration.c
   
 */

#include "parser.h"
#include "nesc-configuration.h"
#include "nesc-component.h"
#include "nesc-env.h"
#include "nesc-cg.h"
#include "semantics.h"
#include "c-parse.h"
#include "edit.h"

#include "galsc-a.h"
#include "galsc-application.h"
#include "galsc-env.h"

static const char *galsc_main = NULL;

// Set the name of the "Main" component, which is external to all actors.
void set_galsc_main(const char *name) {
    galsc_main = name;
}

static const char *get_galsc_main() {
    return galsc_main;
}


// Creates a connection graph for the interface of this application
// and loads internal actors and creates the connection graph for
// them.
//
// Called from build() in nesc-semantics.c, which has already parsed
// the file for this application and created an AST for it.
//
// See build_component() in nesc-component.c
void build_application(region r, nesc_declaration cdecl) {
    application the_application = CAST(application, cdecl->ast);

    the_application->implementation->cdecl = cdecl;
    cdecl->impl = the_application->implementation;

    AST_set_parents(CAST(node, cdecl->ast));

    // Build the default connection graph (just nodes for the external
    // endpoints)
    cdecl->connections = build_external_graph(r, cdecl);

    process_application_implementation(r, CAST(application_implementation, cdecl->impl));
}

// Connect the end points in an application level TinyGUYS connection.
// See connect_function() in nesc-configuration.c
static void connect_parameters(cgraph cg, struct endp from, struct endp to) {
    gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

    // GALSC FIXME: we are currently allowing multiple functions to connect
    // to an inport and multiple functions to connect to an outport.
    assert(from.parameter && to.parameter);

    graph_add_edge(gfrom, gto, NULL);
    // If an endpoint has args, we must also connect the node w/o args.
    if (from.args) // GALSC FIXME: didn't check for from.function!
        graph_add_edge(fn_lookup(cg, from.function), gfrom, NULL);
    if (to.args)
        graph_add_edge(gto, fn_lookup(cg, to.function), NULL);
}

// Similar to connect_function() in nesc-configuration.c, except we do
// not check for a function implementation since we are connecting
// ports together.
static void connect_function(cgraph cg, struct endp from, struct endp to) {
    gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

    // GALSC FIXME: we are currently allowing multiple functions to connect
    // to an inport and multiple functions to connect to an outport.
    //assert(from.port && to.port);

    graph_add_edge(gfrom, gto, NULL);
    // If an endpoint has args, we must also connect the node w/o args.
    if (from.args) // GALSC FIXME: didn't check for from.function!
        graph_add_edge(fn_lookup(cg, from.function), gfrom, NULL);
    if (to.args)
        graph_add_edge(gto, fn_lookup(cg, to.function), NULL);
}

// Called from process_interface_connection() in galsc-application.c
//
// Same as connect_interface() in nesc-configuration.c.  Needed
// because connect_interface() calls connect_function(), which has
// been modified to not check for the function implementation.
static void connect_interface(cgraph cg, struct endp from, struct endp to,
			      bool reverse) {
    env_scanner scanfns;
    const char *fnname;
    void *fnentry;
    
    assert(!from.function && !to.function &&
            from.interface->itype == to.interface->itype);

    // All functions
    interface_scan(to.interface, &scanfns);
    while (env_next(&scanfns, &fnname, &fnentry)) {
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

// Same as process_interface_connection() in nesc-configuration.c.
// Needed because a modified connect_function() is called downstream.
//
// Called by process_application_component() in galsc-application.c
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
	    connect_interface(cg, p1, p2, TRUE);
	}
      else
	{
	  if (p1.interface->required != p2.interface->required)
	    error_with_location(l, "external to internal connections must be both provided or both used");
	  else if (!p1.component)
	    connect_interface(cg, p1, p2, FALSE);
	  else
	    connect_interface(cg, p2, p1, FALSE);
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
      else connect_interface(cg, p2, p1, FALSE);
    }
}

// Process a TinyGUYS application level connection.  Make a connection
// from the local parameter name to the global parameter name.
static void process_parameter_connection(cgraph cg, connection conn, struct endp p1, struct endp p2) {
    bool match = FALSE;

    assert(is_eq_connection(conn));

    assert(p1.parameter && p2.parameter);
    match = type_compatible(p1.parameter->type, p2.parameter->type);

    if (match) {
        // From local to global
        if (p1.parameter->container->kind == l_application) {
            if (p2.parameter->container->kind == l_actor) {
                connect_parameters(cg, p2, p1);
            } else {
                error_with_location(conn->location, "Can only connect global TinyGUYS name to a local (actor) TinyGUYS name");
            }
        } else if (p2.parameter->container->kind == l_application) {
            if (p1.parameter->container->kind == l_actor) {
                connect_parameters(cg, p1, p2);
            } else {
                error_with_location(conn->location, "Can only connect global TinyGUYS name to a local (actor) TinyGUYS name");
            }
        } else {
            error_with_location(conn->location, "Invalid TinyGUYS connection");
        }
    } else {
        error_with_location(conn->location, "TinyGUYS parameter types must match");
    }
}

// Check the port directions in a port connection and make the
// connection.  Full type checking is deferred until the global level.
//
// Called from process_connections() in galsc-application.c
//
// See process_connection() in nesc-configuration.c
static void process_port_connection(cgraph cg, global_connection conn,
        struct endp p1, struct endp p2) {
    // target X source
    // p1 <- p2
    assert(p1.port && p2.port);

    if (p1.port->in && !p2.port->in) {
        p1.port->portsize_definition = conn->args;
        connect_function(cg, p2, p1);
    } else {
        error_with_location(conn->location, "For a '<=' connection, source must be an 'out' port and target must be an 'in' port");
    }
}

// Similar to process_connections() in nesc-configuration.c
//
// Make a graph for the connections between actors (port to port
// connections) in this application.
static void process_connections(application_implementation c) {
    connection conn;
    struct endp p1, p2;
    cgraph cg = c->cdecl->connections;

    scan_connection (conn, c->connections) {
        if (lookup_endpoint(c->ienv, conn->ep1, &p1) &&
                lookup_endpoint(c->ienv, conn->ep2, &p2)) {

            if (p1.port && p2.port) { // port2 => port1
                process_port_connection(cg, CAST(global_connection, conn), p1, p2);
            } else if (p1.parameter && p2.parameter) { // param2 = param1
                process_parameter_connection(cg, conn, p1, p2);
            } else {
                error_with_location(conn->location, "application should only connect (ports with ports) or (global parameter names with local parameter names)");
            }
        }
    }
}

// Make connections from external component x to internal actor a for
// all matching interfaces.
//
// Example: Main.StdControl -> Actor.StdControl
static void process_application_component(region r, application_implementation app) {
    cgraph cg = app->cdecl->connections;
    component_ref comp;

    // List of new connections between x.y and a.y'
    connection connections = NULL;
    connection conn;
                                
    // Look at each external component x.
    scan_component_ref(comp, app->external_components) {
        // from component_functions_iterate()
        nesc_declaration c1 = comp->cdecl;
        const char *ifname1;
        void *ifentry1;
        env_scanner scanifs1;

        // Look at each interface y of x (x.y)
        env_scan(c1->env->id_env, &scanifs1);
        while (env_next(&scanifs1, &ifname1, &ifentry1)) {
            data_declaration idecl1 = ifentry1;
            
            // Found interface y
            if (idecl1->kind == decl_interface_ref) {
                actor_ref actor;
                
                // Look at each internal actor a
                scan_actor_ref (actor, app->actors) {
                    nesc_declaration c2 = actor->cdecl;
                    const char *ifname2;
                    void *ifentry2;
                    env_scanner scanifs2;

                    // Look at each interface y' of a (a.y')
                    env_scan(c2->env->id_env, &scanifs2);
                    while (env_next(&scanifs2, &ifname2, &ifentry2)) {
                        data_declaration idecl2 = ifentry2;

                        // Found interface y'
                        if (idecl2->kind == decl_interface_ref) {
                            // Match interfaces y and y'
                            if (!strcmp(idecl1->name, idecl2->name)){
                                word word1, word2;
                                parameterised_identifier pid1, pid2, temppid;
                                endpoint ep1, ep2;

                                // Make new endpoint for (comp.y)
                                word1 = CAST(interface_ref, idecl1->ast)->word1;
                                pid1 = new_parameterised_identifier(r, dummy_location, word1, NULL);
                                word1 = comp->word1;
                                temppid = new_parameterised_identifier(r, dummy_location, word1, NULL);
                                pid1 = parameterised_identifier_chain(temppid, pid1);
                                ep1 = new_endpoint(r, dummy_location, pid1);

                                // Make new endpoint for (actor.y')
                                word2 = CAST(interface_ref, idecl2->ast)->word1;
                                pid2 = new_parameterised_identifier(r, dummy_location, word2, NULL);
                                word2 = actor->word1;
                                temppid = new_parameterised_identifier(r, dummy_location, word2, NULL);
                                
                                pid2 = parameterised_identifier_chain(temppid, pid2);
                                ep2 = new_endpoint(r, dummy_location, pid2);

                                // Make a new connection (y -> y')
                                connections = connection_chain(CAST(connection, new_rp_connection(r, dummy_location, ep2, ep1)), connections);
                            }
                        }
                    }
                }
            }
        }
    }

    // Connect the endpoints
    // From process_connections()
    scan_connection (conn, connections) {
        struct endp p1, p2;

        if (lookup_endpoint(app->ienv, conn->ep1, &p1) &&
                lookup_endpoint(app->ienv, conn->ep2, &p2)) {
            process_interface_connection(cg, conn, p1, p2);
        }
    }
}

// Same as require_components() in nesc-configuration.c
//
// Loads the actors in this application.  This function is called from
// process_application_implementation() in galsc-application.c.
static void require_actors(region r, application_implementation c) {
    actor_ref actor;
    nesc_declaration cdecl = c->cdecl;

    cdecl->connections = new_cgraph(r);
  
    scan_actor_ref (actor, c->actors) {
        struct data_declaration tempdecl;
        data_declaration old_decl;
        const char *cname = actor->word1->cstring.data;
        const char *asname =
            (actor->word2 ? actor->word2 : actor->word1)->cstring.data;

        actor->cdecl = galsc_require(l_actor, actor->location, cname);
        
        init_data_declaration(&tempdecl, CAST(declaration, actor), asname,
                void_type);
        tempdecl.kind = decl_actor_ref;
        tempdecl.ctype = actor->cdecl;
        
        current.env = c->ienv;
        old_decl = lookup_id(asname, TRUE);
        if (!old_decl) {
            current.env = c->ienv->parent;
            old_decl = lookup_id(asname, TRUE);
        }
        if (old_decl) {
            error_with_location(actor->location, "redefinition of `%s'", asname);
        }
        declare(c->ienv, &tempdecl, FALSE);
    }
}

// Load the "Main" component into the application.
static void require_application_component(region r,
        application_implementation c) {
    const char *name = get_galsc_main();
    assert(name);
    component_ref comp = new_component_ref(r, dummy_location, build_word(r, name), NULL);
  
    // From require_actors()
    struct data_declaration tempdecl;
    data_declaration old_decl;
    const char *cname = comp->word1->cstring.data;
    const char *asname =
	(comp->word2 ? comp->word2 : comp->word1)->cstring.data;
    
    comp->cdecl = require(l_component, comp->location, cname);
    
    init_data_declaration(&tempdecl, CAST(declaration, comp), asname,
            void_type);
    tempdecl.kind = decl_component_ref;
    tempdecl.ctype = comp->cdecl;
    
    current.env = c->ienv;
    old_decl = lookup_id(asname, TRUE);
    if (!old_decl) {
        current.env = c->ienv->parent;
        old_decl = lookup_id(asname, TRUE);
    }
    if (old_decl) {
	error_with_location(comp->location, "redefinition of `%s'", asname);
    }
    declare(c->ienv, &tempdecl, FALSE);

    // Add the Main component to the list of components external to any actors.
    c->external_components = comp;
}

// Loads the actors inside this application, and also loads the "Main"
// component.  Creates the connection graph between the ports of
// actors, then connects the interfaces of Main to the interfaces of
// the actors.
void process_application_implementation(region r, application_implementation c)
{
    require_actors(parse_region, c);
    
    // Load the "Main" component into the application.
    require_application_component(parse_region, c);
    
    process_connections(c);
    
    process_application_component(r, c);
    
    // Don't need to run check_complete_connections() because there
    // are no external connections in an application.
    // FIXME is this still true with TinyGUYS?
}
