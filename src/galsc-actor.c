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

#include "galsc-a.h"
#include "galsc-actor.h"
#include "galsc-types.h"

/* define this to forbid linking a single function from an interface
   independently of the whole interface */
//#define NO_FUNCTION_INTERFACE_MATCHING

// Creates a reference to a port (only once for each port).  This is
// called from the parser (see c-parse.y).
//
// See decls.h for information on "struct data_declaration".
//
// See declare_interface_ref() in nesc-component.c
void declare_port_ref(port_ref iref, declaration gparms,
			   environment genv) {
    const char *iname = (iref->word2 ? iref->word2 : iref->word1)->cstring.data;
    struct data_declaration tempdecl;
    data_declaration old_decl, ddecl;
    
    init_data_declaration(&tempdecl, CAST(declaration, iref), iname, void_type);
    tempdecl.kind = decl_port_ref;
    tempdecl.type = NULL;
    tempdecl.in = current.actor_in;
    tempdecl.gparms = gparms ? make_gparm_typelist(gparms) : NULL;

    old_decl = lookup_id(iname, TRUE);
    if (old_decl)
        error("redefinition of `%s'", iname);
    ddecl = declare(current.env, &tempdecl, FALSE);
}

// Connect the end points for a TinyGUYS GET or GET/PUT connection.
//
// See connect_function() in nesc-configuration.c
static void connect_parameter_get(cgraph cg, struct endp from, struct endp to) {
    gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

    // FIXME should we just use connect_function with no assert?
    
    graph_add_edge(gfrom, gto, NULL);

    // FIXME : is fn_lookup ok?
    /* If an endpoint has args, we must also connect the node w/o args */
    if (from.args)
        graph_add_edge(fn_lookup(cg, from.function), gfrom, NULL);
    if (to.args)
        graph_add_edge(gto, fn_lookup(cg, to.function), NULL);
}

// Connect the end points for a TinyGUYS PUT connection.
//
// See connect_function() in nesc-configuration.c
static void connect_parameter_put(cgraph cg, struct endp from, struct endp to) {
    gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

    // Check that [(from.port ^ from.function) && to.parameter]
    assert(((from.port != 0) ^ (from.function != 0)) && (to.parameter));
    
    graph_add_edge(gfrom, gto, NULL);

    // FIXME : is fn_lookup ok?
    /* If an endpoint has args, we must also connect the node w/o args */
    if (from.args)
        graph_add_edge(fn_lookup(cg, from.function), gfrom, NULL);
    if (to.args)
        graph_add_edge(gto, fn_lookup(cg, to.function), NULL);
}

// Make the connection between a function and a port in the graph.
//
// See connect_function() in nesc-configuration.c
static void connect_function(cgraph cg, struct endp from, struct endp to) {
    gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

    assert((from.port && to.function) || (from.function && to.port));
    
    graph_add_edge(gfrom, gto, NULL);
    // FIXME is fn_lookup ok?
    /* If an endpoint has args, we must also connect the node w/o args */
    if (from.args)
        graph_add_edge(fn_lookup(cg, from.function), gfrom, NULL);
    if (to.args)
        graph_add_edge(gto, fn_lookup(cg, to.function), NULL);
}

// Make the connection between a port and a port in the graph.
//
// See connect_function() in nesc-configuration.c
static void connect_port(cgraph cg, struct endp from, struct endp to) {
    gnode gfrom = endpoint_lookup(cg, &from), gto = endpoint_lookup(cg, &to);

    assert((from.port && to.port));
    
    graph_add_edge(gfrom, gto, NULL);
    
    // FIXME : is fn_lookup ok?
    /* If an endpoint has args, we must also connect the node w/o args */
    if (from.args)
        graph_add_edge(fn_lookup(cg, from.function), gfrom, NULL);
    if (to.args)
        graph_add_edge(gto, fn_lookup(cg, to.function), NULL);
}

// Extract the source endp for a TinyGUYS GET connection.  The
// "endpoint_list" is the list of "endpoint" corresponding to the
// source of the connection to check.  The "configuration_env" is the
// environment of the connection.  This function will fill "p" with
// the following:
//
//   1. - A port x (and its args), or
//      - A function x (and its component, interface, and args)
//   2. p.x.ep and p.x.configuration_env will contain the endpoint_list
//      and environment of the connection that contains this parameter.
static void extract_parameter_get_connection_trigger(connection conn, environment configuration_env, endp p) {
    endpoint endpoint_list = conn->ep2;
    
    endpoint ep;
    struct endp temp;
    int parameter_count = 0;
    int port_count = 0;
    int function_count = 0;

    // Find the trigger port or function in the endpoint_list.
    scan_endpoint (ep, endpoint_list) {
        if (lookup_endpoint(configuration_env, ep, &temp)) {
            if (temp.parameter) {
                parameter_count++;
            } else if (temp.port) {
                p->port = temp.port;
                p->args = temp.args;
                port_count++;
            } else if (temp.function) {
                p->component = temp.component;
                p->interface = temp.interface;
                p->function = temp.function;
                p->args = temp.args;
                function_count++;
            } else {
                assert(0);
            }
        }
    }

    // Make sure only there is only one trigger (either one port or
    // one function).
    if ((port_count > 1) ||
            (function_count > 1) ||
            (p->port && p->function)) {
        error_with_location(conn->location, "more than one trigger for this connection");
    } else {
        assert(parameter_count > 0);

        data_declaration trigger = (p->function) ? p->function : p->port;
        if (!trigger) {
            error_with_location(conn->location, "missing trigger");
        } else {
            if (!trigger->parameters) {
                trigger->parameters = dd_new_list(regionof(trigger));
            }

            // Save the connection in the trigger so that we know the
            // order of the arguments that should be passed to the target.
            galsc_parameter_connection pconn = new_galsc_parameter_connnection(parse_region);
            pconn->conn = conn;
            pconn->configuration_env = configuration_env;
            dd_add_last(regionof(trigger->parameters), trigger->parameters, pconn);
        }
    }
}

// Check port directions for the following type of connection:
// [l X p], [l X (p, l)]
static bool match_parameter_port(struct endp parameter, struct endp port) {
    assert(parameter.parameter && port.port);
    assert(!parameter.function && !parameter.port);
    assert(!port.function);

    // Just check port direction.  Type checking is done at global level.
    if (port.port->in) {
        return TRUE;
    }
    return FALSE;
}

// Check port directions for the following type of connection:
// [p X p], [p X (p, l)]
static bool match_port_port(struct endp port_target, struct endp port_source) {
    assert(port_target.port && port_source.port);
    assert(!port_target.function && !port_target.parameter);
    assert(!port_source.function);

    // Just check port directions.  Type checking is done at global level.
    if (!port_target.port->in && port_source.port->in) {
        return TRUE;
    }
    return FALSE;
}

// Check port directions for the following type of connection:
// [p X f], [p X (f, l)]
static bool match_port_function(struct endp port, struct endp function) {
    assert(port.port && function.function);
    assert(!port.function && !port.parameter);
    assert(!function.port);

    // Just check port direction.  Type checking is done at global level.
    if (!port.port->in) {
        return TRUE;
    }
    return FALSE;
}

// Check port directions for the following type of connection:
// [f X p], [f X (p, l)]
static bool match_function_port(struct endp function, struct endp port) {
    assert(function.function && port.port);
    assert(!function.port && !function.parameter);
    assert(!port.function);
    
    // Just check port directions.  Type checking is done at global level.
    if (port.port->in) {
        return TRUE;
    }
    return FALSE;
}

// Check a TinyGUYS GET or GET/PUT connection [(x,l) -> x].  If the
// connection contains no ports, we can do full type checking.
// However, if the connection contains ports, only check the port
// directions.  Full type checking is deferred until the global level.
//
// The connection has the following format:
// conn->ep1 X conn->ep2, where X is <-
// Connection formats allowed: l|p|f X (p|f) (l)*
static void process_parameter_get_connection(cgraph cg, connection conn,
        actor_implementation c) {
    bool match = FALSE;
    
    // The target ep1 should have only one element.
    assert(chain_length(CAST(node, conn->ep1)) == 1);

    // Get the target endp.
    struct endp p1;
    if (lookup_endpoint(c->ienv, conn->ep1, &p1)) {
        // Get the source (trigger) endp.
        struct endp p2;
        init_endp(&p2);
        extract_parameter_get_connection_trigger(conn, c->ienv, &p2);

        if (p1.parameter) {       // TinyGUYS GET and PUT
            if (p2.port) {
                match = match_parameter_port(p1, p2);    // l X (p, l)
            } else if (p2.function) {
                match = match_parameter_getput(&p1, &p2);// l X (f, l)
            }
        } else if (p1.port) {     // TinyGUYS GET
            if (p2.port) {
                match = match_port_port(p1, p2);         // p X (p, l)
            } else if (p2.function) {
                match = match_port_function(p1, p2);     // p X (f, l)
            }
        } else if (p1.function) { // TinyGUYS GET
            if (p2.port) {
                match = match_function_port(p1, p2);     // f X (p, l)
            } else if (p2.function) {
                match = match_parameter_get(&p1, &p2);   // f X (f, l)
            }
        }
        if (match) {
            connect_parameter_get(cg, p2, p1);
        } else {
            error_with_location(conn->location, "TinyGUYS GET connection must be of the form: [(trigger, parameter) -> target], where trigger is a port or a function, and target is either a port, function, or parameter.");
        }
    }
}

// Check a TinyGUYS PUT connection.  If the connection contains no
// ports, we can do full type checking.  However, if the connection
// contains ports, only check the port directions.  Full type checking
// is deferred until the global level.  Connections have the following
// format:
//     p1 X p2, where X is <-
static void process_parameter_put_connection(cgraph cg, connection conn,
        struct endp p1, struct endp p2) {
    bool match = FALSE;
    
    if (p2.port) {
        match = match_parameter_port(p1, p2);          // l X p
    } else if (p2.function) {
        match = match_parameter_put(&p1, &p2);         // l X f
    }

    if (match) {
        connect_parameter_put(cg, p2, p1);
    } else {
        error_with_location(conn->location, "TinyGUYS PUT connection must be of the form: [port -> parameter] or [function -> parameter].");
    }
}

// Check the port directions in a port connection and make the
// connection.  Full type checking is deferred until the global level.
// Connections have the following format:
//     target X source, p1 <- p2
//
// Called from process_connections() in galsc-actor.c
//
// See process_connection() in nesc-configuration.c
static void process_port_connection(cgraph cg, connection conn,
        struct endp p1, struct endp p2) {
    assert(p1.port || p2.port);

    bool matches = FALSE;
    if (p1.port && p2.function) {        // p X f
        if ((matches = match_port_function(p1, p2))) {
            connect_function(cg, p2, p1);
        }
    } else if (p2.port && p1.function) { // f X p
        if ((matches = match_function_port(p1, p2))) {
            connect_function(cg, p2, p1);
        }
    } else if (p1.port && p2.port) {     // p X p
        if ((matches = match_port_port(p1, p2))) {
            connect_port(cg, p2, p1);
        }
    }

    if (!matches) {
        error_with_location(conn->location, "port directions in this connection are incorrect");
    }
}

// Similar to process_connections() in nesc-configuration.c
//
// Make a graph for the connections between components in this actor.
// Also include in the graph the connections between components and
// the ports of this actor.
//
// Called from process_actor_implementation() in galsc-actor.c
static void process_connections(actor_implementation c) {
    connection conn;
    struct endp p1, p2;
    cgraph cg = c->cdecl->connections;

    scan_connection (conn, c->connections) {
        // The connection is of the form (conn->ep1 X conn->ep2),
        // where X is '<-'.

        // The connection is of the form:
        // '(' endpoint_list ')' POINTSAT endpoint ';'
        if (is_parameter_get_connection(conn)) {
            // TinyGUYS GET or GET/PUT connections [(x, l) -> x]
            // 
            // GET
            // p X (p, l)
            // p X (f, l)
            // f X (p, l)
            // f X (f, l)
            //
            // GET/PUT
            // l X (p, l)
            // l X (f, l)
            process_parameter_get_connection(cg, conn, c);
        } else if (lookup_endpoint(c->ienv, conn->ep1, &p1) &&
                lookup_endpoint(c->ienv, conn->ep2, &p2)) {
            /* There are a lot of kinds of connections here.
               lookup_endpoint has already resolved pseudo-interfaces
               to functions.

               (c is component, i is interface, f is function, X is = or <-)
               c X c, c X i, i X c, c X f, f X c, i X i, i X f, f X i, f X f

               We also have:
               ( p is port, X is <- )
               p X f, f X p, p X p

               // TinyGUYS
               We also have:
               l X p, l X f
               
               We first resolve the case where one of the endpoints is
               a parameter.  Then we resolve the case where one of the
               endpoints is a port.
               
               We first resolve the c X c case, which can lead to
               multiple connections, then handle all remaining cases
               in process_connection.
             */
            if (p1.parameter || p2.parameter) {
                // TinyGUYS PUT connections
                // l X p, l X f
                // (p X l, f X l is invalid) 
                process_parameter_put_connection(cg, conn, p1, p2);
            } else if (p1.port || p2.port) {
                // For either of the cases (p X f, f X p, p X p)
                process_port_connection(cg, conn, p1, p2);
            } else {
                // For the remaining cases

                if (!p1.interface && !p2.interface &&
                        !p1.function && !p2.function)
                    process_component_connection(cg, conn, p1, p2);
                else
                    process_connection(cg, conn, p1, p2);
            }
        }
    }
}

// Process the entries in the actorControl list in the implementation
// body c of this actor.  The main purpose of this list is so that
// internal components that have a StdControl interface can be
// initialized by the Main component.
//
// For each entry "x.y;", where x is an internal component and y is an
// interface of that component, add y as an interface of this actor.
//
// Then add a node for each function in the interface (what
// build_external_graph() would normally do).
//
// Finally, connect the new nodes to the function interface nodes of
// each internal component x (what process_connections() would normally do).
static void process_actorcontrollist(region r, actor_implementation c) {
    connection conn;
    struct endp p1, p2;
    nesc_declaration cdecl = c->cdecl;
    cgraph cg = c->cdecl->connections;

    endpoint ep1, ep2;

    // If actorControl section does not exist or contains no entries, return.
    if (!c->actorcontrollist)
        return;
  
    // Step through each entry "x.y" in the actorControl section, and
    // add interface x to the interface of this actor.
    scan_endpoint (ep1, c->actorcontrollist) {
        interface_ref iref, newiref;
        const char *iname;

        // Get the component and interface name (x.y) in this entry.
        lookup_endpoint(c->ienv, ep1, &p1);
        
        // GALSC FIXME: function names not allowed in entry.
        assert(p1.component && p1.interface && !p1.function);

        // The new interface should be "provides interface y;"
        current.component_requires = FALSE;
        // Set environment to that of actor (not actor_implementation).
        current.env = c->ienv->parent;

        // Get the name (y) of the interface.
        iref = CAST(interface_ref, p1.interface->ast);
        iname = (iref->word2 ? iref->word2 : iref->word1)->cstring.data;

        // Make a new interface_ref and declare the interface (y') if it
        // has not yet been added to the interface of this actor.
        if (!lookup_id(iname, TRUE)) {
            // Make a new interface_ref for the actor.  The old
            // interface_ref is for the connection.
            newiref = new_interface_ref(r, dummy_location, iref->word1, iref->word2, iref->gparms); 
            declare_interface_ref(newiref, NULL, NULL);
        }
    }
  
    // Performs the same function as build_external_graph() for
    // the interfaces just declared.
    cgraph newcg = new_cgraph(r);
    struct beg_data d;
    
    d.cg = newcg;
    component_functions_iterate(cdecl, beg_iterator, &d);

    // Connect actor interface (y') to internal component interface (x.y).
    scan_endpoint (ep1, c->actorcontrollist) {
        parameterised_identifier pid;
        word word1;
        
        // Get the component and interface name (x.y) in this entry.
        lookup_endpoint(c->ienv, ep1, &p1);

        init_endp(&p2);

        // p1 now has the pointers to x.y
        // We need to find p2 pointer to y' (actor interface declared above). 
        //
        // Based on component_functions_iterate()
        nesc_declaration c = cdecl;
        const char *ifname;
        void *ifentry;
        env_scanner scanifs;
        
        env_scan(c->env->id_env, &scanifs);
        while (env_next(&scanifs, &ifname, &ifentry)) {
            data_declaration idecl = ifentry;
            
            if (idecl->kind == decl_interface_ref) {
                if (!strcmp(idecl->name, p1.interface->name)) {
                    p2.interface = idecl;
                    break;
                }
            }
        }

        assert(p2.interface);

        // Make a new connection and endpoint (endp).
        // y' (p2) = x.y (p1)
        
        // GALSC FIXME: what about args to interface?
        word1 = CAST(interface_ref, p2.interface->ast)->word1;
        pid = new_parameterised_identifier(r, dummy_location, word1, NULL);
        ep2 = new_endpoint(r, dummy_location, pid);
        conn = CAST(connection, new_eq_connection(r, dummy_location, ep2, ep1));

        // Connect the endpoints.
        process_connection(cg, conn, p1, p2);
    }
}

// Body of function is same as require_components() in
// nesc-configuration.c
//
// Loads the components in this actor.  This function is called from
// process_actor_implementation() in galsc-actor.c.
static void require_components(region r, actor_implementation c) {
    component_ref comp;
    nesc_declaration cdecl = c->cdecl;

    cdecl->connections = new_cgraph(r);
  
    scan_component_ref (comp, c->components) {
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
        if (old_decl)
            error_with_location(comp->location, "redefinition of `%s'", asname);
        declare(c->ienv, &tempdecl, FALSE);
    }
}

// Data structure used to transfer information to
// check_function_connected(), used with component_functions_iterate()
struct cfc_data {
    location loc;
    cgraph cg;           // The graph to check for connections.
    data_declaration intf_last_error;
};

// Check that all ports and functions in actor interface are connected.
// If this is a port, then
// - If outport, there is an incoming edge
// - If inport, there is an outgoing edge
// Else
//   Check that function fndecl (from the configuration's external interface)
//   is connected, i.e.:
//   - if defined there is an outgoing edge
//   - if used there is an incoming edge
//
// Called from check_complete_connection()
//
// See check_function_connected() in nesc-configuration.c
static void check_function_connected(data_declaration fndecl, void *data) {
    struct cfc_data *d = data;
    gnode epnode;
    data_declaration idecl = fndecl->interface;

#ifdef NO_FUNCTION_INTERFACE_MATCHING
    // Avoid duplicate error messages: if one function not connected
    // in an interface, then none are.
    if (idecl == d->intf_last_error)
        return;
#endif

    if (fndecl->kind == decl_function) {        // function
        epnode = fn_lookup(d->cg, fndecl);
    } else if (fndecl->kind == decl_port_ref) { // port
        epnode = port_lookup(d->cg, fndecl);
    } else if (fndecl->kind == decl_variable) { // parameter
        // Parameter need not be connected to anything.
        epnode = parameter_lookup(d->cg, fndecl);
    } else {
        assert(0);
    }

    if (((fndecl->kind == decl_port_ref) &&
                (((!fndecl->in && !graph_first_edge_in(epnode)) ||
                        (fndecl->in && !graph_first_edge_out(epnode))))) ||
            ((fndecl->kind == decl_function) &&
                    ((fndecl->defined && !graph_first_edge_out(epnode)) ||
                            (!fndecl->defined && !graph_first_edge_in(epnode))))) {
        d->intf_last_error = idecl;

        if (idecl) {
#ifdef NO_FUNCTION_INTERFACE_MATCHING
            error_with_location(d->loc, "`%s' not connected", idecl->name);
#else
            error_with_location(d->loc, "`%s.%s' not connected",
                    idecl->name, fndecl->name);
#endif
        } else {
            error_with_location(d->loc, "`%s' not connected", fndecl->name);
        }
    }
}

// Checks that all external interfaces/functions of the configuration
// are connected somewhere in cg.
//
// See check_complete_connection() in nesc-configuration.c
static void check_complete_connection(actor_implementation c)
{
  struct cfc_data d;

  d.intf_last_error = NULL;
  d.loc = c->location;
  d.cg = c->cdecl->connections;
  component_functions_iterate(c->cdecl, check_function_connected, &d);
}

// Loads the components inside this actor, then creates the connection
// graph between the components and between the components and the
// ports of the actor.  Also processes the actorControl section and
// creates interfaces for the actor.  Checks to make sure all ports
// and interfaces of the actor are connected to a function of an
// internal component.
static void process_actor_implementation(region r, actor_implementation c) {
    int old_errorcount = errorcount;

    require_components(parse_region, c);
    process_connections(c);

    process_actorcontrollist(r, c);
  
    // Don't give error messages for missing connections if we found
    // errors in the connections (to avoid duplicate errors).
    if (old_errorcount == errorcount)
        check_complete_connection(c);
}

// Creates a connection graph for the interface of this actor and
// loads in internal components and creates the connection graph for
// them.
//
// Called from build() in nesc-semantics.c, which has already parsed
// the file for this actor and created an AST for it.
//
// See build_component() in nesc-component.c
void build_actor(region r, nesc_declaration cdecl) {
    actor the_actor = CAST(actor, cdecl->ast);

    the_actor->implementation->cdecl = cdecl;
    cdecl->impl = the_actor->implementation;

    AST_set_parents(CAST(node, cdecl->ast));
    
    // Build the default connection graph (just nodes for the external
    // endpoints)
    cdecl->connections = build_external_graph(r, cdecl);

    process_actor_implementation(r, CAST(actor_implementation, cdecl->impl));
}
