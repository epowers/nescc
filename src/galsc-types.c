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
#include "nesc-semantics.h"
#include "c-parse.h" // FIXME only needed for parse_region
#include "galsc-a.h"

// Construct and return the argument list for the source trigger in
// 'pconn'.
static typelist get_sourceargs(galsc_parameter_connection pconn) {
    assert(pconn);
    endpoint trigger_ep = pconn->conn->ep2;
    environment trigger_configuration_env = pconn->configuration_env;
    
    // FIXME parse_region
    typelist sourceargs = new_typelist(parse_region);
    endpoint ep;
    struct endp temp;
    scan_endpoint(ep, trigger_ep) {
        if (lookup_endpoint(trigger_configuration_env, ep, &temp)) {
            // FIXME: what about args for functions?
            if (temp.parameter) {
                typelist_append(sourceargs, temp.parameter->type);
            } else {
                typelist_scanner scanargs;
                type argt;

                type temptype = (temp.function) ?
                    temp.function->type : temp.port->type;
                assert(temptype);
                
                typelist_scan(type_function_arguments(temptype), &scanargs);

                // FIXME what if void type
                while ((argt = typelist_next(&scanargs))) {
                    typelist_append(sourceargs, argt);
                }
            }
        }
    }
    return sourceargs;
}

// Check the put return type of the parameter (target).  If the
// parameter does not already have a put return type, copy it from the
// source.  Otherwise, check to see if the parameter put return type
// is compatible with the source.
static bool match_parameter_return_type(endp target, endp source) {
    assert(target->parameter != NULL);

    if (!target->parameter->parameter_put_type) {
        if (source->parameter) {
            target->parameter->parameter_put_type = source->parameter->parameter_put_type;
        } else {
            data_declaration trigger = source->port ? source->port : source->function;
            target->parameter->parameter_put_type = trigger->type;
        }
        return TRUE;
    } else {
        type target_return_type = type_function_return_type(get_actual_function_type(target->parameter->parameter_put_type));
        type source_return_type = type_function_return_type(get_actual_function_type(source->parameter->parameter_put_type));
        assert(target_return_type && source_return_type);
        return type_compatible(target_return_type, source_return_type);
    }
}

// Type check a TinyGUYS GET connection.  The connection can be one of
// the following formats:
// p X (p, l)
// p X (f, l)
// f X (p, l)
// f X (f, l)
//
// [target X (trigger, l)], where "target" is a port or function, and
// "source" consists of a trigger port or function and a list of
// parameters.
//
// Assumes that ports have already been assigned types.
//
// Returns TRUE if the end points of the connection match.
bool match_parameter_get(endp target, endp source) {
    // endp is either a function or a port.
    assert( (target->function != NULL) ^ (target->port != NULL) );
    assert( (source->function != NULL) ^ (source->port != NULL) );

    // Trigger is the source function or port, which must also contain
    // a parameter list.
    data_declaration trigger = (source->function) ?
        source->function : source->port;
    assert(trigger->parameters);

    // Get the type of the target function or port.
    type targettype = (target->function) ?
        target->function->type : target->port->type;
    assert(targettype);
    
    // Get the argument list for the target function or port.
    typelist targetargs = type_function_arguments(targettype);

    // Get the connection information for the trigger.
    galsc_parameter_connection pconn = get_galsc_parameter_connection(trigger->parameters, target, NULL);
    
    // Construct the argument list for the source.
    typelist sourceargs = get_sourceargs(pconn);

    // Type check the argument lists and return types.
    if (type_lists_compatible(targetargs, sourceargs) &&
            type_compatible(type_function_return_type(targettype),
                    type_function_return_type(trigger->type))) {
            return TRUE;
    }

    // FIXME check command/event types
    
    return FALSE;
}

// Type check a TinyGUYS GET/PUT connection.  The connection can be
// one of the following formats:
// l X (p, l)
// l X (f, l)
//
// [target X (trigger, l)], where "target" is a parameter, and
// "source" consists of a trigger port or function and a list of
// parameters.
//
// Returns TRUE if the end points of the connection match.
bool match_parameter_getput(endp target, endp source) {
    // "target" is a parameter.
    assert( (target->parameter != NULL) && (target->function == NULL)
            && (target->port == NULL) );
    // "source" is either a function or a port.
    assert( (source->function != NULL) ^ (source->port != NULL) );

    // Trigger is the source function or port, which must also contain
    // a parameter list.
    data_declaration trigger = (source->function) ?
        source->function : source->port;
    assert(trigger->parameters);

    // Construct the argument list for the source.  The source of a
    // GET/PUT connection must have exactly one trigger and one
    // parameter.

    // Get the connection information for the trigger.
    galsc_parameter_connection pconn = get_galsc_parameter_connection(trigger->parameters, target, NULL);
    endpoint trigger_ep = pconn->conn->ep2;
    if (chain_length(CAST(node, trigger_ep)) == 2) {
        typelist sourceargs = get_sourceargs(pconn);

        // Check that there is exactly one source parameter.  (The
        // trigger has no arguments).
        if (chain_length(CAST(node, sourceargs)) == 1) {
            typelist_scanner scanargs;
            type sourcearg;
            
            typelist_scan(sourceargs, &scanargs);
            sourcearg = typelist_next(&scanargs);
            
            // Use type_compatible_unqualified() when you don't care
            // about matching top level qualifiers (like const).  Use
            // this for variable assignment.
            if (type_compatible_unqualified(target->parameter->type,
                        sourcearg) &&
                    match_parameter_return_type(target, source)) {
                return TRUE;
            }
        }
    }
    return FALSE;
}

// Type check a TinyGUYS PUT connection.  The connection can be one of
// the following formats:
// l X p
// l X f
//
// [target X source], where "target" is a parameter, and "source" is a
// trigger port or function.
//
// Returns TRUE if the end points of the connection match.
bool match_parameter_put(endp target, endp source) {
    // "target" is a parameter.
    assert( (target->parameter != NULL) && (target->function == NULL)
            && (target->port == NULL) );
    // "source" is either a function or a port.
    assert( (source->function != NULL) ^ (source->port != NULL) );

    // 'trigger' is the source function or port, and must not contain
    // a parameter list.
    data_declaration trigger = (source->function) ? source->function : source->port;
    assert( !(trigger->parameters) && trigger->type );

    // Get the arguments of the trigger function or port.
    typelist fn_args = type_function_arguments(trigger->type);

    // Construct an argument list consisting only of the type of the
    // target parameter.
    // FIXME parse_region
    typelist parameter_type = new_typelist(parse_region);
    typelist_append(parameter_type, target->parameter->type);

    if (type_lists_compatible(fn_args, parameter_type) &&
            match_parameter_return_type(target, source)) {
        return TRUE;
    }
    
    return FALSE;
}

// Find the source of the function call chain that includes node "n"
// and append the source (gnode) to the "sources" list.  Returns FALSE
// if there was a cycle.
//
// See find_reachable_functions()
static bool galsc_find_source(region r, gnode n, dd_list sources) {
    if (graph_node_markedp(n)) {
        // We've already seen this node; there is a cycle in the graph.
        return FALSE;
        // FIXME should allow cycles around actors.
    } else {
        gedge connection;
        graph_mark_node(n);
        bool found_source = TRUE;
        graph_scan_in (connection, n) {
            found_source = FALSE;
            if (!galsc_find_source(r, graph_edge_from(connection), sources))
                return FALSE;
        }
        if (found_source) {
            // We've reached the beginning of the chain of function
            // calls; add this source function to the list.
            if (!dd_find(sources, n)) {
                dd_add_last(regionof(sources), sources, n);
            }
        }
        graph_unmark_node(n);
    }
    return TRUE;
}

// Return the type of the function/port/interface in p.
//
// See endpoint_type() in nesc-configuration.c
static type galsc_endpoint_type(endp p) {
    type t = NULL;

    // p should not be both a function and a port.
    assert((p->function != NULL) ^ (p->port != NULL));
    
    if (p->args) {
        if (p->function) {
            // FIXME need get_actual_function_type()?
            t = type_function_return_type(p->function->type);
        } else if (p->port) {
            t = type_function_return_type(p->port->type);
        } else if (p->interface) {
            t = p->interface->type;
        }
    } else {
        if (p->function) {
            t = p->function->type;
        } else if (p->port) {
            t = p->port->type;
        } else if (p->interface) {
            t = p->interface->type;
            
            // We don't normally include the generic parameters in the
            // interface's type, but we do here to allow correct
            // matching.
            if (p->interface->gparms)
                t = make_generic_type(t, p->interface->gparms);
        }
    }
    return t;
}

// Check that the types for the endpoints match.  Use this only for
// functions/ports/interfaces.
//
// See match_endpoints() in nesc-configuration.c
static int galsc_match_endpoints(endp p1, endp p2, endp amatch) {
    // Should this be type_equal ? unclear (only real diff, given that
    // we will forbid old style parameter lists, is transparent union
    // handling).
    if (type_compatible(galsc_endpoint_type(p1), galsc_endpoint_type(p2))) {
        if (amatch) {
            *amatch = *p2;
        }
        return 1;
    } else {
        return 0;
    }
}

// Type check the function call chain (containing a port) that begins
// with the node "n".  If "n" does not match, copy it to "badnode".
// Returns FALSE if there was a bad type or a cycle in the graph.
//
// This is a recursive function.  The first time it is called, node
// "n" should be the source of a function call chain containing a
// port, and "badnode" is NULL.  It examines each of the sources of
// node "n" and check that the source type matches the type of node
// "n".  It then examines each of the targets of node "n" by calling
// this function recursively.  A cycle is detected if it encounters a
// previously marked node.
//
// See find_reachable_functions() in nesc-generate.c
static bool galsc_type_check_port_chain(region r, gnode n, gnode badnode) {
    // Get the endp for node "n".
    endp target = NODE_GET(endp, n);

    if (graph_node_markedp(n) || badnode) {
        // Cycle in the graph or a node that doesn't type check.
        return FALSE;
        // FIXME should allow cycles around actors.
    } else {
        // Mark the node to detect cycles.
        graph_mark_node(n);

        // Type check each function/port that points to node "n".  If
        // node "n" is a port and contains no type, make the port type
        // equal to the type of the first function/port that points to
        // the target.  If node "n" is a function or a port that
        // already contains a type, do regular type checking (includes
        // TinyGUYS GET calls).  If node "n" is a parameter, then do
        // type checking for TinyGUYS PUT or GETPUT call.
        
        gedge in;
        graph_scan_in (in, n) {
            gnode msource = graph_edge_from(in);
            endp source = NODE_GET(endp, msource);

            // Parameters cannot be sources.
            assert(source->parameter == NULL);

            // The trigger is a function or a port.
            data_declaration trigger = (source->function) ?
                source->function : source->port;
            assert(trigger);

            // Get the connection information for the trigger.
            galsc_parameter_connection trigger_pconn = get_galsc_parameter_connection(trigger->parameters, target, NULL);
            
            if ((target->port != NULL) ^ (target->function != NULL)) {
                if (target->port && !target->port->type) {
                    // Port type takes the same type as its trigger. 
                    if (trigger_pconn) { // (x, l) -> p

                        // Construct the argument list for the source.
                        typelist sourceargs = get_sourceargs(trigger_pconn);
                        type t = trigger->type;

                        if (type_command(t)) {
                            target->port->type = make_command_type(type_function_return_type(t), sourceargs, type_function_varargs(t));
                        } else if (type_event(t)) {
                            target->port->type = make_event_type(type_function_return_type(t), sourceargs, type_function_varargs(t));
                        } else {
                            assert(0);
                        }
                    } else {             // x -> p
                        target->port->type = trigger->type;
                    }
                } else {
                    if (trigger_pconn) { // (x, l) -> f
                        if (!match_parameter_get(target, source)) {
                            badnode = n;
                            return FALSE;
                        }
                    } else {             // x -> f
                        if (!galsc_match_endpoints(target, source, NULL)) {
                            badnode = n;
                            return FALSE;
                        }
                    }
                }
            } else if (target->parameter) {
                if (trigger_pconn) {     // (x, l) -> l
                    if (!match_parameter_getput(target, source)) {
                        badnode = n;
                        return FALSE;
                    }
                } else {                 // x -> l
                    if (!match_parameter_put(target, source)) {
                        badnode = n;
                        return FALSE;
                    }
                }
            } else {
                assert(0); // Error.
            }
        }

        gedge out;
        graph_scan_out (out, n) {
            if (!galsc_type_check_port_chain(r, graph_edge_to(out), badnode))
                return FALSE;
        }
        // This node type checks.  Unmark it.
        graph_unmark_node(n);
    }
    return TRUE;
}

// Type check the global parameters.  Assumes that "master" is the
// connection graph for the entire application.  Assumes that the
// ports in the connection graph have already been assigned types by
// type_check_ports().  "parameters" should contain the parameters
// (data_declaration) for the connections to be checked.  Returns
// FALSE if there was a bad type in the graph.
static bool galsc_type_check_parameter(region r, gnode n) {
    bool match = TRUE;
    
    // Get the endp for node "n".
    endp target = NODE_GET(endp, n);

    assert(target->parameter && target->parameter->container && target->parameter->container->kind == l_application);

    gedge in;
    // Look at each local parameter
    graph_scan_in (in, n) {
        gnode msource = graph_edge_from(in);
        endp source = NODE_GET(endp, msource);

        assert(source->parameter);

        // Local parameter will have a put type only if it is being used for
        // a put call.
        if (source->parameter->parameter_put_type) {
            // If the global parameter does not already have a put
            // return type, copy it from the local parameter.
            // Otherwise, check to see if the local parameter matches
            // the put return type as the global parameter put return
            // type.
            if (match) { // Don't overwrite previous bad matches.
                match = match_parameter_return_type(target, source);
            }
        }
    }
    return match;
}

// Type check all port chains and parameter PUT chains in the
// application.  Assumes that:
// - "master" contains the connection graph for the entire application.
// - "ports" contains the ports (data_declaration) for the
//    connections to be checked.
//
// See find_connected_functions() in nesc-generate.c
void galsc_type_check(region r, cgraph master, dd_list ports, dd_list parameters) {
    dd_list sources = dd_new_list(r);   // List of gnodes that are
                                        // sources for function call
                                        // chains containing a port.
    dd_list_pos pos;
    
    // Find the sources of all function call chains containing a port.
    graph_clear_all_marks(cgraph_graph(master));
    dd_scan (pos, ports) {
        data_declaration port = DD_GET(data_declaration, pos);
        struct endp target;
        init_endp(&target);
        target.port = port;
        gnode mtarget = endpoint_lookup(master, &target);

        // Find the source of the call chain containing this port.
        if (!galsc_find_source(r, mtarget, sources)) {
            error_with_location(port->ast->location,
                    "cycle in application (for %s.%s)",
                    port->container->name,
                    port->name);
        }
    }

    // Type check each port function call chain.
    graph_clear_all_marks(cgraph_graph(master));
    dd_scan (pos, sources) {
        gnode n = DD_GET(gnode, pos);
        gnode badnode = NULL;

        // Type check the function call chain that starts with this source.
        if (!galsc_type_check_port_chain(r, n, badnode)) {
            if (badnode) {
                endp ep = NODE_GET(endp, badnode);
                data_declaration ep_data = NULL;
                if (ep->function)
                    ep_data = ep->function;
                else if (ep->port)
                    ep_data = ep->port;
                else if (ep->parameter)
                    ep_data = ep->parameter;
                else
                    assert(0);
                error_with_location(ep_data->ast->location,
                        "Connection (for %s%s%s.%s) does not type check",
                        ep_data->container->name,
                        ep_data->interface ? "." : "",
                        ep_data->interface ? ep_data->interface->name : "",
                        ep_data->name);
            } else {
                endp ep = NODE_GET(endp, n);
                data_declaration ep_data = NULL;
                if (ep->function)
                    ep_data = ep->function;
                else if (ep->port)
                    ep_data = ep->port;
                else if (ep->parameter)
                    ep_data = ep->parameter;
                else
                    assert(0);
                error_with_location(ep_data->ast->location,
                        "Connection (for %s%s%s.%s) does not type check",
                        ep_data->container->name,
                        ep_data->interface ? "." : "",
                        ep_data->interface ? ep_data->interface->name : "",
                        ep_data->name);
            }
        }
    }

    // Type check all parameter PUT chains
    graph_clear_all_marks(cgraph_graph(master));
    dd_scan (pos, parameters) {
        data_declaration parameter = DD_GET(data_declaration, pos);
        struct endp target;
        init_endp(&target);
        target.parameter = parameter;
        gnode mtarget = endpoint_lookup(master, &target);

        if (!galsc_type_check_parameter(r, mtarget)) {
            error_with_location(parameter->ast->location,
                    "Return types for global parameter %s do not match",
                    parameter->container->name,
                    parameter->name);
        }

    }
}

