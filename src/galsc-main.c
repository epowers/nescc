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

#include "nesc-main.h"
#include "galsc-a.h"
#include "galsc-types.h"
#include "galsc-generate.h"

// Adds the component graph 'component' to the whole program graph
// 'master'.  Add any new ports to the ports list.
//
// See connect_graph() in nesc-main.c
static void galsc_connect_graph(cgraph master, cgraph component, dd_list ports, dd_list parameters)
{
    ggraph cg = cgraph_graph(component);
    gnode n;
    gedge connection;

    // Add all edges from component to master
    graph_scan_nodes (n, cg) {
        endp from = NODE_GET(endp, n);
        gnode mfrom = endpoint_lookup(master, from);
        
        // Add to master port list if this is a port
        if (from->port) {
            if (!dd_find(ports, from->port)) {
                dd_add_last(regionof(ports), ports, from->port);
            }
        } else if (from->parameter) {
            if (!dd_find(parameters, from->parameter)) {
                // Just add global parameters
                if (from->parameter->container->kind == l_application) {
                    dd_add_last(regionof(parameters), parameters, from->parameter);
                }
            }
        }
      
        graph_scan_out (connection, n) {
            endp to = NODE_GET(endp, graph_edge_to(connection));
            gnode mto = endpoint_lookup(master, to);

            graph_add_edge(mfrom, mto, NULL);	  
	}
    }
}

// Make the master connection graph (cg) from the application (cdecl).
// First, examine each actor and make the nodes for the ports and
// connections between actors and any external components.  Then, for
// each actor, make the connections for its internal components.
// Finally, add the internal connections for the components that are
// external to any actor (i.e. Main).
//
// See connect() in nesc-main.c
static void galsc_connect(nesc_declaration cdecl,
        cgraph cg, dd_list modules, dd_list components,
        dd_list ports, dd_list parameters) {

    // GALSC FIXME: is it ok to add application to components list?
    if (!dd_find(components, cdecl)) {
        dd_add_last(regionof(components), components, cdecl);
        
        // Connect internals of application, including external
        // connections for external components (i.e. Main).
        galsc_connect_graph(cg, cdecl->connections, ports, parameters);

        actor_ref actordecl;
        application_implementation app = CAST(application_implementation, cdecl->impl);
        scan_actor_ref (actordecl, app->actors) {
            nesc_declaration adecl = actordecl->cdecl;
            // GALSC FIXME: is it ok to add actor to components list?
            if (!dd_find(components, adecl)) {
                dd_add_last(regionof(components), components, adecl);
                // connect internals of actor
                galsc_connect_graph(cg, adecl->connections, ports, parameters);
                
                component_ref comp;
                actor_implementation actor = CAST(actor_implementation, adecl->impl);
                // connect internals of internal components of actor
                scan_component_ref (comp, actor->components) {
                    connect(comp->cdecl, cg, modules, components);
                }
            }
        }

        // Add the internal connections for the external components
        // (i.e. Main).
        component_ref comp;
        scan_component_ref (comp, CAST(application_implementation, cdecl->impl)->external_components) {
            connect(comp->cdecl, cg, modules, components);
        }
    }
}

// Fill the appstart list with data from the appstart section of the
// application file.  Example:
//
//    appstart {
//        MyCount.outputComplete(SUCCESS);
//    }
//
// An entry in the appstart list will contain the endp corresponding
// to the port MyCount.outputComplete and the AST node
// (start_function_call) for that line in the appstart section.
static void process_appstart(nesc_declaration cdecl, dd_list appstart) {
    application_implementation app = CAST(application_implementation, cdecl->impl);
    struct endp *ep = ralloc(regionof(appstart), struct endp);

    start_function_call call;
    
    scan_start_function_call (call, app->startlist) {
        if (lookup_endpoint(app->ienv, call->ep1, ep)) {
            struct appstart_data *d = ralloc(regionof(appstart), struct appstart_data);
            d->ep = ep;
            d->call = call;
            
            dd_add_last(regionof(appstart), appstart, d);
        }
    }

}

// Make the master connection graph and process the appstart section
// of the application declaration (the initial tokens for the ports).
//
// See connect_graphs() in nesc-main.c
void galsc_connect_graphs(region r, nesc_declaration program,
        cgraph *cg, dd_list *modules, dd_list *components,
        dd_list *ports, dd_list *parameters, dd_list *appstart) {

    *cg = new_cgraph(r);
    *modules = dd_new_list(r);     // list of modules (nesc_declaration)
    *components = dd_new_list(r);  // list of components (nesc_declaration)
    *ports = dd_new_list(r);       // list of ports (data_declaration: kind = decl_port_ref)
    *parameters = dd_new_list(r);  // list of parameters (data_declaration: kind = decl_variable)
    *appstart = dd_new_list(r);    // list of initial port tokens (struct appstart_data)
    
    galsc_connect(program, *cg, *modules, *components, *ports, *parameters);
    galsc_type_check(r, *cg, *ports, *parameters);
    process_appstart(program, *appstart);
}
