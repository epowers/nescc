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

#include "galsc-a.h"
#include "galsc-generate.h"

// FIXMEa: comment
// The string to use to separate substrings in
// functions/variables/names of ports/parameters.
static char *galsc_separator = "$";

// FIXMEa: comment
// Print the full port name (using the galsc_separator string) when
// given an endp.
//
// Example:
//   MyLeds$display
static void prt_galsc_port_name(endp ep) {
    output_string(ep->actor->name);
    output_string(galsc_separator);
    output_string(ep->port->name);
}

// FIXMEa: comment
// Print the full name (using the galsc_separator string) when given a
// data_declaration.
//
// Example:
//   MyLeds$display
static void prt_galsc_name_ddecl(data_declaration p) {
    output_string(p->container->name);
    output_string(galsc_separator);
    output_string(p->name);
}

// FIXME comment
//
// Print argument list to pass to the target of a TinyGUYS GET or
// GET/PUT call.
//
// Called from prt_ncf_direct_call() in nesc-generate.c
int galsc_prt_parameter_get_call(struct connections *c,
                                 full_connection ccall,
                                 psd_options options,
                                 function_declarator called_fd,
                                 int first_arg) {
    dd_list_pos pos;
    dd_scan (pos, ccall->parameters) {
        galsc_parameter_connection pconn =
            DD_GET(galsc_parameter_connection, pos);
        endpoint ep;
        struct endp temp;
        scan_endpoint (ep, pconn->conn->ep2) {
            if (lookup_endpoint(pconn->configuration_env, ep, &temp)) {
                if (!first_arg)
                    output(", ");
                if (temp.parameter) { // TinyGUYS GET call
                    gnode nlocal =
                        parameter_lookup(c->cg, temp.parameter);
                    bool first_param = TRUE;
                    gedge out;
                    graph_scan_out (out, nlocal) {
                        gnode nglobal = graph_edge_to(out);
                        endp global = NODE_GET(endp, nglobal);
                        if (first_param) {
                            assert(global->parameter && global->parameter->container && global->parameter->container->kind == l_application);
                            output("GALSC_PARAM__");
                            prt_ddecl_full_name(global->parameter, options);
                            output_string(galsc_separator);
                            output("get()");
                            first_arg = FALSE;
                        } else {
                            error_with_location(c->called->ast->location,
                                    "trying to read from more than one TinyGUYS for the same function/port argument");
                            // FIXME fix error message
                        }
                        first_param = FALSE;
                    }
                } else if (temp.port) {
                    if (c->called->kind == decl_port_ref) {
                        // See prt_galsc_ports() in galsc-generate.c
                              
                        declarator tdeclarator;
                        type_element tmodifiers;
                        type2ast(c->r, dummy_location, c->called->type, NULL, &tdeclarator, &tmodifiers);
                        function_declarator fd = CAST(function_declarator, tdeclarator);
                        bool void_parms = TRUE;
                        if ( (!fd->gparms || is_void_parms(fd->gparms)) &&
                                (!fd->parms || is_void_parms(fd->parms)) )
                            void_parms = TRUE;
                        if (!void_parms) {
                            prt_simple_declarator(tdeclarator, c->called, psd_galsc_print_port_get_call);
                            first_arg = FALSE;
                        }
                    } else { // Error - more than one trigger.
                        assert(0);
                    }
                } else if (temp.function) {
                    if (c->called->kind == decl_function) {
                        // FIXME void args
                        if (called_fd->parms) {
                            prt_arguments(called_fd->parms, first_arg);
                            first_arg = FALSE;
                        }
                    } else { // Error - more than one trigger.
                        assert(0);
                    }
                } else {
                    assert(0); // Error.
                }
            }
        }
    }
    return first_arg;
}

// FIXME comment
static bool galsc_prt_parameter(declaration d, data_declaration ddecl, psd_options options, bool first, int numargs) {
    if (!is_void_parms(d)) {
        if (options & psd_galsc_print_port_struct) {
            prt_parameter(d, TRUE, FALSE, options);
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output("arg%d", numargs);
            output_string("[GALSC_PORT_SIZE_");
            prt_galsc_name_ddecl(ddecl);
            output_string("]");
            outputln(";");
            first = FALSE;
        } else if (options & psd_galsc_print_port_get_call) {
            if (!first)
                output(", ");
            output_string("GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output("arg%d", numargs);
            output_string("[ oldhead ]");
            first = FALSE;
        } else if (options & psd_galsc_print_port_put_function_header) {
            prt_parameter(d, first, FALSE, options);
            output("arg%d", numargs);
            first = FALSE;
        } else if (options & psd_galsc_print_port_put_function_body) {
            // FIXME if void?
            output_string("GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output("arg%d", numargs);
            output("[ temp ] = arg%d", numargs);
            outputln(";");
        }
    }
        return first;
}

// FIXME comment
// ddecl is the port p
// For prt_galsc_ports() in galsc-generate.c
// See prt_parameters() in unparse.c
void galsc_prt_parameters(declaration gparms, declaration parms, psd_options options, data_declaration ddecl) {
    assert(ddecl->kind = decl_port_ref);
    
    declaration d;
    bool first = TRUE;

    // Check if void argument list
    bool void_parms = FALSE;
    if ( (!gparms || is_void_parms(gparms)) &&
            (!parms || is_void_parms(parms)) )
        void_parms = TRUE;

    // If there are args
    // FIXME void
    if ( !void_parms ) {//gparms || parms ) {
        if (options & psd_galsc_print_port_get_function) {
            output_string("int oldhead = GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            outputln("head;");

            output_string("GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("head = ((GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("head + 1) >= GALSC_PORT_SIZE_");
            prt_galsc_name_ddecl(ddecl);
            output_string(") ? 0 : GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("head + 1");
            outputln(";");
        } else if (options & psd_galsc_print_port_put_function_body) {
            output_string("int temp = ((GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("head + GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("count) >= GALSC_PORT_SIZE_");
            prt_galsc_name_ddecl(ddecl);
            output_string(") ? ((GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("head + GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("count) - GALSC_PORT_SIZE_");
            prt_galsc_name_ddecl(ddecl);
            output_string(") : GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("head + GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            outputln("count;");
        } else if (options & psd_galsc_print_sched_init) {
            output_string("GALSC_ports.");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            outputln("head = 0;");
        }
    }
    
    if (options & psd_galsc_print_port_put_function_header) {
        output("(");
    }
  
    int numargs = 0;
    scan_declaration (d, gparms) {
        first = galsc_prt_parameter(d, ddecl, options, first, numargs);
        if (!first)
            numargs++;
    }
    scan_declaration (d, parms) {
        first = galsc_prt_parameter(d, ddecl, options, first, numargs);
        if (!first)
            numargs++;
    }
    if (options & psd_galsc_print_port_put_function_header) {
        if (!gparms && !parms)
            output("void");
        output(")");
    }
    // If there are args
    //if ( (gparms || parms) && (numargs != 0)) {
    if (!void_parms) {
        if (options & psd_galsc_print_port_struct) {
            // Print the port queue pointers.
            output_string("int ");
            prt_galsc_name_ddecl(ddecl);
            output_string(galsc_separator);
            output_string("head");
            outputln(";");
        }
    }

    if (options & psd_galsc_print_port_struct) {
        output_string("int ");
        prt_galsc_name_ddecl(ddecl);
        output_string(galsc_separator);
        output_string("count");
        outputln(";\n");
    }
}

// FIXME comment
// FIXME what if a port is connected instead of a function
// Print call to connected function with arguments.  Example:
// IntToLedsM$IntOutput$output(GALSC_ports.MyLeds$display$arg0[ oldhead ]);
//
// ep contains the port to which these calls are connected.
//
// Called from prt_galsc_port_get_function() in galsc-generate.c
static void prt_galsc_port_call(data_declaration p) {

    // From prt_nesc_connection_function();

    struct connections *c = p->connections;
    type return_type = type_function_return_type(get_actual_function_type(c->called->type));

    set_fixed_location(c->called->ast->location);

    if (c->called->gparms) {
        bool first_call;
        
        first_call = prt_ncf_direct_calls(c, c->generic_calls, return_type);
        prt_ncf_conditional_calls(c, first_call, return_type);
    } else {
        if (dd_is_empty(c->normal_calls))
            prt_ncf_default_call(c, return_type,
                    ddecl_get_fdeclarator(c->called));
        else
            prt_ncf_direct_calls(c, c->normal_calls, return_type);
    }
}

// FIXME comment
void prt_galsc_parameter_put_function_header(data_declaration p) {
    if (p->makeinline)
        output("inline ");

    output("static ");

    // Print return type.
    {
        assert(p->parameter_put_type);
        declarator tdeclarator;
        type_element tmodifiers;
        type return_type = type_function_return_type(get_actual_function_type(p->parameter_put_type));
        // FIXME parse_region
        type2ast(parse_region, dummy_location, return_type, NULL, &tdeclarator, &tmodifiers);
        
        // Printing out tmodifiers from return_type gives "static"; does
        // not when you print out tmodifiers from p->type.
        prt_type_elements(tmodifiers, 0);
    }
    
    output("GALSC_PARAM__");
    prt_galsc_name_ddecl(p);
    output_string(galsc_separator);
    output("put(");

    // Print the type of the parameter.
    {
        declarator tdeclarator;
        type_element tmodifiers;
        type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);
        prt_type_elements(tmodifiers, 0);
        
        output("arg)");
    }
}
    
// FIXME comment
//
// static result_t GALSC_PARAM__SenseToLeds$sensorData$put(uint16_t arg_0x85020f0) {
//     GALSC_params_buffer.sensorData = arg_0x85020f0;
//     GALSC_params_buffer_flag = TRUE;
//     return SUCCESS;
// }
//
void prt_galsc_parameter_put_function(data_declaration p) {
    prt_galsc_parameter_put_function_header(p);
    
    outputln(" {");

    indent();

    output("GALSC_params_buffer.");
    prt_galsc_name_ddecl(p);
    outputln(" = arg;");

    outputln("GALSC_params_buffer_flag = TRUE;");
    outputln("return SUCCESS;");
    
    unindent();
    outputln("}");
}
    
// FIXME comment
void prt_galsc_parameter_get_function_header(data_declaration p) {
    if (p->makeinline)
        output("inline ");

    output("static ");

    // Print the type of the parameter.
    declarator tdeclarator;
    type_element tmodifiers;
    type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);
    prt_type_elements(tmodifiers, 0);
    
    output("GALSC_PARAM__");
    prt_galsc_name_ddecl(p);
    output_string(galsc_separator);
    output("get()");
}

// FIXME comment
//
// uint16_t GALSC_PARAM__SenseToLeds$sensorData$get() {
//     return GALSC_params.sensorData;
// }
void prt_galsc_parameter_get_function(data_declaration p) {
    prt_galsc_parameter_get_function_header(p);
        
    outputln(" {");
    
    indent();
    
    output("return ");
    output("GALSC_params.");
    prt_galsc_name_ddecl(p);
    outputln(";");
    
    unindent();
    outputln("}");
}

void prt_galsc_parameter_functions(dd_list parameters) {
    dd_list_pos pos;
    dd_scan (pos, parameters) {
        data_declaration p = DD_GET(data_declaration, pos);
        prt_galsc_parameter_get_function(p);
        prt_galsc_parameter_put_function(p);
    }
}

// FIXME comment
// Print put() and get() function for each parameter.
void prt_galsc_parameter_function_declarations(dd_list parameters) {
    dd_list_pos pos;
    dd_scan (pos, parameters) {
        data_declaration p = DD_GET(data_declaration, pos);
        prt_galsc_parameter_get_function_header(p);
        outputln(";");
        prt_galsc_parameter_put_function_header(p);
        outputln(";");
    }
}

// FIXMEa: comment
// Print the header for the  port queue get() function.
//
// void GALSC__MyLeds$display$get()
void prt_galsc_port_get_function_header(data_declaration p) {
    if (p->makeinline)
        output("inline ");

    output("static ");
    output("void ");
    output("GALSC__");
    prt_galsc_name_ddecl(p);
    output_string(galsc_separator);
    output_string("get() ");
}
    
// FIXMEa: comment
// Print the port queue get() function.
//
// void GALSC__MyLeds$display$get() {
//     if (GALSC_ports.MyLeds$display$count > 0) {
//         int oldhead = GALSC_ports.MyLeds$display$head;
//         // Note this does not have to be in non-interrupt context since this function is not reentrant.
//         GALSC_ports.MyLeds$display$head = ((GALSC_ports.MyLeds$display$head + 1) >= GALSC_PORT_SIZE_MyLeds$display) ? 0 : GALSC_ports.MyLeds$display$head + 1;
//         GALSC_ports.MyLeds$display$count--;
//         // For each connected function.
//         IntToLedsM$IntOutput$output(GALSC_ports.MyLeds$display$arg0[ oldhead ]);
//     }
// }
//
void prt_galsc_port_get_function(data_declaration p) {
    prt_galsc_port_get_function_header(p);
    outputln(" {");
    indent();

    {
        output_string("if (GALSC_ports.");
        prt_galsc_name_ddecl(p);
        output_string(galsc_separator);
        output_string("count > 0) ");
        outputln("{");
        indent();
        
        {
            declarator tdeclarator;
            type_element tmodifiers;
            // FIXME parse_region
            type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);
            prt_simple_declarator(tdeclarator, p, psd_galsc_print_port_get_function);
            
            output_string("GALSC_ports.");
            prt_galsc_name_ddecl(p);
            output_string(galsc_separator);
            outputln("count--;");

            prt_galsc_port_call(p);
        }
        unindent();
        outputln("}");
    }
    unindent();
    outputln("}");
}

// FIXMEa: comment
// Print the header for the port queue put() function.
//
// static result_t GALSC__MyLeds$display$put(uint16_t arg_0x85020f0)
void prt_galsc_port_put_function_header(data_declaration p) {
    output("static ");

    declarator tdeclarator;
    type_element tmodifiers;
    type return_type = type_function_return_type(get_actual_function_type(p->type));
    // FIXME parse_region
    type2ast(parse_region, dummy_location, return_type, NULL, &tdeclarator, &tmodifiers);
    
    // Printing out tmodifiers from return_type gives "static"; does
    // not when you print out tmodifiers from p->type.
    prt_type_elements(tmodifiers, 0); 

    output("GALSC__");
    prt_galsc_name_ddecl(p);
    output_string(galsc_separator);
    output_string("put");
    
    type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);
    prt_simple_declarator(tdeclarator, p, psd_galsc_print_port_put_function_header);
}

// FIXME comment
// Print the port queue put() function.
//
// static result_t GALSC__MyLeds$display$put(uint16_t arg_0x85020f0) {
//     if (GALSC_ports.MyLeds$display$count < GALSC_PORT_SIZE_MyLeds$display) {
//         // Note this does not have to be in non-interrupt context since this function is not reentrant.
//         int temp = ((GALSC_ports.MyLeds$display$head + GALSC_ports.MyLeds$display$count) >= GALSC_PORT_SIZE_MyLeds$display) ? ((GALSC_ports.MyLeds$display$head + GALSC_ports.MyLeds$display$count) - GALSC_PORT_SIZE_MyLeds$display) : GALSC_ports.MyLeds$display$head + GALSC_ports.MyLeds$display$count;
//         // For each arg.
//         GALSC_ports.MyLeds$display$arg0[ temp ] = arg_0x85020f0;
//
//         GALSC_ports.MyLeds$display$count++;
// 
//         {
//             __nesc_atomic_t __nesc_atomic = __nesc_atomic_start();
//            GALSC_eventqueue[ ((GALSC_eventqueue_head + GALSC_eventqueue_count) >= GALSC_EVENTQUEUE_SIZE) ? ((GALSC_eventqueue_head + GALSC_eventqueue_count) - GALSC_EVENTQUEUE_SIZE) : GALSC_eventqueue_head + GALSC_eventqueue_count ].tp = GALSC__MyLeds$display$get;
//             GALSC_eventqueue_count++;
//             __nesc_atomic_end(__nesc_atomic);
//         }
//         return SUCCESS;
//     } else {
//         return FAIL;
//     }
// }
//
void prt_galsc_port_put_function(data_declaration p) {
    prt_galsc_port_put_function_header(p);
    outputln(" {");
    indent();

    {
        output_string("if (GALSC_ports.");
        prt_galsc_name_ddecl(p);
        output_string(galsc_separator);
        output_string("count < GALSC_PORT_SIZE_");
        prt_galsc_name_ddecl(p);
        output_string(") ");
        outputln(" {");
        indent();
    }

    // Store each function argument.
    declarator tdeclarator;
    type_element tmodifiers;
    type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);
    prt_simple_declarator(tdeclarator, p, psd_galsc_print_port_put_function_body);

    {
        // Update the queue counter.
        output_string("GALSC_ports.");
        prt_galsc_name_ddecl(p);
        output_string(galsc_separator);
        output_string("count++");
        outputln(";");
    }
    
    {
        // From prt_atomic_stmt
        outputln("{ __nesc_atomic_t __nesc_atomic = __nesc_atomic_start();");
        indent();
        
        output_string("GALSC_eventqueue[ ((GALSC_eventqueue_head + GALSC_eventqueue_count) >= GALSC_EVENTQUEUE_SIZE) ? ((GALSC_eventqueue_head + GALSC_eventqueue_count) - GALSC_EVENTQUEUE_SIZE) : GALSC_eventqueue_head + GALSC_eventqueue_count ].tp = ");
        output("GALSC__");
        prt_galsc_name_ddecl(p);
        output_string(galsc_separator);
        output_string("get");
        outputln(";");
        
        outputln("GALSC_eventqueue_count++;");
        
        outputln("__nesc_atomic_end(__nesc_atomic);");
        unindent();
        outputln("}");
    }

    {
        outputln("return SUCCESS;");
        unindent();

        outputln("} else {");
    
        indent();
        outputln("return FAIL;");
        unindent();
    
        outputln("}");
        
        unindent();
        outputln("}");
    }
}

// FIXMEa: comment
// For each inport, print the port size:
//
//  enum {
//      GALSC_PORT_SIZE_MyLeds$display = 10
//  };
//
// Also print the eventqueue size:
//
//  enum {
//      GALSC_EVENTQUEUE_SIZE = (GALSC_PORT_SIZE_MyLeds$display) + (GALSC_PORT_SIZE_MyCount$outputComplete)
//  };
//
static void prt_galsc_port_sizes(dd_list ports) {
    dd_list_pos port;
    bool ismultiple = FALSE;
    
    // Print the enum declaring the size for each port
    dd_scan (port, ports) {
        data_declaration p = DD_GET(data_declaration, port);
        if (p->in) {
            outputln("enum {");

            indent();
            output_string("GALSC_PORT_SIZE_");
            prt_galsc_name_ddecl(p);
            output_string(" = ");

            // Print the specified size of the port.  If there was not
            // size specified, default to 1.
            if (p->portsize_definition) {
                prt_expression(CAST(expression, p->portsize_definition), P_ASSIGN);
            } else {
                output_string("1");
            }
            
            newline();
            unindent();
            outputln("};");
        }
    }

    // Print the enum for the eventqueue size.
    outputln("enum {");
    indent();
    output_string("GALSC_EVENTQUEUE_SIZE = ");
    dd_scan (port, ports) {
        // Add up the all the port sizes.
        data_declaration p = DD_GET(data_declaration, port);
        if (p->in) {
            if (ismultiple) {
                output_string(" + ");
            }
            output_string("(");
            output_string("GALSC_PORT_SIZE_");
            prt_galsc_name_ddecl(p);
            output_string(")");
            ismultiple = TRUE;
        }
    }
    newline();
    unindent();
    outputln("};");
}

// FIXMEa: comment
// This function is for use with prt_galsc_ports to print the
// following type of line for each parameter in the xparms declaration
// list:
//
//     uint16_t MyLeds$display$arg0[GALSC_PORT_SIZE_MyLeds$display];
//
// FIXME comment

// Print scheduler data type containing port queues and queue
// pointers.
//
// struct GALSC_ports_t {
//     // queue for arg0 tokens
//     uint16_t MyLeds$display$arg0[GALSC_PORT_SIZE_MyLeds$display];
//    
//     int MyLeds$display$head;  // head of queue
//     int MyLeds$display$count; // number of tokens in queue
//
//     // queue for arg0 tokens
//     result_t MyCount$outputComplete$arg0[GALSC_PORT_SIZE_MyCount$outputComplete];
//     int MyCount$outputComplete$head;  // head of queue
//     int MyCount$outputComplete$count; // number of tokens in queue
// } GALSC_ports;
//
static void prt_galsc_ports(dd_list ports) {
    outputln("struct GALSC_ports_t {");
    indent();
    
    dd_list_pos port;
    dd_scan (port, ports) {
        data_declaration p = DD_GET(data_declaration, port);
        if (p->in) {
            declarator tdeclarator;
            type_element tmodifiers;
            // FIXME parse_region
            type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);

            prt_simple_declarator(tdeclarator, p, psd_galsc_print_port_struct);
        }
    }
        
    unindent();
    outputln("} GALSC_ports;");
}

// FIXME comment
// struct _GALSC_params_t {
//     uint16_t sensorData;
// };
static void prt_galsc_parameter_struct(dd_list parameters) {
    outputln("struct _GALSC_params_t {");
    indent();
    
    dd_list_pos pos;
    dd_scan(pos, parameters) {
        data_declaration p = DD_GET(data_declaration, pos);

        // Print the type of the parameter.
        declarator tdeclarator;
        type_element tmodifiers;
        type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);
        prt_type_elements(tmodifiers, 0);

        // Print the name of the parameter.
        prt_galsc_name_ddecl(p);
        outputln(";");
    }

    unindent();
    outputln("};");
}

// FIXME comment
// Print put() and get() function for each port.
void prt_galsc_port_function_declarations(dd_list ports) {
    dd_list_pos port;
    dd_scan (port, ports) {
        data_declaration p = DD_GET(data_declaration, port);
        if (p->in) {
            prt_galsc_port_get_function_header(p);
            outputln(";");
            prt_galsc_port_put_function_header(p);
            outputln(";");
        }
    }
}

// FIXME comment
// Print put() and get() function for each port.
void prt_galsc_port_functions(dd_list ports) {
    dd_list_pos port;
    dd_scan (port, ports) {
        data_declaration p = DD_GET(data_declaration, port);
        if (p->in) {
            prt_galsc_port_get_function(p);
            prt_galsc_port_put_function(p);
        }
    }
}

// FIXMEa: comment
// Print the GALSC_sched_init() function, which initializes the galsC
// scheduler data structures.
//
// void GALSC_sched_init(void) {
//     // Interrupts are not yet enabled.
// 
//     int i;
//   
//     GALSC_ports.MyLeds$display$head = 0;
//     GALSC_ports.MyLeds$display$count = 0;
// 
//     GALSC_ports.MyCount$outputComplete$head = 0;
//     GALSC_ports.MyCount$outputComplete$count = 0;
// 
//     GALSC_eventqueue_head = 0;
//     GALSC_eventqueue_count = 0;
// 
//     for (i = 0; i < GALSC_EVENTQUEUE_SIZE; i++)
//         GALSC_eventqueue[i].tp = 0;
//
//     ...
//
//     // Initialize TinyGUYS and TinyGUYS buffer with user values.
//     GALSC_params.sensorData = 0;
//     GALSC_params_buffer.sensorData = 0;
//
//     GALSC_params_buffer_flag = FALSE;
// }
//
static void prt_galsc_sched_init_function(dd_list ports, dd_list parameters) {
    outputln("void GALSC_sched_init(void) {");
    indent();

    outputln("int i;");
    
    // Initialize the port queue pointers.
    dd_list_pos pos;
    dd_scan (pos, ports) {
        data_declaration p = DD_GET(data_declaration, pos);
        if (p->in) {
            declarator tdeclarator;
            type_element tmodifiers;
            // FIXME parse_region
            type2ast(parse_region, dummy_location, p->type, NULL, &tdeclarator, &tmodifiers);
            prt_simple_declarator(tdeclarator, p, psd_galsc_print_sched_init); 

            output_string("GALSC_ports.");
            prt_galsc_name_ddecl(p);
            output_string(galsc_separator);
            outputln("count = 0;\n");
        }
    }

    // Initialize the event queue.
    outputln("GALSC_eventqueue_head = 0;");
    outputln("GALSC_eventqueue_count = 0;\n");

    outputln("for (i = 0; i < GALSC_EVENTQUEUE_SIZE; i++)");
    indent();
    outputln("GALSC_eventqueue[i].tp = 0;\n");
    unindent();

    dd_scan (pos, parameters) {
        data_declaration p = DD_GET(data_declaration, pos);
        output("GALSC_params.");
        prt_variable_decl( CAST(variable_decl, p->ast) );
        outputln(";");
        output("GALSC_params_buffer.");
        prt_variable_decl( CAST(variable_decl, p->ast) );
        outputln(";\n");
    }

    outputln("GALSC_params_buffer_flag = FALSE;");
    
    unindent();
    outputln("}");
}

// FIXMEa: comment
// Print the GALSC_sched_start() function, which places initial tokens in port.
//
// void GALSC_sched_start(void) {
//     // Interrupts are not yet enabled.
//     GALSC__MyCount$outputComplete$put(SUCCESS);
// }
//
static void prt_galsc_sched_start_function(dd_list appstart) {
    dd_list_pos startline;

    outputln("void GALSC_sched_start(void) {");
    indent();

    dd_scan (startline, appstart) {
        struct appstart_data *d = DD_GET(struct appstart_data *, startline);
        bool isfirst = TRUE;

        output("GALSC__");
        prt_galsc_port_name(d->ep);
        output_string(galsc_separator);
        output_string("put");
        output_string("(");

        prt_expressions(d->call->args, isfirst);
        
        output_string(")");
        outputln(";");
    }

    unindent();
    outputln("}");
}

// FIXMEa: comment
// Top level function for generating C code for a galsC application.
//
// Called from nesc_compile in nesc-main.c
//
// See generate_c_code() in nesc-generate.c
void galsc_generate_c_code(nesc_declaration program, const char *target_name,
		     cgraph cg, dd_list modules, dd_list ports, dd_list parameters, dd_list appstart) {
    dd_list_pos mod;
    cgraph callgraph;
    FILE *output = NULL;

    if (target_name) {
        output = fopen(target_name, "w");
        if (!output) {
            perror("couldn't create output file");
            exit(2);
	}
    }

    unparse_start(output ? output : stdout);
    disable_line_directives();

    // Suppress debug functions if necessary
    if (flag_no_debug) {
        suppress_function("dbg");
        suppress_function("dbg_clear");
        suppress_function("dbg_active");
        outputln("#define dbg(mode, format, ...) ((void)0)");
        outputln("#define dbg_clear(mode, format, ...) ((void)0)");
        outputln("#define dbg_active(mode) 0");
    }
  
    // We start by finding each module's identifier uses and
    // connections and marking uncallable functions.
    collect_uses(all_cdecls);
    
    dd_scan (mod, modules) {
        nesc_declaration m = DD_GET(nesc_declaration, mod);

        //fprintf(stderr, "Marking module %s.\n", m->name);

        collect_uses(CAST(module, m->impl)->decls);
        
        // Add the connections for this module to the cg.
        find_connections(cg, m);
    }

    // Then we set the 'isused' bit on all functions that are
    // reachable from spontaneous_calls or global_uses.
    callgraph = mark_reachable_code();

    check_async(callgraph);
    check_races(callgraph);

    inline_functions(callgraph);

    // Print the enum's that declare the galsC port and eventqueue sizes.
    prt_galsc_port_sizes(ports);

    // Print the struct that declares the TinyGUYS.
    prt_galsc_parameter_struct(parameters);
  
    // Then we print the code.
    
    // The C declarations first.
    enable_line_directives();
    prt_toplevel_declarations(all_cdecls);
    disable_line_directives();

    // Print declaration for the port queues, along with queue pointers.
    //prt_galsc_ports(connections);
    prt_galsc_ports(ports);

    // Print the GALSC_sched_init() function, which initializes the
    // galsC scheduler data structures.
    prt_galsc_sched_init_function(ports, parameters);

    dd_scan (mod, modules)
        prt_nesc_function_declarations(DD_GET(nesc_declaration, mod));

    // FIXME comment
    prt_galsc_port_function_declarations(ports);
    prt_galsc_parameter_function_declarations(parameters);

    enable_line_directives();

    // Print the function implementations and the module variables.
    dd_scan (mod, modules)
        prt_nesc_module(cg, DD_GET(nesc_declaration, mod));

    // Call chain:
    // prt_inline_functions() -> topological_prt() -> prt_nesc_function() ->
    //   prt_nesc_connection_function().
    //
    // prt_nesc_connection_function() prints an intermediate function
    // for each function in a call chain.  If the call chain contains
    // a port, then print the port$get() and port$put()
    // functions. Then print an intermediate function which calls the
    // port$put() function instead of the next function in the call
    // chain.
    prt_inline_functions(callgraph);
    prt_noninline_functions(callgraph);

  // GALSC FIXME: nido
  /*
  if (use_nido)
    {
      disable_line_directives();
      prt_nido_initialize(modules); 
    }
   */

    disable_line_directives();
    // FIXME comment
    prt_galsc_port_functions(ports);
    prt_galsc_parameter_functions(parameters);
    
    // Print the GALSC_sched_start() function, which creates initial
    // tokens in the ports.
    prt_galsc_sched_start_function(appstart);

    unparse_end();
    
    if (output)
        fclose(output);
}
