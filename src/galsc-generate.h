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

#ifndef GALSC_GENERATE_H
#define GALSC_GENERATE_H

#include "nesc-cg.h"
void galsc_generate_c_code(nesc_declaration program, const char *target_name,
        cgraph cg, dd_list modules, dd_list ports, dd_list parameters, dd_list appstart);

// Data structure for one line of appstart section.
struct appstart_data {
    // The end point corresponding to the port.
    endp ep;

    // The AST node for the corresponding line of the appstart section.
    start_function_call call;
};

void prt_galsc_port_get_function_header(data_declaration p);
void prt_galsc_port_put_function_header(data_declaration p);

void prt_galsc_port_get_function(data_declaration p);
void prt_galsc_port_put_function(data_declaration p);
//void prt_galsc_port_put_function(struct connections *c, type return_type);
//void prt_galsc_port_get_function(struct connections *c, type return_type);

bool galsc_prt_ncf_direct_calls(struct connections *c,
			  dd_list/*<full_connection>*/ calls,
        type return_type);

#include "unparse.h"
void galsc_prt_parameters(declaration gparms, declaration parms, psd_options options, data_declaration ddecl);
#include "nesc-generate.h"
int galsc_prt_parameter_get_call(struct connections *c,
        full_connection ccall, psd_options options, function_declarator called_fd, int first_arg);

#endif
