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

#ifndef NESC_COMPONENT_H
#define NESC_COMPONENT_H

void build_component(region r, nesc_declaration cdecl);

void declare_interface_ref(interface_ref iref, declaration gparms,
			   environment genv);

void make_implicit_interface(data_declaration fndecl,
			     function_declarator fdeclarator);

void check_generic_parameter_type(location l, data_declaration gparm);

environment start_implementation(void);

void interface_scan(data_declaration iref, env_scanner *scan);
data_declaration interface_lookup(data_declaration iref, const char *name);

void component_functions_iterate(nesc_declaration c,
				 void (*iterator)(data_declaration fndecl,
						  void *data),
				 void *data);

#ifdef GALSC
typelist make_gparm_typelist(declaration gparms);

#include "nesc-cg.h"

struct beg_data
{
  cgraph cg;
};
void beg_iterator(data_declaration fndecl, void *data);
cgraph build_external_graph(region r, nesc_declaration cdecl);
#endif

#endif
