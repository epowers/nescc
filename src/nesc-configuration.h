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

#ifndef NESC_CONFIGURATION_H
#define NESC_CONFIGURATION_H

void process_configuration(configuration c);

void component_scan(data_declaration cref, env_scanner *scan);

#ifdef GALSC
#include "nesc-cg.h"
void process_component_connection(cgraph cg, connection conn,
        struct endp p1, struct endp p2);
void process_connection(cgraph cg, connection conn,
        struct endp p1, struct endp p2);
void check_generic_arguments(expression args, typelist gparms);
int match_endpoints(endp p1, endp p2, endp amatch);
#endif

#endif
