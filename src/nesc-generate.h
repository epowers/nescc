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

#ifndef NESC_GENERATE_H
#define NESC_GENERATE_H

#include "nesc-cg.h"

#define NESC_THISPTR_LITERAL "__NESC_THIS"
#define NESC_INSTANCEARR_LITERAL "__NESC_INSTANCES"
#define NESC_INSTANCETYPE_LITERAL "__NESC_INSTANCETYPE"
#define NESC_INSTANCENUM_LITERAL "_INSTANCENUM"

void output_instanceref(nesc_declaration module, int instancenum);
void output_instancetype(nesc_declaration mod);
void output_thisptr(nesc_declaration module);

void generate_c_code(nesc_declaration program, const char *target_name,
		     cgraph cg, dd_list modules);

#endif
