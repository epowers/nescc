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

void generate_c_code(nesc_declaration program, const char *target_name,
		     cgraph cg, dd_list modules);

/* The address of these variables shows up on the edges of callgraphs 
   to indicate edges which are calls, respectively atomic calls
   (use edges have NULL data) */
extern int call_edge;
extern int atomic_call_edge;


#endif
