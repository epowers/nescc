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

#ifndef NESC_SEMANTICS_H
#define NESC_SEMANTICS_H

struct environment;

#include "nesc-decls.h"

extern declaration cdecls;

bool nesc_filename(const char *name);

#ifdef GALSC
// Retrns true if "name" is a galsC file (.gc).
bool galsc_filename(const char *name);
#endif

const char *element_name(region r, const char *path);
/* Returns: Return the "identifier part"
     of path, i.e., remove any directory and extension
     The returned string is allocated in region r.
*/

nesc_decl compile(location loc, source_language l,
		  const char *name, bool name_is_path,
		  nesc_declaration container, struct environment *parent_env);

nesc_declaration load(source_language sl, location l,
		      const char *name, bool name_is_path);

type get_actual_function_type(type t);
/* Returns: The actual function type for a (possibly generic) type t
     representing the type of a function/command/event
 */

function_declarator ddecl_get_fdeclarator(data_declaration fndecl);
/* Effects: Returns fndecl's function_declarator
   Requires: fndecl represent a function
*/

declaration ddecl_get_gparms(data_declaration fndecl);
/* Effects: Returns the declaration list for fndecl's generic parameters 
   Requires: fndecl represent a function
*/

bool ddecl_is_command_or_event(data_declaration decl);

bool nesc_attribute(attribute a);
/* Returns: TRUE if a is a nesc-specific attribute
 */

const char *language_name(source_language l);

nesc_decl dummy_nesc_decl(source_language sl, location loc, const char *name);
void build(nesc_declaration decl, nesc_decl ast);

bool is_module_variable(data_declaration ddecl);
/* Returns: TRUE if ddecl is a module variable
   (this includes is_module_local_static)
 */

bool is_module_local_static(data_declaration ddecl);
/* Returns: TRUE if ddecl is a local static variable inside a module
 */

const char *make_intf_printname(const char *iname, const char *fname);
/* Returns: string "iname.fname" allocated in current.fileregion
 */

const char *decl_printname(data_declaration ddecl);
/* Returns: The printable name for ddecl 
     (interface.name for commands or events in interfaces,
      just the name otherwise)
     Any necessary memory is allocated in current.fileregion
*/

data_declaration get_function_ddecl(expression e);
/* Returns: If e denotes a specific function, return its data_declaration
     Otherwise return NULL
*/

void handle_combine_attribute(location loc, const char *combiner, type *t);
/* Effects: handle combine attribute specifying function 'combiner', 
     modifying *t as appropriate
 */

/* Some macros to make nesc_error easier to deal with */
#define nesc_warning (nesc_error ? error : warning)
#define nesc_warning_with_location (nesc_error ? error_with_location : warning_with_location)

#endif
