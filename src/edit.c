/* This file is part of the galsC compiler.

This file is derived from the nesC compiler, which is derived from the
RC Compiler. It is thus
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
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

#include "parser.h"
#include "edit.h"
#include "semantics.h"
#include "stmt.h"
#include "constants.h"
#include "AST_utils.h"
#include "unparse.h"

nesc_decl parsed_nesc_decl;

/* Declare a new temporary that can be assigned a value of type t.
   Place the declaration at the start of block. 
   XXX: See discussion in types.c:tag2ast about the (lack of) correctness of
   this approach.
   Return it's declaration */
data_decl build_declaration(region r, type t, const char *name)
{
  identifier_declarator id;
  variable_decl vd;
  data_decl dd;
  declarator tdeclarator;
  type_element tmodifiers;

  /* Compute real type, name */
  if (type_array(t))
    t = make_pointer_type(type_array_of(t));
  else if (type_function(t))
    t = make_pointer_type(t);
  /* Qualifiers must not be present on the temp (the qualifiers of t apply
     to the original location we are building a temp for only) */
  t = make_qualified_type(t, no_qualifiers);

  /* Build AST for the declaration */
  id = new_identifier_declarator(r, dummy_location, str2cstring(r, name));
  type2ast(r, dummy_location, t, CAST(declarator, id), &tdeclarator, &tmodifiers);
  vd = new_variable_decl(r, dummy_location, tdeclarator, NULL, NULL, NULL, NULL);
  vd->declared_type = t;
  dd = new_data_decl(r, dummy_location, tmodifiers, CAST(declaration, vd));

  return dd;
}

word build_word(region r, const char *cword)
{
  return new_word(r, dummy_location, str2cstring(r, cword));
}
