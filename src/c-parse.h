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

#ifndef C_PARSE_H
#define C_PARSE_H

#include "AST.h"

/* EC: YYSTYPE is used by Bison to set the type of a symbol (a
 * terminal or non-terminal). */
struct yystype {
  union {
    void *ptr;
    asm_operand asm_operand;
    asm_stmt asm_stmt;
    attribute attribute;
    lexical_cst constant;
    string_cst string_cst;
    declaration decl;
    declarator declarator;
    nested_declarator nested;
    expression expr;
    id_label id_label;
    label label;
    node node;
    statement stmt;
    conditional_stmt cstmt;
    for_stmt for_stmt;
    string string;
    type_element telement;
    asttype type;
    word word;
    designator designator;
    rp_interface rplist;
    interface_ref iref;
    component_ref cref;
    connection conn;
    endpoint ep;
    parameterised_identifier pid;
    implementation impl;
    environment env;
    dd_list fields;
    char *docstring;

#ifdef GALSC
      // See nodetypes.def for AST field definitions.
      // See c-parse.y for usage of these fields in building the AST.
      //   Example: %type <u.start> ...
    start_function_call start;
      //global_connection gconn;
    actor_ref aref;
    port_ref pref;
    port_interface portlist;
#endif
      
    struct {
      location location;
      int i;
    } itoken;

    struct {
      expression expr;
      int i;
    } iexpr;

    struct {
      statement stmt;
      int i;
    } istmt;

  } u;

  struct {
    location location;
    cstring id;
    data_declaration decl;
  } idtoken;
};

#define YYSTYPE struct yystype

/* Region in which to allocate parse structures. Idea: the AST user can set
   this to different regions at appropriate junctures depending on what's
   being done with the AST */
extern region parse_region;

/* TRUE if currently parsing an expression that will not be evaluated (argument
   to alignof, sizeof. Currently not typeof though that could be considered
   a bug) */
bool unevaluated_expression(void);

nesc_decl parse(void) deletes;

declaration make_error_decl(void);

#endif
