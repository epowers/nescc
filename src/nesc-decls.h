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

#ifndef NESC_DECLS_H
#define NESC_DECLS_H

typedef struct nesc_declaration {
  source_language kind; /* l_interface or l_component */
  const char *name;
  nesc_decl ast;
  struct environment *env;
  char *short_docstring;  /* For documentation comments */
  char *long_docstring;

  /* for components */
  implementation impl;
  struct cgraph *connections;
  bool is_abstract;
  int abstract_instance_count;
  typelist abs_param_typelist;

} *nesc_declaration;

#endif
