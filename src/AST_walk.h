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

#ifndef AST_WALK_H
#define AST_WALK_H

/* A generic, OO-ish AST walker.
   This is probably a visitor, for those into that kind of thing. */

/* Untyped to make declaration easier. Actual signature is
     AST_walker_result AST_walker_fn(AST_walker spec, void *data,
  				     <your node type> n);
*/
typedef enum {
  aw_walk, /* walk children */
  aw_call_parent, /* call parent function */
  aw_done  /* don't walk children */
} AST_walker_result;

typedef AST_walker_result (*AST_walker_fn)();
typedef struct AST_walker *AST_walker;

AST_walker new_AST_walker(region r);
/* Effcts: creates a new AST walker in region r. Default behaviour
     is to just walk through the children (i.e., a function returning
     aw_walk) */

void AST_walker_handle(AST_walker spec, AST_kind kind, AST_walker_fn fn);
/* Effects: Sets walker function for node kind and all its children to
     fn
*/

/* Recursive walk from n */
void AST_walk(AST_walker spec, void *data, node n);

void AST_walk_list(AST_walker spec, void *data, node n);

/* Walk children of n */
void AST_walk_children(AST_walker spec, void *data, node n);

/* Just execute the walker function for node-type 'kind' */
AST_walker_result AST_walker_call(AST_walker spec, AST_kind kind,
				  void *data, node n);

#endif
