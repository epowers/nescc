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

/* Magic function support. Magic functions are constant folded at compile-time,
   all their arguments must be constants (or string constants).

   They are used to pick unique numbers for generic interfaces, lookup
   active message types, etc
*/

#ifndef NESC_MAGIC_H
#define NESC_MAGIC_H

void init_magic_functions(void);

expression magic_reduce(function_call fcall);
bool magic_print(function_call fcall);

#endif
