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

#ifndef NESC_ENV_H
#define NESC_ENV_H

/* Top-level nesc environment. Keeps track of loaded interfaces and
   components, loads them on demand */

nesc_declaration new_nesc_declaration(region r, source_language kind,
				      const char *name);

void init_nesc_env(region r);
env get_nesc_env(void);
void nesc_declare(nesc_declaration d);
nesc_declaration nesc_lookup(const char *name);
nesc_declaration require(source_language sl, location l, const char *name);
void require_c(location l, const char *name);

#endif
