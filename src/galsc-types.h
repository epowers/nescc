/* This file is part of the galsC compiler.
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

#ifndef GALSC_TYPES_H
#define GALSC_TYPES_H

bool match_parameter_get(endp target, endp source);
bool match_parameter_getput(endp target, endp source);
bool match_parameter_put(endp target, endp source);

void galsc_type_check(region r, cgraph master, dd_list ports, dd_list parameters);

#endif
