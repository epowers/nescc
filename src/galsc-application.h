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

/*
   This file is based on:
   - nesc-component.h
   - nesc-configuration.h
   
 */

#ifndef GALSC_APPLICATION_H
#define GALSC_APPLICATION_H

void set_galsc_main(const char *name);
void build_application(region r, nesc_declaration cdecl);

#endif
