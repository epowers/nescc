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

#ifndef CSTRING_H
#define CSTRING_H

#include <regions.h>

#include "regions.h"

/* A C string is like a regular C string, but with a length. A null byte is
   added AFTER the string for convenience */
typedef struct {
  char *data;
  int length;
} cstring;

/* Make a new cstring with a copy of s, length l */
cstring make_cstring(region r, const char *s, int l);


/* Make a new cstring with a copy of regular C string s */
cstring str2cstring(region r, const char *s);

#endif
