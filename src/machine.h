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

#ifndef MACHINE_H
#define MACHINE_H

typedef struct {
  size_t size, align;
} machine_type_spec;

typedef struct {
  const char *machine_name;

  bool pcc_bitfield_type_matters;
  machine_type_spec tptr, tfloat, tdouble, tlong_double, tshort, tint,
    tlong, tlong_long;
  size_t int1_align, int2_align, int4_align, int8_align;
  size_t wchar_t_size, size_t_size;
  bool char_signed, wchar_t_signed;

  const char *gcc_compiler;

  bool (*decl_attribute)(attribute attr, data_declaration ddecl);
  bool (*tag_attribute)(attribute attr, tag_declaration tdecl);
  bool (*field_attribute)(attribute attr, field_declaration fdecl);
  bool (*type_attribute)(attribute attr, type *t);
  
} machine_spec;

extern machine_spec *target;

bool select_target(const char *targetname);

#endif
