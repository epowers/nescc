/* This file is part of the nc2MoMLlib generator.

   Copyright (C) 2004 Elaine Cheong

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

/*
  Author: Elaine Cheong
  Created: 12 March 2004
  
 */

#ifndef MOML_GENERATE_H
#define MOML_GENERATE_H

struct momllib_nesccomponent_t {
    nesc_declaration component;
    const char *filename;
};

typedef struct momllib_nesccomponent_t *nesccomponent_t;

void moml_set_outdir(const char *dir);

void momllib_set();

bool is_momllib_set();

void moml_ptinyos_component_set(const char *classname);
void moml_ptinyos_prefix_set(const char *classname);
const char *moml_ptinyos_prefix_get();

bool generate_momllib(dd_list nesccomponents, const char *inputpathname);

#endif
