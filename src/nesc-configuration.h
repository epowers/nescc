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

#ifndef NESC_CONFIGURATION_H
#define NESC_CONFIGURATION_H

typedef struct nesc_configuration_instance {
  configuration configuration;
  int instance_number;
  environment ienv;
} *nesc_configuration_instance;

void init_configuration_instance(nesc_configuration_instance cinst, configuration conf);
void process_configuration(configuration c, nesc_configuration_instance cinst);
void component_scan(data_declaration cref, env_scanner *scan);
void process_abstract_params(configuration c);

#endif
