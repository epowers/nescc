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

#ifndef GALSC_A_H
#define GALSC_A_H

bool lookup_endpoint(environment configuration_env, endpoint ep,
        endp lep);

struct galsc_parameter_connection {
    connection conn;
    environment configuration_env;
};
typedef struct galsc_parameter_connection *galsc_parameter_connection;

galsc_parameter_connection new_galsc_parameter_connnection(region r);
galsc_parameter_connection get_galsc_parameter_connection(dd_list pconns, endp target, endp source);

#endif
