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


#ifndef MOML_PATHS_H
#define MOML_PATHS_H

#include <sys/types.h>
#include <dirent.h>

DIR * init_nesc_dir(region r, const char *path);
const char *get_next_nesc_file_in_dir(DIR *nescdir);
bool is_nesc_file(const char *filename);

#endif
