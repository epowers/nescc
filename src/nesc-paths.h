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

#ifndef NESC_PATHS_H
#define NESC_PATHS_H
/* Locate components/interfaces from their name */

void init_nesc_paths_start(region r);
void add_nesc_path(const char *path);
void add_nesc_dir(const char *path);
void init_nesc_paths_end(void);

const char *find_nesc_file(region r, source_language l, const char *name);

extern char **path_argv;
extern int path_argv_count;

#endif
