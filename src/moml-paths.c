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

#include "parser.h"
#include <sys/stat.h>
#include <unistd.h>

#include <sys/types.h>
#include <dirent.h>

#include "nesc-paths.h"

static region momlpathregion;

struct nesc_dir_t {
    const char *path;
};

static struct nesc_dir_t nesc_dir_info;

// Given the full path to a directory containing the .nc files, store
// the path, and return a pointer to the directory.
DIR * init_nesc_dir(region r, const char *path) {
    momlpathregion = r;
    // Make sure last character of the path is '/'
    nesc_dir_info.path = canonicalise(momlpathregion, path, strlen(path));
    return opendir(path);
}

// Return the filename (path + name) of the next .nc file in nescdir.
// Advance through files that are not .nc files.  If no more files,
// return NULL.
const char *get_next_nesc_file_in_dir(DIR *nescdir) {
    struct dirent *nescfile;
    struct stat sbuf;

    while ((nescfile = readdir(nescdir))) {
        char *nescfilename = rarrayalloc(momlpathregion, strlen(nescfile->d_name) + 1, char);
        sprintf(nescfilename, "%s", nescfile->d_name);

        char *full_nescfilename = rarrayalloc(momlpathregion, strlen(nesc_dir_info.path) + strlen(nescfilename) + 1, char);
        sprintf(full_nescfilename, "%s%s", nesc_dir_info.path, nescfilename);

        // Get the information on the file in full_nescfilename
        stat(full_nescfilename, &sbuf);

        // If it is a regular file (not a directory), return the short filename.
        if (S_ISREG(sbuf.st_mode)) {
            return full_nescfilename;
        }
    }
    return NULL;
}

bool is_nesc_file(const char *filename) {
    if (strstr(filename, ".nc"))
        return TRUE;
    return FALSE;
}

// See element_name() in nesc-semantics.c
const char *element_pathandname(region r, const char *path)
/* Returns: Return the "extended identifier part"
     of path, i.e., remove any extension (but not directory)
     The returned string is allocated in region r.
*/
{
  const char *base, *dot;

  //base = basename((char *)path);
  base = path;
  dot = strrchr(base, '.');

  if (dot)
    {
      /* Extract id */
      char *copy = rarrayalloc(r, dot - base + 1, char);

      memcpy(copy, base, dot - base);
      copy[dot - base] = '\0';

      return copy;
    }
  else
    return rstrdup(r, base);
}

   

