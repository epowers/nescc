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
   This file is based on nesc-env.c

 */

#include "parser.h"
#include "env.h"
#include "nesc-env.h"
#include "nesc-decls.h"
#include "nesc-semantics.h"
#include "c-parse.h"
#include "semantics.h"

#include "galsc-env.h"

/* Top-level galsc environment. Keeps track of loaded interfaces and
   components, loads them on demand */

// Separating galsc-env.c from nesc-env.c allows .gc and .nc files to
// have different name spaces.

/* The environments for actors */
static env galsc_env;


/* hack, to give the doc generation an easy way to list interfaces & components */
env get_galsc_env(void)
{
  return galsc_env;
}

void init_galsc_env(region r)
{
  galsc_env = new_env(r, NULL);
}

void galsc_declare(nesc_declaration d)
{
  env_add(galsc_env, d->name, d);
}

nesc_declaration galsc_lookup(const char *name)
{
  return env_lookup(galsc_env, name, FALSE);
}

nesc_declaration galsc_require(source_language sl, location l, const char *name)
{
    assert(sl == l_actor);
    
  nesc_declaration d = galsc_lookup(name);

  if (!d)
    d = load(sl, l, name, FALSE);

  if (sl != d->kind)
    {
      error_with_location(l, "expected %s `%s', but got %s %s",
			  language_name(sl), name,
			  d->kind == l_interface ? "an" : "a",
			  language_name(d->kind));

      /* Make a dummy declaration to make everyone happy */
      d = new_nesc_declaration(parse_region, sl, name);
      d->env = new_environment(parse_region, global_env, TRUE, FALSE);
      build(d, dummy_nesc_decl(sl, d->ast->location, name));
    }

  return d;
}
