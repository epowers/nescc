#include <stdlib.h>

#include "parser.h"
#include "nesc-magic.h"
#include "semantics.h"
#include "AST_utils.h"
#include "c-parse.h"
#include "constants.h"
#include "unparse.h"

static void declare_magic(const char *name,
			  type return_type, typelist argument_types,
			  known_cst (*magic_fold)(function_call fcall, int pass))
{
  struct data_declaration tempdecl;
  type ftype = make_function_type(return_type, argument_types, FALSE, FALSE);

  init_data_declaration(&tempdecl, new_error_decl(parse_region, dummy_location), name, ftype);
  tempdecl.kind = decl_magic_function;
  tempdecl.magic_fold = magic_fold;
  tempdecl.ftype = function_normal;

  declare(global_env, &tempdecl, FALSE);
}

data_declaration get_magic(function_call fcall)
/* Returns: magic function called by fcall if it's a magic function call,
     NULL otherwise
*/
{
  if (is_identifier(fcall->arg1))
    {
      identifier called = CAST(identifier, fcall->arg1);

      if (called->ddecl->kind == decl_magic_function)
	return called->ddecl;
    }
  return NULL;
}


known_cst fold_magic(function_call fcall, int pass)
{
  data_declaration called = get_magic(fcall);

  if (called)
    {
      /* we can assume arguments are of valid type and number,
	 check that they are constants in the parse phase */

      if (pass == 0)
	{
	  bool all_constant = TRUE;
	  expression arg;
	  int argn = 1;

	  scan_expression (arg, fcall->args)
	    {
	      if (!(arg->cst || is_string(arg)))
		{
		  error("argument %d to magic function `%s' is not constant",
			argn, called->name);
		  all_constant = FALSE;
		}
	      argn++;
	    }
	  
	  if (!all_constant)
	    return NULL;
	}

      return called->magic_fold(fcall, pass);
    }
  return NULL;
}

static env unique_env;
static region unique_region;

static unsigned int *unique_parse(const char *uname, function_call fcall)
{
  expression name = fcall->args;
  unsigned int *lastval;
  const wchar_t *name_wstr;
  char *name_str;
  int length_as_str;

  if (!is_string(name))
    {
      error("argument to `%s' must be a string", uname);
      return NULL;
    }

  name_wstr = CAST(string, name)->ddecl->chars;
  length_as_str = wcs_mb_size(name_wstr);
  if (length_as_str < 0)
    {
      error("can't handle this string as argument to `%s'", uname);
      return NULL;
    }
  name_str = alloca(length_as_str);
  length_as_str = wcstombs(name_str, name_wstr, length_as_str);
  assert(length_as_str >= 0);

  lastval = env_lookup(unique_env, name_str, FALSE);
  if (!lastval)
    {
      lastval = ralloc(unique_region, int);
      name_str = rstrdup(unique_region, name_str);
      env_add(unique_env, name_str, lastval);
    }

  return lastval;
}

static known_cst unique_fold(function_call fcall, int pass)
{
  unsigned int *lastval = unique_parse("unique", fcall);

  if (lastval)
    {
      /* On pass 0, we don't know the value
	 On pass 1, we pick a value
	 On subsequent passes, we stick to our choice
      */
      if (pass == 0)
	return make_unknown_cst(unsigned_int_type);
      else if (pass == 1)
	return make_unsigned_cst((*lastval)++, unsigned_int_type);
      else
	return fcall->cst;
    }
  else
    return NULL;
}

static known_cst uniqueCount_fold(function_call fcall, int pass)
{
  unsigned int *lastval = unique_parse("unique", fcall);

  if (lastval)
    {
      /* On pass 0, we don't know the value
	 On pass 1, we still don't know (haven't seen all uniques)
	 On pass 2 and subsequent, we get the value from the unique env
      */
      if (pass < 2)
	return make_unknown_cst(unsigned_int_type);
      else
	return make_unsigned_cst(*lastval, unsigned_int_type);
    }
  else
    return NULL;
}

static void unique_init(void)
{
  typelist string_args;

  string_args = new_typelist(parse_region);
  typelist_append(string_args, make_pointer_type(char_type));
  declare_magic("unique", unsigned_int_type, string_args, unique_fold);
  declare_magic("uniqueCount", unsigned_int_type, string_args,
		uniqueCount_fold);
  unique_region = newregion();
  unique_env = new_env(unique_region, NULL);
}

void init_magic_functions(void)
{
  unique_init();
}
