/* This file is part of the nesC compiler.

This file is derived from the RC Compiler. It is thus
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
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
Boston, MA 02111-1307, USA. */

/* ASSUME: (float)(long double)f == f
           (double)(long double)d == d
*/
/* XXX: overflow detection (signed ints only, gcc doesn't do reals)
	initialised arrays are missing their size
*/

/* Note: legal ops on complex are:
   +, -, *, /, ==, !=, &&, ||, __real__, __imag__, ~ (conjugate)
*/

#define _ISOC99_SOURCE
#include "parser.h"
#include "constants.h"
#include "c-parse.h"
#include "semantics.h"
#include "cval.h"
#include "nesc-magic.h"
#include <stdlib.h>
#include "AST_utils.h"

static known_cst new_known_cst(region r, type t, cval c)
{
  known_cst kc = ralloc(parse_region, struct known_cst);

  kc->type = t;
  kc->cval = c;

  return kc;
}
  
known_cst make_unknown_cst(type t)
{
  return new_known_cst(parse_region, t, cval_unknown);
}

known_cst make_cst(cval c, type t)
{
  if (cval_istop(c))
    return NULL;

  return new_known_cst(parse_region, t, cval_cast(c, t));
}

known_cst make_address_cst(data_declaration ddecl, label_declaration ldecl,
			   largest_int offset, type t)
{
  return make_cst(make_cval_address(ddecl, ldecl, offset), t);
}

known_cst make_unsigned_cst(largest_uint x, type t)
{
  return make_cst(make_cval_unsigned(x, t), t);
}

known_cst make_signed_cst(largest_int x, type t)
{
  return make_cst(make_cval_signed(x, t), t);
}

static known_cst cast_constant(known_cst c, type to)
{
  if (type_equal(c->type, to))
    return c;

  return make_cst(cval_cast(c->cval, to), to);
}

known_cst fold_label_address(expression e)
{
  return make_address_cst(NULL, CAST(label_address, e)->id_label->ldecl, 0, e->type);
}

known_cst fold_sizeof(expression e, type stype)
{
  if (type_size_cc(stype))
    return make_unsigned_cst(type_size(stype), e->type);
  else
    return NULL;
}

known_cst fold_alignof(expression e, type atype)
{
  if (type_has_size(atype))
    return make_unsigned_cst(type_alignment(atype), e->type);
  else
    /* compile-time error, alignof incomplete type */
    return make_unsigned_cst(1, e->type); 
}

known_cst fold_cast(expression e)
{
  cast cc = CAST(cast, e);

  if (cc->arg1->cst && !type_void(e->type))
    return cast_constant(cc->arg1->cst, e->type);
  else
    return NULL;
}

/* +, -, ! and ~ only */
known_cst fold_unary(expression e)
{
  unary u = CAST(unary, e);
  known_cst arg = u->arg1->cst;
  type t = u->type;

  if (arg)
    {
      switch (u->kind)
	{
	case kind_unary_plus:
	  /* Note that this allows +(int)&x to be a constant... 
	     This is consistent with gcc */
	  return cast_constant(arg, t); /* essentially a no-op */

	case kind_unary_minus:
	  return make_cst(cval_negate(cval_cast(arg->cval, t)), t);

	case kind_not:
	  return make_cst(cval_not(cval_cast(arg->cval, t)), t);

	case kind_bitnot:
	  return make_cst(cval_bitnot(cval_cast(arg->cval, t)), t);

	case kind_conjugate:
	  return make_cst(cval_conjugate(cval_cast(arg->cval, t)), t);

	case kind_realpart:
	  if (!type_complex(u->arg1->type))
	    return cast_constant(arg, t);
	  else
	    return make_cst(cval_realpart(cval_cast(arg->cval, t)), t);

	case kind_imagpart:
	  if (!type_complex(u->arg1->type))
	    return make_cst(cval_cast(cval_zero, t), t);
	  else
	    return make_cst(cval_imagpart(cval_cast(arg->cval, t)), t);

	default:
	  assert(0);
	  break;
	}
    }
  return NULL;
}

/* XXX: overflow */
static known_cst fold_sub(type restype, known_cst c1, known_cst c2)
{
  type t1 = c1->type, t2 = c2->type, basetype, ct;
  cval s = make_cval_unsigned(1, size_t_type), res;

  basetype = type_pointer(t1) ? t1 : type_pointer(t2) ? t2 : NULL;
  if (basetype)
    {
      basetype = type_points_to(basetype);

      if (!type_size_cc(basetype))
	return NULL;

      s = make_cval_unsigned(type_size(basetype), size_t_type);
      ct = intptr_type;
    }
  else
    ct = restype;

  s = cval_cast(s, ct);
  if (type_pointer(t1) && type_pointer(t2))
    res = cval_divide(cval_cast(cval_sub(cval_cast(c1->cval, ct), cval_cast(c2->cval, ct)), ct),
		      s);
  else
    res = cval_sub(cval_cast(c1->cval, ct),
		   cval_divide(cval_cast(c2->cval, ct), s));

  return make_cst(res, restype);
}

/* XXX: overflow */
known_cst fold_add(type restype, known_cst c1, known_cst c2)
{
  type t1 = c1->type, t2 = c2->type, ct;
  cval s = make_cval_unsigned(1, size_t_type);

  if (type_pointer(t2))
    {
      known_cst ctmp;
      type ttmp;

      ctmp = c1; c1 = c2; c2 = ctmp;
      ttmp = t1; t1 = t2; t2 = ttmp;
    }

  if (type_pointer(t1))
    {
      type basetype = type_points_to(t1);

      if (!type_size_cc(basetype))
	return NULL;

      s = make_cval_unsigned(type_size(basetype), size_t_type);
      ct = intptr_type;
    }
  else
    ct = restype;

  return make_cst(cval_add(cval_cast(c1->cval, ct),
			   cval_times(cval_cast(c2->cval, ct), cval_cast(s, ct))),
		  restype);
}

known_cst fold_binary(type t, expression e)
{
  binary b = CAST(binary, e);
  known_cst c1 = b->arg1->cst, c2 = b->arg2->cst;
  type t1 = type_default_conversion(b->arg1->type),
    t2 = type_default_conversion(b->arg2->type);
  
  if (b->kind == kind_andand || b->kind == kind_oror)
    {
      if (c1 && constant_knownbool(c1))
	{
	  bool c1val = constant_boolvalue(c1);

	  if (b->kind == kind_andand ? !c1val : c1val)
	    return make_signed_cst(b->kind == kind_oror, t);
	}

      if (c1 && c2)
	{
	  if (constant_knownbool(c2))
	    return make_signed_cst(constant_boolvalue(c2), t);
	  else
	    return make_unknown_cst(t);
	}
    }
  else if (c1 && c2)
    {
      cval cv1 = c1->cval, cv2 = c2->cval;

      switch (b->kind)
	{
	case kind_plus: case kind_array_ref:
	  return fold_add(t, c1, c2);

	case kind_minus:
	  return fold_sub(t, c1, c2);

	case kind_times: case kind_divide: case kind_modulo:
	case kind_lshift: case kind_rshift:
	case kind_bitand: case kind_bitor: case kind_bitxor: {
	  cval res;

	  cv1 = cval_cast(cv1, t);
	  cv2 = cval_cast(cv2, t);

	  switch (b->kind) {
	  case kind_times: res = cval_times(cv1, cv2); break;
	  case kind_divide: res = cval_divide(cv1, cv2); break;
	  case kind_modulo: res = cval_modulo(cv1, cv2); break;
	  case kind_lshift: res = cval_lshift(cv1, cv2); break;
	  case kind_rshift: res = cval_rshift(cv1, cv2); break;
	  case kind_bitand: res = cval_bitand(cv1, cv2); break;
	  case kind_bitor: res = cval_bitor(cv1, cv2); break;
	  case kind_bitxor: res = cval_bitxor(cv1, cv2); break;
	  default: abort(); return NULL;
	  }
	  return make_cst(res, t);
	}
	case kind_eq: case kind_ne:
	case kind_leq: case kind_geq: case kind_lt: case kind_gt: {
	  cval res;
	  type ct;

	  /* Pointers win. */
	  if (type_pointer(t1) || type_pointer(t2))
	    ct = intptr_type; 
	  else
	    ct = common_type(t1, t2);

	  cv1 = cval_cast(cv1, ct);
	  cv2 = cval_cast(cv2, ct);

	  switch (b->kind) {
	  case kind_eq: res = cval_eq(cv1, cv2); break;
	  case kind_ne: res = cval_ne(cv1, cv2); break;
	  case kind_leq: res = cval_leq(cv1, cv2); break;
	  case kind_geq: res = cval_geq(cv1, cv2); break;
	  case kind_lt: res = cval_lt(cv1, cv2); break;
	  case kind_gt: res = cval_gt(cv1, cv2); break;
	  default: abort(); return NULL;
	  }
	  return make_cst(res, t);
	}
	default:
	  assert(0); return NULL;
	}
      }

  return NULL;
}

known_cst fold_conditional(expression e)
{
  conditional c = CAST(conditional, e);
  known_cst cond = c->condition->cst;

  if (cond && constant_knownbool(cond))
    {
      expression value =
	constant_boolvalue(cond) ? (c->arg1 ? c->arg1 : c->condition) : c->arg2;

      e->static_address = value->static_address;
      if (value->cst)
	return cast_constant(value->cst, e->type);
      else
	return NULL;
    }
  else if (cond && (!c->arg1 || c->arg1->cst) && c->arg2->cst)
    return make_unknown_cst(e->type);

  return NULL;
}

known_cst fold_function_call(expression e, int pass)
{
  function_call fce = CAST(function_call, e);

  if (call_to(builtin_constant_p, fce) && fce->args)
    return make_signed_cst(fce->args->cst != NULL, int_type);

  return fold_magic(fce, pass);
}

known_cst fold_identifier(expression e, data_declaration decl, int pass)
{
  if (decl->kind == decl_constant)
    // We don't know template arg values at parse time
    return pass == 0 && decl->substitute ?
      make_unknown_cst(e->type) :
      decl->value;
  else
    return NULL;
}

known_cst foldaddress_identifier(expression e, data_declaration decl)
{
  if ((decl->kind == decl_function && decl->ftype != function_nested) ||
      (decl->kind == decl_variable && !decl->islocal) ||
      decl->kind == decl_magic_string)
    return make_address_cst(decl, NULL, 0, make_pointer_type(e->type));
  else
    return NULL;
}

known_cst foldaddress_string(string s)
{
  return make_address_cst(s->ddecl, NULL, 0, s->type);
}

known_cst foldaddress_field_ref(expression e)
{
  field_ref fref = CAST(field_ref, e);
  field_declaration fdecl = fref->fdecl;
  known_cst object = fref->arg1->static_address;

  if (!object || !fdecl->offset_cc || fdecl->bitwidth >= 0)
    return NULL;

  if (constant_unknown(object))
    return make_unknown_cst(object->type);

  return make_cst(cval_add(object->cval,
			   make_cval_unsigned(fdecl->offset / BITSPERBYTE, size_t_type)),
		  make_pointer_type(fdecl->type));
}

#ifndef HAVE_STRTOLD
#define strtold strtod
#endif

lexical_cst fold_lexical_real(type realtype, location loc, cstring tok)
{
  lexical_cst c = new_lexical_cst(parse_region, loc, tok);
  cval realvalue = make_cval_float(strtold(tok.data, NULL));

  if (type_complex(realtype))
    realvalue = make_cval_complex(cval_cast(cval_zero, realtype), realvalue);
  c->cst = make_cst(realvalue, realtype);
  c->type = realtype;
  return c;
}

lexical_cst fold_lexical_char(location loc, cstring tok,
			      bool wide_flag, int charvalue)
{
  lexical_cst c = new_lexical_cst(parse_region, loc, tok);
  type ctype = wide_flag ? wchar_type : int_type;

  c->type = ctype;
  c->cst = make_cst(type_unsigned(ctype) ?
		    make_cval_unsigned(charvalue, ctype) :
		    make_cval_signed(charvalue, ctype),
		    ctype);
  return c;
}

string_cst fold_lexical_string(location loc, cstring tok,
			       bool wide_flag, wchar_array stringvalue)
{
  size_t length = wchar_array_length(stringvalue);
  wchar_t *chars = rarrayalloc(parse_region, length + 1, wchar_t);
  string_cst c = new_string_cst(parse_region, loc, tok, chars, length);

  c->type = wide_flag ? wchar_array_type : char_array_type;
  memcpy(chars, wchar_array_data(stringvalue), length * sizeof(wchar_t));
  chars[length] = 0;

  /* We don't set c->cst as a C string constant is a sequence of string_cst,
     not a single one. See make_string. */
  return c;
}

lexical_cst fold_lexical_int(type itype, location loc, cstring tok,
			     bool iscomplex, largest_uint intvalue, bool overflow)
{
  lexical_cst c = new_lexical_cst(parse_region, loc, tok);
  cval cv;

  if (overflow)
    {
      warning_with_location(loc, "integer constant out of range");
      itype = unsigned_long_long_type;
    }
  else
    {
      /* Do some range checks */
      if (!uint_inrange(intvalue, itype))
	{
	  if (uint_inrange(intvalue, unsigned_int_type))
	    {
	      warning_with_location(loc, "decimal constant is so large that it is unsigned");
	      itype = unsigned_int_type;
	    }
	  /* These other cases cause no warnings */
	  else if (uint_inrange(intvalue, long_type))
	    itype = long_type;
	  else if (uint_inrange(intvalue, unsigned_long_type))
	    itype = unsigned_long_type;
	  else if (uint_inrange(intvalue, long_long_type))
	    itype = long_long_type;
	  else if (uint_inrange(intvalue, unsigned_long_long_type))
	    itype = unsigned_long_long_type;
	}
    }

  cv = type_unsigned(itype) ? make_cval_unsigned(intvalue, itype) :
    make_cval_signed(intvalue, itype);
  if (iscomplex)
    {
      cv = make_cval_complex(cval_cast(cval_zero, itype), cv);
      itype = make_complex_type(itype);
    }

  c->type = itype;
  c->cst = make_cst(cv, itype);

  return c;
}


bool definite_null(expression e)
{
  return
    (type_integer(e->type) ||
     (type_pointer(e->type) && type_void(type_points_to(e->type)))) &&
    definite_zero(e);
}

bool definite_zero(expression e)
{
  return e->cst && is_zero_constant(e->cst);
}

bool is_zero_constant(known_cst c)
{
  return cval_knownbool(c->cval) && !constant_boolvalue(c);
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression. */
void constant_overflow_warning(known_cst c)
{
}

char *string_cst_to_c(region r, string_cst s)
{
  const wchar_t *wstr = s->chars;
  int length_as_str = wcs_mb_size(wstr);
  char *str;

  if (length_as_str < 0)
    return NULL;

  str = rarrayalloc(r, length_as_str, char);
  length_as_str = wcstombs(str, wstr, length_as_str);
  assert(length_as_str >= 0);

  return str;
}

static void complex_print(FILE *f, known_cst c)
{
  assert(0);
}

void constant_print(FILE *f, known_cst c)
/* Requires: (constant_integral(c) || constant_float(c)) &&
   	     type_arithmetic(c->type)
   Effects: prints a parsable representable of c to f
 */
{
  type t = c->type;

  if (type_complex(t))
    {
      complex_print(f, c);
      return;
    }

  if (type_floating(t))
    {
      /* XXX: hacky version */
      fprintf(f, "%.20Le", constant_float_value(c));
    }
  else
    {
      assert(type_integral(t));

      if (type_unsigned(t))
	fprintf(f, "%llu", constant_uint_value(c));
      else
	fprintf(f, "%lld", constant_sint_value(c));

      if (type_size(t) <= type_size(int_type))
	{
	  if (type_unsigned(t))
	    putc('U', f);
	}
      else if (type_long(t))
	putc('L', f);
      else if (type_unsigned_long(t))
	fputs("UL", f);
      else if (type_long_long(t))
	fputs("LL", f);
      else if (type_unsigned_long_long(t))
	fputs("ULL", f);
      else
	assert(0);
    }
}
