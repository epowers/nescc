
#include "parser.h"
#include "dhash.h"
#include "c-parse.h"
#include "AST_utils.h"
#include "semantics.h"
#include "constants.h"

#include "unparse.h"

#include "nesc-findvars.h"


// static vars, to keep track of current state, maintain lists, etc.
static dhash_table fv_var_list = NULL;
static dhash_table fv_pointer_use_list = NULL;
static dhash_table fv_aliased_var_list = NULL;
static region fv_region = NULL;

// the current function being operated on, so we don't have to look it up all the time
static data_declaration current_function = NULL;
static const char *current_module_name = NULL;

// whether or not we are currently in an atomic statement
static int in_atomic;


// ugly, non-reentrant print function.  See below
static char* ddecl_name(data_declaration ddecl);




//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
////
////     Data structure management types & functions
////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////
// linked list of variable references
//////////////////////////////////////////////////////////////////////


typedef enum {
  USE_WRITE = 1,
  USE_READ = 2,
  USE_IN_ATOMIC = 4,
  USE_VIA_POINTER = 8
} var_use_flags;


// Track the uses of a variable within a specific function
typedef struct var_use {
  data_declaration function;
  expression expr;
  location location;
  var_use_flags flags;
  struct var_use *next;
} *var_use;


static var_use add_var_use(var_use *first, var_use *last, data_declaration function, expression expr, var_use_flags flags)
{
  var_use u;
  
  u = ralloc(fv_region, struct var_use);
  u->function = function;
  u->expr = expr;
  u->location = expr->location;
  u->flags = flags;
  u->next = NULL;
  
  if( !(*first) )
    *first = *last = u;
  else {
    (*last)->next = u;
    *last = u;
  }

  if( flags == 0 )
    assert(0);

  return u;
}


//////////////////////////////////////////////////////////////////////
// linked list of pointer assignments
//////////////////////////////////////////////////////////////////////

// Track use of & and passing parts of arrays
typedef struct pointer_assignment {
  data_declaration function;
  location location;
  type type;
  struct pointer_assignment *next;
} *pointer_assignment;


static pointer_assignment add_pointer_assignment(pointer_assignment *first, pointer_assignment *last, 
                                                 location loc, type type)
{
  pointer_assignment p;

  p = ralloc(fv_region, struct pointer_assignment);
  p->function = current_function;
  p->location = loc;
  p->type = type;
  p->next = NULL;

  if( !(*first) )
    *first = *last = p;
  else {
    (*last)->next = p;
    *last = p;
  }

  return p;
}




//////////////////////////////////////////////////////////////////////
// main variable list
//////////////////////////////////////////////////////////////////////


// Track all of the uses (program-wide) of a particular set of aliased
// variables.  nesc-findvars generates a global list of var_list_entry
// structures.
typedef struct var_list_entry { 
  data_declaration ddecl;  // key in hash
  var_use refs;  // list of struct var_use, that define how the var is used in each function that uses it
  var_use last_ref;
} *var_list_entry;


// struct var_list_entry
static int fv_var_list_compare(void *entry1, void *entry2)
{
  var_list_entry v1 = (var_list_entry) entry1;
  var_list_entry v2 = (var_list_entry) entry2;

  return v1->ddecl == v2->ddecl;
}

static unsigned long fv_var_list_hash(void *entry)
{
  var_list_entry v = (var_list_entry) entry;
 
  return hashPtr(v->ddecl);
}

static var_list_entry get_var_list_entry(data_declaration ddecl)
{
  var_list_entry v;
  struct var_list_entry vstruct;
  
  // look for an existing entry
  vstruct.ddecl = base_identifier(ddecl);
  v = dhlookup(fv_var_list, &vstruct); 
  if( v ) 
    return v;

  // allocate a new entry
  v = ralloc(fv_region, struct var_list_entry);
  v->ddecl = base_identifier(ddecl);
  
  dhadd(fv_var_list, v);

  return v;
}


static void note_var_use(expression e, bool is_read, bool is_write)
{
  var_list_entry v;
  identifier id;
  var_use_flags flags;

  // ignore null expressions (as from empty return statements
  if (!e)
    return;

  // ignore things that aren't identifiers
  if( !is_identifier(e) )    
    return;
  id = CAST(identifier,e);

  // ignore non-variables, and non-static local variables
  if(id->ddecl->kind != decl_variable ||
     (id->ddecl->islocal && id->ddecl->vtype != variable_static)) {
    return; 
  }
  
  // get / add an entry for this variable
  v = get_var_list_entry(id->ddecl);

  // add this ref
  flags = 0;
  if(is_read) flags |= USE_READ;
  if(is_write) flags |= USE_WRITE;
  if(in_atomic) flags |= USE_IN_ATOMIC;

  add_var_use(&(v->refs), &(v->last_ref), current_function, e, flags);

}




//////////////////////////////////////////////////////////////////////
// list of pointer uses
//////////////////////////////////////////////////////////////////////


// track the list of pointer uses
typedef struct pointer_use_list_entry {
  type type;   // key in hash
  var_use refs;  // list of struct var_use, that define how the var is used in each function that uses it
  var_use last_ref;
} *pointer_use_list_entry;


static int fv_pointer_use_list_compare(void *entry1, void *entry2)
{
  pointer_use_list_entry a1 = (pointer_use_list_entry) entry1;
  pointer_use_list_entry a2 = (pointer_use_list_entry) entry2;
  
  return type_equal(a1->type, a2->type);
}

static unsigned long fv_pointer_use_list_hash(void *entry)
{
  pointer_use_list_entry a = (pointer_use_list_entry) entry;
  return type_hash(a->type);
}


// find an existing entry, or create one, if necessary 
static pointer_use_list_entry get_pointer_use_list_entry(type type) 
{
  pointer_use_list_entry a;
  struct pointer_use_list_entry temp;

  temp.type = type;

  a = dhlookup(fv_pointer_use_list, &temp);
  if( a ) 
    return a;

  a = ralloc(fv_region, struct pointer_use_list_entry);
  a->type = type;
  dhadd(fv_pointer_use_list, a);

  return a;
}

// record a new pointer use
static void note_pointer_use(expression e, bool is_read, bool is_write)
{
  pointer_use_list_entry a;
  var_use_flags flags;

  // find an entry for this type of pointer
  a = get_pointer_use_list_entry( type_points_to(e->type) );

  // FIXME: need to special case for things like *(&x) ??

  // add this ref
  flags = USE_VIA_POINTER;
  if(is_read) flags |= USE_READ;
  if(is_write) flags |= USE_WRITE;
  if(in_atomic) flags |= USE_IN_ATOMIC;

  add_var_use(&(a->refs), &(a->last_ref), current_function, e, flags);
}






//////////////////////////////////////////////////////////////////////
// list of aliased variables
//////////////////////////////////////////////////////////////////////

typedef struct aliased_var_list_entry {
  data_declaration ddecl; // key in hash
  expression expr;
  type type;
  pointer_assignment refs;
  pointer_assignment last_ref;
} *aliased_var_list_entry;


static int fv_aliased_var_list_compare(void *entry1, void *entry2)
{
  aliased_var_list_entry v1 = (aliased_var_list_entry) entry1;
  aliased_var_list_entry v2 = (aliased_var_list_entry) entry2;

  return v1->ddecl == v2->ddecl;
}

static unsigned long fv_aliased_var_list_hash(void *entry)
{
  aliased_var_list_entry v = (aliased_var_list_entry) entry;
 
  return hashPtr(v->ddecl);
}


static aliased_var_list_entry get_aliased_var_list_entry(identifier id, expression e)
{
  aliased_var_list_entry v;
  struct aliased_var_list_entry vstruct;
  
  // look for an existing entry
  vstruct.ddecl = base_identifier(id->ddecl);
  v = dhlookup(fv_aliased_var_list, &vstruct); 
  if( v ) {
    if( !type_equal(v->type, id->type) ) {
      // FIXME: need to do something here - why are these different?
      warning_with_location(v->expr->location, "COMPILER BUG: %s  v: %s   e: %s",
                            ddecl_name(v->ddecl), type_name(fv_region,v->type), type_name(fv_region,e->type));
      warning_with_location(e->location, "COMPILER BUG: %s  v: %s   e: %s",
                            ddecl_name(v->ddecl), type_name(fv_region,v->type), type_name(fv_region,e->type));
      set_unparse_outfile(stderr);
      fprintf(stderr, "v->expr:  "); prt_expression(v->expr,TRUE); fprintf(stderr, "\n");
      fprintf(stderr, "e:        "); prt_expression(e,TRUE); fprintf(stderr, "\n");

      v->type = id->type;
    }
    return v;
  }

  // allocate a new entry
  v = ralloc(fv_region, struct aliased_var_list_entry);
  v->ddecl = base_identifier(id->ddecl);
  v->type = id->type;
  v->expr = e;
  
  dhadd(fv_aliased_var_list, v);

  return v;
}

// FIXME: sometime later, do flow analysis on the lvalue, to see if the pointer value is really saved, or just used temporarily.
// FIXME: need to handle function args as well
static void note_pointer_assignment(identifier id, expression e)
{
  aliased_var_list_entry v;

  // print an error for non-variables, and non-static local variables
  if(id->ddecl->kind != decl_variable) {
    error_with_location(id->location, "COMPILER BUG: taking address of non-variable %s", ddecl_name(id->ddecl));
    set_unparse_outfile(stderr);
    fprintf(stderr, "expr:  "); prt_expression(e, TRUE); fprintf(stderr,"\n");
    return;
  }
  if(id->ddecl->islocal) {
    warning_with_location(id->location, "taking address of local '%s' in function %s", 
                          ddecl_name(id->ddecl), current_function->name);
  }


  // find an entry for this type of pointer
  v = get_aliased_var_list_entry(id, e);

  // FIXME: the real type of the assignment is actually lost now!!!
  add_pointer_assignment(&(v->refs), &(v->last_ref), e->location, e->type);
}



//////////////////////////////////////////////////////////////////////
// functions for trapsing through the AST
//////////////////////////////////////////////////////////////////////

static void find_statement_vars(statement stmt, bool is_read,
				expression addressof_expr);

static void find_expression_vars(expression expr, bool is_read, bool is_write,
				 expression addressof_expr)
{
  if (!expr)
    return;

  if (is_string(expr))
    return;

  // skip constant expressions that are neither pointers nor arrays
  if(expr->cst &&  !type_array(expr->type) && !type_pointer(expr->type) && !type_tagged(expr->type)) {
#if 0
    printf("SKIPPING CST: ");
    set_unparse_outfile(stdout);
    prt_expression(expr,TRUE);
    printf("    %s:%ld\n", expr->location->filename, expr->location->lineno);
#endif
    return;
  }
#if 0
  if(expr->cst) {
    printf("KEEPING CST: ");
    set_unparse_outfile(stdout);
    prt_expression(expr,TRUE);
    printf("    %s:%ld  (%s)\n", expr->location->filename, expr->location->lineno, type_name(fv_region,expr->type));
  }
#endif

  switch (expr->kind)
    {
    case kind_identifier:
      if (addressof_expr)
	note_pointer_assignment(CAST(identifier, expr), addressof_expr);
      else if (is_read && type_array(expr->type))
        note_pointer_assignment(CAST(identifier,expr), expr);
      else 
        note_var_use(expr, is_read, is_write);
      break;

    case kind_interface_deref:
      break;

    case kind_comma: {
      expression e;

      // the last exrepssion in a comma list can be an lvalue and an rvalue
      scan_expression (e, CAST(comma, expr)->arg1) {
        if( e->next )
          find_expression_vars(e, FALSE, FALSE, NULL);
        else 
          find_expression_vars(e, is_read, is_write, addressof_expr);
      }
      break;
    }

    // FIXME: none of these are really handled properly
    case kind_cast_list: {
      assert( 0 ); 
      find_expression_vars(CAST(cast_list, expr)->init_expr, is_read, FALSE, addressof_expr);
      break;
    }
    case kind_init_index: {
      assert( 0 ); 
      find_expression_vars(CAST(init_index,expr)->init_expr, FALSE, FALSE, NULL);
      break;
    }
    case kind_init_field: {
      assert( 0 ); 
      find_expression_vars(CAST(init_field,expr)->init_expr, FALSE, FALSE, NULL);
      break;
    }
    case kind_init_list: {
      expression e;
      assert( 0 ); 
      scan_expression (e, CAST(init_list,expr)->args)
        find_expression_vars(e, TRUE, FALSE, NULL);
      break;
    }
    case kind_extension_expr: {
      find_expression_vars(CAST(unary, expr)->arg1, is_read, is_write, addressof_expr);
      break;
    }

    case kind_conditional: {
      conditional ce = CAST(conditional, expr);

      if (ce->condition->cst)
	{
	  if (definite_zero(ce->condition))
	    find_expression_vars(ce->arg2, is_read, is_write, addressof_expr);
	  else
	    find_expression_vars(ce->arg1, is_read, is_write, addressof_expr);
	}
      else
	{
	  find_expression_vars(ce->condition, TRUE, FALSE, NULL);
	  find_expression_vars(ce->arg1, is_read, is_write, addressof_expr);
	  find_expression_vars(ce->arg2, is_read, is_write, addressof_expr);
	}
      break;
    }
    case kind_compound_expr:
      find_statement_vars(CAST(compound_expr, expr)->stmt, is_read, addressof_expr);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);
      expression e;

      // FIXME: remove this when better mechanism is available
      // special case for dbg() calls
      if( is_identifier(fce->arg1) && strcmp(CAST(identifier,fce->arg1)->cstring.data,"dbg")==0 )
        break;

      // don't count direct fn calls as refs
      if (!(is_interface_deref(fce->arg1) ||
	    is_generic_call(fce->arg1) ||
	    (is_identifier(fce->arg1) &&
	     CAST(identifier, fce->arg1)->ddecl->kind == decl_function)))
	{
	  warning_with_location(fce->location, "call via function pointer");
	  find_expression_vars(fce->arg1, TRUE, FALSE, NULL);
	}

      scan_expression (e, fce->args) {
	find_expression_vars(e, TRUE, FALSE, NULL);
      }
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);
      expression e;

      scan_expression (e, fce->args)
	find_expression_vars(e, TRUE, FALSE, NULL);
      break;
    }

    case kind_array_ref: {
      array_ref are = CAST(array_ref, expr);

      /* Check for direct array uses. I'll ignore 1[a] for now */
      if (is_identifier(are->arg1) && type_array(are->type) && !addressof_expr)
	{
	  note_var_use(are->arg1, is_read, is_write);
	  find_expression_vars(are->arg2, TRUE, FALSE, NULL);
	  break;
	}

      find_expression_vars(are->arg1, is_read, is_write, addressof_expr);
      find_expression_vars(are->arg2, TRUE, FALSE, NULL);
      break;
    }
    case kind_field_ref: {
      field_ref fre = CAST(field_ref, expr);
      find_expression_vars(fre->arg1, is_read, is_write, addressof_expr);
      // FIXME: possibly track specific fields seperately?
      break;
    }
    case kind_dereference: {
      expression arg = CAST(dereference, expr)->arg1;

      if (!addressof_expr) /* &*x is just x, not a pointer deref */
	note_pointer_use(arg, is_read, is_write);
      find_expression_vars(arg, TRUE, FALSE, NULL);
      break;
    }
    case kind_address_of: {
      expression arg = CAST(unary, expr)->arg1;

      /* We don't handle *&x at this point */
      find_expression_vars(arg, FALSE, FALSE, expr);
      break;
    }

    // UNARY OPERATORS

    // updates
    case kind_preincrement: case kind_postincrement: case kind_predecrement: case kind_postdecrement: {
      unary ue = CAST(unary, expr);
      find_expression_vars(ue->arg1, is_read, TRUE, NULL);
      break;
    }
    // pass r/w/a flags
    case kind_cast: case kind_realpart: case kind_imagpart: case kind_conjugate: {
      find_expression_vars(CAST(unary,expr)->arg1, is_read, is_write, addressof_expr);
      break;
    }
    // don't pass r/w/a flags
    case kind_bitnot: case kind_not: case kind_unary_minus: case kind_unary_plus: {
      assert( !is_write );
      find_expression_vars(CAST(unary,expr)->arg1, TRUE, FALSE, NULL);
      break;
    }

    // BINARY OPERATORS

    // simple assignment allowed for pointers
    case kind_assign:  {
      binary be = CAST(binary, expr);
      find_expression_vars(be->arg1, FALSE, TRUE, NULL);
      find_expression_vars(be->arg2, TRUE, FALSE, NULL);
      break;
    }
    case kind_plus_assign:  case kind_minus_assign: 
    case kind_times_assign:  case kind_divide_assign:  case kind_modulo_assign:  
    case kind_bitand_assign:  case kind_bitor_assign:  case kind_bitxor_assign: 
    case kind_lshift_assign:  case kind_rshift_assign: {
      binary be = CAST(binary, expr);
      find_expression_vars(be->arg1, TRUE, TRUE, NULL);
      find_expression_vars(be->arg2, TRUE, FALSE, NULL);
      break;
    }
    
    case kind_plus: case kind_minus:
    case kind_times:  case kind_divide:  case kind_modulo:  case kind_lshift:  case kind_rshift: {
      binary be = CAST(binary, expr);

      find_expression_vars(be->arg1, TRUE, FALSE, NULL);
      find_expression_vars(be->arg2, TRUE, FALSE, NULL);
      break;
    }
    
    // logical operations just read 
    case kind_leq:  case kind_geq:  case kind_lt:  case kind_gt:  case kind_eq:  case kind_ne:
    case kind_bitand:  case kind_bitor:  case kind_bitxor:  case kind_andand:  case kind_oror: {
      binary be = CAST(binary, expr);
      find_expression_vars(be->arg1, TRUE, FALSE, NULL);
      find_expression_vars(be->arg2, TRUE, FALSE, NULL);
      break;
    }

    default:
      assert(0);
      break;
    }

}


// NOTE: the is_read flag is _only_ set TRUE when processing a
// kind_compound_expr.  This is because the last statement in a
// compound expression is the lvalue for the entire compound
// expression.
static void find_statement_vars(statement stmt, bool is_read, expression addressof_expr)
{
  if (!stmt)
    return;

  switch (stmt->kind)
    {
    case kind_asm_stmt: {
      break;
    }
    case kind_compound_stmt: {
      compound_stmt cs = CAST(compound_stmt, stmt);
      statement s;
      declaration d;

      // go through initializers
      scan_declaration (d, cs->decls)
	if (is_data_decl(d))
	  {
	    variable_decl vd;

	    scan_variable_decl (vd, CAST(variable_decl, CAST(data_decl, d)->decls)) {
	      if (vd->ddecl->kind == decl_variable && vd->ddecl->vtype != variable_static) {
                if( type_pointer(vd->declared_type) || type_array(vd->declared_type) )
                  find_expression_vars(vd->arg1, TRUE, FALSE, vd->arg1);
                else
                  find_expression_vars(vd->arg1, TRUE, FALSE, NULL);
              }
            }
	  }

      // pass the is_read and addressof_expr flags on to the last statement
      scan_statement (s, cs->stmts) {
        if( s->next ) 
          find_statement_vars(s, FALSE, NULL);
        else 
          find_statement_vars(s, is_read, addressof_expr);
      }

      break;
    }
    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);

      if (is->condition->cst)
	{
	  if (definite_zero(is->condition))
	    find_statement_vars(is->stmt2, FALSE, NULL);
	  else
	    find_statement_vars(is->stmt1, FALSE, NULL);
	}
      else
	{
	  find_expression_vars(is->condition, TRUE, FALSE, NULL);
	  find_statement_vars(is->stmt1, FALSE, NULL);
	  find_statement_vars(is->stmt2, FALSE, NULL);
	}
      break;
    }
    case kind_labeled_stmt: {
      labeled_stmt ls = CAST(labeled_stmt, stmt);

      find_statement_vars(ls->stmt, is_read, addressof_expr);
      break;
    }
    case kind_expression_stmt: {
      expression_stmt es = CAST(expression_stmt, stmt);

      find_expression_vars(es->arg1, is_read, FALSE, addressof_expr);
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);

      if (cs->condition->cst && stmt->kind != kind_switch_stmt &&
	  definite_zero(cs->condition))
	{
          // for while (0) s: don't analyze s
	  if (stmt->kind == kind_dowhile_stmt)
	    find_statement_vars(cs->stmt, FALSE, NULL);
	  break;
	}
      find_expression_vars(cs->condition, TRUE, FALSE, NULL);
      find_statement_vars(cs->stmt, FALSE, NULL);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);

      find_statement_vars(fs->stmt, FALSE, NULL);
      find_expression_vars(fs->arg1, TRUE, FALSE, NULL);
      find_expression_vars(fs->arg2, TRUE, FALSE, NULL);
      find_expression_vars(fs->arg3, TRUE, FALSE, NULL);
      break;
    }
    case kind_break_stmt: case kind_continue_stmt: case kind_goto_stmt:
      break;

    case kind_empty_stmt:
      break;

    case kind_computed_goto_stmt: {
      computed_goto_stmt cgs = CAST(computed_goto_stmt, stmt);

      find_expression_vars(cgs->arg1, TRUE, FALSE, NULL);
      break;
    }
    case kind_return_stmt: {
      return_stmt rs = CAST(return_stmt, stmt);
      find_expression_vars(rs->arg1, TRUE, FALSE, NULL);
      break;
    }
    case kind_atomic_stmt: {
      in_atomic++;
      find_statement_vars(CAST(atomic_stmt,stmt)->stmt, FALSE, NULL);
      in_atomic--;
      break;
    }
    default: assert(0);
    }

}








//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
////
////    analysis and error message functions
////
//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
 

// Check a function list for any accesses from interrupt context
static bool accesses_only_in_tasks(var_use u)
{
  while( u ) {
    if (u->function->contexts & ~(c_atomic_task | c_task))
      return FALSE;

    u = u->next;
  }

  // made it through the list w/o finding a non-task function.
  return TRUE;
}

static const char *context_name(data_declaration fn)
{
  bool intask, inintr;

  intask = (fn->contexts & (c_task | c_atomic_task)) != 0;
  inintr = (fn->contexts & ~(c_task | c_atomic_task)) != 0;
  if (intask && inintr)
    return "task/intr";
  else if (inintr)
    return "intr";
  else 
    return "task";

}

// Ugly, non-reentrant function, to make printing messages cleaner.
// This could be done with memory allocation or a buf parameter
// instead, but it's more conveninent this way.
static char* ddecl_name(data_declaration ddecl)
{
  static char buf[1024];

  buf[0] = '\0';
  if(ddecl->container) {
    strcat(buf, ddecl->container->name);
    strcat(buf, ".");
  }
  strcat(buf, ddecl->name);
  return buf;
}

static char* get_atype(var_use u)
{
  static char* atype[] = {
    "??",
    "r",
    "w",
    "rw",
    "atomic ??",
    "atomic r",
    "atomic w",
    "atomic rw",
    "aliased ??",
    "aliased r",
    "aliased w",
    "aliased rw",
    "aliased atomic ??",
    "aliased atomic r",
    "aliased atomic w",
    "aliased atomic rw",
  };
  
  return atype[ u->flags & 0xF ];
}

static void print_var_conflict_error_message(var_list_entry v)
{
  var_use u;
  
  error("Detected data conflict for %s.  List of accesses follow:", ddecl_name(v->ddecl));

  // print unsafe refs
  u = v->refs;
  while( u ) {
    if( !(u->flags & USE_IN_ATOMIC) )
      fprintf(stderr,"%s:%ld   %s %s in %s.  context=%s, use=%s\n", 
              u->location->filename, u->location->lineno,
              u->flags & USE_VIA_POINTER ? "UNSAFE pointer ref to" : "UNSAFE use of",
              v->ddecl->name, 
              u->function->name, 
              context_name(u->function), 
              get_atype(u));
    
    u = u->next;
  }

  // print safe refs 
  u = v->refs;
  while( u ) {
    if( u->flags & USE_IN_ATOMIC )
      fprintf(stderr,"%s:%ld   %s %s in %s.  context=%s, use=%s\n", 
              u->location->filename, u->location->lineno,
              u->flags & USE_VIA_POINTER ? "safe pointer ref to" : "safe use of",
              v->ddecl->name, 
              u->function->name, 
              context_name(u->function), 
              get_atype(u));
    
    u = u->next;
  }
  

  fprintf(stderr, "\n");
}


static void print_var_debug_summary(var_list_entry v, bool conflict, 
                                    bool read, bool write, bool aread, bool awrite) 
{ 
  var_use u;
  data_declaration f;

  printf("  %c  %c  %c  %c  %c   : %s\n",
         conflict ? 'X' : ' ',
         read ? 'r' : ' ',
         write ? 'w' : ' ',
         aread ? 'r' : ' ',
         awrite ? 'w' : ' ',
         ddecl_name(v->ddecl));
  
  u = v->refs;
  while( u ) {
    f = u->function;
    printf("                    - %s (%s, %s)\n", 
           ddecl_name(f),
           context_name(f),
           get_atype(u));
    u = u->next;
  }
}



// look through a list of refs, and produce a summary for conflict detection purposes
static void summarize_var_use(var_use u, bool *read, bool *write, bool *aread, bool *awrite)
{
  *read = FALSE;
  *write = FALSE;
  *aread = FALSE;
  *awrite = FALSE;

  while( u ) {
    if( u->flags & USE_IN_ATOMIC ) {
      if(u->flags & USE_READ) *aread = TRUE;
      if(u->flags & USE_WRITE) *awrite = TRUE;
    } else {
      if(u->flags & USE_READ) *read = TRUE;
      if(u->flags & USE_WRITE) *write = TRUE;
    }
    u = u->next;
  }
}



static void process_aliased_vars()
{
  dhash_scan alias_scanner, use_scanner;
  aliased_var_list_entry a;
  var_list_entry v;
  pointer_use_list_entry p;
  var_use u;
  
  // scan the list of aliased vars
  alias_scanner = dhscan(fv_aliased_var_list);
  while( (a=dhnext(&alias_scanner)) ) {

    v = NULL;

    // check the pointer_use_list for possible references
    use_scanner = dhscan(fv_pointer_use_list);
    while( (p=dhnext(&use_scanner)) )  {

      // could the pointer use have been to part of a?
      if( type_contains(a->type, p->type) ) {
        
        if( p->refs && !v ) v = get_var_list_entry(a->ddecl);
        
        // add this ref
        for(u=p->refs; u; u=u->next)
          add_var_use(&(v->refs), &(v->last_ref), u->function, u->expr, u->flags);
      }

    }
  }
}

static void print_aliased_vars_debug_summary()
{
  dhash_scan scanner;
  aliased_var_list_entry a;
  pointer_use_list_entry p;
  
  // scan the list of aliased vars
  printf("--- ALIASED VARS ---\n");
  scanner = dhscan(fv_aliased_var_list);
  while( (a=dhnext(&scanner)) ) {
    pointer_assignment pa;
    printf("%s   type=%s\n", ddecl_name(a->ddecl), type_name(fv_region,a->type));

    for(pa=a->refs; pa; pa=pa->next)
      printf("%s:%ld:  &%s   type=%s\n", pa->location->filename, pa->location->lineno, 
             ddecl_name(a->ddecl),
             type_name(fv_region,pa->type));
    // FIXME: print the expr here

    printf("\n");
  }


  // scan the pointer_use_list
  printf("\n--- POINTER USES ---\n");
  scanner = dhscan(fv_pointer_use_list);
  while( (p=dhnext(&scanner)) )  {
    var_use u;
    printf("type: %s\n", type_name(fv_region,p->type));
    
    if( type_tagged(p->type) ) {
      tag_declaration tdecl = type_tag(p->type);
      printf("%s:%ld:  %s definition\n", 
             tdecl->ast->location->filename, tdecl->ast->location->lineno,
             tdecl->kind == kind_struct_ref ? "struct" : (tdecl->kind == kind_enum_ref ? "enum" : "union"));
    }

    // add this ref
    for(u=p->refs; u; u=u->next)
      printf("%s:%ld:  %s\n", u->location->filename, u->location->lineno, get_atype(u));
    // FIXME: print the used expr here


    printf("\n");
  }

}


//////////////////////////////////////////////////////////////////////
// external routines
//////////////////////////////////////////////////////////////////////

void fv_init() 
{
  if(fv_region == NULL) 
    fv_region = newregion();

  fv_var_list = new_dhash_table(fv_region, 512, fv_var_list_compare, fv_var_list_hash);
  fv_aliased_var_list = new_dhash_table(fv_region, 512, fv_aliased_var_list_compare, fv_aliased_var_list_hash);
  fv_pointer_use_list = new_dhash_table(fv_region, 512, fv_pointer_use_list_compare, fv_pointer_use_list_hash);
}

void fv_cleanup()
{
  deleteregion(fv_region);
  fv_region = NULL;

  fv_var_list = NULL;
  fv_aliased_var_list = NULL;
  fv_pointer_use_list = NULL;
}

void find_function_vars(data_declaration fn)
{
  function_decl fdecl;

  if( is_function_decl(fn->ast) )
    fdecl = CAST(function_decl, fn->ast);
  else if( fn->definition ) 
    fdecl = CAST(function_decl, fn->definition);
  else {
#if 0
    // FIXME: deal with these
    error("can't find definition for function %s!!", ddecl_name(fn));
#endif
    return;
  }
    

  current_function = fn;
  if( fn->container ) {
    current_module_name = fn->container->name;
  } else {
    current_module_name = NULL;
  }


  // 1 if always atomic, 0 otherwise 
  in_atomic =
    (fn->contexts & (c_task | c_int | c_reentrant_int)) == 0;
  find_statement_vars( fdecl->stmt, FALSE, NULL );


  current_function = NULL;
  current_module_name = NULL;
}



// walk the list of variables, and find data races
void check_for_conflicts(void) 
{
  dhash_scan scanner;
  var_list_entry v;
  bool conflict;
  bool read, write, aread, awrite;

  
  // go through the aliased vars, and add conflicts as appropriate
#define ALIAS_ERRORS 1
#ifdef ALIAS_ERRORS
  process_aliased_vars();
#endif  
  
  // check regular variable conflicts
  scanner = dhscan(fv_var_list);
  while( (v=dhnext(&scanner)) ) {
    conflict = TRUE;
    
    summarize_var_use(v->refs, &read, &write, &aread, &awrite);

    // no conflict if all reads
    if( !write && !awrite )
      conflict = FALSE;
    
    // no conflict if all accesses are in atomic
    else if( !read && !write )
      conflict = FALSE;

    // no conflict if all accesses are in tasks
    else if( accesses_only_in_tasks(v->refs) ) 
      conflict = FALSE;

    // otherwise, there is a conflict!
    
    // print summary info
    print_var_debug_summary(v, conflict, read, write, aread, awrite);

    if( conflict ) 
      print_var_conflict_error_message( v );
  } 

  printf("\n--------------------------------------------------\n\n");


#ifdef ALIAS_ERRORS
  print_aliased_vars_debug_summary();
#endif

}



