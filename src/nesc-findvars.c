
#include "parser.h"
#include "dhash.h"
#include "c-parse.h"
#include "AST_utils.h"
#include "semantics.h"
#include "constants.h"

#include "unparse.h"

#include "nesc-findvars.h"


static void note_var_use(expression e, bool is_read, bool is_write);
static void note_pointer_use(expression e, bool is_read, bool is_write);
static void note_address_of(expression e);

typedef struct {
  bool read;
  bool write;
  bool read_in_atomic;
  bool write_in_atomic;
} usage_flags;



// Track the uses of a variable within a specific function
typedef struct var_use {
  data_declaration function;
  usage_flags flags;
} *var_use;



// Track all of the uses (program-wide) of a particular set of aliased
// variables.  nesc-findvars generates a global list of var_list_entry
// structures.
typedef struct var_list_entry { 
  const char *module;
  const char *name;
  
  dhash_table funcs;      // list of struct var_use, that define how the var is used in each function that uses it

  usage_flags flags;
} *var_list_entry;


// track the list of aliased variables
typedef struct alias_list_entry {
  type pointer_type;

  dhash_table vars;  // list of data declarations for the variables aliased together
  dhash_table funcs; // list of struct var_use, that define how the alias is used 

  usage_flags flags;
} *alias_list_entry;



static dhash_table fv_var_list = NULL;
static dhash_table fv_alias_list = NULL;
static region fv_region = NULL;

// the current function being operated on, so we don't have to look it up all the time
static data_declaration current_function = NULL;
static const char *current_module_name = NULL;

// whether or not we are currently in an atomic statement
bool in_atomic = FALSE;




//////////////////////////////////////////////////////////////////////
// functions for trapsing through the AST
//////////////////////////////////////////////////////////////////////

static void find_statement_vars(statement stmt);
static void find_expression_vars(expression expr, bool is_read, bool is_write);

static void find_elist_vars(expression elist)
{
  expression e;

  scan_expression (e, elist)
    find_expression_vars(e, TRUE, FALSE);
}


static void find_expression_vars(expression expr, bool is_read, bool is_write)
{
  if (!expr)
    return;

  if (is_string(expr))
    return;

  // skip constant expressions that are neither pointers nor arrays
  // FIXME: there's more to this.
  if(expr->cst && !is_identifier(expr) && !type_array(expr->type) && !type_pointer(expr->type))
    return;

#if 0
  if (expr->cst) {
    set_unparse_outfile(stdout);
    prt_expression(expr,TRUE);
    printf("\n");
  }
#endif
  

  switch (expr->kind)
    {
    case kind_identifier:
      // default for identifiers: read access to the specified variable
      note_var_use(expr, is_read, is_write);
      break;

    case kind_interface_deref:
      break;

    case kind_comma:
      find_elist_vars(CAST(comma, expr)->arg1);
      break;

    case kind_cast_list: {
      find_expression_vars(CAST(cast_list, expr)->init_expr, FALSE, FALSE);
      break;
    }
    case kind_init_index: {
      init_index init = CAST(init_index, expr);
      
      find_expression_vars(init->init_expr, FALSE, FALSE);
      break;
    }
    case kind_init_field: {
      init_field init = CAST(init_field, expr);

      find_expression_vars(init->init_expr, FALSE, FALSE);
      break;
    }
    case kind_init_list: {
      find_elist_vars(CAST(init_list, expr)->args);
      break;
    }
    case kind_conditional: {
      conditional ce = CAST(conditional, expr);

      if (ce->condition->cst)
	{
	  if (definite_zero(ce->condition))
	    find_expression_vars(ce->arg2, TRUE, FALSE);
	  else
	    find_expression_vars(ce->arg1, TRUE, FALSE);
	}
      else
	{
	  find_expression_vars(ce->condition, TRUE, FALSE);
	  find_expression_vars(ce->arg1, TRUE, FALSE);
	  find_expression_vars(ce->arg2, TRUE, FALSE);
	}
      break;
    }
    case kind_compound_expr:
      /* BUG: last statement is "read" */
      find_statement_vars(CAST(compound_expr, expr)->stmt);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);

      // don't follow identifiers
      if( !is_identifier(fce->arg1) )
        find_expression_vars(fce->arg1, TRUE, FALSE);
      find_elist_vars(fce->args);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);

      // don't follow identifiers
      if( !is_identifier(fce->arg1) )
        find_expression_vars(fce->arg1, TRUE, FALSE);

      find_elist_vars(fce->args);
      break;
    }
    // FIXME: what is this?
    case kind_extension_expr: {
      find_expression_vars(CAST(unary, expr)->arg1, FALSE, FALSE);
      break;
    }
    case kind_array_ref: {
      array_ref are = CAST(array_ref, expr);

      find_expression_vars(are->arg1, is_read, is_write);
      find_expression_vars(are->arg2, TRUE, FALSE);

      break;
    }
    case kind_field_ref: {
      field_ref fre = CAST(field_ref, expr);
      find_expression_vars(fre->arg1, is_read, is_write);
      // FIXME: possibly track specific fields seperately?
      break;
    }
    case kind_dereference: {
      expression arg = CAST(dereference, expr)->arg1;
      note_pointer_use(arg, is_read, is_write);
      find_expression_vars(arg, TRUE, FALSE);
      break;
    }
    case kind_address_of: {
      expression arg = CAST(unary, expr)->arg1;
      note_address_of(arg);
      find_expression_vars(arg, FALSE, FALSE);
    }
    // unary updates
    case kind_preincrement: case kind_postincrement: case kind_predecrement: case kind_postdecrement: {
      unary ue = CAST(unary, expr);
      find_expression_vars(ue->arg1, is_read, TRUE);
      break;
    }
    // binary updates
    case kind_assign: case kind_plus_assign:  case kind_minus_assign:  case kind_times_assign:  
    case kind_divide_assign:  case kind_modulo_assign:  case kind_lshift_assign:  case kind_rshift_assign:  
    case kind_bitand_assign:  case kind_bitor_assign:  case kind_bitxor_assign: {
      binary be = CAST(binary, expr);
      find_expression_vars(be->arg1, is_read || expr->kind != kind_assign, TRUE);
      find_expression_vars(be->arg2, TRUE, FALSE);

      // special case for assignment to a pointer
      // FIXME: need this when the rvalue is an array.  Are there other times?
      break;
    }
    /* BUG: gcc allows (int)x = 2 (i.e., cast as lvalue) */
    /*      gcc allows (1, x) = 2 (i.e., comma lists as lvalues) */
    /*      gcc allows (x ? x : x) = 2 (i.e., ?: as lvalue) */
    /* Note that ({x;}) = 2 is an error at least ;-) */
    default:
      // FIXME: are these read/write flags reasonable?
      if (is_unary(expr)) {
        unary ue = CAST(unary, expr);
	find_expression_vars(ue->arg1, TRUE, FALSE);
      }
      else if (is_binary(expr)) {
        binary be = CAST(binary, expr);
        find_expression_vars(be->arg1, TRUE, FALSE);
        find_expression_vars(be->arg2, TRUE, FALSE);
      }
      else 
	assert(0);
      break;
    }

}

static void find_statement_vars(statement stmt)
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

	    scan_variable_decl (vd, CAST(variable_decl, CAST(data_decl, d)->decls))
	      if (vd->ddecl->kind == decl_variable && vd->ddecl->vtype != variable_static)
		find_expression_vars(vd->arg1, TRUE, FALSE);
	  }

      scan_statement (s, cs->stmts)
	find_statement_vars(s);

      break;
    }
    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);

      if (is->condition->cst)
	{
	  if (definite_zero(is->condition))
	    find_statement_vars(is->stmt2);
	  else
	    find_statement_vars(is->stmt1);
	}
      else
	{
	  find_expression_vars(is->condition, TRUE, FALSE);
	  find_statement_vars(is->stmt1);
	  find_statement_vars(is->stmt2);
	}
      break;
    }
    case kind_labeled_stmt: {
      labeled_stmt ls = CAST(labeled_stmt, stmt);

      find_statement_vars(ls->stmt);
      break;
    }
    case kind_expression_stmt: {
      expression_stmt es = CAST(expression_stmt, stmt);

      find_expression_vars(es->arg1, FALSE, FALSE);
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);

      if (cs->condition->cst && stmt->kind != kind_switch_stmt &&
	  definite_zero(cs->condition))
	{
	  // do s while (0): just include size of s
          // while (0) s: size is 0
	  if (stmt->kind == kind_dowhile_stmt)
	    find_statement_vars(cs->stmt);
	  break;
	}
      find_expression_vars(cs->condition, TRUE, FALSE);
      find_statement_vars(cs->stmt);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);

      find_statement_vars(fs->stmt);
      find_expression_vars(fs->arg1, TRUE, FALSE);
      find_expression_vars(fs->arg2, TRUE, FALSE);
      find_expression_vars(fs->arg3, TRUE, FALSE);
      break;
    }
    case kind_break_stmt: case kind_continue_stmt: case kind_goto_stmt:
      break;

    case kind_empty_stmt:
      break;

    case kind_computed_goto_stmt: {
      computed_goto_stmt cgs = CAST(computed_goto_stmt, stmt);

      find_expression_vars(cgs->arg1, TRUE, FALSE);
      break;
    }
    case kind_return_stmt: {
      return_stmt rs = CAST(return_stmt, stmt);
      find_expression_vars(rs->arg1, TRUE, FALSE);
      break;
    }
    case kind_atomic_stmt: {
      in_atomic = 1;
      find_statement_vars(CAST(atomic_stmt,stmt)->stmt);
      in_atomic = 0;
      break;
    }
    default: assert(0);
    }

}




//////////////////////////////////////////////////////////////////////
// hash & compare  functions
//////////////////////////////////////////////////////////////////////

// struct var_list_entry
static int fv_var_list_compare(void *entry1, void *entry2)
{
  var_list_entry v1 = (var_list_entry) entry1;
  var_list_entry v2 = (var_list_entry) entry2;

  if ( safe_strcmp(v1->name,v2->name) ) return 0;
  if ( safe_strcmp(v1->module,v2->module) ) return 0;
  return 1;
}

static unsigned long fv_var_list_hash(void *entry)
{
  var_list_entry v = (var_list_entry) entry;
 
  return hashStr(v->name) ^ hashStr(v->module);
}


// struct alias_list_entry
static int fv_alias_list_compare(void *entry1, void *entry2)
{
  alias_list_entry a1 = (alias_list_entry) entry1;
  alias_list_entry a2 = (alias_list_entry) entry2;
  return a1->pointer_type == a2->pointer_type;
}

static unsigned long fv_alias_list_hash(void *entry)
{
  alias_list_entry a = (alias_list_entry) entry;
  return hashPtr(a->pointer_type);
}


// struct var_use
static int fv_var_use_compare(void *entry1, void *entry2)
{
  var_use u1 = (var_use) entry1;
  var_use u2 = (var_use) entry2;

  return u1->function == u2->function;
}

static unsigned long fv_var_use_hash(void *entry)
{
  var_use u = (var_use) entry;
 
  return hashPtr(u->function);
}


//////////////////////////////////////////////////////////////////////
// Manage function lists
//////////////////////////////////////////////////////////////////////

static var_use get_var_use(dhash_table table)
{
  var_use u;
  struct var_use ustruct;
  ustruct.function = current_function;

  u = dhlookup(table, &ustruct);
  if(u) 
    return u;
  
  u = ralloc(fv_region, struct var_use);
  u->function = ustruct.function;
  u->flags.read = FALSE;
  u->flags.write = FALSE;
  u->flags.read_in_atomic = FALSE;
  u->flags.write_in_atomic = FALSE;
  dhadd(table, u);
  
  return u;
}

static void update_usage(usage_flags *f, bool is_read, bool is_write)
{
  if(is_write) {
    if(in_atomic)
      f->write_in_atomic = TRUE;
    else 
      f->write = TRUE;
  }

  if(is_read) {
    if(in_atomic)
      f->read_in_atomic = TRUE;
    else 
      f->read = TRUE;
  }
}


//////////////////////////////////////////////////////////////////////
// Manage data structures for pointer aliasing info
//////////////////////////////////////////////////////////////////////

// find an existing entry, or create one, if necessary 
static alias_list_entry get_alias_list_entry(type pointer_type) 
{
  alias_list_entry a;
  struct alias_list_entry temp;

  temp.pointer_type = pointer_type;

  a = dhlookup(fv_alias_list, &temp);
  if( a ) 
    return a;

  a = ralloc(fv_region, struct alias_list_entry);
  a->pointer_type = pointer_type;
  a->vars = new_dhash_table(fv_region, 16, comparePtr, hashPtr);
  a->funcs = new_dhash_table(fv_region, 16, fv_var_use_compare, fv_var_use_hash);
  dhadd(fv_alias_list, a);

  return a;
}

static void note_pointer_use(expression e, bool is_read, bool is_write)
{
  alias_list_entry a;
  var_use u;

  /* BUG: this is too early. Aliases are not yet known.
     (i.e., the note_address_of has to happen in an earlier pass) */
  // find an entry for this type of pointer
  a = get_alias_list_entry( type_points_to(e->type) );

  // find or add the entry for this function
  u = get_var_use(a->funcs);
  
  // update usage info for this function, and for the alias as a whole
  update_usage(&a->flags, is_read, is_write);
  update_usage(&u->flags, is_read, is_write);
}


static void note_address_of(expression e)
{
  alias_list_entry a;
  identifier id;

  // find an entry for this type of pointer
  a = get_alias_list_entry(e->type);

  // find the top-level entity, for arrays and structures
  {
    expression top = e;
    while( !is_identifier(top) ) {
      switch(top->kind) 
      {
      case kind_field_ref: 
        top = CAST(field_ref,top)->arg1;
        break;
      case kind_array_ref:
        top = CAST(binary,top)->arg1;
        break;
      case kind_dereference:
	/* BUG &*x, &(*x).y, etc do not take the address of x */
	error_with_location(e->location, "alias bug");
        top = CAST(unary,top)->arg1;
        break;
      default:
        error_with_location(e->location, "taking address of complicated expression - alias analysis thwarted");
        return;
      }
    }
    id = CAST(identifier,top);
  }

  // add information about this variable
  if( !dhlookup(a->vars, id->ddecl) ) 
    dhadd(a->vars, id->ddecl);
}





//////////////////////////////////////////////////////////////////////
// manage the global list of variables
//////////////////////////////////////////////////////////////////////

static var_list_entry new_var_list_entry(const char *module, const char *name)
{
  var_list_entry v;

  v = ralloc(fv_region, struct var_list_entry);
  v->module = module;
  v->name = name;
  
  v->funcs = new_dhash_table(fv_region, 16, fv_var_use_compare, fv_var_use_hash);

  return v;
}

static void note_var_use(expression e, bool is_read, bool is_write)
{
  var_list_entry v;
  var_use u;
  identifier id;

  // ignore null expressions (as from empty return statements
  if (!e)
    return;

  // ignore things that aren't identifiers
  if( !is_identifier(e) )    
    return;
  id = CAST(identifier,e);

  // ignore static values
  // actually don't - array names come out as static.....
  //if( id->cst )
  //return;

  // ignore local variables
  if(id->ddecl->islocal) {
    //printf(" -         : %s.%s\n", current_module_name, id->cstring.data);
    return; 
  }
  

  // look for an existing entry
  {
    struct var_list_entry vstruct;
    
    vstruct.module = current_module_name;
    vstruct.name = id->cstring.data;
    v = dhlookup(fv_var_list, &vstruct); 
  }

  // allocate a new entry, if necessary
  if( !v ) {
    v = new_var_list_entry(current_module_name, id->cstring.data);
    dhadd(fv_var_list, v);
  }

  // get / add a function entry for this variable ref
  u = get_var_use(v->funcs);
  
  // update usage info for this function, and for the var as a whole
  update_usage(&v->flags, is_read, is_write);
  update_usage(&u->flags, is_read, is_write);
}
 

bool accesses_only_in_tasks(dhash_table tab)
{
  dhash_scan scanner;
  var_use u;

  scanner = dhscan(tab);
  
  while( (u=dhnext(&scanner)) ) {
    //printf("    %s\n", u->function->name);
    if( u->function->reentrant_interrupt_context || 
        u->function->atomic_interrupt_context )
      return FALSE;
  }

  // made it through the list w/o finding a non-task function.
  return TRUE;
}


void print_var_conflict_error_message(var_list_entry v)
{
  dhash_scan scanner;
  var_use u;
  char *ftype;
  char atype[20];
  data_declaration f;
  location loc;
  

  error("Detected data conflict for %s%s%s.  List of accesses follow:",
        v->module ? v->module : "",
        v->module ? "." : "",
        v->name);

  scanner = dhscan(v->funcs);
  
  while( (u=dhnext(&scanner)) ) {
    f = u->function;

    // function context
    ftype = NULL;
    if( !f->task_context ) 
      ftype = "intr";
    else if(f->reentrant_interrupt_context || f->atomic_interrupt_context)
      ftype = "task/intr";
    else 
      ftype = "task";

    // access type
    atype[0] = '\0';
    if(u->flags.read) strcat(atype, "r");
    if(u->flags.write) strcat(atype, "w");
    if((u->flags.read | u->flags.write) && (u->flags.read_in_atomic | u->flags.write_in_atomic)) strcat(atype, ", ");
    if(u->flags.read_in_atomic | u->flags.write_in_atomic) strcat(atype, "atomic ");
    if(u->flags.read_in_atomic) strcat(atype, "r");
    if(u->flags.write_in_atomic) strcat(atype, "w");

    // location
    loc = f->ast->location;
    if( f->definition ) 
      loc = f->definition->location;

    fprintf(stderr,"%s:%ld   unsafe use of %s in %s.  context=%s, use=%s\n", 
            loc->filename, loc->lineno,
            v->name, f->name, 
            ftype, atype);
  }

  fprintf(stderr, "\n");
}


void print_var_debug_summary(var_list_entry v, bool conflict) 
{
  dhash_scan s;
  var_use u;
  char *ftype;
  data_declaration f;

  printf("  %c  %c  %c  %c  %c   : %s%s%s\n",
         conflict ? 'X' : ' ',
         v->flags.read ? 'r' : ' ',
         v->flags.write ? 'w' : ' ',
         v->flags.read_in_atomic ? 'r' : ' ',
         v->flags.write_in_atomic ? 'w' : ' ',
         v->module ? v->module : "", v->module ? "." : "", v->name);
  
  s = dhscan(v->funcs);
  
  while( (u=dhnext(&s)) ) {
    f = u->function;

    ftype = NULL;
    if( !f->task_context ) 
      ftype = "interrupt";
    else if(f->reentrant_interrupt_context || f->atomic_interrupt_context)
      ftype = "task/interrupt";
    else 
      ftype = "task";
    
    printf("                    ");
    
    printf("- %s%s%s (%s, %s%s%s%s%s%s)\n", 
           f->container ? f->container->name : "",
           f->container ? "." : "",
           f->name, 
           
           ftype,
           
           u->flags.read ? "r" : "",
           u->flags.write ? "w" : "",
           (u->flags.read | u->flags.write) && (u->flags.read_in_atomic | u->flags.write_in_atomic) ? ", " : "",
           (u->flags.read_in_atomic | u->flags.write_in_atomic) ? "atomic " : "",
           u->flags.read_in_atomic ? "r" : "",
           u->flags.write_in_atomic ? "w" : "");
  }
}


void print_alias_debug_summary(alias_list_entry a, bool conflict) 
{
  dhash_scan s;
  var_use u;
  char *ftype;
  data_declaration d,f;

  printf("  %c  %c  %c  %c  %c   : alias for type %p\n",
         conflict ? 'X' : ' ',
         a->flags.read ? 'r' : ' ',
         a->flags.write ? 'w' : ' ',
         a->flags.read_in_atomic ? 'r' : ' ',
         a->flags.write_in_atomic ? 'w' : ' ',
         a->pointer_type);

  
  s = dhscan(a->funcs);
  while( (u=dhnext(&s)) ) {
    f = u->function;

    ftype = NULL;
    if( !f->task_context ) 
      ftype = "interrupt";
    else if(f->reentrant_interrupt_context || f->atomic_interrupt_context)
      ftype = "task/interrupt";
    else 
      ftype = "task";
    
    printf("                    ");
    
    printf("- %s%s%s (%s, %s%s%s%s%s%s)\n", 
           f->container ? f->container->name : "",
           f->container ? "." : "",
           f->name, 
           
           ftype,
           
           u->flags.read ? "r" : "",
           u->flags.write ? "w" : "",
           (u->flags.read | u->flags.write) && (u->flags.read_in_atomic | u->flags.write_in_atomic) ? ", " : "",
           (u->flags.read_in_atomic | u->flags.write_in_atomic) ? "atomic " : "",
           u->flags.read_in_atomic ? "r" : "",
           u->flags.write_in_atomic ? "w" : "");
  }


  s = dhscan(a->vars);
  while( (d=dhnext(&s)) ) {
    printf("                    ");
    printf("+ %s%s%s\n", 
           d->container ? d->container->name : "",
           d->container ? "." : "",
           d->name);
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
  fv_alias_list = new_dhash_table(fv_region, 512, fv_alias_list_compare, fv_alias_list_hash);
}

void fv_cleanup()
{
  deleteregion(fv_region);
  fv_region = NULL;

  fv_var_list = NULL;
  fv_alias_list = NULL;
}

void find_function_vars(data_declaration fn)
{
  function_decl fdecl;

  if( is_function_decl(fn->ast) )
    fdecl = CAST(function_decl, fn->ast);
  else if( fn->definition ) 
    fdecl = CAST(function_decl, fn->definition);
  else {
    // FIXME: deal with these
    error("can't find definition for function %s%s%s!!", 
          fn->container ? fn->container->name : "",
          fn->container ? "." : "", 
          fn->name);
    return;
  }
    

  current_function = fn;
  if( fn->container ) {
    current_module_name = fn->container->name;
  } else {
    current_module_name = NULL;
  }


  find_statement_vars( fdecl->stmt );


  current_function = NULL;
  current_module_name = NULL;
}


// walk the list of variables, and find data races
void check_for_conflicts(void) 
{
  dhash_scan scanner;
  var_list_entry v;
  alias_list_entry a;
  bool conflict;

  
  // check regular variable conflicts
  scanner = dhscan(fv_var_list);
  while( (v=dhnext(&scanner)) ) {
    conflict = TRUE;
    
    // no conflict if all reads
    if( !v->flags.write && !v->flags.write_in_atomic )
      conflict = FALSE;
    
    // no conflict if all accesses are in atomic
    else if( !v->flags.read && !v->flags.write )
      conflict = FALSE;

    // no conflict if all accesses are in tasks
    else if( accesses_only_in_tasks(v->funcs) ) 
      conflict = FALSE;

    // otherwise, there is a conflict!
    
    // print summary info
    print_var_debug_summary(v, conflict);

    if( conflict ) 
      print_var_conflict_error_message( v );
  } 

  printf("\n--------------------------------------------------\n\n");

  // check pointer alias conflicts
  scanner = dhscan(fv_alias_list);
  while( (a=dhnext(&scanner)) ) {
    conflict = TRUE;

    // no conflict if all reads
    if( !a->flags.write && !a->flags.write_in_atomic )
      conflict = FALSE;
    
    // no conflict if all accesses are in atomic
    else if( !a->flags.read && !a->flags.write )
      conflict = FALSE;

    // no conflict if all accesses are in tasks
    else if( accesses_only_in_tasks(a->funcs) ) 
      conflict = FALSE;

    // otherwise, there is a conflict!
    
    // print summary info
    print_alias_debug_summary(a, conflict);

    if( conflict ) 
      ; //FIXME print_alias_conflict_error_message( a );
  }

}
