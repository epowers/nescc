
/*

AST_field_ref           foo.bar
AST_binary: array_ref   foo[i]
AST_unary: dereference  *

foo->bar   =>   *foo.bar

*/


#include "parser.h"
#include "dhash.h"
#include "c-parse.h"
#include "AST_utils.h"
#include "semantics.h"
#include "constants.h"

#include "nesc-findvars.h"

#include <limits.h>
#include <stdlib.h>

static void add_ref(expression e, bool is_write);
static void add_pointer_use(expression e, bool is_write);
static char path_buf[PATH_MAX+1];

// Track the uses of a variable within a specific function
typedef struct var_use {
  data_declaration function;
  bool read;
  bool write;
  bool read_in_atomic;
  bool write_in_atomic;
} *var_use;



// Track all of the uses (program-wide) of a particular set of aliased
// variables.  nesc-findvars generates a global list of var_list_entry
// structures.
typedef struct var_list_entry { 
  const char *module;
  const char *name;
  
  dhash_table vars;   // list of data declarations for the variables
  dhash_table funcs;  // list of struct var_use, that define how the var is used in each function that uses it

  bool read;
  bool write;
  bool read_in_atomic;
  bool write_in_atomic;

} *var_list_entry;



static dhash_table fv_table = NULL;
static region fv_region = NULL;

// the current function being operated on, so we don't have to look it up all the time
static data_declaration current_function = NULL;
static const char *current_module_name = NULL;

static env fv_current_env = NULL;
static env fv_module_env = NULL;


// whether or not we are currently in an atomic statement
bool in_atomic = FALSE;




//////////////////////////////////////////////////////////////////////
// functions for trapsing through the AST
//////////////////////////////////////////////////////////////////////

static void find_statement_vars(statement stmt);
static void find_expression_vars(expression expr, bool in_update);

static void find_elist_vars(expression elist)
{
  expression e;

  scan_expression (e, elist) {
    find_expression_vars(e, FALSE);
  }
}


static void find_expression_vars(expression expr, bool in_update)
{
  if (!expr)
    return;

  if (expr->cst || is_string(expr))
    return;

  switch (expr->kind)
    {
    case kind_identifier:
      // default for identifiers: read access to the specified variable
      add_ref(expr,FALSE);
      break;

    case kind_interface_deref:
      break;

    case kind_comma:
      find_elist_vars(CAST(comma, expr)->arg1);
      break;

    case kind_cast_list: {
      find_expression_vars(CAST(cast_list, expr)->init_expr, FALSE);
      break;
    }
    case kind_init_index: {
      init_index init = CAST(init_index, expr);
      
      find_expression_vars(init->init_expr, FALSE);
      break;
    }
    case kind_init_field: {
      init_field init = CAST(init_field, expr);

      find_expression_vars(init->init_expr, FALSE);
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
	    find_expression_vars(ce->arg2, FALSE);
	  else
	    find_expression_vars(ce->arg1, FALSE);
	}
      else
	{
	  find_expression_vars(ce->condition, FALSE);
	  find_expression_vars(ce->arg1, FALSE);
	  find_expression_vars(ce->arg2, FALSE);
	}
      break;
    }
    case kind_compound_expr:
      find_statement_vars(CAST(compound_expr, expr)->stmt);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);

      // don't follow identifiers
      if( !is_identifier(fce->arg1) )
        find_expression_vars(fce->arg1, FALSE);
      find_elist_vars(fce->args);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);

      // don't follow identifiers
      if( !is_identifier(fce->arg1) )
        find_expression_vars(fce->arg1, FALSE);

      find_elist_vars(fce->args);
      break;
    }
    case kind_extension_expr: {
      find_expression_vars(CAST(unary, expr)->arg1, FALSE);
      break;
    }
    case kind_array_ref: {
      array_ref are = CAST(array_ref, expr);

      if( is_identifier(are->arg1) ) {
        // simple array name.  Add directly, rather than via
        // find_expression_vars, since array names end up as constant
        // expressions.
        add_ref(are->arg1, in_update);
      } else {
        // complicated array name.  trace down to the highest-level structure
        find_expression_vars(are->arg1, in_update);
      }

      // add read refs for vars mentioned in the array index
      find_expression_vars(are->arg2, FALSE);

      break;
    }
    case kind_field_ref: {
      field_ref fre = CAST(field_ref, expr);
      find_expression_vars(fre->arg1, in_update);

      // FIXME: possibly track specific fields seperately?  Something like this:
      // add_ref(fre->cstring.data, in_update);
      break;
    }
    case kind_dereference: {
      dereference de = CAST(dereference, expr);
      
      if( is_identifier(de->arg1) ) {
        add_pointer_use(de->arg1, in_update);
      } else {
        fprintf(stderr,"%s:%ld:  complicated pointer deref\n",
                realpath(expr->location->filename,path_buf), expr->location->lineno);
        find_expression_vars(de->arg1, in_update);
      } 
    }
    // unary updates
    case kind_preincrement: case kind_postincrement: case kind_predecrement: case kind_postdecrement: {
      unary ue = CAST(unary, expr);

      if( is_identifier(ue->arg1) )
        add_ref(ue->arg1,TRUE);
      else {
        if( !is_field_ref(expr) && !is_dereference(expr) && !is_array_ref(expr) ) 
          fprintf(stderr,"%s:%ld:  complicated lvalue in inc/dec\n",
                  realpath(expr->location->filename,path_buf), expr->location->lineno);
        find_expression_vars(ue->arg1, TRUE);
      }
      break;
    }
    // binary updates
    case kind_assign:  case kind_plus_assign:  case kind_minus_assign:  case kind_times_assign:  
    case kind_divide_assign:  case kind_modulo_assign:  case kind_lshift_assign:  case kind_rshift_assign:  
    case kind_bitand_assign:  case kind_bitor_assign:  case kind_bitxor_assign: {
      binary be = CAST(binary, expr);
      
      if( is_identifier(be->arg1) )
        add_ref(be->arg1, TRUE);
      else {
        if( !is_field_ref(expr) && !is_dereference(expr) && !is_array_ref(expr) ) 
          fprintf(stderr,"%s:%ld:  complicated lvalue in assignment\n",
                  realpath(expr->location->filename,path_buf), expr->location->lineno);
        find_expression_vars(be->arg1, TRUE);
      }

      find_expression_vars(be->arg2, FALSE);
      break;
    }
    default:
      if (is_unary(expr)) {
        unary ue = CAST(unary, expr);
	find_expression_vars(ue->arg1, in_update);
      }
      else if (is_binary(expr)) {
        binary be = CAST(binary, expr);
        find_expression_vars(be->arg1, in_update);
        find_expression_vars(be->arg2, in_update);
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
      env temp_env;

      temp_env = fv_current_env;
      fv_current_env = cs->env->id_env;

      scan_declaration (d, cs->decls)
	if (is_data_decl(d))
	  {
	    variable_decl vd;

	    // Include size of initialisers of non-static variables
	    scan_variable_decl (vd, CAST(variable_decl, CAST(data_decl, d)->decls))
	      if (vd->ddecl->kind == decl_variable && vd->ddecl->vtype != variable_static)
		find_expression_vars(vd->arg1, FALSE);
	  }

      scan_statement (s, cs->stmts)
	find_statement_vars(s);

      fv_current_env = temp_env;

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
	  find_expression_vars(is->condition, FALSE);
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

      find_expression_vars(es->arg1, FALSE);
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
      find_expression_vars(cs->condition, FALSE);
      find_statement_vars(cs->stmt);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);

      find_statement_vars(fs->stmt);
      find_expression_vars(fs->arg1, FALSE);
      find_expression_vars(fs->arg2, FALSE);
      find_expression_vars(fs->arg3, FALSE);
      break;
    }
    case kind_break_stmt: case kind_continue_stmt: case kind_goto_stmt:
      break;

    case kind_empty_stmt:
      break;

    case kind_computed_goto_stmt: {
      computed_goto_stmt cgs = CAST(computed_goto_stmt, stmt);

      find_expression_vars(cgs->arg1, FALSE);
      break;
    }
    case kind_return_stmt: {
      return_stmt rs = CAST(return_stmt, stmt);
      find_expression_vars(rs->arg1, FALSE);
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

static int fv_compare(void *entry1, void *entry2)
{
  var_list_entry v1 = (var_list_entry) entry1;
  var_list_entry v2 = (var_list_entry) entry2;

  // FIXME: change this around, once aliasing support is added
  if ( safe_strcmp(v1->name,v2->name) ) return 0;
  if ( safe_strcmp(v1->module,v2->module) ) return 0;
  return 1;

  //return strcmp(v1->name,v2->name)==0 && strcmp(v1->module,v2->module)==0;
}


static unsigned long fv_hash(void *entry)
{
  var_list_entry v = (var_list_entry) entry;
 
  return hashStr(v->name) ^ hashStr(v->module);
}


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
// manage the global list of variables
//////////////////////////////////////////////////////////////////////

// returns TRUE if the var belongs to a local scope
static inline bool is_local_variable(identifier id)
{
  if( env_lookup_stop(fv_current_env, id->cstring.data, fv_module_env) )
    return TRUE;

  return FALSE;
}


static var_list_entry new_table_entry(const char *module, const char *name)
{
  var_list_entry v;

  v = ralloc(fv_region, struct var_list_entry);
  v->module = module;
  v->name = name;
  
  v->vars = new_dhash_table(fv_region, 16, comparePtr, hashPtr);
  v->funcs = new_dhash_table(fv_region, 16, fv_var_use_compare, fv_var_use_hash);

  return v;
}


static void add_pointer_use(expression e, bool is_write)
{
  // FIXME: do something here
}

static void add_ref(expression e, bool is_write)
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
  id->marked = TRUE;

  // ignore static values
  // actually don't - array names come out as static.....
  //if( id->cst )
  //return;

  // ignore local variables
  if(is_local_variable(id) ) {
    //printf(" -         : %s.%s\n", current_module_name, id->cstring.data);
    return; 
  }
  
#if 0
  printf("    %c  %c   : %s.%s\n", 
         is_write ? 'w' : ' ',
         in_atomic ? 'a' : ' ',
         current_module_name, id->cstring.data);
#endif

  // look for an existing entry
  {
    struct var_list_entry vstruct;
    
    vstruct.module = current_module_name;
    vstruct.name = id->cstring.data;
    v = dhlookup(fv_table, &vstruct); 
  }

  // allocate a new entry, if necessary
  if( !v ) {
    v = new_table_entry(current_module_name, id->cstring.data);
    dhadd(fv_table, v);
  }

  // get / add a function entry for this variable ref
  {
    struct var_use ustruct;
    ustruct.function = current_function;
    u = dhlookup(v->funcs, &ustruct);

    if( !u ) {
      u = ralloc(fv_region, struct var_use);
      u->function = ustruct.function;
      u->read = FALSE;
      u->write = FALSE;
      u->read_in_atomic = FALSE;
      u->write_in_atomic = FALSE;
      dhadd(v->funcs, u);
    } 
  }
  
  // update usage info for this function, and for the var as a whole
  if(in_atomic) {
    if(is_write) {
      u->write_in_atomic = TRUE;
      v->write_in_atomic = TRUE;
    } else {
      u->read_in_atomic = TRUE;
      v->read_in_atomic = TRUE;
    }
  } else {
    if(is_write) {
      u->write = TRUE;
      v->write = TRUE;
    } else {
      u->read = TRUE;
      v->read = TRUE;
    }
  }
}
 

bool accesses_only_in_tasks(var_list_entry v)
{
  dhash_scan scanner;
  var_use u;

  scanner = dhscan(v->funcs);
  
  while( (u=dhnext(&scanner)) ) {
    //printf("    %s\n", u->function->name);
    if( u->function->reentrant_interrupt_context || 
        u->function->atomic_interrupt_context )
      return FALSE;
  }

  // made it through the list w/o finding a non-task function.
  return TRUE;
}


void print_conflict_error_message(var_list_entry v)
{
  dhash_scan scanner;
  var_use u;
  char *ftype;
  data_declaration f;
  

  error("Detected data conflict for %s%s%s.  List of accesses follow:",
        v->module ? v->module : "",
        v->module ? "." : "",
        v->name);

  scanner = dhscan(v->funcs);
  
  while( (u=dhnext(&scanner)) ) {
    f = u->function;

    ftype = NULL;
    if( !f->task_context ) 
      ftype = "interrupt";
    else if(f->reentrant_interrupt_context || f->atomic_interrupt_context)
      ftype = "task/interrupt";
    else 
      ftype = "task";

    fprintf(stderr,"%s:%ld:\n    Access to %s%s%s in %s%s%s (%s, %s%s%s%s%s%s)\n", 
            f->definition ? f->definition->location->filename : f->ast->location->filename,
            f->definition ? f->definition->location->lineno : f->ast->location->lineno,

            v->module ? v->module : "",
            v->module ? "." : "",
            v->name, 

            f->container ? f->container->name : "",
            f->container ? "." : "",
            f->name, 

            ftype,
            
            u->read ? "r" : "",
            u->write ? "w" : "",
            (u->read | u->write) && (u->read_in_atomic | u->write_in_atomic) ? ", " : "",
            (u->read_in_atomic | u->write_in_atomic) ? "atomic " : "",
            u->read_in_atomic ? "r" : "",
            u->write_in_atomic ? "w" : "");
  }

  fprintf(stderr, "\n");
}


void print_debug_summary(var_list_entry v, bool conflict) 
{
  dhash_scan s;
  var_use u;
  char *ftype;
  data_declaration f;

  printf("  %c  %c  %c  %c  %c   : %s%s%s\n",
         conflict ? 'X' : ' ',
         v->read ? 'r' : ' ',
         v->write ? 'w' : ' ',
         v->read_in_atomic ? 'r' : ' ',
         v->write_in_atomic ? 'w' : ' ',
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
           
           u->read ? "r" : "",
           u->write ? "w" : "",
           (u->read | u->write) && (u->read_in_atomic | u->write_in_atomic) ? ", " : "",
           (u->read_in_atomic | u->write_in_atomic) ? "atomic " : "",
           u->read_in_atomic ? "r" : "",
           u->write_in_atomic ? "w" : "");
  }
}




//////////////////////////////////////////////////////////////////////
// external routines
//////////////////////////////////////////////////////////////////////

void fv_init() 
{
  if(fv_region == NULL) 
    fv_region = newregion();

  fv_table = new_dhash_table(fv_region, 512, fv_compare, fv_hash);
}

void fv_cleanup()
{
  deleteregion(fv_region);
  fv_region = NULL;

  fv_table = NULL;
}

void find_function_vars(data_declaration fn)
{
  function_decl fdecl = CAST(function_decl,fn->ast);

  current_function = fn;
  if( fn->container ) {
    fv_current_env = fv_module_env = fn->container->impl->ienv->id_env;
    current_module_name = fn->container->name;
  } else {
    fv_current_env = fv_module_env = global_env->id_env;
    current_module_name = NULL;
  }


  find_statement_vars( fdecl->stmt );


  current_function = NULL;
  current_module_name = NULL;
  fv_current_env = NULL;
  fv_module_env = NULL;
}


// walk the list of variables, and find data races
void check_for_conflicts(void) 
{
  dhash_scan scanner;
  var_list_entry v;
  bool conflict;

  scanner = dhscan(fv_table);
  
  while( (v=dhnext(&scanner)) ) {
    conflict = TRUE;
    
    // no conflict if all writes
    if( !v->write && !v->write_in_atomic )
      conflict = FALSE;
    
    // no conflict if all accesses are in atomic
    else if( !v->read && !v->write )
      conflict = FALSE;

    // no conflict if all accesses are in tasks
    else if( accesses_only_in_tasks(v) ) 
      conflict = FALSE;

    // otherwise, there is a conflict!
    
    // print summary info
    print_debug_summary(v, conflict);

    if( conflict ) 
      print_conflict_error_message( v );
  } 
  
}


