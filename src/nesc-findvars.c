

#include "parser.h"
#include "dhash.h"
#include "c-parse.h"
#include "AST_utils.h"
#include "nesc-cg.h"
#include "graph.h"

#include "nesc-findvars.h"

/*
static void add_ref(expression e);



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
  char *module;
  char *name;
  
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

// whether or not we are currently in an atomic statement
bool in_atomic = FALSE;




//////////////////////////////////////////////////////////////////////
// functions for trapsing through the AST
//////////////////////////////////////////////////////////////////////

static void find_statement_vars(statement stmt);
static void find_expression_vars(expression expr);

static void find_elist_vars(expression elist)
{
  expression e;

  scan_expression (e, elist)
    find_expression_vars(e);
}


static void find_expression_vars(expression expr)
{
  if (!expr)
    return;

  if (expr->cst || is_string(expr))
    return;

  switch (expr->kind)
    {
    case kind_identifier: 
      add_ref(expr):
      break;

    case kind_atomic_stmt:
      in_atomic = 1;
      find_elist_vars(CAST(atomic_stmt,expr)->stmt);
      in_atomic = 0;
      break;

    case kind_comma:
      find_elist_vars(CAST(comma, expr)->arg1);
      break;

    case kind_cast_list: {
      find_expression_vars(CAST(cast_list, expr)->init_expr);
      break;
    }
    case kind_init_index: {
      init_index init = CAST(init_index, expr);

      find_expression_vars(init->init_expr);
      break;
    }
    case kind_init_field: {
      init_field init = CAST(init_field, expr);

      find_expression_vars(init->init_expr);
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
	    find_expression_vars(ce->arg2);
	  else
	    find_expression_vars(ce->arg1);
	}
      else
	{
	  find_expression_vars(ce->condition);
	  find_expression_vars(ce->arg1);
	  find_expression_vars(ce->arg2);
	}
      break;
    }
    case kind_compound_expr:
      find_statement_vars(CAST(compound_expr, expr)->stmt);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);

      find_expression_vars(fce->arg1);
      find_elist_vars(fce->args);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);

      find_expression_vars(fce->arg1);
      find_elist_vars(fce->args);
      break;
    }
    case kind_extension_expr:
      find_expression_vars(CAST(unary, expr)->arg1);
      break;

    default:
      if (is_unary(expr))
	find_expression_vars(CAST(unary, expr)->arg1);
      else if (is_binary(expr))
	{
	  binary be = CAST(binary, expr);
          
	  find_expression_vars(be->arg1);
	  find_expression_vars(be->arg2);
	}
      else 
	assert(0);
      break;
    }

  return sum;
}

static void find_statement_vars(statement stmt)
{
  if (!stmt)
    return 0;

  switch (stmt->kind)
    {
    case kind_asm_stmt: {
      1;
      break;
    }
    case kind_compound_stmt: {
      compound_stmt cs = CAST(compound_stmt, stmt);
      statement s;
      declaration d;

      scan_declaration (d, cs->decls)
	if (is_data_decl(d))
	  {
	    variable_decl vd;

	    // Include size of initialisers of non-static variables
	    scan_variable_decl (vd, CAST(variable_decl,
					 CAST(data_decl, d)->decls))
	      if (vd->ddecl->kind == decl_variable &&
		  vd->ddecl->vtype != variable_static)
		find_expression_vars(vd->arg1);
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
	  2 + find_expression_vars(is->condition);
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

      find_expression_vars(es->arg1);
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
      2 + find_expression_vars(cs->condition);
      find_statement_vars(cs->stmt);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);

      find_statement_vars(fs->stmt);
      find_expression_vars(fs->arg1);
      find_expression_vars(fs->arg2);
      find_expression_vars(fs->arg3);
      break;
    }
    case kind_break_stmt: case kind_continue_stmt: case kind_goto_stmt:
      break;

    case kind_empty_stmt:
      break;

    case kind_computed_goto_stmt: {
      computed_goto_stmt cgs = CAST(computed_goto_stmt, stmt);

      find_expression_vars(cgs->arg1);
      break;
    }
    case kind_return_stmt: {
      return_stmt rs = CAST(return_stmt, stmt);

      find_expression_vars(rs->arg1);
      break;
    }
    case kind_atomic_stmt: {
      atomic_stmt as = CAST(atomic_stmt, stmt);

      find_statement_vars(as->stmt);
      break;
    }
    default: assert(0);
    }

  return sum;
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


static unsigned long fv_func_hash(void *entry)
{
  var_use u = (var_use) entry;
 
  return hashPtr(u->function);
}





//////////////////////////////////////////////////////////////////////
// manage the global list of variables
//////////////////////////////////////////////////////////////////////

// returns TRUE if the var belongs to a local scope
static inline bool is_local_variable(expression e)
{
  // FIXME: DO THIS 
  return FALSE;
}

// returns TRUE if the identifier is just a function name
static inline bool is_function_name(expression e)
{
  // FIXME
  return FALSE;
}

// returns TRUE if the access is a write access
static inline bool check_write(expression e)
{
  // FIXME: 
  return TRUE;
}



static var_list_entry new_table_entry(char *module, char *name)
{
  var_list_entry v;

  v = ralloc(fv_region, struct var_list_entry);
  v->module = module;
  v->nane = name;
  
  vars = new_dhash_table(fv_region, 16, comparePtr, hashPtr);
  funcs = new_dhash_table(fv_region, 16, fv_var_use_compare, fv_var_use_hash);

  return v;
}


static void add_ref(expression e)
{
  bool is_write;
  var_list_entry v;
  var_use u;


  // ignore things that aren't identifiers
  if( !is_identifier(e) )    
    return;

  // ignore function calls
  if( is_function_name(e) )  
    return;

  // ignore local variables
  if(is_local_variable e)    
    return; 

  // determine if we have a read or a write
  is_write = check_write(e);

  // look for an existing entry
  {
    struct var_list_entry vstruct;
    
    vstruct.module = "foo";
    vstruct.name = "bar";
    vp = dhlookup(fv_table, &vstruct); 
  }

  // allocate a new entry, if necessary
  if( !v ) {
    v = new_table_entry("foo", "bar");
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
      v->write = TRUE;
    }
  }
}
 


//////////////////////////////////////////////////////////////////////
// external routines
//////////////////////////////////////////////////////////////////////

void fv_init() 
{
  if(fv_region == NULL) 
    fv_region = newregion();

  dhash_table fv_table = new_dhash_table(fv_region, 64, fv_compare, fv_hash);
}

void fv_cleanup()
{
  deleteregion(fv_region);
  fv_region = NULL;

  fv_table = NULL;
}

void find_function_vars(data_declaration fn)
{
  current_function = fn;

  find_expression_vars(fn->stmt);

  current_function = NULL;
}

*/
