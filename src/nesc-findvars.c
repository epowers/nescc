


static void find_statement_vars(statement stmt, dhash_table list);
static void find_expression_vars(expression expr, dhash_table list);

static void find_elist_vars(expression elist, dhash_table list)
{
  expression e;

  scan_expression (e, elist)
    find_expression_vars(e, list);
}


static void find_expression_vars(expression expr, dhash_table list)
{
  if (!expr)
    return;

  if (expr->cst || is_string(expr))
    return;

  switch (expr->kind)
    {
    case kind_identifier: 
      // FIXME: add the variable
      break;

    case kind_atomic_stmt:
      // skip atomic statements
      break;

    case kind_comma:
      find_elist_vars(CAST(comma, expr)->arg1, list);
      break;

    case kind_cast_list: {
      find_expression_vars(CAST(cast_list, expr)->init_expr, list);
      break;
    }
    case kind_init_index: {
      init_index init = CAST(init_index, expr);

      find_expression_vars(init->init_expr, list);
      break;
    }
    case kind_init_field: {
      init_field init = CAST(init_field, expr);

      find_expression_vars(init->init_expr, list);
      break;
    }
    case kind_init_list: {
      find_elist_vars(CAST(init_list, expr)->args, list);
      break;
    }
    case kind_conditional: {
      conditional ce = CAST(conditional, expr);

      if (ce->condition->cst)
	{
	  if (definite_zero(ce->condition))
	    find_expression_vars(ce->arg2, list);
	  else
	    find_expression_vars(ce->arg1, list);
	}
      else
	{
	  find_expression_vars(ce->condition, list);
	  find_expression_vars(ce->arg1, list);
	  find_expression_vars(ce->arg2, list);
	}
      break;
    }
    case kind_compound_expr:
      find_statement_vars(CAST(compound_expr, expr)->stmt, list);
      break;

    case kind_function_call: {
      function_call fce = CAST(function_call, expr);

      find_expression_vars(fce->arg1, list);
      find_elist_vars(fce->args, list);
      break;
    }
    case kind_generic_call: {
      generic_call fce = CAST(generic_call, expr);

      find_expression_vars(fce->arg1, list);
      find_elist_vars(fce->args, list);
      break;
    }
    case kind_extension_expr:
      find_expression_vars(CAST(unary, expr)->arg1, list);
      break;

    default:
      if (is_unary(expr))
	find_expression_vars(CAST(unary, expr)->arg1, list);
      else if (is_binary(expr))
	{
	  binary be = CAST(binary, expr);
          
	  find_expression_vars(be->arg1, list);
	  find_expression_vars(be->arg2, list);
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

	    /* Include size of initialisers of non-static variables */
	    scan_variable_decl (vd, CAST(variable_decl,
					 CAST(data_decl, d)->decls))
	      if (vd->ddecl->kind == decl_variable &&
		  vd->ddecl->vtype != variable_static)
		1 + find_expression_vars(vd->arg1, list);
	  }

      scan_statement (s, cs->stmts)
	find_statement_vars(s, list);
      break;
    }
    case kind_if_stmt: {
      if_stmt is = CAST(if_stmt, stmt);

      if (is->condition->cst)
	{
	  if (definite_zero(is->condition))
	    find_statement_vars(is->stmt2, list);
	  else
	    find_statement_vars(is->stmt1, list);
	}
      else
	{
	  2 + find_expression_vars(is->condition, list);
	  find_statement_vars(is->stmt1, list);
	  find_statement_vars(is->stmt2, list);
	}
      break;
    }
    case kind_labeled_stmt: {
      labeled_stmt ls = CAST(labeled_stmt, stmt);

      find_statement_vars(ls->stmt, list);
      break;
    }
    case kind_expression_stmt: {
      expression_stmt es = CAST(expression_stmt, stmt);

      find_expression_vars(es->arg1, list);
      break;
    }
    case kind_while_stmt: case kind_dowhile_stmt: case kind_switch_stmt: {
      conditional_stmt cs = CAST(conditional_stmt, stmt);

      if (cs->condition->cst && stmt->kind != kind_switch_stmt &&
	  definite_zero(cs->condition))
	{
	  /* do s while (0): just include size of s
	     while (0) s: size is 0 */
	  if (stmt->kind == kind_dowhile_stmt)
	    find_statement_vars(cs->stmt, list);
	  break;
	}
      2 + find_expression_vars(cs->condition, list);
      find_statement_vars(cs->stmt, list);
      break;
    }
    case kind_for_stmt: {
      for_stmt fs = CAST(for_stmt, stmt);

      find_statement_vars(fs->stmt, list);
      find_expression_vars(fs->arg1, list);
      find_expression_vars(fs->arg2, list);
      find_expression_vars(fs->arg3, list);
      break;
    }
    case kind_break_stmt: case kind_continue_stmt: case kind_goto_stmt:
      break;

    case kind_empty_stmt:
      break;

    case kind_computed_goto_stmt: {
      computed_goto_stmt cgs = CAST(computed_goto_stmt, stmt);

      find_expression_vars(cgs->arg1, list);
      break;
    }
    case kind_return_stmt: {
      return_stmt rs = CAST(return_stmt, stmt);

      find_expression_vars(rs->arg1, list);
      break;
    }
    case kind_atomic_stmt: {
      atomic_stmt as = CAST(atomic_stmt, stmt);

      find_statement_vars(as->stmt, list);
      break;
    }
    default: assert(0);
    }

  return sum;
}

void find_function_vars(function_decl fd)
{
  return find_statement_vars(fd->stmt, list);
}

