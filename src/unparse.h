/* This file is part of the galsC compiler.

This file is derived from the nesC compiler, which is derived from the
RC Compiler. It is thus
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
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

#ifndef UNPARSE_H
#define UNPARSE_H

/* string which accesses current mote number in nido */
extern char *nido_mote_number;

void unparse(FILE *to, declaration program) deletes;
void unparse_start(FILE *to);
void unparse_end(void) deletes;
void enable_line_directives(void);
void disable_line_directives(void);
void set_function_separator(char *sep);
FILE *set_unparse_outfile(FILE *newout);
void enable_documentation_mode(void);
void disable_documentation_mode(void);

void set_location(location l);
void set_fixed_location(location l);
void clear_fixed_location(void);
struct location output_location(void);
void output(char *format, ...) __attribute__((format (printf, 1, 2)));
void outputln(char *format, ...) __attribute__((format (printf, 1, 2)));
void output_stripped_cstring(cstring s);
void output_string(const char *s);
void output_cstring(cstring s);
void output_stripped_string(const char *s);
void output_stripped_string_dollar(const char *s);
void copy_file_to_output(char *filename);
void newline(void);
void indent(void);
void unindent(void);
void startline(void);
void startline_noindent(void);

const char *binary_op_name(AST_kind kind);

/* Precedence levels */
#define P_TOP -1
#define P_COMMA 0
#define P_ASSIGN 1
#define P_COND 2
#define P_OR 3
#define P_AND 4
#define P_BITOR 5
#define P_BITXOR 6
#define P_BITAND 7
#define P_EQUALS 8
#define P_REL 9
#define P_SHIFT 10
#define P_PLUS 11
#define P_TIMES 12
#define P_CAST 13
#define P_CALL 14

void prt_data_decl(data_decl d);
void prt_variable_decl(variable_decl d);
void prt_toplevel_declarations(declaration d);
void prt_toplevel_declaration(declaration d);
void prt_nelements(expression array);
void prt_expressions(expression elist, bool isfirst);
void prt_expression(expression e, int context_priority);

typedef enum {
  pte_duplicate = 1,
  pte_noextern = 2,
  pte_skip_command_event = 4
} pte_options;

void prt_type_elements(type_element elements, pte_options options);

typedef enum {
  psd_need_paren_for_star = 1,
  psd_need_paren_for_qual = 2,
  psd_rename_parameters = 4,
  psd_rename_identifier = 8,
  psd_print_default = 16,
  psd_skip_container = 32
#ifdef GALSC
  ,
  // 64 is already taken
  psd_galsc_print_port_put_call = 128,
  psd_galsc_print_port_struct = 256,
  psd_galsc_print_port_get_function = 512,
  psd_galsc_print_port_get_call = 1024,
  psd_galsc_print_port_put_function_header = 2048,
  psd_galsc_print_port_put_function_body = 4096,
  psd_galsc_print_sched_init = 8192
#endif
} psd_options;

void prt_declarator(declarator d, type_element elements, attribute attributes,
		    data_declaration ddecl, psd_options options);
bool prt_simple_declarator(declarator d, data_declaration ddecl,
			   psd_options options);
void prt_parameters(declaration gparms, declaration parms, psd_options options);
bool prt_parameter(declaration parm, bool first, bool lastforward, psd_options options);
void prt_ddecl_full_name(data_declaration ddecl, psd_options options);
void prt_plain_ddecl(data_declaration ddecl, psd_options options);

void prt_function_body(function_decl d);

#endif
