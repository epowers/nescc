/* This file is part of the nc2MoMLlib generator.

   Copyright (C) 2004 Elaine Cheong

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

/*
  Author: Elaine Cheong
  Created: 12 March 2004

  This file contains routines for generating MoML from nesC source code.

  momllib routines generate a MoML listing of all .nc components in
  the input directory.  Pass the following flags to nesc1 (include the
  dash '-') :
      -_fmomldir=<outputdirname>
      -_fmomllib

  For more information, see:
      http://ptolemy.eecs.berkeley.edu/publications/papers/00/moml/
  
 */

#include "parser.h"
#include "c-parse.h"
#include "nesc-generate.h"
#include "nesc-inline.h"
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "nesc-c.h"
#include "unparse.h"
#include "AST_utils.h"
#include "edit.h"
#include "semantics.h"
#include "constants.h"
#include "nesc-concurrency.h"
#include "nesc-uses.h"

#include "regions.h"
#include "nesc-paths.h"
#include "nesc-doc.h"
#include "moml-generate.h"
#include "moml-paths.h"

// Output directory for generated moml files.
static const char *momldir = NULL;

// Indicates if the compiler flag -_fmomllib was set.
static bool using_momllib = FALSE;

static region moml_region = NULL;

// The name of the Ptolemy II class which represents a nesC component.
static const char *ptinyos_component_class = NULL;

// The string that should be used when generating the path to root of
// the TinyOS tree.
static const char *ptinyos_prefix = NULL;

// Initialize the memory region for the moml tools.
static void init_moml_region() {
    if (moml_region == NULL) {
        moml_region = newregion();
    }
}

// Set the output directory for the generated MoML code.
void moml_set_outdir(const char *dir) {
    assert(dir);
    init_moml_region();
    momldir = canonicalise(moml_region, dir, strlen(dir));
}

// Turn on momllib generation.
void momllib_set() {
    using_momllib = TRUE;
}

// Indicates if we should generate momllib.
bool is_momllib_set() {
    return using_momllib;
}

// Set the name of the Ptolemy II class which represents a nesC component.
void moml_ptinyos_component_set(const char *classname) {
    ptinyos_component_class = classname;
}

const char *moml_ptinyos_component_get() {
    return ptinyos_component_class;
}

// Set the string that should be used when generating the path to root
// of the TinyOS tree.
void moml_ptinyos_prefix_set(const char *prefix) {
    ptinyos_prefix = prefix;
}

const char *moml_ptinyos_prefix_get() {
    return ptinyos_prefix;
}

// Print the standard MoML boilerplate code.
static void prt_moml_header() {
    outputln("<?xml version=\"1.0\" standalone=\"no\"?>");
    outputln("<!DOCTYPE plot PUBLIC \"-//UC Berkeley//DTD MoML 1//EN\"");
    outputln("     \"http://ptolemy.eecs.berkeley.edu/xml/dtd/MoML_1.dtd\">");
}

// Print the opening entity code.
static void prt_moml_begin_entitity(const char *pathname) {
    outputln("<entity name=\"%s\" class=\"ptolemy.moml.EntityLibrary\">", element_name(moml_region, pathname));
    indent();
}

// Print the closing entity code.
static void prt_moml_end_entitity() {
    unindent();
    outputln("</entity>");
}

// Print the ports for the component "c".
// From component_functions_iterate() in nesc-component.c
static void prt_moml_component_ports(nesc_declaration c) {
  const char *ifname;
  void *ifentry;
  env_scanner scanifs;

  env_scan(c->env->id_env, &scanifs);
  while (env_next(&scanifs, &ifname, &ifentry))
    {
      data_declaration idecl = ifentry;
      
      outputln("<port name=\"%s\" class=\"ptolemy.actor.IOPort\">", idecl->name);
      if (idecl->kind == decl_interface_ref)
	{
            indent();
            if (idecl->required) {
                outputln("<property name=\"output\"/>");
            } else {
                outputln("<property name=\"input\"/>");
            }

            // See if this interface is parameterised.
            if (idecl->gparms != NULL) {
                typelist_scanner scan_gparms;
                typelist_scan(idecl->gparms, &scan_gparms);

                // Look at first type in list.
                typelist_next(&scan_gparms);

                // Look at next type in list.
                if (!typelist_next(&scan_gparms)) {
                    outputln("<property name=\"multiport\"/>");
                } else {
                    fprintf(stderr, "Don't know how to deal with more than one index for a parameterized interface in %s", idecl->name);
                }
            }
            
            unindent();

	}
      else if (idecl->kind == decl_function) {
          if (!idecl->defined) {
              outputln("<property name=\"output\"/>");
          } else {
              outputln("<property name=\"input\"/>");
          }
      }
      
      outputln("<property name=\"_showName\" class=\"ptolemy.kernel.util.SingletonAttribute\"/>");
      outputln("</port>");
    }
}

// Print the MoML code for the component in the argument named "c".
static void prt_moml_component(nesc_declaration c, const char *filename) {
    outputln("<class name=\"%s\" extends=\"%s\">", c->name, moml_ptinyos_component_get());
    indent();
    outputln("<property name=\"source\" value=\"%s\"/>", moml_source_filename(filename));
    prt_moml_component_ports(c);
    unindent();
    outputln("</class>");
}

// Print the MoML listing for the component in the argument named "c".
static void prt_moml_entity(nesc_declaration c, const char *filename) {
    outputln("<entity name=\"%s\" class=\"%s\"/>", c->name, moml_java_filename(filename));
}

// Generate momllib code.
//
// "nesccomponents" is a list of nesccomponent_t containing each parsed
// component and the full pathname to the component.
//
// "inputpathname" is the original input path for which we are generating the
// MoML lib.
//
// See generate_c_code() in nesc-generate.c
// See generate_docs() in nesc-doc.c
bool generate_momllib(dd_list nesccomponents, const char *inputpathname) {
    // If no momldir is specified, then the user didn't request
    // momllib generation.
    if (!momldir || !is_momllib_set())
        return FALSE;

    FILE *output = NULL;

    // Create an .xml for each nesC component
    {
        dd_list_pos nesccomponents_element;

        dd_scan(nesccomponents_element, nesccomponents) {
            nesccomponent_t n = DD_GET(nesccomponent_t, nesccomponents_element);
            nesc_declaration c = n->component;
            const char *filename = n->filename;
            
            char *targetname = rarrayalloc(moml_region, strlen(filename) + 1 + 1, char);
            sprintf(targetname, "%s.xml", element_pathandname(moml_region, filename));
                    
            if (targetname) {
                output = fopen(targetname, "w");
                if (!output) {
                    perror("couldn't create output file");
                    exit(2);
                }
            }
            
            unparse_start(output ? output : stdout, NULL);
            disable_line_directives();

            // Print the standard .xml header
            prt_moml_header();
            prt_moml_component(c, filename);
        
            unparse_end();

            if (output) {
                fclose(output);
                printf("Created %s\n", targetname);
            }
        }
    }

    // Create a .xml that lists the .xml components created above.
    {
        if (!dd_is_empty(nesccomponents)) {
            char *targetname = rarrayalloc(moml_region,
                    strlen(momldir) + strlen("index") + 4 + 1, char);
            sprintf(targetname, "%s%s.xml", momldir, "index");
            
            if (targetname) {
                output = fopen(targetname, "w");
                if (!output) {
                    perror("couldn't create output file");
                    exit(2);
                }
            }

            unparse_start(output ? output : stdout, NULL);
            disable_line_directives();
            
            dd_list_pos nesccomponents_element;
        
            // Print the standard .xml header
            prt_moml_header();
            
            prt_moml_begin_entitity(inputpathname);

            outputln("<configure>");
            outputln("<?moml");
            outputln("<group>");
            
            dd_scan(nesccomponents_element, nesccomponents) {
                nesccomponent_t n = DD_GET(nesccomponent_t, nesccomponents_element);
                nesc_declaration c = n->component;
                const char *filename = n->filename;
                prt_moml_entity(c, filename);
            }

            outputln("</group>");
            outputln("?>");
            outputln("</configure>");
            
            prt_moml_end_entitity();

            unparse_end();
        
            if (output) {
                fclose(output);
                printf("Created %s\n", targetname);
            }
        }
    }
    
    return TRUE;
}

