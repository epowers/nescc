/* This file is part of the galsC compiler.

This file is derived from the nesC compiler and RC and the GNU C Compiler.
It is thus
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.
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

#ifndef ERRORS_H
#define ERRORS_H

/* Name of program invoked, sans directories.  */
extern const char *progname;

extern int errorcount;
extern int warningcount;

/* Report error msg at filename, lineno */
void verror_with_file_and_line(const char *filename, int lineno,
			       const char *format, va_list args);

/* Report error msg at l */
void verror_with_location(location l, const char *format, va_list args);

/* Report error msg at decl */
void verror_with_decl(declaration d, const char *format, va_list args);

/* Report error msg at current filename, lineno */
void verror(const char *format, va_list args);

/* Report error msg at current filename, lineno */
void error(const char *format, ...);

/* Report error msg at decl */
void error_with_decl(declaration d, const char *format, ...);

/* Report error msg at l */
void error_with_location(location l, const char *format, ...);

/* Report a fatal error at the current line number.  */
void vfatal(const char *format, va_list args);

void fatal(const char *format, ...);

/* Report warning msg at filename, lineno */
void vwarning_with_file_and_line(const char *filename, int lineno,
				 const char *format, va_list args);

/* Report warning msg at l */
void vwarning_with_location(location l, const char *format, va_list args);

/* Report warning msg at decl */
void vwarning_with_decl(declaration d, const char *format, va_list args);

/* Report warning msg at current filename, lineno */
void vwarning(const char *format, va_list args);

/* Report warning msg at current filename, lineno */
void warning(const char *format, ...);


/* Report warning msg at filename, lineno */
void warning_with_file_and_line(const char *filename, int lineno,
				const char *format, ...);

/* Report warning msg at decl */
void warning_with_decl(declaration d, const char *format, ...);

/* Report warning msg at l */
void warning_with_location(location l, const char *format, ...);

/* Report warning msg at current filename, lineno */
void warning_or_error(bool iswarning, const char *format, ...);


/* Report warning msg at filename, lineno */
void warning_or_error_with_file_and_line(bool iswarning,
					 const char *filename, int lineno,
					 const char *format, ...);

/* Report warning msg at decl */
void warning_or_error_with_decl(bool iswarning, declaration d,
				const char *format, ...);

/* Report warning msg at l */
void warning_or_error_with_location(bool iswarning, location l,
				    const char *format, ...);

/* Report pedantic warning or error msg at current filename, lineno */
void pedwarn(const char *format, ...);

/* Report pedantic warning or error msg at d */
void pedwarn_with_decl(declaration d, const char *format, ...);

/* Report pedantic warning or error msg at l */
void pedwarn_with_location(location l, const char *format, ...);

#endif
