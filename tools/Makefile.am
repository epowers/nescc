# This file is part of the galsC compiler.
#
# This file is derived from the nesC compiler.  It is thus
#    Copyright (C) 2002 Intel Corporation
# Changes for galsC are
#    Copyright (C) 2003-2004 Palo Alto Research Center
# 
# The attached "galsC" software is provided to you under the terms and
# conditions of the GNU General Public License Version 2 as published by the
# Free Software Foundation.
# 
# galsC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with galsC; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

AUTOMAKE_OPTIONS = foreign

SUBDIRS = avr-as

ncclibdir=$(libdir)/ncc
editordir=$(ncclibdir)/editor-modes
emacsdir=$(editordir)/emacs
vimdir=$(editordir)/vim
kdedir=$(editordir)/kde

bin_SCRIPTS = mig ncg ncc nescc nesdoc galscc

ncclib_DATA = genjava.pm gencstjava.pm migdecode.pm
emacs_DATA =					\
	editor-modes/emacs/nesc.el 		\
	editor-modes/emacs/new-nesc.el		\
	editor-modes/emacs/old-nesc.el		\
	editor-modes/emacs/readme.txt
vim_DATA =					\
	editor-modes/vim/nesc.vim		\
	editor-modes/vim/filetype.vim		\
        editor-modes/emacs/readme.txt
kde_DATA = 					\
	editor-modes/kde/nesc.xml		\
	editor-modes/kde/readme.txt


EXTRA_DIST =					\
	$(ncclib_DATA)				\
	$(emacs_DATA)				\
	$(vim_DATA)				\
	$(kde_DATA)


