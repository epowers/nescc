// $Id: Xcomponents.java,v 1.5 2005/01/27 21:33:55 idgay Exp $
/*									tab:4
 * Copyright (c) 2004-2005 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE     
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
 * 94704.  Attention:  Intel License Inquiry.
 */

package net.tinyos.nesc.dump.xml;

import java.util.*;

/**
 * Top-level component list.
 */
public class Xcomponents extends NDList
{
    /**
     * The list of all components in the dump.
     */
    public static LinkedList/*Xcomponent*/ list;

    public NDElement end() {
	list = l;
	return this;
    }
}