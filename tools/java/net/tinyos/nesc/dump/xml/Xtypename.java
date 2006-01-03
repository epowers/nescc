// $Id: Xtypename.java,v 1.1 2006/01/03 23:50:52 idgay Exp $
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

import org.xml.sax.*;

/**
 * The typedef used to define a type.
 */
public class Xtypename extends NDElement
{
    /**
     * The typedef stored in this typename element
     */
    public Xtypedef tdef;

    public void child(NDElement subElement) {
	if (subElement instanceof Xtypedef)
	    tdef = (Xtypedef)subElement;
    }
}
