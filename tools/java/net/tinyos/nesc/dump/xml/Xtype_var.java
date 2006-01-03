// $Id: Xtype_var.java,v 1.4 2006/01/03 23:50:52 idgay Exp $
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
 * A type variable.
 */
public class Xtype_var extends Type
{
    /**
     * A typedef object which uniquely identifies this type variable.
     */
    public Xtypedef var;

    public void child(NDElement subElement) {
	if (subElement instanceof Xtypedef)
	    var = (Xtypedef)subElement;
	super.child(subElement);
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof Xtype_var))
	    return false;
	return var == ((Xtype_var)obj).var;
    }
}
