// $Id: Xarguments.java,v 1.3 2005/01/27 21:33:55 idgay Exp $
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
 * A list of arguments. Each argument is a Constant or Type.
 * <p>
 * equals() compares these arguments lists.
 */
public class Xarguments extends NDList
{
    public boolean equals(Object obj) {
	if (!(obj instanceof Xarguments))
	    return false;
	return l.equals(((Xarguments)obj).l);
    }
}
