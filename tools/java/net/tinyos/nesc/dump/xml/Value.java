// $Id: Value.java,v 1.3 2005/02/03 20:15:21 idgay Exp $
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
 * Base class for elements representing C initialisers (the {} stuff in
 *  struct foo x = { ... }
 * Attributes use these initialisers too.
 * <p>
 * equals() is defined only for initialisers representing C constants.
 */
abstract public class Value extends NDElement
{
    /**
     * Type of this value.
     */
    public Type type;

    public void child(NDElement subElement) {
	if (subElement instanceof Type)
	    type = (Type)subElement;
    }
}
