// $Id: Type.java,v 1.2 2005/01/27 21:33:55 idgay Exp $
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
 * Base class for all elements representing types.
 * <p>
 * equals() is defined on all Type objects and represents C type equality.
 */
abstract public class Type extends NDElement
{
    /**
     * Size of this type in bytes. May be a non or unknown constant.
     */
    public Constant size;

    /**
     * Alignment for this type in bytes. May be a non or unknown constant.
     */
    public Constant alignment;

    public NDElement start(Attributes attrs) {
	size = Constant.decode(attrs.getValue("size"));
	alignment = Constant.decode(attrs.getValue("alignment"));
	return this;
    }
}
