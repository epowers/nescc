// $Id: SimpleType.java,v 1.2 2005/01/17 22:57:26 idgay Exp $
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

public class SimpleType extends Type
{
    public String cname; /* C name of type */

    public NDElement start(Attributes attrs) {
	super.start(attrs);
	cname = attrs.getValue("cname");
	return this;
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof SimpleType))
	    return false;
	return cname.equals(((SimpleType)obj).cname);
    }
}
