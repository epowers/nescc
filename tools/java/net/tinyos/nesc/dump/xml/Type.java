// $Id: Type.java,v 1.1 2005/01/07 18:29:16 idgay Exp $
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

public class Type extends NDElement
{
    public Constant size, alignment;

    public NDElement start(Attributes attrs) {
	size = Constant.decode(attrs.getValue("size"));
	alignment = Constant.decode(attrs.getValue("alignment"));
	return this;
    }
}
