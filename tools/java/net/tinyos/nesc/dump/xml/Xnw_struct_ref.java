// $Id: Xnw_struct_ref.java,v 1.2 2005/01/11 23:27:53 idgay Exp $
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

import net.tinyos.nesc.dump.*;
import org.xml.sax.*;

public class Xnw_struct_ref extends NDElement
{
    public NDElement start(NDReader reader, Attributes attrs) {
	return TagDefinition.lookup(reader, attrs, "nw_struct");
    }
}
