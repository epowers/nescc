// $Id: Xnx_union_ref.java,v 1.1 2005/02/03 20:15:21 idgay Exp $
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

/**
 * Class for external-representation union references. These objects go
 * away, replaced by the Xnx_union object representing the definition.
 * @see net.tinyos.nesc.dump.xml.Xnx_union
 * @see net.tinyos.nesc.dump.xml.Definition
 */
public class Xnx_union_ref extends NDElement
{
    public NDElement start(NDReader reader, Attributes attrs) {
	return TagDefinition.lookup(reader, attrs, "nw_union");
    }
}
