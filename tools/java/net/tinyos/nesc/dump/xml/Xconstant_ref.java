// $Id: Xconstant_ref.java,v 1.6 2005/01/27 21:33:55 idgay Exp $
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
 * Class for enum constant references. These objects go away, replaced
 * by the Xconstant object representing the definition.
 * @see net.tinyos.nesc.dump.xml.Xconstant
 * @see net.tinyos.nesc.dump.xml.Definition
 */
public class Xconstant_ref extends NDElement
{
    public NDElement start(NDReader reader, Attributes attrs) {
	return DataDefinition.lookup(reader, attrs, "constant");
    }
}
