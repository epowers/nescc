// $Id: Xstructured_element.java,v 1.5 2005/02/03 20:15:21 idgay Exp $
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
 * Elements of a structure initialiser (Xvalue_structured)
 * @see net.tinyos.nesc.dump.xml.Xvalue_structured
 */
public class Xstructured_element extends NDElement
{
    /**
     * What field is being initialised.
     */
    public Xfield field;

    /**
     * Initialiser value.
     */
    public Value value;

    public NDElement start(NDReader reader, Attributes attrs) {
	field = (Xfield)Xfield.lookup(reader, attrs);
	return this;
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Value)
	    value = (Value)subElement;
    }
}
