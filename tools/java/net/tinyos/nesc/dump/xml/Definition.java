// $Id: Definition.java,v 1.6 2005/01/19 23:00:23 idgay Exp $
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
import java.util.*;

abstract public class Definition extends NDElement
{
    public boolean definitionAvailable;
    public LinkedList/*Xattribute_value*/ attributes;
    public Location location; /* may be null */

    public void init(Attributes attrs) {
	location = Location.decode(attrs.getValue("loc"));
    }

    public void child(NDElement subElement) {
	if (subElement instanceof Xattribute_value) {
	    if (attributes == null)
		attributes = new LinkedList();
	    attributes.add(subElement);
	}
    }

    /* Returns an attribute called name, or null for none */
    public Xattribute_value attributeLookup(String name) {
	if (attributes == null)
	    return null;

	ListIterator elems = attributes.listIterator();

	while (elems.hasNext()) {
	    Xattribute_value attr = (Xattribute_value)elems.next();
	    Xattribute a = attr.attribute;
	    String n = a.name;

	    if (n.equals(name))
		return attr;
	}
	return null;
    }
}
