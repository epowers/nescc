// $Id: Xinterface.java,v 1.3 2005/01/07 22:17:51 idgay Exp $
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

public class Xinterface extends DataDefinition
{
    public boolean provided;
    public Xinstance instance;
    public LinkedList/*Type*/ parameters; /* null for non-parameterised interfaces */

    public NDElement start(Attributes attrs) {
	Xinterface me = (Xinterface)super.start(attrs);
	me.provided = attrs.getValue("provided").equals("1");
	return me;
    }

    public void child(NDElement subElement) {
	super.child(subElement);
	if (subElement instanceof Xinstance)
	    instance = (Xinstance)subElement;
	if (subElement instanceof Xinterface_parameters)
	    parameters = ((Xinterface_parameters)subElement).l;
    }
}
