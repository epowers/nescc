// $Id: Xcomponent.java,v 1.3 2005/01/07 22:17:50 idgay Exp $
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

public class Xcomponent extends NescDefinition implements Container
{
    public Xinstance instance; /* optional */
    public Xparameters parameters; /* present iff component is generic */
    public Implementation implementation;

    public void child(NDElement subElement) {
	if (subElement instanceof Xinstance)
	    instance = (Xinstance)subElement;
	if (subElement instanceof Xparameters)
	    parameters = (Xparameters)subElement;
	if (subElement instanceof Implementation)
	    implementation = (Implementation)subElement;
    }

}
