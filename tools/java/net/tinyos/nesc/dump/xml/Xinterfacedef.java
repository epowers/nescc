// $Id: Xinterfacedef.java,v 1.4 2005/01/27 21:33:55 idgay Exp $
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

/**
 * An interface definition.
 */
public class Xinterfacedef extends NescDefinition
{
    /**
     * (definition only) Type parameters of generic interfaces.
     */
    public Xparameters parameters;
    public LinkedList/*Xfunction*/ functions = new LinkedList();

    public void child(NDElement subElement) {
	if (subElement instanceof Xparameters)
	    parameters = (Xparameters)subElement;
	if (subElement instanceof Xfunction)
	    functions.add(subElement);
    }
}
