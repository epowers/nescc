// $Id: WiringGraph.java,v 1.3 2005/01/19 23:00:23 idgay Exp $
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

import java.util.*;

public class WiringGraph
{
    protected Hashtable endpoints = new Hashtable();

    public WiringNode lookup(DataDefinition epDecl) {
	WiringNode found = (WiringNode)endpoints.get(epDecl);

	if (found == null) {
	    found = new WiringNode(epDecl);
	    endpoints.put(epDecl, found);
	}
	return found;
    }

    public void addEdge(Xwire wire) {
	wire.from.node = lookup(wire.from.node.ep);
	wire.from.node.addToEdge(wire);

	wire.to.node = lookup(wire.to.node.ep);
	wire.to.node.addFromEdge(wire);
    }
}
