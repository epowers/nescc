// $Id: WiringScanBackwards.java,v 1.3 2005/01/27 21:33:55 idgay Exp $
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

/**
 * A scanner to scan backwards in a wiring graph
 */
public class WiringScanBackwards extends WiringScan
{
    /**
     * Create a new backwards wiring scanner starting at wiring graph node n,
     * with no arguments.
     * @param n Wiring node graph to start at.
     */
    public WiringScanBackwards(WiringNode n) { 
	node = n;
    }

    /**
     * Create a new backwards wiring scanner starting at wiring graph node n,
     * with arguments a.
     * @param n Wiring node graph to start at.
     * @param a Position arguments.
     */
    public WiringScanBackwards(WiringNode n, Xarguments a) { 
	node = n;
	arguments = a;
    }

    public boolean isForwards() {
	return false;
    }

    public java.util.ListIterator edges() {
	return node.incomingEdges();
    }

    public boolean follow(Xwire e) {
	return e.followBackwards(this);
    }
}
