// $Id: WiringPosition.java,v 1.1 2005/01/11 23:27:53 idgay Exp $
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

public class WiringPosition
{
    public WiringNode node;
    public Xarguments args;

    public void copy(WiringPosition from) {
	node = from.node;
	args = from.args;
    }
}
