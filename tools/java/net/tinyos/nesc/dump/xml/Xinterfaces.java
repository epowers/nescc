// $Id: Xinterfaces.java,v 1.6 2005/09/27 04:05:39 celaine Exp $
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
 * Top-level interface list.
 */
public class Xinterfaces extends NDList
{
    public NDElement end() {
        Xnesc.interfaceList = l;
	return this;
    }
}
