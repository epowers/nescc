// $Id: Xtags.java,v 1.3 2005/02/03 20:15:21 idgay Exp $
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
 * Top-level tag-based (enum, struct, etc) types list.
 */
public class Xtags extends NDList
{
    public static LinkedList/*TagDefinition*/ list;

    public NDElement end() {
	list = l;
	return this;
    }
}
