// $Id: Xnesc.java,v 1.2 2005/02/03 20:15:21 idgay Exp $
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

/**
 * Top-level XML dump element. Doesn't do anything useful, see the elements
 * which can be embedded here to actually get at the dumped information.
 * @see net.tinyos.nesc.dump.xml.Xwiring
 * @see net.tinyos.nesc.dump.xml.Xcomponents
 * @see net.tinyos.nesc.dump.xml.Xinterfaces
 * @see net.tinyos.nesc.dump.xml.Xinterfacedefs
 * @see net.tinyos.nesc.dump.xml.Xtags
 */
public class Xnesc extends NDElement
{
}
