// $Id: StringConstant.java,v 1.2 2005/01/17 22:57:27 idgay Exp $
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

public class StringConstant extends KnownConstant
{
    public String value;

    public StringConstant(String s) {
	value = s.substring(2);
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof StringConstant))
	    return false;
	return value.equals(((StringConstant)obj).value);
    }
}
