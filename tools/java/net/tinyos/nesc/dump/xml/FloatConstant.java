// $Id: FloatConstant.java,v 1.3 2005/01/27 21:33:55 idgay Exp $
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
 * Class representing floating point constants
 */
public class FloatConstant extends KnownConstant
{
    /**
     * Value of this floating point constant
     */
    public double value;

    public FloatConstant(String s) {
	value = NDElement.realDecode(s.substring(2), 0.0);
    }

    public boolean equals(Object obj) {
	if (!(obj instanceof FloatConstant))
	    return false;
	return value == ((FloatConstant)obj).value;
    }
}
