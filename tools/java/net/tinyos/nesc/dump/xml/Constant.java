// $Id: Constant.java,v 1.2 2005/01/17 22:57:16 idgay Exp $
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

abstract public class Constant
{
    public static Constant decode(String s) {
	switch (s.charAt(0)) {
	case 'I': return new IntegerConstant(s);
	case 'F': return new FloatConstant(s);
	case 'S': return new StringConstant(s);
	case 'V': return new NonConstant();
	case 'U': default: return new UnknownConstant();
	}
    }

    public boolean known() {
	return false;
    }

    public boolean constant() {
	return false;
    }
}
