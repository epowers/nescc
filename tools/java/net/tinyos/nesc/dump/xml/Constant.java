// $Id: Constant.java,v 1.3 2005/01/27 21:33:55 idgay Exp $
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
 * Base class for nesC constants. nesC constants are encoded in attribute
 * strings. Note that there are representations for "unknown constant" and
 * "non constant" values.
 * <p>
 * equals() is defined on Constant objects. Unknown and non constant
 * objects compare different with every other constant (including themselves).
 */
abstract public class Constant
{
    /**
     * Decode a nesC constant string.
     * @param s String to decode.
     * @return An object representing the constant encoded by s
     */
    public static Constant decode(String s) {
	switch (s.charAt(0)) {
	case 'I': return new IntegerConstant(s);
	case 'F': return new FloatConstant(s);
	case 'S': return new StringConstant(s);
	case 'V': return new NonConstant();
	case 'U': default: return new UnknownConstant();
	}
    }

    /** 
     * Is the constant a known constant? 
     * @return true of the object represents an integer, floating
     * point or string
     */
    public boolean known() {
	return false;
    }

    /**
     * Does this object represent an actual constant?
     * @return true if the object represents a constant
     */
    public boolean constant() {
	return false;
    }
}