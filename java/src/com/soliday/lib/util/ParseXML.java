// ===== BEGIN FILE =========================================================================
// **                              P R O P R I E T A R Y                                   **
// ==========================================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2002, Stephen W. Soliday                                              **
// **                      stephen@soliday.com                                             **
// **                      http://www.soliday.com/stephen                                  **
// **                                                                                      **
// **  ----------------------------------------------------------------------------------  **
// **                                                                                      **
// **  This file, and associated source code, is not free software; you may not            **
// **  redistribute it and/or modify it. This library is currently in an on going          **
// **  development phase by its author and has, as yet, not been publicly distributed.     **
// **  Development of this library has been at the sole cost in both time and funding by   **
// **  its author. Until such a public release is made the author retains ALL RIGHTS to    **
// **  this software. It is expected that if and when this library is deemed releasable    **
// **  it will be released under the GNU Public license for non-commercial use or with a   **
// **  restricted rights for government use. At that time each source file will contain    **
// **  either/both the standard GPL statement/disclaimer, and/or the DFARS Restricted      **
// **  Rights Legend.                                                                      **
// **                                                                                      **
// **  This library exists at the present time WITHOUT ANY WARRANTY; without even the      **
// **  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.            **
// **  As you are not supposed to be in possession of this file if you use it,             **
// **  you use this code AT YOUR OWN RISK.                                                 **
// **                                                                                      **
// ==========================================================================================

package com.soliday.lib.util;

import javax.xml.xpath.*;
import org.xml.sax.*;
import java.io.*;

// ==========================================================================================
// ------------------------------------------------------------------------------------------
public class ParseXML {
    // --------------------------------------------------------------------------------------
    protected String      xmlContext  = null;
    protected InputSource inputSource = null;
    protected XPath       xPath       = null;

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public ParseXML() {
	// ----------------------------------------------------------------------------------
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public boolean load(String xmlFile, String xc) {
	// ----------------------------------------------------------------------------------
	xPath = XPathFactory.newInstance().newXPath();
	inputSource = new InputSource(xmlFile);

	xmlContext = new String(xc+"/");
	
	return true;
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public String xpath(String express) {
	// ----------------------------------------------------------------------------------
	String rs = null;
	try {
	    rs =  new String(xPath.evaluate( xmlContext+express, inputSource ));
	    xPath.reset();
	} catch (XPathExpressionException e) {
	    System.err.println(xmlContext+express);
	    System.err.println(e.toString());
	    return null;
	}
	
	return rs;
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public String count(String express) {
	// ----------------------------------------------------------------------------------
	String rs = null;
	try {
	    rs = new String(xPath.evaluate( xmlContext+express, inputSource  ));
	    xPath.reset();
	} catch (XPathExpressionException e) {
	    System.err.println(xmlContext+express);
	    System.err.println(e.toString());
	    return null;
	}
	
	return rs;
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    static public void main( String[] args ) {
	// ----------------------------------------------------------------------------------

	if (args.length > 2) {

	    ParseXML P = new ParseXML();

	    P.load( args[0], args[1] );

	    int n = args.length;

	    for (int i=2; i<n; i++) {
		String s = P.xpath(args[i]);
		
		System.out.println( args[1]+"/"+args[i] + " = " + s );
	    }
	} else {
	    System.out.println("USAGE: V2 java -cp ./build/lib/SolLib.jar com.soliday.lib.util.ParseXML");
	    System.out.println("USAGE: ... XMLFile XMLContext XPathExp1 [XPathExp2 ... ]");
	}
    }
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
