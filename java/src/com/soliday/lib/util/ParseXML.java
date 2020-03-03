// ===== BEGIN FILE =========================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2002, Stephen W. Soliday                                              **
// **                      stephen@soliday.com                                             **
// **                      http://www.soliday.com/stephen                                  **
// **                                                                                      **
// **  This program is free software: you can redistribute it and/or modify it under       **
// **  the terms of the GNU General Public License as published by the Free Software       **
// **  Foundation, either version 3 of the License, or (at your option)                    **
// **  any later version.                                                                  **
// **                                                                                      **
// **  This program is distributed in the hope that it will be useful, but WITHOUT         **
// **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS       **
// **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.      **
// **                                                                                      **
// **  You should have received a copy of the GNU General Public License along with        **
// **  this program. If not, see <http://www.gnu.org/licenses/>.                           **
// **  ----------------------------------------------------------------------------------  **
// **                                                                                      **
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

// =========================================================================== END FILE =====
