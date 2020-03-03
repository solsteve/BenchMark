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

import java.io.*;
import java.util.*;

// ==========================================================================================
/**
 * US MTF Class
 *
 * $Log$
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class USMTF {
    // --------------------------------------------------------------------------------------
    Vector<SET> setList;

    // --------------------------------------------------------------------------------------
    /**
     * Constructor Function.
     */
    // --------------------------------------------------------------------------------------
    public USMTF() {
	// ----------------------------------------------------------------------------------
	    setList = new Vector<SET>();
    }

    // --------------------------------------------------------------------------------------
    /**
     * Constructor Function.
     */
    // --------------------------------------------------------------------------------------
    public USMTF(String msg) {
	// ----------------------------------------------------------------------------------
	    setList = new Vector<SET>();
	    addMessage(msg);
    }

    // --------------------------------------------------------------------------------------
    /**
     * provides the number of sets in this message.
     * @return number of sets
     */
    // --------------------------------------------------------------------------------------
    public int nSet() {
	return setList.size();
    }

    // --------------------------------------------------------------------------------------
    /**
     * provides the number of sets in this message.
     * @return number of sets
     */
    // --------------------------------------------------------------------------------------
    public int nField(int idx) {
	return setref(idx).nField();
    }

    // --------------------------------------------------------------------------------------
    /**
     * provides the number of sets in this message.
     * @return number of sets
     */
    // --------------------------------------------------------------------------------------
    public int addMessage(String msg) {

	StringTokenizer st = new StringTokenizer(msg, "|", true);

	while (st.hasMoreTokens()) {
	    String sn = st.nextToken();
	    String d1 = st.nextToken();
	    SET s = new SET(sn);
	    setList.add(s);
//  	    System.out.println("SET :"+sn+ "   >"+d1+"<");
	    while (st.hasMoreTokens()) {
		String fv = st.nextToken();
		if (fv.startsWith("|")) {
//  		    System.out.println("   END");
		    break;
		}
		String d2 = st.nextToken();
		s.add(fv);
//  		System.out.println("   FIELD :"+fv+ "   >"+d2+"<");
	    }
	}
	
	return 0;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Get set.
     * Return a reference to a set.
     * @param idx index of the set (first set is idx=0).
     * @return reference to the set.
     */
    // --------------------------------------------------------------------------------------
    private SET setref(int idx) {
	return (SET)setList.get(idx);
    }

    // --------------------------------------------------------------------------------------
    /**
     */
    // --------------------------------------------------------------------------------------
    public String set(int idx) {
	return ((SET)setList.get(idx)).name;
    }

    // --------------------------------------------------------------------------------------
    /**
     */
    // --------------------------------------------------------------------------------------
    public String field(int si, int fi) {
	return ((SET)setList.get(si)).field(fi);
    }

    // --------------------------------------------------------------------------------------
    /**
     */
    // --------------------------------------------------------------------------------------
    public int index(String str) {
	int n = nSet();

	for (int j=0; j<n; j++) {
	    if (set(j).equalsIgnoreCase(str)) return j;
	}

	return -1;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Querry for set name.
     * Search the set list for the string (str)
     * @param str String to querey for a set name
     * @return true if set is found false if not
     */
    // --------------------------------------------------------------------------------------
    public boolean hasSet(String str) {
	int n = nSet();

	for (int j=0; j<n; j++) {
	    if (set(j).equalsIgnoreCase(str)) return true;
	}

	return false;
    }

    // --------------------------------------------------------------------------------------
    public int writeToFile(String fspc) {
	// ----------------------------------------------------------------------------------
	PrintWriter out = null;

	try  {
	    FileWriter     FW  = new FileWriter(fspc);
	    BufferedWriter BW  = new BufferedWriter(FW);
	    out = new PrintWriter(BW);
	} catch (IOException e) {
	    System.err.println(e.toString());
	}

	if (out == null) {
	    return 0;
	}

	int n = nSet();

	for (int i=0; i<n; i++) {
	    SET s = setref(i);
	    s.println(out);
	}

	out.close();
	return n;
    }

    // --------------------------------------------------------------------------------------
    public int readFromFile(String fspc) {
	// ----------------------------------------------------------------------------------
	FileReader     fr = null;
	BufferedReader br = null;

	String buffer;
	String message = new String("");

	try {
	     fr = new FileReader(fspc);
	     br = new BufferedReader(fr);
	} catch (FileNotFoundException e) {
	    System.err.println(e.toString());
	    return 0;
	}
	
	if (null == br) {
	    System.err.println("Something went wrong opening the file.");
	    return 0;
	}

	// --------------------- read the file and construct a string -----------------------

	try {
	    while(null != (buffer = br.readLine()))
		{
		    message = message + buffer;
		}
	} catch (IOException e) {
	    System.err.println(e.toString());
	    return 0;
	}
	
	// ----------------------------------------------------------------------------------

	this.addMessage( message );

	return this.nSet();
    }

// ==========================================================================================
/**
 * USMTF Set of fields
 *
 * $Log$
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
    private class SET {
	// ----------------------------------------------------------------------------------
	private String name;
	private Vector<String> fieldList;

	// ----------------------------------------------------------------------------------
	/**
	 * Constructor Function.
	 */
	// ----------------------------------------------------------------------------------
	private SET() {
	    name      = new String("");
	    fieldList = new Vector<String>();
	}

	// ----------------------------------------------------------------------------------
	/**
	 * Constructor Function.
	 */
	// ----------------------------------------------------------------------------------
	private SET(String nm) {
	    name      = new String(nm);
	    fieldList = new Vector<String>();
	}

	// ----------------------------------------------------------------------------------
	/**
	 * Add a field to a set
	 */
	// ----------------------------------------------------------------------------------
	private void add(String f) {
	    fieldList.add(f);
	}

	// ----------------------------------------------------------------------------------
	/**
	 * provides the number of fields in this set.
	 * @return number of fields
	 */
	// ----------------------------------------------------------------------------------
	private int nField() {
	    return fieldList.size();
	}

	// ----------------------------------------------------------------------------------
	/**
	 * Get field contents.
	 * Return a String with the contents of a field.
	 * @param idx index of the field (first field is idx=0)
	 * @return String with the contents of the field
	 */
	// ----------------------------------------------------------------------------------
	private String field(int idx) {
	    return new String((String)fieldList.get(idx));
	}

	// ----------------------------------------------------------------------------------
	// ----------------------------------------------------------------------------------
	public void println(PrintWriter o) {
	    // ------------------------------------------------------------------------------
	    int n = nField();
	    o.print(name+"|");
	    for (int i=0; i<n; i++) {
		o.print(field(i)+"|");
	    }
	    o.println("|");
	}
    }



    // --------------------------------------------------------------------------------------
    /**
     * Test Function.
     * Test the USMTF Class by reading a file.
     * @param args String the contains the full path name of the file to
     *             be read and parsed.
     */
    // --------------------------------------------------------------------------------------
    public static void main(String[] args) {
	// ----------------------------------------------------------------------------------
	switch(args.length) {
	case 1:
	    break;
	default:
	    System.out.println("java com.soliday.lib.util.USMTF " +
			       "./Data/message.usmtf");
	    System.exit(1);
	}

	USMTF mtf = new USMTF();

	mtf.readFromFile(args[0]);

	int n = mtf.nSet();

	System.out.println("The USMTF Message contains " + n + " sets.");

	for (int i=0; i<n; i++) {
	    String sn = mtf.set(i);

	    System.out.println("Set: " + sn);
	    int k = mtf.nField(i);

	    for (int j=0; j<k; j++) {
		String fn = mtf.field(i,j);
		System.out.println("  F: " + j + " : "+fn);
	    }
	}

	mtf.addMessage("OQU|STEVE|HEATHER|REBEKAH|JACOB||");

	// ----- test queries ---------------------------------------------------------------

	n = mtf.nSet();
	System.out.println("The USMTF Message contains " + n + " sets.");

	for (int i=0; i<n; i++) {
	    String sn = mtf.set(i);

	    System.out.println("Set: " + sn);
	    int k = mtf.nField(i);

	    for (int j=0; j<k; j++) {
		String fn = mtf.field(i,j);
		System.out.println("  F: " + j + " : "+fn);
	    }
	}

	mtf.writeToFile("test.mtf");

	// ----- test queries ---------------------------------------------------------------

	int h = mtf.index("ADDR");

	if (h == -1) {
	    System.out.println("Set: ADDR was not found.");
	} else {
	    System.out.println("Set: "+h+" is ADDR.");
	}

    }
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
