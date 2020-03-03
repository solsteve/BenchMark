// ===== BEGIN FILE =========================================================================
// **                              P R O P R I E T A R Y                                   **
// ==========================================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2004, Stephen W. Soliday                                              **
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

import java.awt.*;
import java.io.*;
import java.util.*;

// ==========================================================================================
/**
 * Write numbers and Strings in a C like manner
 *
 * $Log: WriteData.java,v $
 * Revision 1.1  1997/02/23 14:35:22  soliday
 * Initial revision
 *
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class WriteData {
    // --------------------------------------------------------------------------------------
    public boolean  isOpen;
    int             returnType;
    FileWriter      file;
    private String  fileName;

    // ======================================================================================
    /**
     * Constructor function.
     * Open a file for WRITE
     *
     * @param fspc path name for file
     */
    // --------------------------------------------------------------------------------------
    public WriteData( String fspc ) {
        // ----------------------------------------------------------------------------------
        isOpen = false;
        fileName = new String( fspc );
        
        try {
            file = new FileWriter(fspc);
            isOpen = true;
        } catch (IOException f) {
            System.out.println("File Not Found: "+fspc);
            isOpen = false;
        }
    }

    // ======================================================================================
    /**
     * Close a file after WRITE
     */
    // --------------------------------------------------------------------------------------
    public void close( ) {
        // ----------------------------------------------------------------------------------
        try {
            isOpen = false;
            file.flush();
            file.close();
            file = null;
        } catch (IOException f) {
            System.out.println("File [" + fileName +
                               "] failed during close");
        }
    }

    // ======================================================================================
    /**
     * Write a String.
     * Write a raw string with no leading whitespace to a file
     *
     * @param str String value to write
     */
    // --------------------------------------------------------------------------------------
    public void writeStringNoLead( String str ) {
        // ----------------------------------------------------------------------------------
        try {
            file.write(str);
        } catch (IOException f) {
            System.out.println("Error writting [" + str +
                               "] to binary stream["+fileName+"] .");
        }
    }

    // ======================================================================================
    /**
     * Write a String.
     * Lead a string with a single whitespace and write it to the file
     *
     * @param str String value to write
     */
    // --------------------------------------------------------------------------------------
    public void writeString( String str ) {
        // ----------------------------------------------------------------------------------
        writeStringNoLead(" "+str);
    }

    // ======================================================================================
    /**
     * Write a Delimited String.
     * Enclose a String in user defined delimeters and write it to the file.
     *
     * @param str String value to write
     * @param d String representation of the delimiters
     */
    // --------------------------------------------------------------------------------------
    public void writeStringDelim( String str, String d ) {
        // ----------------------------------------------------------------------------------
        writeString(d+str+d);
    }

    // ======================================================================================
    /**
     * Write a Quoted String.
     * Enclose a String in double-qoutes and write it to the file.
     *
     * @param str String value to write
     */
    // --------------------------------------------------------------------------------------
    public void writeStringQuote( String str ) {
        // ----------------------------------------------------------------------------------
        writeString("\""+str+"\"");
    }

    // ======================================================================================
    /**
     * Write a new-line character
     * Convert a '\n' value to a string and write it to the file.
     */
    // --------------------------------------------------------------------------------------
    public void writeBreak( ) {
        // ----------------------------------------------------------------------------------
        writeString("\n");
    }

    // ======================================================================================
    /**
     * Write an Integer.
     * Convert a int value to a formated string and write it to the file.
     *
     * @param v int value to write
     */
    // --------------------------------------------------------------------------------------
    public void writeInt( int v ) {
        // ----------------------------------------------------------------------------------
        writeString(Integer.toString(v));
    }

    // ======================================================================================
    /**
     * Write a Long.
     * Convert a long value to a formated string and write it to the file.
     *
     * @param v long value to write
     */
    // --------------------------------------------------------------------------------------
    public void writeLong( long v ) {
        // ----------------------------------------------------------------------------------
        writeString(Long.toString(v));
    }

    // ======================================================================================
    /**
     * Write a Float.
     * Convert a float value to a formated string and write it to the file.
     *
     * @param v float value to write
     */
    // --------------------------------------------------------------------------------------
    public void writeFloat( float v ) {
        // ----------------------------------------------------------------------------------
        writeString(Float.toString(v));
    }

    // ======================================================================================
    /**
     * Write a Double.
     * Convert a double value to a formated string and write it to the file.
     *
     * @param v double value to write
     */
    // --------------------------------------------------------------------------------------
    public void writeDouble( double v ) {
        // ----------------------------------------------------------------------------------
        writeString(Double.toString(v));
    }
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
