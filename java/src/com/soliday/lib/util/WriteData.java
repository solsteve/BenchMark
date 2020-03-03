// ===== BEGIN FILE =========================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2004, Stephen W. Soliday                                              **
// **                      stephen@soliday.com                                             **
// **                      http://www.soliday.com/stephen                                  **
// **                                                                                      **
// **  ----------------------------------------------------------------------------------  **
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
// **                                                                                      **
// **  ----- Modification History -------------------------------------------------------  **
// **                                                                                      **
// **  Author Stephen W. Soliday                                                           **
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

// =========================================================================== END FILE =====
