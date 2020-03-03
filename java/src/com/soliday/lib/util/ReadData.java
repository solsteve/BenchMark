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
import java.net.*;
import java.util.*;

// ==========================================================================================
/**
 * Read numbers and Strings in a C like manner.
 *
 * $Log: ReadData.java,v $
 * Revision 1.1  1997/02/23 14:35:22  soliday
 * Initial revision
 *
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class ReadData {
    // --------------------------------------------------------------------------------------
    public boolean  isOpen;
    public boolean  EOF;

    int             returnType;
    BufferedReader  br;
    StreamTokenizer st;
    private String  fileName;

    // ======================================================================================
    /**
     * Contructor function.
     * Open a file for READ.
     *
     * @param fspc path name or URL
     */
    // --------------------------------------------------------------------------------------
    public ReadData( String fspc ) {
        // ----------------------------------------------------------------------------------
        isOpen = false;
        EOF    = false;

        br     = null;

        fileName = new String( fspc );

        if ((fspc.startsWith("http:"))||
            (fspc.startsWith("file:"))) {
            try {
                URL url = new URL(fspc);
                try {
                    br = new BufferedReader(new
                        InputStreamReader(url.openStream()));
                } catch(IOException e) {
                    System.err.println("url [" + fspc +
                                       "] did not produce an " +
                                       "InputStream");
                }
            } catch(MalformedURLException e) {
                System.err.println("["+fspc+"] is a bad URL");
            }
        } else {
            try {
                br = new BufferedReader(new FileReader(fspc));
            } catch (FileNotFoundException e) {
                System.err.println("file ["+fspc+"] not found");
            }
        }

        if (br != null) {
            st = new StreamTokenizer((Reader)br);
            st.commentChar('#');
            st.quoteChar('"');
            st.wordChars('?', '?');
            isOpen = true;
            getNextToken();
        }
    }

    // ======================================================================================
    /**
     * Close file of URL connection after WRITE.
     */
    // --------------------------------------------------------------------------------------
    public void close( ) {
        // ----------------------------------------------------------------------------------
        try {
            isOpen = false;
            br.close();
            br = null;
        } catch(IOException f) {
            System.out.println("File [" + fileName +
                               "] failed during close");
        }
    }

    // ======================================================================================
    /**
     * Get the next token.
     * Get the next token from the file and check for an EOF condition.
     */
    // --------------------------------------------------------------------------------------
    void getNextToken() {
        // ----------------------------------------------------------------------------------
        try {
            returnType = st.nextToken();
            if (returnType == StreamTokenizer.TT_EOF)
                {
                    EOF = true;
                }
        } catch (IOException x) {
        }
    }

    // ======================================================================================
    /**
     * Read a String.
     * Test to see if the next token is a (string) and return it if it is.
     *
     * @return String read from next token
     */
    // --------------------------------------------------------------------------------------
    public String readString() {
        // ----------------------------------------------------------------------------------
        switch(returnType) {
            // ------------------------------------------------------------------------------
        case '"':
        case StreamTokenizer.TT_WORD:
            String s = st.sval;
            getNextToken();
            return s;
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_EOF:
            System.out.println("ReadData::Read ["+fileName+"] Past End");
        default:
            System.out.println("ReadData:: Error Reading String. Read "+
                               returnType+" instead");
            break;
        }

        return "ERROR";
    }

    // ======================================================================================
    /**
     * Read an Integer.
     * Test to see if the next token is a (int) and return it if it is.
     *
     * @return int read from next token
     */
    // --------------------------------------------------------------------------------------
    public int readInt() {
        // ----------------------------------------------------------------------------------
        switch(returnType) {
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_NUMBER:
            int n = (int)st.nval;
            getNextToken();
            return n;
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_EOF:
            System.out.println("ReadData::Read ["+fileName+"] Past End");
        default:
            System.out.println("ReadData::Error Reading Int. Read "+
                               returnType+" instead");
            break;
        }

        return (int)0;
    }

    // ======================================================================================
    /**
     * Read a Long.
     * Test to see if the next token is a (long) and return it if it is.
     *
     * @return long read from next token
     */
    // --------------------------------------------------------------------------------------
    public long readLong() {
        // ----------------------------------------------------------------------------------
        switch(returnType) {
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_NUMBER:
            long n = (long)st.nval;
            getNextToken();
            return n;
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_EOF:
            System.out.println("ReadData::Read ["+fileName+"] Past End");
        default:
            System.out.println("ReadData::Error Reading Int. Read "+
                               returnType+" instead");
            break;
        }

        return (long)0;
    }

    // ======================================================================================
    /**
     * Read a Float.
     * Test to see if the next token is a (float) and return it if it is.
     *
     * @return float read from next token
     */
    // --------------------------------------------------------------------------------------
    public float readFloat() {
        // ----------------------------------------------------------------------------------
        switch(returnType) {
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_NUMBER:
            float n = (float)st.nval;
            getNextToken();
            return n;
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_EOF:
            System.out.println("ReadData::Read ["+fileName+"] Past End");
        default:
            System.out.println("ReadData::Error Reading Float. Read "+
                               returnType+" instead");
            break;
        }

        return 0.0f;
    }

    // ======================================================================================
    /**
     * Read a Double.
     * Test to see if the next token is a (double) and return it if it is.
     *
     * @return  double read from next token
     */
    // --------------------------------------------------------------------------------------
    public double readDouble() {
        // ----------------------------------------------------------------------------------
        switch(returnType) {
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_NUMBER:
            double n = st.nval;
            getNextToken();
            return n;
            // ------------------------------------------------------------------------------
        case StreamTokenizer.TT_EOF:
            System.out.println("ReadData::Read ["+fileName+"] Past End");
        default:
            System.out.println("ReadData::Error Reading Double. Read "+
                               returnType+" instead");
            System.out.println("["+st.sval+"]");
            break;
        }

        return 0.0d;
    }
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
