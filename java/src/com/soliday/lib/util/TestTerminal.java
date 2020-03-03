// ===== BEGIN FILE =========================================================================
// **                              P R O P R I E T A R Y                                   **
// ==========================================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2005, Stephen W. Soliday                                              **
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
import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

// ==========================================================================================
/**
 * Provides a simple terminal.
 *
 * $Log: Terminal.java, v $
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class TestTerminal implements TerminalListener {
    // --------------------------------------------------------------------------------------
    Terminal myTerm = null;

    // ======================================================================================
    /**
     * Contructor function.
     */
    // --------------------------------------------------------------------------------------
    public TestTerminal() {
        // ----------------------------------------------------------------------------------
	myTerm = new Terminal("Test Term");
	myTerm.addTerminalListener(this);
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void clearButtonPressed() {
	// ----------------------------------------------------------------------------------
	System.err.println("The terminal issued a clear action.");
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void quitButtonPressed() {
	// ----------------------------------------------------------------------------------
	System.err.println("The terminal issued a quit action.");
	System.exit(0);
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void enterButtonPressed() {
	// ----------------------------------------------------------------------------------
	String text = myTerm.getEntryString();
	System.err.println(text);
    }

    // ======================================================================================
    /** Entry Point.
     *  Standard CPP entry point.
     * @param args list of command line fields.
     */
    // --------------------------------------------------------------------------------------
    static public void main( String[] args ) {
        // ----------------------------------------------------------------------------------
	TestTerminal TT = new TestTerminal();
    }
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
