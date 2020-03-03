// ===== BEGIN FILE =========================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2005, Stephen W. Soliday                                              **
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

// =========================================================================== END FILE =====
