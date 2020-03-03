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
 * Provides a simple terminal widgit.
 *
 * THINGS TO DO: Add history
 *
 * $Log: EntryPanel.java, v $
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class EntryPanel extends JPanel {
    // --------------------------------------------------------------------------------------
    JTextArea           display    = null;
    JTextField          entry      = null;
    boolean             shouldEcho = true;
    EntryPanelListener  listener   = null;
    static final String newline    = "\n";

    // ======================================================================================
    /** Build the GUI.
     */
    // --------------------------------------------------------------------------------------
    public EntryPanel(int rows, int cols ) {
        // ----------------------------------------------------------------------------------
	this.setLayout(new BorderLayout());

	display = new JTextArea( rows, cols );
	display.setEditable(false);

        JScrollPane scrollPane =
	    new JScrollPane(display,
			    JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			    JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	this.add( scrollPane, BorderLayout.CENTER);

	entry = new JTextField(cols);
	entry.addActionListener( new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    pressEnter();
		}
	    });

	this.add( entry, BorderLayout.SOUTH);

    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void makeDefaultFocus( JFrame frame ) {
	// ----------------------------------------------------------------------------------
	frame.addWindowListener(new WindowAdapter() {
		public void windowActivated(WindowEvent e) {
		    entry.requestFocusInWindow();
		}
	    });
    }
    
    // ======================================================================================
    /** Set echo condition.
     *  If this condition is set to true, then the entry line will automatically append to
     *  the text display if the enter button or keyboard enter is pressed. If it is set to
     *  false it is the programers responsibility to print to the text area.
     * @param state vale to set the echo condition to.
     */
    // --------------------------------------------------------------------------------------
    public void setEcho(boolean state) {
	// ----------------------------------------------------------------------------------
	shouldEcho = state;
    }

    // ======================================================================================
    /** Print to text display.
     *  Print a string to the text display with no newline.
     */
    // --------------------------------------------------------------------------------------
    public void print(String str) {
	// ----------------------------------------------------------------------------------
	display.append(str);
    }

    // ======================================================================================
    /** Print to text display.
     *  Print a string to the text display with a newline.
     */
    // --------------------------------------------------------------------------------------
    public void println(String str) {
	// ----------------------------------------------------------------------------------
	display.append(str + newline);
    }

    // ======================================================================================
    /** Return entry String.
     * @return String containing the text in the entry field.
     */
    // --------------------------------------------------------------------------------------
    public String getEntryString() {
	// ----------------------------------------------------------------------------------
	return new String( entry.getText() );
    }

    // ======================================================================================
    /** Clear display
     */
    // --------------------------------------------------------------------------------------
    public void cls() {
	// ----------------------------------------------------------------------------------
	display.setText("");
	entry.setText("");
    }

    // ======================================================================================
    /** Force Enter.
     *
     */
    // --------------------------------------------------------------------------------------
    public void pressEnter() {
	// ----------------------------------------------------------------------------------
        entry.selectAll();
	if (shouldEcho) { println(entry.getText()); }
	if (null != listener) {
	    listener.enterPressed();
	}
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void addEntryPanelListener(EntryPanelListener lis) {
	// ----------------------------------------------------------------------------------
	if (null == listener) {
	    listener = lis;
	} else {
	    System.err.println("*** Only one EntryPanelListener can be added right now");
	}
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void removeEntryPanelListener() {
	// ----------------------------------------------------------------------------------
	if (null == listener) {
	    System.err.println("*** No listener was added");
	} else {
	    listener = null;
	}
    }

}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
