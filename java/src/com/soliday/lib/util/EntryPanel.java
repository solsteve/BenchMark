// ===== BEGIN FILE =========================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2005, Stephen W. Soliday                                              **
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

// =========================================================================== END FILE =====
