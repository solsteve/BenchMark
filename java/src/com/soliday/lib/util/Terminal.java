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
 * Provides a simple terminal.
 *
 * THINGS TO DO: Add history
 *
 * $Log: Terminal.java, v $
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class Terminal extends JFrame implements ActionListener, EntryPanelListener {
    // --------------------------------------------------------------------------------------
    JButton clearButton   = null;
    JButton quitButton    = null;

    EntryPanel entryPanel = null;

    TerminalListener listener = null;

    // ======================================================================================
    /**
     * Contructor function.
     * @param name Title of this window
     */
    // --------------------------------------------------------------------------------------
    public Terminal( String name ) {
        // ----------------------------------------------------------------------------------
	this.build(name);
	this.pack();
	this.setVisible(true);
    }

    // ======================================================================================
    /** Build the GUI.
     * @param name Title of this window
     */
    // --------------------------------------------------------------------------------------
    void build( String name ) {
        // ----------------------------------------------------------------------------------
	setTitle(name);

	JPanel topPane = new JPanel();
	topPane.setLayout(new BorderLayout());
	getContentPane().add( topPane );

	clearButton = new JButton("Clear"); clearButton.addActionListener(this);
	quitButton  = new JButton("Quit");  quitButton.addActionListener(this);

	JPanel ctrlPanel  = new JPanel();
	ctrlPanel.setLayout(new FlowLayout());
	ctrlPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
	ctrlPanel.add(clearButton);
	ctrlPanel.add(quitButton);

	topPane.add( ctrlPanel,  BorderLayout.NORTH);

	entryPanel = new EntryPanel( 24, 80 );
	entryPanel.makeDefaultFocus(this);
	entryPanel.addEntryPanelListener(this);

	topPane.add( entryPanel, BorderLayout.CENTER);
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
	entryPanel.setEcho(state);
    }

    // ======================================================================================
    /** Print to text display.
     *  Print a string to the text display with no newline.
     */
    // --------------------------------------------------------------------------------------
    public void print(String str) {
	// ----------------------------------------------------------------------------------
	entryPanel.print(str);
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void println(String str) {
	// ----------------------------------------------------------------------------------
	entryPanel.println(str);
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public String getEntryString() {
	// ----------------------------------------------------------------------------------
	return entryPanel.getEntryString();
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void cls() {
	// ----------------------------------------------------------------------------------
	entryPanel.cls();
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void pressClear() {
	// ----------------------------------------------------------------------------------
	cls();

	if (null != listener) {
	    listener.clearButtonPressed();
	}
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void pressQuit() {
	// ----------------------------------------------------------------------------------
	if (null == listener) {
	    System.exit(0);
	} else {
	    listener.quitButtonPressed();
	}
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void enterPressed() {
	// ----------------------------------------------------------------------------------
	if (null != listener) {
	    listener.enterButtonPressed();
	}
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void actionPerformed(ActionEvent e) {
	// ----------------------------------------------------------------------------------
	Object eo = e.getSource();
	if (eo == clearButton) { pressClear(); }
	else if (eo == quitButton)  { pressQuit();  }
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void addTerminalListener(TerminalListener lis) {
	// ----------------------------------------------------------------------------------
	if (null == listener) {
	    listener = lis;
	} else {
	    System.err.println("*** Only one TerminalListener can be added right now");
	}
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    public void removeTerminalListener() {
	// ----------------------------------------------------------------------------------
	if (null == listener) {
	    System.err.println("*** No listener was added");
	} else {
	    listener = null;
	}
    }

    // ======================================================================================
    // --------------------------------------------------------------------------------------
    protected void processWindowEvent(WindowEvent e) {
	// ----------------------------------------------------------------------------------
	if (e.getID() == WindowEvent.WINDOW_CLOSING) {
	    dispose();
	    System.err.println("Use the Quit button instead");
	    System.exit(0);
	}
	super.processWindowEvent(e);
    }
}

// =========================================================================== END FILE =====
