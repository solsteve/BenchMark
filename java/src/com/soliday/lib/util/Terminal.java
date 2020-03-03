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

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
