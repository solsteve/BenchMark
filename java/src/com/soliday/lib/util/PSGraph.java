// ===== BEGIN FILE =========================================================================
// **                              P R O P R I E T A R Y                                   **
// ==========================================================================================
// **                                                                                      **
// **  Copyright (c) 2006, Stephen W. Soliday                                              **
// **                      stephen@soliday.com                                             **
// **                      http://www.soliday.com/stephen                                  **
// **                                                                                      **
// **  This file, and the associated algorithms, are not free software; you may not        **
// **  redistribute them and/or modify them. These algorithms were developed and           **
// **  implemented for the purpose of an internal assessment and have, as yet, not been    **
// **  publicly distributed. Development of these algorithms have been at the sole cost    **
// **  in both time and funding by their author. Until such a public release is made,      **
// **  the author retains ALL RIGHTS to these algorithms. It is expected that if this      **
// **  program or any of the algorithms contained herein are deemed releasable they will   **
// **  be released under the GNU Public license for non-commercial use and/or with         **
// **  restricted rights for government use. At that time each source file will contain    **
// **  either/both the standard GPL statement/disclaimer, and/or                           **
// **  the DFARS Restricted Rights Legend.                                                 **
// **                                                                                      **
// **  These algorithms exists at the present time WITHOUT ANY WARRANTY; without even      **
// **  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        **
// **  As you are not supposed to be in possession of this file if you use it,             **
// **  you do so AT YOUR OWN RISK.                                                         **
// **                                                                                      **
// **  ----- Modification History -------------------------------------------------------  **
// **                                                                                      **
// **  Author Stephen W. Soliday                                                           **
// **  Date   2006-12-03                                                                   **
// **                                                                                      **
// ==========================================================================================

package com.soliday.lib.util;

import com.soliday.lib.math.Math2;
import java.io.*;

// ==========================================================================================
/** Implementation.
 *  Provides the implementation for the wrapers for the PostScript graphing utility.
 */
// ------------------------------------------------------------------------------------------
public class PSGraph {
    // --------------------------------------------------------------------------------------

    /** output file handle */
    protected PrintStream ps;

    /** output postscript file name */
    protected String fileName;

    /** Number of pixels (72/inch) in the printable box. */
    protected final int BOX = 540;

    /** minimum X coordinate */
    protected double minX;

    /** minimum Y coordinate */
    protected double minY;

    /** width */
    protected double difX;

    /** height */
    protected double difY;
    
    // ======================================================================================
    /**
     * Constructor function.
     * Open a file for PSGraph
     *
     * @param fspc path name for file
     */
    // --------------------------------------------------------------------------------------
    public PSGraph( String fspc ) {
        // ----------------------------------------------------------------------------------
	fileName = null;
	try {
	    fileName = new String(fspc);
	    ps       = new PrintStream(fspc);
	} catch(FileNotFoundException e) {
	    System.err.println("File Not Found: "+fspc);
	}
    }

    // ======================================================================================
    /**
     * Close the postscript file when finished
     */
    // --------------------------------------------------------------------------------------
    public boolean close() {
        // ----------------------------------------------------------------------------------
	if (null == fileName) { return true; }
        write_ps_tail();
	ps.close();
	return true;
    }

    // ======================================================================================
    /** PostScript header.
     *  Write a standard version 1 PostScript header. Scale and translation are based on
     *  the world coordinates provided in the initialization of the graphics state data
     *  structure.
     * @return success=false, failure=true
     */
    // --------------------------------------------------------------------------------------
    protected boolean write_ps_head() {
        // ----------------------------------------------------------------------------------
	if (null == fileName) { return true; }
	ps.println("%!PS-Adobe-3.0");
	ps.println("%%Title: " + fileName);
	ps.println("%%Creator: PSGraph");
	ps.println("%%Orientation: Landscape");
	ps.println("%%%%Pages: 1");
	ps.println("%%BoundingBox: 0 0 612 792");
	ps.println("%%DocumentPaperSizes: Letter");
	ps.println("%%BeginSetup");
	ps.println("[{");
	ps.println("%%BeginFeature: *PageRegion Letter");
	ps.println("<</PageSize [612 792]>> setpagedevice");
	ps.println("%%EndFeature");
	ps.println("} stopped cleartomark");
	ps.println("%%EndSetup");
	ps.println("%%Magnification: 1.0000");
	ps.println("%%EndComments");

	ps.println("%%BeginProlog");
	ps.println("/dl { newpath moveto lineto stroke } bind def");
	ps.println("/dr { newpath rectstroke } bind def");
	ps.println("/dc { newpath 0 360 arc closepath stroke } bind def");
	ps.println("%EndProlog");

	ps.println("%%Page: 1 1");

	ps.println("gsave");

	ps.println("0 setlinewidth");

	ps.println("576 162 translate 90 rotate");

	ps.println("0 0 " + BOX + " " + BOX + " dr");

	ps.println(" " + BOX + " " + difX + " div " + BOX + " " + difY +
		   " div scale " + (-minX) + " " + (-minY) + " translate");

	return false;
    }

    // ======================================================================================
    /** PostScript Trailer.
     *  Write a standard version 1 PostScript trailer.
     * @return success=0, failure=non-zero
     */
    // --------------------------------------------------------------------------------------
    protected boolean write_ps_tail() {
        // ----------------------------------------------------------------------------------
	if (null == fileName) { return true; }
	ps.println("grestore");
	ps.println("showpage");
	ps.println("%%Trailer");
	ps.println("%%EOF");
	return false;
    }

    // ======================================================================================
    /** Sets the world coordinates of the structure \a G.
     *  Coordinates are set in device and world coordinates. This function is used
     *  to define the world coordinate window.
     *  @param x0 minimum X coordinate
     *  @param y0 minimum Y coordinate
     *  @param x1 maximum X coordinate
     *  @param y1 maximum Y coordinate
     *  @return success=false, failure=true. 
     */
    // --------------------------------------------------------------------------------------
    public boolean setWorldCo( double x0, double y0,  double x1, double y1 ) {
	// ----------------------------------------------------------------------------------
	minX = x0;
	minY = y0;
	difX = (x1-x0);
	difY = (y1-y0);
	return true;
    }

    // ======================================================================================
    /** Start Graphics.
     *  Begin the capture of graphics commands.
     * @return success=false, failure=true
     */
    // --------------------------------------------------------------------------------------
    public boolean initGraphics( ) {
	// ----------------------------------------------------------------------------------
	return write_ps_head();
    }
    
    // ======================================================================================
    /** Draw Line.
     *  Draw a line between two points.
     * @param x0 starting X coordinate.
     * @param y0 starting Y coordinate.
     * @param x1 ending X coordinate.
     * @param y1 ending Y coordinate.
     */
    // --------------------------------------------------------------------------------------
    public void drawLine( double x0, double y0,  double x1, double y1 ) {
	// ----------------------------------------------------------------------------------
	ps.println("" + x0 + " " + y0 + " " + x1 + " " + y1 + " dl");
    }

    // ======================================================================================
    /** Draw Circle.
     *  Draw a circle with an arbitrary radius centered on a point.
     * @param xc center X coordinate.
     * @param yc center Y coordinate.
     * @param r radius of the circle.
     */
    // --------------------------------------------------------------------------------------
    public void drawCircle( double xc, double yc, double r ) {
	// ----------------------------------------------------------------------------------
	ps.println("" + xc + " " + yc + " " + r + " dc");
    }

    // ======================================================================================
    /** Entry Point.
     *  Standard C entry point.
     * @param args list of command line fields.
     */
    // --------------------------------------------------------------------------------------
    public static void main(String[] args) {
	// ----------------------------------------------------------------------------------
	/** number of edges */
	final int STEP = 17;
	
	/** Semi major axis of the test ellipse */
	final double SemiMajorAxis = 9000.0;
	
	/**< Semi minor axis of the test ellipse */
	final double SemiMinorAxis = 7500;
	
	switch(args.length) {
	case 1:
	    break;
	default:
	    System.err.println("USAGE:  java com.soliday.lib.util.PSGraph test.ps");
	    System.exit(1);
	}

	PSGraph PSG = new PSGraph( args[0] );

	double t, dt, x0, y0, x1, y1;

	double maxr = (((SemiMinorAxis)>(SemiMajorAxis)) ? (SemiMinorAxis) : (SemiMajorAxis));

	maxr *= 1.05;

	PSG.setWorldCo( -maxr, -maxr, maxr, maxr );
	PSG.initGraphics();

	t = 0.0;
	dt = Math2.N_2PI / ((double)STEP);

	x0 = SemiMajorAxis * Math.cos(t);
	y0 = SemiMinorAxis * Math.sin(t);

	for (int i=0; i<STEP; i++) {
	    t += dt;
	    x1 = SemiMajorAxis * Math.cos(t);
	    y1 = SemiMinorAxis * Math.sin(t);

	    PSG.drawLine( x0, y0, x1, y1 );

	    x0 = x1;
	    y0 = y1;
	}

	PSG.drawCircle( 0.0, 0.0, maxr*0.8 );
    
	PSG.close();
	System.exit(0);
    }
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
