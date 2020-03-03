// ===== BEGIN FILE =========================================================================
// **                                                                                      **
// **  Copyright (c) 2006, Stephen W. Soliday                                              **
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
// **                                                                                      **
// **  ----- Modification History -------------------------------------------------------  **
// **                                                                                      **
// **  Author Stephen W. Soliday                                                           **
// **  Date   2006-11-24                                                                   **
// **                                                                                      **
/**   \file rktest.cc
 *    \brief Test Numerical Integrator.
 *     This program tests the functionality of \a RK4. \a RK4 is a fourth order Runge-Kutta
 *     numerical integrator.
 */
// ==========================================================================================

package com.soliday.apps.benchmark.test;
import  com.soliday.apps.benchmark.RK4;

import com.soliday.lib.math.*;
import com.soliday.lib.util.*;
import java.io.*;

// ==========================================================================================
// ------------------------------------------------------------------------------------------
public class RKTest extends RK4 implements P2Data {
    // --------------------------------------------------------------------------------------
    public int done = 0;

    public RKTest() { super(8); };

    // ======================================================================================
    /** Check State.
     *  Determine if any two bodies are closer than the sum of thier radii.
     *  At a predetermined time, add velocity to the smallest body.
     * @param Q current state vector.
     * @param t current time.
     * @param P parameter vector.
     * @return success=0, failure=non-zero.
     */
    // --------------------------------------------------------------------------------------
    public int CHECK( double[] Q, double t, double[] P ) {
	// ----------------------------------------------------------------------------------
	double dx = Q[6] - Q[4];
	double dy = Q[7] - Q[5];
	double r2 = P[1] + P[3];

	return (((r2*r2) < ((dx*dx)+(dy*dy))) ? (0) : (1));
    }

    // ======================================================================================
    /** Integrate.
     *  Integrate for time \a t0 to \a t1. Using a fourth order Runge-Kutta numerical integrator.
     *  The equations of mothion describe a generic two body gravitational problem.
     * @param Qd first time derivative of the current state vector.
     * @param Q current state vector.
     * @param t current time.
     * @param P parameter vector.
     */
    // --------------------------------------------------------------------------------------
    public void DIFEQ( double[] Qd, double[] Q, double t, double[] P ) {
	// ----------------------------------------------------------------------------------
	if (done == 0) {

	    double xd1 = Q[0];  double x1 = Q[4];
	    double yd1 = Q[1];  double y1 = Q[5];
	    double xd2 = Q[2];  double x2 = Q[6];
	    double yd2 = Q[3];  double y2 = Q[7];

	    double m1   = P[0];
	    double m2   = P[2];

	    double dx21 = x2 - x1;
	    double dy21 = y2 - y1;
	    double dx12 = x1 - x2;
	    double dy12 = y1 - y2;

	    double r12sq = (dx12*dx12) + (dy12*dy12);
	    double den   = Math2.power(r12sq,1.5);

	    if (den > 0.0) {} else { System.err.println( "INFINITY" ); }

	    double a1 = N_G*m2*dx21;
	    double a2 = N_G*m2*dy21;
	    double a3 = N_G*m1*dx12;
	    double a4 = N_G*m1*dy12;

	    Qd[0] = (a1/den);
	    Qd[1] = (a2/den);
	    Qd[2] = (a3/den);
	    Qd[3] = (a4/den);
	    Qd[4] = xd1;
	    Qd[5] = yd1;
	    Qd[6] = xd2;
	    Qd[7] = yd2;
	}
    }

    // ======================================================================================
    /** Entry Point.
     *  Standard CPP entry point.
     * @param args list of command line fields.
     */
    // --------------------------------------------------------------------------------------
    public static void main(String[] args) {
	// ----------------------------------------------------------------------------------

	switch(args.length) {
	case 1:
	    break;
	default:
	    System.err.println("USAGE:  java com.soliday.apps.benchmark.test.RKTest test.ps");
	    System.exit(1);
	}

	double[] param = new double[4];
	double[] state = new double[8];
	double t     = 0.0;
	double dt    = (MAXT * 86400.0) / ((double) ITER);

	param[0] = Me;
	param[1] = Re;
	param[2] = Mm;
	param[3] = Rm;

	state[0] = VXe;       state[4] = Xe;
	state[1] = VYe;       state[5] = Ye;
	state[2] = VXm;       state[6] = Xm;
	state[3] = VYm;       state[7] = Ym;

	double x1 = state[4];
	double y1 = state[5];
	double x2 = state[6];
	double y2 = state[7];

	PSGraph PSG = new PSGraph( args[0] );

	PSG.setWorldCo( -maxr, -maxr, maxr, maxr );
	PSG.initGraphics();

	RKTest MM = new RKTest();

	for (int i=0; i<ITER; i++) {

	    t = MM.integrate( state, t, t+dt, ISTEP, param );

	    PSG.drawLine( x1, y1, state[4], state[5]);
	    PSG.drawLine( x2, y2, state[6], state[7] );
 
	    x1 = state[4];
	    y1 = state[5];
	    x2 = state[6];
	    y2 = state[7];

	    if (MM.done == 1) { break; }
	}

	PSG.drawCircle( state[4], state[5], param[1] );
	PSG.drawCircle( state[6], state[7], param[3] );
  
  	PSG.close();
	System.exit(0);
    }
}

// =========================================================================== END FILE =====
