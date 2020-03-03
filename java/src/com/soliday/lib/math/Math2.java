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
// **  Date   2006-12-03                                                                   **
// **                                                                                      **
// ==========================================================================================

package com.soliday.lib.math;

// ==========================================================================================
/**
 * Provides Advanced math operations
 *
 * $Log: Math2.java,v $
 * Revision 1.3  2002/03/05 23:09:40  soliday
 * Fixed bug in ArcTan x and y were reversed
 *
 * Revision 1.2  2002/03/05 22:00:16  soliday
 * Fixed some more JavaDoc Comments
 *
 * Revision 1.1  1997/02/23 14:35:22  soliday
 * Initial revision
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class Math2 implements MathConstants {
    // --------------------------------------------------------------------------------------
    /**
     * Arc Tangent.
     * Provides a full four quadrant arc tangent.
     * @param y numerator coordinate
     * @param x denominator coordinate
     * @return radian in range 0 <= r < 2PI
     */
    // --------------------------------------------------------------------------------------
    public static double ArcTan( double y, double x ) {
        // ----------------------------------------------------------------------------------
        if (x == N_ZERO)
            if (y > N_ZERO)       return  N_PI_2;
            else if (y < N_ZERO)  return  N_3PI_2;
            else                  return  N_ZERO;

        if (y > N_ZERO)           return  Math.atan2( y, x );
        else if (y < N_ZERO)      return  N_2PI + Math.atan2( y, x );
        else if (x > N_ZERO)      return  N_ZERO;

        return  N_PI;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Arc Tangent.
     * Provides a full four quadrant arc tangent.
     * @param y numerator coordinate
     * @param x denominator coordinate
     * @return radian in range -Pi <= r < PI
     */
    // --------------------------------------------------------------------------------------
    public static double ArcTanPM( double y, double x ) {
        // ----------------------------------------------------------------------------------
        if (x == N_ZERO)
            if (y > N_ZERO)       return  N_PI_2;
            else if (y < N_ZERO)  return -N_PI_2;
            else                  return  N_ZERO;

        if (y > N_ZERO)           return  Math.atan2( y, x );
        else if (y < N_ZERO)      return  Math.atan2( y, x );
        else if (x > N_ZERO)      return  N_ZERO;

        return  N_PI;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Mod Radians.
     * Correct radians to 0 <= r < 2PI
     * @param r radian
     * @return radian in range 0 <= r < 2PI
     */
    // --------------------------------------------------------------------------------------
    public static double radCorrect( double r ) {
        // ----------------------------------------------------------------------------------
        if (r > 0.0)
            if (r <= N_2PI) return r;
            else            return Math.IEEEremainder(r, N_2PI);

        if (r < 0.0)
            if (r >= -N_2PI) return N_2PI + r;
            else             return N_2PI + Math.IEEEremainder(r, N_2PI);

        return 0.0;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Mod Radians.
     * Correct radians to -PI <= r < PI
     * @param r radian
     * @return radian in range -PI <= r < PI
     */
    // --------------------------------------------------------------------------------------
    public static double radCorrectPM( double r ) {
        // ----------------------------------------------------------------------------------
        double ar = radCorrect(r);

        if (ar > N_PI) return (ar - N_2PI);

        return ar;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Find the center of mass.
     * traditional linear center of mass formula:
     *            X = SUM( Xi*Mi, i, 0, n-1 ) / SUM( Mi, i, 0, n-1 )
     * @param M What is it
     * @param X What is it
     * @param n What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    public double centerOfMass( double[] M, double[] X, int n ) {
        // ----------------------------------------------------------------------------------
        double    sum = 0.0,
            div = 0.0;
        int i;

        for (i=0; i<n; i++) {
            sum += (M[i] * X[i]);
            div +=  M[i];
        }

        if (div == 0.0) return 0.0;

        return (sum / div);
    }

    // --------------------------------------------------------------------------------------
    /**
     * Short Description.
     * Normalize the array and return the scale value.
     * @param vec What is it
     * @param N What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    public double normalize( double[] vec, int N ) {
        // ----------------------------------------------------------------------------------
        int i;
        double maxValue = vec[0];

        for (i=1; i<N; i++) if (vec[i] > maxValue) maxValue = vec[i];

        for (i=0; i<N; i++) vec[i] /= maxValue;

        return maxValue;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Short Description.
     * Long desciption of the function.
     * @param desired What is it
     * @param actual What is it
     * @param n What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    public double RelErr( double[] desired, double[] actual, int n ) {
        // ----------------------------------------------------------------------------------
        double    div, d, sum = 0.0;
        int i;
        for (i=0; i<n; i++) {
            if ((div = desired[i]) == 0.0) div = actual[i];
            if (div != 0) {
                d = (desired[i] - actual[i]) / div;
                sum += (d*d);
            }
        }

        return (sum / ((double)n));
    }

    // --------------------------------------------------------------------------------------
    /**
     * Short Description.
     * Long desciption of the function.
     * @param mat What is it
     * @param V   What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    public static int GaussJordan( Matrix mat, Matrix V ) {
        // ----------------------------------------------------------------------------------
        int      nr = mat.R();
        double[] dv = new double[nr];

        V.save( dv, 0 );

        int rv =  GaussJordan( mat, dv );

        if (1 == rv) V.load( dv, 0 );

        return rv;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Gauss-Jordan elimination.
     * Perform Gauss-Jordan elimination on [mat] in place
     * on return [mat] should be [Ident] and vec should countain the solution
     * return 1 on success and 0 on failure
     * @param mat What is it
     * @param vec What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    public static int GaussJordan( Matrix mat, double[] vec ) {
        // ----------------------------------------------------------------------------------
        int nr = mat.R();

        for (int k=0; k<nr; k++) {
            double t = mat.data(k,k);
            if (0.0 != t) {
                if (1.0 != t)
                    GJ_MUL( mat, vec, k, 1.0/t );
            } else {
                int c = -1;
                for (int i=k; i<nr; i++) {
                    if (0.0 != mat.data(i,k)) {
                        c = i;
                        break;
                    }
                }
                if (-1 == c) {
                    System.err.println("* Fatal * Matix is singular " +
                                       "problem is over determined");
                    mat.println( System.err );
                    return 0;
                } else {
                    GJ_ADD(mat, vec, c, k, 1.0/mat.data(c,k));
                }
            }
            for (int j=0; j<nr; j++) if (j != k) {
                double x = mat.data(j,k);
                if (0.0 != x)
                    GJ_ADD(mat, vec, k, j, -x);
            }
        }

        return 1;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Short Description.
     * Multiply the elements in row[i] with the value (c)
     * @param mat What is it
     * @param v What is it
     * @param i What is it
     * @param c What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    private static void GJ_MUL( Matrix mat, double[] v, int i, double c ) {
        // ----------------------------------------------------------------------------------
        double[] x  = mat.row(i);
        int      nc = mat.C();
        for (int j=0; j<nc; j++) {
            x[j] *= c;
        }
        v[i] *= c;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Short Description.
     * multiply the elements in row[i] with the value (c) and add to row[j]
     * leave row[i] unaltered
     * @param mat What is it
     * @param v What is it
     * @param i What is it
     * @param j What is it
     * @param c What is it
     */
    // --------------------------------------------------------------------------------------
    private static void GJ_ADD( Matrix mat, double[] v,
                                int i, int j, double c ) {
        // ----------------------------------------------------------------------------------
        double[] x  = mat.row(i);
        double[] y  = mat.row(j);
        int      nc = mat.C();
        for (int k=0; k<nc; k++) {
            y[k] += (c * x[k]);
        }
        v[j] += (c * v[i]);
    }

    // --------------------------------------------------------------------------------------
    /**
     *  Solve  a x = 1 (mod n)   for x
     * @param a What is it
     * @param n What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    public static long InvMod( long a, long n ) {
        // ----------------------------------------------------------------------------------
        long g0, g1, g2;
        long u0, u1, u2;
        long v0, v1, v2;

        g0 = n;  u0 = 1;  v0 = 0;
        g1 = a;  u1 = 0;  v1 = 1;

        while (g1 != 0) {
            long y = g0 / g1;
            g2 = g0 - y * g1;
            u2 = u0 - y * u1;
            v2 = v0 - y * v1;

            g0 = g1; g1= g2;
            u0 = u1; u1= u2;
            v0 = v1; v1= v2;
        }

        if (v0 >= 0) return v0;
        return v0 + n;
    }

    // --------------------------------------------------------------------------------------
    /**
     *  Solve  a x = b (mod n)   for x
     * @param a What is it
     * @param b What is it
     * @param n What is it
     * @return something
     */
    // --------------------------------------------------------------------------------------
    public static long InvMod( long a, long b, long n ) {
        // ----------------------------------------------------------------------------------
        return (b * InvMod( a, n )) % n;
    }

    // --------------------------------------------------------------------------------------
    /**
     * @param a What is it
     * @param z What is it
     * @param n What is it
     *  @return a^z (mod n)
     */
    // --------------------------------------------------------------------------------------
    public static long FastExp( long a, long z, long n ) {
        // ----------------------------------------------------------------------------------
        long x = 1;

        while (z != 0) {
            while ((z % 2) == 0) {
                z /= 2;
                a = ((a % n)*(a % n)) % n;
            }
            z--;
            x = ((x % n)*(a % n)) % n;
        }
        return x;
    }

    public static boolean isZero(double a) {
	if (a < 0.0) { return false; }
	if (a > 0.0) { return false; }
	return true;
    }

    public static boolean isZero(float a) {
	if (a < 0.0f) { return false; }
	if (a > 0.0f) { return false; }
	return true;
    }

    public static boolean isEqual(double a, double b) {
	if (a < b) { return false; }
	if (a > b) { return false; }
	return true;
    }

    public static boolean isEqual(float a, float b) {
	if (a < b) { return false; }
	if (a > b) { return false; }
	return true;
    }

    public static double Min( double a, double b ) { if (b < a) { return b; } return a; }
    public static float  Min( float  a, float  b ) { if (b < a) { return b; } return a; }
    public static int    Min( int    a, int    b ) { if (b < a) { return b; } return a; }
    public static long   Min( long   a, long   b ) { if (b < a) { return b; } return a; }

    public static double Max( double a, double b ) { if (b > a) { return b; } return a; }
    public static float  Max( float  a, float  b ) { if (b > a) { return b; } return a; }
    public static int    Max( int    a, int    b ) { if (b > a) { return b; } return a; }
    public static long   Max( long   a, long   b ) { if (b > a) { return b; } return a; }

    // --------------------------------------------------------------------------------------
    /**
     *  Round V to d decimal places
     * @param v number to round
     * @param d number of decimal places to round to
     * @return number rounded to d decimal places
     */
    // --------------------------------------------------------------------------------------
    public static double round( double v, int d ) {
        // ----------------------------------------------------------------------------------
	double div = 1.0;
        for (int i=0; i<d; i++) {
	    div *= 10.0;
	}

	double temp = v * div;

	return Math.floor(temp+0.5) / div;
    }

    // --------------------------------------------------------------------------------------
    /**
     *  Round V to d decimal places
     * @param A t=0 value
     * @param B t=1 value
     * @param t parameter 0<=0t<=1
     * @return parametric
     */
    // --------------------------------------------------------------------------------------
    public static double PARAMETRIC( double A, double B, double t ) {
        // ----------------------------------------------------------------------------------
	return (1.0-t)*A + t*B;
    }

    // ======================================================================================
    /** Arbitray Root.
     *  Find the \a d root of \a x using the log identity.
     *  @param x number.
     *  @param d root.
     *  @return the \a d root of \a x.
     */
    // --------------------------------------------------------------------------------------
    public static double root( double x, double d ) {
	// ----------------------------------------------------------------------------------
	return Math.exp(Math.log(x)/(d));
    }

    // ======================================================================================
    /** Arbitray Power.
     *  Find the \a d power of \a x using the log identity.
     *  @param x base.
     *  @param d power.
     *  @return the \a d power of \a x.
     */
    // --------------------------------------------------------------------------------------
    public static double power( double x, double d ) {
	// ----------------------------------------------------------------------------------
	return Math.exp(Math.log(x)*(d));
    }

}

// =========================================================================== END FILE =====
