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

import com.soliday.lib.util.*;
import java.util.*;
import java.io.*;

// ==========================================================================================
/**
 * Random number generator.
 * Dice is a portable random number generator based on the random number
 * generator developed for the MILib package for C/C++ developed by this
 * author.
 *
 * $Log: Dice.java,v $
 * Revision 1.2  2002/02/17 01:32:30  soliday
 * Added void makeSeed and loop to start the rnd gens
 *
 * Revision 1.1  1997/02/23 14:35:22  soliday
 * Initial revision
 *
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class Dice {
    /** LC Multiplier. Multiply seed by this each itereation. */
    private final static int   MULTIPLIER       = 0x015A4E35;
    /** LC Increment. Increment seed by this each itereation. */
    private final static int   INCREMENT        = 0x00000001;
    /** 32-bit High Mask. Mask Results by this. */
    private final static int   MASKHIGH         = 0x00007FFF;
    /** 32-bit Low Mask. Mask Results by this. */
    private final static int   MASKLOW          = 0x00000FFF;
    /** Maximum Value. Maximum 32-bit value. */
    public  final static int   MAX_RAND         = 0x07FFFFFF;
    /** Default file name. Used for repeatable test values.*/
    public  final static String DEFAULT_SEEDFILE = "SEED.DAT";
    /** Default seed. Used for repeatable test values.*/
    private int[] DiceSeedNumbers = {3141592, 2718281};

    // --------------------------------------------------------------------------------------
    /**
     * Void Constructor Function
     */
    // --------------------------------------------------------------------------------------
    public Dice( ) {
        // ----------------------------------------------------------------------------------
    }

    // --------------------------------------------------------------------------------------
    /**
     * Set the random number generator seed value
     * @param  p vector, p[0] = high order seed & p[1] = low order seed
     * @return the high order seed
     */
    // --------------------------------------------------------------------------------------
    public synchronized long setSeed( int[] p ) {
        // ----------------------------------------------------------------------------------
        DiceSeedNumbers[1] = p[1];
        return (DiceSeedNumbers[0] = p[0]);
    }

    // --------------------------------------------------------------------------------------
    /**
     * Get the random number generator seed value
     * @param  p vector, p[0] = high order seed & p[1] = low order seed
     * @return the high order seed
     */
    // --------------------------------------------------------------------------------------
    public synchronized long getSeed( int[] p ) {
        // ----------------------------------------------------------------------------------
        p[1] = DiceSeedNumbers[1];
        return (p[0] = DiceSeedNumbers[0]);
    }

    // --------------------------------------------------------------------------------------
    /**
     * 27-Bit Uniform Deviate Random Number Generator
     * @return random integer from 0 to MAX_RAND
     */
    // --------------------------------------------------------------------------------------
    public synchronized int rnd_32( ) {
        // ----------------------------------------------------------------------------------
        int hi, lo;
        DiceSeedNumbers[1] = MULTIPLIER * DiceSeedNumbers[1] + INCREMENT;
        DiceSeedNumbers[0] = MULTIPLIER * DiceSeedNumbers[0] + INCREMENT;

        hi  = (DiceSeedNumbers[0] >> 16) & MASKHIGH;
        lo  = (DiceSeedNumbers[1] >> 16) & MASKLOW;

        return (hi << 12) + lo;
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform deviate random number generator
     * @return return a float random number [0,1]
     */
    // --------------------------------------------------------------------------------------
    public float rnd_R4( ) {
        // ----------------------------------------------------------------------------------
        return  (((float)rnd_32())/((float)MAX_RAND));
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform deviate random number generator
     * @return return a double random number [0,1]
     */
    // --------------------------------------------------------------------------------------
    public double rnd_R8( ) {
        // ----------------------------------------------------------------------------------
        return  (((double)rnd_32())/((double)MAX_RAND));
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform deviate random number generator
     * @param  a scale [0,a]
     * @return return a int random number
     */
    // --------------------------------------------------------------------------------------
    public int ROLL( short a ) {
        // ----------------------------------------------------------------------------------
        return (short)(rnd_32() % (int)a);
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform deviate random number generator
     * @param  a scale [0,a]
     * @return return a long random number
     */
    // --------------------------------------------------------------------------------------
    public int ROLL( int a ) {
        // ----------------------------------------------------------------------------------
        return rnd_32() % a;
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform deviate random number generator
     * @param  a scale [0,a]
     * @return return a float random number
     */
    // --------------------------------------------------------------------------------------
    public float RND( float a ) {
        // ----------------------------------------------------------------------------------
        return rnd_R4() * a;
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform deviate random number generator
     * @param  a from
     * @param  b to
     * @return return a int random number
     */
    // --------------------------------------------------------------------------------------
    public int RND( int a, int b ) {
        // ----------------------------------------------------------------------------------
        int d = b - a + 1;
	return a + ROLL(d);
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform deviate random number generator
     * @param  a scale [0,a]
     * @return return a double random number
     */
    // --------------------------------------------------------------------------------------
    public double RND( double a ) {
        // ----------------------------------------------------------------------------------
        return rnd_R8() * a;
    }

    // --------------------------------------------------------------------------------------
    /**
     * find a seed for the random number generator
     * @param  sa high order seed
     * @param  sb low order seed
     */
    // --------------------------------------------------------------------------------------
    public void makeSeed( int sa, int sb ) {
        // ----------------------------------------------------------------------------------
        int[]  dummy = {0, 0};

        //   ----- get something usefull to seed with ----------

        if (sa==0) dummy[1] = (int)System.currentTimeMillis() * 317;
        else       dummy[1] = sa;

        if (sb==0) dummy[0] = (int)System.currentTimeMillis() * 297;
        else       dummy[0] = sb;

        //   ----- make sure seed is odd -----------------------

        dummy[0] *= 2;   dummy[1] *= 2;
        dummy[0] += 1;   dummy[1] += 1;

        setSeed(dummy);

	for (int i=0; i<1000; i++) {
	    rnd_32();
	}
    }

    // --------------------------------------------------------------------------------------
    /**
     * find a seed for the random number generator
     */
    // --------------------------------------------------------------------------------------
    public void makeSeed( ) {
        // ----------------------------------------------------------------------------------
        int[]  dummy = {0, 0};

        //   ----- get something usefull to seed with ----------

        dummy[1] = (int)System.currentTimeMillis() * 317;
        dummy[0] = (int)System.currentTimeMillis() * 297;

        //   ----- make sure seed is odd -----------------------

        dummy[0] *= 2;   dummy[1] *= 2;
        dummy[0] += 1;   dummy[1] += 1;

        setSeed(dummy);

	for (int i=0; i<1000; i++) {
	    rnd_32();
	}
    }

    // --------------------------------------------------------------------------------------
    /**
     * Dice style random throw.
     * sum (number) throws of a (sides) sided dice and add (bonus).
     *
     * @param  number number of throws
     * @param  sides  number of sides
     * @param  bonus  adjustment
     * @return return a long random number [n+b, n*s+b]
     */
    // --------------------------------------------------------------------------------------
    public int Roll( int number, int sides, int bonus ) {
        // ----------------------------------------------------------------------------------
        int      i;
        int sum;

        for (i=0,sum=0; i<number; i++)
            sum += ROLL(sides);

        return (sum + bonus + number);
    }

    // --------------------------------------------------------------------------------------
    /**
     * Dice style random throw.
     * sum (number) throws of a (sides) sided dice and add (bonus).
     *
     * @param  p dice parameters p[0] = number of throws, 
     *                           p[1] = number of sides, p[2] = adjustment
     * @return return a long random number [n+b, n*s+b]
     */
    // --------------------------------------------------------------------------------------
    public int Roll( int[] p ) {
        // ----------------------------------------------------------------------------------
        return Roll(p[0], p[1], p[2]);
    }

    // --------------------------------------------------------------------------------------
    /**
     * Dice style random throw.
     * sum (number) throws of a (sides) sided dice and add (bonus).
     *
     * @param  number number of throws
     * @param  sides  number of sides
     * @param  bonus  adjustment
     * @return return a double random number [n+b, n*s+b]
     */
    // --------------------------------------------------------------------------------------
    public double  Roll_R8( int number, double sides, double bonus ) {
        // ----------------------------------------------------------------------------------
        int i;
        double    sum = 0.0;

        for (i=0; i<number; i++) sum += RND(sides);

        return (sum + bonus);
    }

    // --------------------------------------------------------------------------------------
    /**
     * uniform random double
     * @param  range range of the return
     * @param  base  lowest number to return
     * @return uniform random with the range [base <= n < (base + range)]
     */
    // --------------------------------------------------------------------------------------
    public double rndRange( double range, double base ) {
        // ----------------------------------------------------------------------------------
        return  (rnd_R8() * range) + base;
    }

    // --------------------------------------------------------------------------------------
    /**
     * use Monte Carlo to create Gaussian distribution
     * @param  low  minimum value to return
     * @param  high maximum value to return
     * @param  mean mean
     * @param  std  one standard deviation
     * @return random number that fits the gaussian distribution
     */
    // --------------------------------------------------------------------------------------
    public double rndGauss( double low,  double high,
                            double mean, double std ) {
        // ----------------------------------------------------------------------------------
        double x,y,yt,d,r, m;

        m = ((mean<low)?(low):((mean>high)?(high):(mean)));

        if (std == 0.0) return m;

        r = (high-low);

        do {
            x  = (r*rnd_R8()) + low;
            y  = rnd_R8();
            d  = (x - m)/std;
            yt = Math.exp(-d*d/2);
        } while (y > yt);

        return x;
    }

    // --------------------------------------------------------------------------------------
    /**
     * use Monte Carlo to create uniform polar distribution
     * @param P vector, P[0] = Angle, P[1] = Radius
     * @param maxR Maximum value to return for radius
     * @return Angle
     */
    // --------------------------------------------------------------------------------------
    public double rndPolar( double[] P, double maxR ) {
        // ----------------------------------------------------------------------------------
        double x, y, r;

        do {
            x = (rnd_R8()*2.0) - 1.0;
            y = (rnd_R8()*2.0) - 1.0;
            r = (x*x + y*y);
        } while (r > 1.0);

        P[1] = maxR * Math.sqrt(r);

        return (P[0] = Math2.ArcTan(y, x));
    }

    // --------------------------------------------------------------------------------------
    /**
     * position shuffle the array
     * @param array of bytes
     * @param n number of elements in the array
     */
    // --------------------------------------------------------------------------------------
    public synchronized void scramble_B( byte[] array, int n ) {
        // ----------------------------------------------------------------------------------
        byte temp;

        int a, b, i, j;
        j = (n/2);
        for (i = 0; i < j; i++) {
            while ((a = ROLL(n)) == (b = ROLL(n))) {};
            temp     = array[a];
            array[a] = array[b];
            array[b] = temp;
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * position shuffle the array
     * @param array of int
     * @param n number of elements in the array
     */
    // --------------------------------------------------------------------------------------
    public synchronized void scramble_I2( short[] array, int n ) {
        // ----------------------------------------------------------------------------------
        short temp;

        int a, b, i, j;
        j = (n/2);
        for (i = 0; i < j; i++) {
            while ((a = ROLL(n)) == (b = ROLL(n))) {};
            temp     = array[a];
            array[a] = array[b];
            array[b] = temp;
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * position shuffle the array
     * @param array of long
     * @param n number of elements in the array
     */
    // --------------------------------------------------------------------------------------
    public synchronized void scramble_I4( int[] array, int n ) {
        // ----------------------------------------------------------------------------------
        int temp;

        int a, b, i, j;
        j = (n/2);
        for (i = 0; i < j; i++) {
            while ((a = ROLL(n)) == (b = ROLL(n))) {};
            temp     = array[a];
            array[a] = array[b];
            array[b] = temp;
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * position shuffle the array
     * @param array of long
     * @param n number of elements in the array
     */
    // --------------------------------------------------------------------------------------
    public synchronized void   scramble_I8( long[] array, int n ) {
        // ----------------------------------------------------------------------------------
        long temp;

        int a, b, i, j;
        j = (n/2);
        for (i = 0; i < j; i++) {
            while ((a = ROLL(n)) == (b = ROLL(n))) {};
            temp     = array[a];
            array[a] = array[b];
            array[b] = temp;
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * position shuffle the array
     * @param array of float
     * @param n number of elements in the array
     */
    // --------------------------------------------------------------------------------------
    public synchronized void scramble_R4( float[] array, int n ) {
        // ----------------------------------------------------------------------------------
        float temp;

        int a, b, i, j;
        j = (n/2);
        for (i = 0; i < j; i++) {
            while ((a = ROLL(n)) == (b = ROLL(n))) {};
            temp     = array[a];
            array[a] = array[b];
            array[b] = temp;
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * position shuffle the array
     * @param array of double
     * @param n number of elements in the array
     */
    // --------------------------------------------------------------------------------------
    public synchronized void scramble_R8( double[] array, int n ) {
        // ----------------------------------------------------------------------------------
        double temp;

        int a, b, i, j;
        j = (n/2);
        for (i = 0; i < j; i++) {
            while ((a = ROLL(n)) == (b = ROLL(n))) {};
            temp     = array[a];
            array[a] = array[b];
            array[b] = temp;
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * Generate an X and Y that fit the 2-Dimensional Gaussian Distribution
     * @param  C vector, C[0] = x, C[1] = y
     * @param  a semiMajor
     * @param  b semiMinor
     * @param  s number of Sigma to clip
     */
    // --------------------------------------------------------------------------------------
    public void gauss2D( double[] C, double a, double b, double s ) {
        // ----------------------------------------------------------------------------------
        double    a2, b2, a2x, b2x, x2, y2, r2;

        a2x = s*s*(a2 = a*a);
        b2x = s*s*(b2 = b*b);

        do {
            do {
                C[0] = (rnd_R8() * 2.0) - 1.0;
                C[1] = (rnd_R8() * 2.0) - 1.0;
                x2 = C[0]*C[0];
                y2 = C[1]*C[1];
            } while (((x2/a2x)+(y2/b2x)) > 1.0);
        
            r2 = a2*b2*(x2+y2)/((b2*x2)+(a2*y2));
        
        } while (rnd_R8() > Math.exp(-0.5*(x2+y2)/r2));
    }


    // --------------------------------------------------------------------------------------
    /**
     * Get Seed from default file
     */
    // --------------------------------------------------------------------------------------
    public void getFSeed( ) {
        // ----------------------------------------------------------------------------------
        getFSeed( DEFAULT_SEEDFILE );
    }

    // --------------------------------------------------------------------------------------
    /**
     * Put Seed in default file
     */
    // --------------------------------------------------------------------------------------
    public void putFSeed( ) {
        // ----------------------------------------------------------------------------------
        getFSeed( DEFAULT_SEEDFILE );
    }

    // --------------------------------------------------------------------------------------
    /**
     * Get Seed from file
     * @param fspc file name
     */
    // --------------------------------------------------------------------------------------
    public void getFSeed( String fspc ) {
        // ----------------------------------------------------------------------------------
        int[] s = new int[2];
        ReadData rd = new ReadData( fspc );
        s[0] = rd.readInt();
        s[1] = rd.readInt();
        setSeed( s );
        rd.close();
    }

    // --------------------------------------------------------------------------------------
    /**
     * Put Seed in file
     * @param fspc file name
     */
    // --------------------------------------------------------------------------------------
    public void putFSeed( String fspc ) {
        // ----------------------------------------------------------------------------------
        int[] s = new int[2];
        WriteData wd = new WriteData( fspc );
        getSeed( s );
        wd.writeInt(s[0]);
        wd.writeInt(s[1]);
        wd.writeBreak();
        wd.close();
    }

    // --------------------------------------------------------------------------------------
    /**
     * Use Monte Carlo to Index
     * @param  wgt normalized weight array [0,1]
     * @param  n   number of items in the array
     * @return index [0,n-1] that is distributed like wgt array
     */
    // --------------------------------------------------------------------------------------
    public int montecarlo( double[] wgt, int n ) {
        // ----------------------------------------------------------------------------------
	double y, yt;
        int    x;

        do {
            x  = ROLL(n);
            y  = rnd_R8();
            yt = wgt[x];
        } while (y > yt);

        return x;  
    }

    // --------------------------------------------------------------------------------------
    /**
     * Used with Monte Carlo to Index
     * @param  wgt unnormalized weight array [0,Inf]
     * @param  n   number of items in the array
     * @return multiplier to return array to orignial values
     */
    // --------------------------------------------------------------------------------------
    public double normalize( double[] wgt, int n ) {
        // ----------------------------------------------------------------------------------
        double maxV = wgt[0];

	for (int i=0; i<n; i++) {
	    if (wgt[i] > maxV) {
		maxV = wgt[i];
	    }
	}
	
	for (int i=0; i<n; i++) {
	    wgt[i] /= maxV;
	}
	
	return maxV;
    }

    // --------------------------------------------------------------------------------------
    /**
     */
    // --------------------------------------------------------------------------------------
    public int parseDice( int[] P, String str ) {
        // ----------------------------------------------------------------------------------
        int num   = 1;
	int sides = 6;
	int bonus = 0;
	boolean isNeg = true;

	int ePos = str.indexOf('\n');
	if (ePos == -1) { ePos = str.length(); }

        int dPos = str.indexOf('d');
	if (dPos == -1) { dPos = str.indexOf('D'); }

	if (dPos == -1) {
	    System.err.println("No 'd': Dice must be of the form  3d6+2");
	    return 0;
	}

	int bPos = str.indexOf('-');
	if (bPos == -1) {
	    bPos = str.indexOf('+');
	    isNeg = false;
	}

	try {
	    if (dPos > 0) {
		num = Integer.parseInt(str.substring(0,dPos));
	    }
	    
	    
	    if (bPos == -1) {
		sides = Integer.parseInt(str.substring(dPos+1,ePos));
	    } else {
		sides = Integer.parseInt(str.substring(dPos+1,bPos));
		bonus = Integer.parseInt(str.substring(bPos+1,ePos));
		if (isNeg) { bonus = -bonus; }
	    }
	} catch (IndexOutOfBoundsException e) {
	    System.err.println(e.toString());
	    return 0;
	} catch (NumberFormatException e) {
	    System.err.println("Str  =["+str+"]");
	    System.err.println("Pos d="+dPos+" b="+bPos+" e="+ePos);
	    System.err.println(e.toString());
	    e.printStackTrace(System.err);
	    return 0;
	}

	if (P != null) {
	    P[0]=num;
	    P[1]=sides;
	    P[2]=bonus;
	}
	
	return Roll(num, sides, bonus);
    }
    
    // --------------------------------------------------------------------------------------
    /**
     */
    // --------------------------------------------------------------------------------------
    public int parseDice( String str ) {
        // ----------------------------------------------------------------------------------
	return parseDice(null, str);
    }

    // --------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------
    public static String notation(int n, int s, int b) {
        // ----------------------------------------------------------------------------------
	String str;
	if (n > 1) {
	    str = new String(Integer.toString(n) + "d" + Integer.toString(s));
	} else {
	    str = new String("d" + Integer.toString(s));
	}

	if (b > 0) {
	    str += "+";
	    str += Integer.toString(b);
	} else {
	    if (b < 0) {
	    str += Integer.toString(b);
	    }
	}

	return str;
    }

    // --------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------
    public static String notation(int[] p) {
        // ----------------------------------------------------------------------------------
	return Dice.notation(p[0], p[1], p[2]);
    }

    // --------------------------------------------------------------------------------------
    // --------------------------------------------------------------------------------------
    public static void main(String[] args) {
        // ----------------------------------------------------------------------------------
	Dice dd = new Dice();

	byte[] buffer = new byte[64];
	int[] dp = {1, 6, 0};
	int k;

	try {
	    while(true) {
		System.out.print("Enter: ");
		System.in.read(buffer);
		String myStr = new String(buffer);

		if (myStr.charAt(0) == 'q') { break; }
		if (myStr.charAt(0) == 'Q') { break; }
		if (myStr.charAt(0) == 'x') { break; }
		if (myStr.charAt(0) == 'X') { break; }

		if (myStr.charAt(0) == '\n') {
		    k = dd.Roll(dp);
		} else {
		    k = dd.parseDice(dp, myStr);
		}

		System.out.println(k+"  ("+Dice.notation(dp)+")");
	    }
	} catch (IOException e) {
	    System.err.println(e.toString());
	}
	
    }
}

// =========================================================================== END FILE =====
