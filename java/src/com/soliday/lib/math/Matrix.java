// ===== BEGIN FILE =========================================================================
// **                                                                                      **
// **  This File is Part of the jSolLib Package for Machine Intelligence                   **
// **                                                                                      **
// **  Copyright (c) 2004, Stephen W. Soliday                                              **
// **                      stephen@soliday.com                                             **
// **                      http://www.soliday.com/stephen                                  **
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
// **  ----------------------------------------------------------------------------------  **
// **                                                                                      **
// **                                                                                      **
// ==========================================================================================

package com.soliday.lib.math;
import java.io.*;

// ==========================================================================================
/**
 * Provides basic Matrix operations.
 *
 * $Log: Matrix.java,v $
 * Revision 1.1  1997/02/23 14:35:22  soliday
 * Initial revision
 *
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public class Matrix
    implements MathConstants {
    // --------------------------------------------------------------------------------------
    /** Column count. Number of Columns in the Matrix */
    int nc;
    /** Row count. Number of Rows in the Matrix */
    int nr;
    /** Buffer. Matrix data is contained in a linear array. */
    double[][] buffer;
    /** temp Space. Temporary row data for functions.
     *  Prevents excessive reallocation of space. */
    double[] workRow;
    /** temp Space. Temporary column data for functions.
     *  Prevents excessive reallocation of space. */
    double[] workCol;

    // --------------------------------------------------------------------------------------
    /**
     * Constructor Function.
     * Initalize a matrix[n x n] all elements = 0.0
     * @param n size of square matrix
     */
    // --------------------------------------------------------------------------------------
    public Matrix( int n ) {
        // ----------------------------------------------------------------------------------
        init( n, n, 0.0 );
    }

    // --------------------------------------------------------------------------------------
    /**
     * Constructor Function.
     * Initalize a matrix[n x n] all diagonal elements = v
     * @param n size of square matix
     * @param v diagonal elements
     */
    // --------------------------------------------------------------------------------------
    public Matrix( int n, double v ) {
        // ----------------------------------------------------------------------------------
        init( n, n, 0.0 );
        for (int i=0; i<n; i++) {
            buffer[i][i] = v;
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * Constructor Function.
     * Initalize a matrix [this rows=r columns=c] all elements = 0.0
     * @param r number of rows
     * @param c number of columns
     */
    // --------------------------------------------------------------------------------------
    public Matrix( int r, int c ) {
        // ----------------------------------------------------------------------------------
        init( r, c, 0.0 );
    }

    // --------------------------------------------------------------------------------------
    /**
     * Constructor Function.
     * Initalize a matrix [this rows=r columns=c] all elements = v
     * @param r number of rows
     * @param c number of columns
     * @param v initial value for all elements
     */
    // --------------------------------------------------------------------------------------
    public Matrix( int r, int c, double v ) {
        // ----------------------------------------------------------------------------------
        init( r, c, v );
    }

    // --------------------------------------------------------------------------------------
    /**
     * Allocate space for the matrix
     * @param r number of rows
     * @param c number of columns
     */
    // --------------------------------------------------------------------------------------
    private void init( int r, int c ) {
        // ----------------------------------------------------------------------------------
        nc = c;
        nr = r;
        buffer  = new double[nr][nc];
        workRow = new double[nc];
        workCol = new double[nr];
    }

    // --------------------------------------------------------------------------------------
    /**
     * Allocate space for the matrix
     * @param r number of rows
     * @param c number of columns
     * @param v initial value for all elements
     */
    // --------------------------------------------------------------------------------------
    public void init( int r, int c, double v ) {
        // ----------------------------------------------------------------------------------
        init(r,c);
        fill(v);
    }

    // --------------------------------------------------------------------------------------
    /**
     * Allocate space for the matrix and copy [sm]
     * @param sm matrix to copy
     */
    // --------------------------------------------------------------------------------------
    public void init( Matrix sm ) {
        // ----------------------------------------------------------------------------------
        init(sm.nr, sm.nc);
        copy(sm);
    }

    // --------------------------------------------------------------------------------------
    /**
     * Uninitialize the matrix
     */
    // --------------------------------------------------------------------------------------
    public void destroy() {
        // ----------------------------------------------------------------------------------
        nr      = 0;
        nc      = 0;
        buffer  = null;
        workRow = null;
        workCol = null;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Row count.
     * Return the number of allocated rows
     * @return the number of allocated rows
     */
    // --------------------------------------------------------------------------------------
    public int R() {
        // ----------------------------------------------------------------------------------
        return nr;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Column count.
     * Return the number of allocated columns
     * @return the number of allocated columns
     */
    // --------------------------------------------------------------------------------------
    public int C() {
        // ----------------------------------------------------------------------------------
        return nc;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Access matrix ellement.
     * Return the element [this row=r column=c]
     * @param r row address
     * @param c column address
     * @return the element at (r,c)
     */
    // --------------------------------------------------------------------------------------
    public double data( int r, int c ) {
        // ----------------------------------------------------------------------------------
        return (buffer[r][c]);
    }

    // --------------------------------------------------------------------------------------
    // set and return the element [this row=r column=c]
    /**
     * Access matrix ellement.
     * Set and return the element [this row=r column=c]
     * @param r row address
     * @param c column address
     * @return the element at (r,c)
     */
    // ----------------------------------------------------------------------------------------
    public double data( int r, int c, double v ) {
        // ----------------------------------------------------------------------------------
        return (buffer[r][c] = v);
    }

    // --------------------------------------------------------------------------------------
    /**
     * Return the (r) row of the matrix
     * @param r row address
     * @return row vector
     */
    // --------------------------------------------------------------------------------------
    public double[] row( int r ) {
        // ----------------------------------------------------------------------------------
        return buffer[r];
    }

    // --------------------------------------------------------------------------------------
    /**
     * Fill all elements with v
     * @param v value to fill with
     */
    // --------------------------------------------------------------------------------------
    public void fill( double v ) {
        // ----------------------------------------------------------------------------------
        for (int ir=0; ir<nr; ir++) {
            for (int ic=0; ic<nc; ic++) {
                buffer[ir][ic] = v;
            }
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * Copy matrix.
     * copy matrix [this] = [sm]
     * @param sm matrix to copy from
     */
    // --------------------------------------------------------------------------------------
    public void copy( Matrix sm ) {
        // ----------------------------------------------------------------------------------
        if ((this.nr != sm.nr)||(this.nc != sm.nc)) {
            this.destroy();
            this.init(sm);
        } else {
            for (int ir=0; ir<nr; ir++) {
                for (int ic=0; ic<nc; ic++) {
                    this.buffer[ir][ic] = sm.buffer[ir][ic];
                }
            }
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * Add in place.
     * Add matrix     [this] = [this] + [sm]
     * @param sm matrix to add from
     */
    // --------------------------------------------------------------------------------------
    public void add( Matrix sm ) {
        // ----------------------------------------------------------------------------------
        if ((this.nr != sm.nr)||(this.nc != sm.nc)) {
            System.err.println( "Matrix:add ** ERROR ** incompatable dims" );
        } else {
            for (int ir=0; ir<nr; ir++) {
                for (int ic=0; ic<nc; ic++) {
                    this.buffer[ir][ic] += sm.buffer[ir][ic];
                }
            }
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * Multiply two Matricies
     * Multiply matrix     [this] = [lhs] DOT [rhs]
     * @param lhs left hand side matrix
     * @param rhs right hand side matrix
     */
    // --------------------------------------------------------------------------------------
    public void mul( Matrix lhs, Matrix rhs ) {
        // ----------------------------------------------------------------------------------
        int n = lhs.nc;
        if (lhs.nc == rhs.nr) {
            boolean redo = false;
        
            if (this.nr != lhs.nr) redo = true;
            if (this.nc != rhs.nc) redo = true;

            if (redo) {
                this.destroy();
                this.init( lhs.nr, rhs.nc );
            }

            for (int ir=0; ir<nr; ir++) {
                for (int ic=0; ic<nc; ic++) {
                    this.buffer[ir][ic] =
                        (lhs.buffer[ir][0] * rhs.buffer[0][ic]);
                    for (int j=1; j<n; j++) {
                        this.buffer[ir][ic] +=
                            (lhs.buffer[ir][j] * rhs.buffer[j][ic]);
                    }
                }
            }
        } else {
            System.err.println( "Matrix:mul ** ERROR ** incompatable dims" );
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * Print Matrix.
     * Output a Mathematica compatible matrix
     * @param str prefix string
     * @param o output stream
     */
    // --------------------------------------------------------------------------------------
    public void println( String str, PrintStream o ) {
        // ----------------------------------------------------------------------------------
        o.print( str );
        this.println( o );
    }

    // --------------------------------------------------------------------------------------
    /**
     * Print Matrix
     * Output a Mathematica compatible matrix
     * @param o output stream
     */
    // --------------------------------------------------------------------------------------
    public void println( PrintStream o ) {
        // ----------------------------------------------------------------------------------
        o.print("{ ");
        for (int ir=0; ir<nr; ir++) {
            o.print("{"+buffer[ir][0]);

            for (int ic=1; ic<nc; ic++) {
                o.print(","+buffer[ir][ic]);
            }
            if (ir == nr-1) o.println("} }");
            else            o.println("},");
        }
    }

    // --------------------------------------------------------------------------------------
    /**
     * Load Matrix
     * Load matrix from a double[]
     * @param v array containing data
     * @param ptr current position in array v to start from
     * @return next position in array
     */
    // --------------------------------------------------------------------------------------
    public int load( double[] v, int ptr ) {
        // ----------------------------------------------------------------------------------
        int p = ptr;

        for (int ir=0; ir<nr; ir++) {
            for (int ic=0; ic<nc; ic++) {
                this.data( ir, ic, v[p] );
                p++;
            }
        }

        return p;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Store Matrix
     * save matrix to a double[]
     * @param v array containing data
     * @param ptr current position in array v to start from
     * @return next position in array
     */
    // --------------------------------------------------------------------------------------
    public int save( double[] v, int ptr ) {
        // ----------------------------------------------------------------------------------
        int p = ptr;

        for (int ir=0; ir<nr; ir++) {
            for (int ic=0; ic<nc; ic++) {
                v[p] = this.data( ir, ic );
                p++;
            }
        }

        return p;
    }

    // --------------------------------------------------------------------------------------
    /**
     * Size of buffer.
     * used to allocate double[] for use with load/save
     * double[] someDouble = new double[ someMatrix.size() ]
     * @return size of buffer
     */
    // --------------------------------------------------------------------------------------
    public int size( ) {
        // ----------------------------------------------------------------------------------
        return (nr*nc);
    }
}

// =========================================================================== END FILE =====
