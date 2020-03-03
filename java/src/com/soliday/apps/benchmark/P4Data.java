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
// ==========================================================================================

package com.soliday.apps.benchmark;

// ==========================================================================================
/**  Constants.
 *   This file provides constants relating to a two body problem involving the moon
 *   orbiting the earth.
 */
// ------------------------------------------------------------------------------------------
public interface P4Data {
    // --------------------------------------------------------------------------------------
    final double N_G       = 6.6742e-11;         /** Newton's Gravitational Constant       */
    final double MAXT      =  1200.0;            /** Time in days of plot                  */
    final double DELTATIME = 21600.0;            /** Delta Time (seconds) between plots    */
    final int    ISTEP     =   360;              /** Number of Steps per Plot              */
    // --------------------------------------------------------------------------------------

    final double M1 = 1.988435e+30;              /** Mass of the SUN                       */
    final double M2 = 1.899e+27;                 /** Mass of the JUPITER                   */
    final double M3 = 5.9742e+24;                /** Mass of the EARTH                     */
    final double M4 = 7.347673e+22;              /** Mass of the MOON                      */

    final double R1 = 695000000.0;               /** Average Radius of the SUN             */
    final double R2 = 69924754.6;                /** Average Radius of the JUPITER         */
    final double R3 = 6370996.16;                /** Average Radius of the EARTH           */
    final double R4 = 1737146.5;                 /** Average Radius of the MOON            */

    // --------------------------------------------------------------------------------------

    final double X1 = -7.0675082353e+8;          /** Initial Y Pos of the SUN              */
    final double Y1 = 0.0;                       /** Initial Y Pos of the SUN              */
    final double X2 = 7.40035847176e+11;         /** Initial X Pos of the JUPITER          */
    final double Y2 = 0.0;                       /** Initial Y Pos of the JUPITER          */
    final double X3 = 1.47102485561e+11;         /** Initial X Pos of the EARTH            */
    final double Y3 = 0.0;                       /** Initial Y Pos of the EARTH            */
    final double X4 = 1.46739381561e+11;         /** Initial X Pos of the MOON             */
    final double Y4 = 0.0;                       /** Initial Y Pos of the MOON             */

    // --------------------------------------------------------------------------------------

    final double VX1 = 0.0;                      /** Init X Velocity of the SUN            */
    final double VY1 = -11.861;                  /** Init Y Velocity of the SUN            */
    final double VX2 = 0.0;                      /** Init X Velocity of the JUPITER        */
    final double VY2 = 13712.0;                  /** Init Y Velocity of the JUPITER        */
    final double VX3 = 0.0;                      /** Init X Velocity of the EARTH          */
    final double VY3 = 30287.0;                  /** Init Y Velocity of the EARTH          */
    final double VX4 = 0.0;                      /** Init X Velocity of the MOON           */
    final double VY4 = 29205.0;                  /** Init Y Velocity of the MOON           */

    // ======================================================================================

    final int iVX1 =  0;
    final int iVY1 =  1;
    final int  iX1 =  2;
    final int  iY1 =  3;
    final int iVX2 =  4;
    final int iVY2 =  5;
    final int  iX2 =  6;
    final int  iY2 =  7;
    final int iVX3 =  8;
    final int iVY3 =  9;
    final int  iX3 = 10;
    final int  iY3 = 11;
    final int iVX4 = 12;
    final int iVY4 = 13;
    final int  iX4 = 14;
    final int  iY4 = 15;

    final int   jG = 0;
    final int  jM1 = 1;
    final int  jR1 = 2;
    final int  jM2 = 3;
    final int  jR2 = 4;
    final int  jM3 = 5;
    final int  jR3 = 6;
    final int  jM4 = 7;
    final int  jR4 = 8;

    final int   NN = 4;
}

// =========================================================================== END FILE =====
