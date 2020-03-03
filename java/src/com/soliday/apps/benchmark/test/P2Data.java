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

package com.soliday.apps.benchmark.test;

// ==========================================================================================
/**  Constants.
 *   This file provides constants relating to a two body problem involving the moon
 *   orbiting the earth.
 */
// ------------------------------------------------------------------------------------------
public interface P2Data {
    // --------------------------------------------------------------------------------------
    final double N_G   = 6.6742e-11;   /** Newton's Gravitational Constant       */

    final double maxr  = 4.6e+8;       /** Maximum Plot Radius                   */
    final int    ITER  = 10000;        /** Number of Plot Iterations             */
    final int    ISTEP = 60;           /** Number of Integration Steps per Plot  */
    final double MAXT  = 29.0;         /** Time in days of plot                  */

    // --------------------------------------------------------------------------------------

    final double Me    = 5.9742e+24;   /** Mass of the Earth                     */
    final double Mm    = 7.347673e+22; /** Mass of the Moon                      */

    final double Re    = 6370996.0;    /** Average Radius of the Earth           */
    final double Rm    = 1737146.0;    /** Average Radius of the Moon            */

    // --------------------------------------------------------------------------------------

    final double Xe    = -4.41156e+6;  /** Initial X Pos of the Earth            */
    final double Ye    = 0.0;          /** Initial Y Pos of the Earth            */
    final double Xm    = 3.58692e+8;   /** Initial X Pos of the Moon             */
    final double Ym    = 0.0;          /** Initial Y Pos of the Moon             */

    // --------------------------------------------------------------------------------------

    final double VXe   = 0.0;          /** Initial X Velocity of the Earth       */
    final double VYe   = -11.7422;     /** Initial Y Velocity of the Earth       */
    final double VXm   = 0.0;          /** Initial X Velocity of the Moon        */
    final double VYm   = 1082.0;       /** Initial Y Velocity of the Moon        */
}

// =========================================================================== END FILE =====
