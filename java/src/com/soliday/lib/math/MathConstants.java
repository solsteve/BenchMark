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
 * Provides high precision constants.
 *
 * $Log: MathConstants.java,v $
 * Revision 1.2  2002/04/02 20:24:26  soliday
 * Added Pi/8
 *
 * Revision 1.1  1997/02/23 14:35:22  soliday
 * Initial revision
 *
 *
 * @author  Stephen W. Soliday
 */
// ------------------------------------------------------------------------------------------
public interface MathConstants {
    // --------------------------------------------------------------------------------------
    final double N_ZERO      =   0.0;

    final double N_PIN2      =   9.869604401089358618834491;
    final double N_PI_8      =   0.392699081698724154807830;
    final double N_PI_4      =   0.785398163397448309615661;
    final double N_PI_2      =   1.570796326794896619231322;
    final double N_3PI_4     =   2.356194490192344928846983;
    final double N_PI        =   3.141592653589793238462643;
    final double N_5PI_4     =   3.926990816987241548078304;
    final double N_3PI_2     =   4.712388980384689857693965;
    final double N_7PI_4     =   5.497787143782138167309626;
    final double N_2PI       =   6.283185307179586476925287;
    final double N_4PI       =  12.566370614359172953850574;
    final double N_1_PI      =   0.318309886183790671537768;
    final double N_2_PI      =   0.636619772367581343075535;
    final double N_2_SQRTPI  =   1.128379167095512573896159;
    final double N_180_PI    =  57.295779513082320876798155;
    final double N_PI_180    =   0.017453292519943295769237;
    final double N_SQRT2     =   1.414213562373095048801689;
    final double N_SQRT1_2   =   0.707106781186547524400844;
    final double N_SQRT3     =   1.732050807568877293527446;
    final double N_SQRT1_3   =   0.577350269189625764509149;

    final double N_E         =   2.718281828459045235360287;
    final double N_LOG2E     =   1.442695040888963407359925;
    final double N_LOG10E    =   0.434294481903251827651129;
    final double N_LN2       =   0.693147180559945309417232;
    final double N_LN10      =   2.302585092994045684017991;

    final double DEG2RAD     = N_PI_180;
    final double RAD2DEG     = N_180_PI;
}

// =========================================================================== END FILE =====
