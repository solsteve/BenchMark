/* ===== BEGIN FILE ========================================================================= */
/* **                                                                                      ** */
/* **  Copyright (c) 2006, Stephen W. Soliday                                              ** */
/* **                      stephen@soliday.com                                             ** */
/* **                      http://www.soliday.com/stephen                                  ** */
/* **                                                                                      ** */
/* **  This program is free software: you can redistribute it and/or modify it under       ** */
/* **  the terms of the GNU General Public License as published by the Free Software       ** */
/* **  Foundation, either version 3 of the License, or (at your option)                    ** */
/* **  any later version.                                                                  ** */
/* **                                                                                      ** */
/* **  This program is distributed in the hope that it will be useful, but WITHOUT         ** */
/* **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS       ** */
/* **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.      ** */
/* **                                                                                      ** */
/* **  You should have received a copy of the GNU General Public License along with        ** */
/* **  this program. If not, see <http://www.gnu.org/licenses/>.                           ** */
/* **                                                                                      ** */
/* **  ----- Modification History -------------------------------------------------------  ** */
/* **                                                                                      ** */
/* **  Author Stephen W. Soliday                                                           ** */
/* **  Date   2006-11-24                                                                   ** */
/* **                                                                                      ** */
/**   \file 4body.h
 *    \brief Constants.
 *     This file provides constants relating to a 4 body problems.
 */
/* ========================================================================================== */

#define MAXT       ((double)  1200.0)                /**< Time in days of plot                */
#define DELTATIME  ((double) 21600.0)                /**< Delta Time (seconds) between plots  */
#define ISTEP      ((int)      360)                  /**< Number of Steps per Plot            */

/* ------------------------------------------------------------------------------------------ */

#define M1  ((double)  1.988435e+30)                 /**< Mass of the SUN                     */
#define M2  ((double)  1.899e+27)                    /**< Mass of the JUPITER                 */
#define M3  ((double)  5.9742e+24)                   /**< Mass of the EARTH                   */
#define M4  ((double)  7.347673e+22)                 /**< Mass of the MOON                    */

#define R1  ((double)  695000000.0)                  /**< Average Radius of the SUN           */
#define R2  ((double)  69924754.6)                   /**< Average Radius of the JUPITER       */
#define R3  ((double)  6370996.16)                   /**< Average Radius of the EARTH         */
#define R4  ((double)  1737146.5)                    /**< Average Radius of the MOON          */

/* ------------------------------------------------------------------------------------------ */

#define X1  ((double)  -7.0675082353e+8)             /**< Initial Y Pos of the SUN            */
#define Y1  ((double)  0.0)                          /**< Initial Y Pos of the SUN            */
#define X2  ((double)  7.40035847176e+11)            /**< Initial X Pos of the JUPITER        */
#define Y2  ((double)  0.0)                          /**< Initial Y Pos of the JUPITER        */
#define X3  ((double)  1.47102485561e+11)            /**< Initial X Pos of the EARTH          */
#define Y3  ((double)  0.0)                          /**< Initial Y Pos of the EARTH          */
#define X4  ((double)  1.46739381561e+11)            /**< Initial X Pos of the MOON           */
#define Y4  ((double)  0.0)                          /**< Initial Y Pos of the MOON           */

/* ------------------------------------------------------------------------------------------ */

#define VX1 ((double)  0.0)                          /**< Init X Velocity of the SUN          */
#define VY1 ((double)  -11.861)                      /**< Init Y Velocity of the SUN          */
#define VX2 ((double)  0.0)                          /**< Init X Velocity of the JUPITER      */
#define VY2 ((double)  13712.0)                      /**< Init Y Velocity of the JUPITER      */
#define VX3 ((double)  0.0)                          /**< Init X Velocity of the EARTH        */
#define VY3 ((double)  30287.0)                      /**< Init Y Velocity of the EARTH        */
#define VX4 ((double)  0.0)                          /**< Init X Velocity of the MOON         */
#define VY4 ((double)  29205.0)                      /**< Init Y Velocity of the MOON         */

/* =========================================================================== END FILE ===== */
