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
/**   \file 2body.h
 *    \brief Constants.
 *     This file provides constants relating to a two body problem involving the moon
 *     orbiting the earth.
 */
/* ========================================================================================== */

#define maxr  ((double) (4.6e+8))                  /**< Newton's Gravitational Constant       */
#define ITER  ((int)    (10000))                   /**< Number of Plot Iterations             */
#define ISTEP ((int)    (60))                      /**< Number of Integration Steps per Plot  */

#define MAXT  ((double) 29.0)                      /**< Time in days of plot                  */

/* ------------------------------------------------------------------------------------------ */

#define Me  ((double)  5.9742e+24)                 /**< Mass of the Earth                     */
#define Mm  ((double)  7.347673e+22)               /**< Mass of the Moon                      */

#define Re  ((double)  6370996.0)                  /**< Average Radius of the Earth           */
#define Rm  ((double)  1737146.0)                  /**< Average Radius of the Moon            */

/* ------------------------------------------------------------------------------------------ */

#define Xe  ((double) -4.41156e+6)                 /**< Initial X Pos of the Earth            */
#define Ye  ((double)  0.0)                        /**< Initial Y Pos of the Earth            */
#define Xm  ((double)  3.58692e+8)                 /**< Initial X Pos of the Moon             */
#define Ym  ((double)  0.0)                        /**< Initial Y Pos of the Moon             */

/* ------------------------------------------------------------------------------------------ */

#define VXe ((double)  0.0)                        /**< Initial X Velocity of the Earth       */
#define VYe ((double) -11.7422)                    /**< Initial Y Velocity of the Earth       */
#define VXm ((double)  0.0)                        /**< Initial X Velocity of the Moon        */
#define VYm ((double)  1082.0)                     /**< Initial Y Velocity of the Moon        */

/* =========================================================================== END FILE ===== */
