/* ===== BEGIN FILE ========================================================================= */
/* **                              P R O P R I E T A R Y                                   ** */
/* ========================================================================================== */
/* **                                                                                      ** */
/* **  Copyright (c) 2006, Stephen W. Soliday                                              ** */
/* **                      stephen@soliday.com                                             ** */
/* **                      http://www.soliday.com/stephen                                  ** */
/* **                                                                                      ** */
/* **  This file, and the associated algorithms, are not free software; you may not        ** */
/* **  redistribute them and/or modify them. These algorithms were developed and           ** */
/* **  implemented for the purpose of an internal assessment and have, as yet, not been    ** */
/* **  publicly distributed. Development of these algorithms have been at the sole cost    ** */
/* **  in both time and funding by their author. Until such a public release is made,      ** */
/* **  the author retains ALL RIGHTS to these algorithms. It is expected that if this      ** */
/* **  program or any of the algorithms contained herein are deemed releasable they will   ** */
/* **  be released under the GNU Public license for non-commercial use and/or with         ** */
/* **  restricted rights for government use. At that time each source file will contain    ** */
/* **  either/both the standard GPL statement/disclaimer, and/or                           ** */
/* **  the DFARS Restricted Rights Legend.                                                 ** */
/* **                                                                                      ** */
/* **  These algorithms exists at the present time WITHOUT ANY WARRANTY; without even      ** */
/* **  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        ** */
/* **  As you are not supposed to be in possession of this file if you use it,             ** */
/* **  you do so AT YOUR OWN RISK.                                                         ** */
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

/* ========================================================================================== */
/* **                              P R O P R I E T A R Y                                   ** */
/* =========================================================================== END FILE ===== */
