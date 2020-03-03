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
/**   \file 4body.h
 *    \brief Constants.
 *     This file provides constants relating to a 4 body problems.
 */
/* ========================================================================================== */

#define MAXT       ((double)  1200.0)                /**< Time in days of plot                */
#define DELTATIME  ((double) 21600.0)                /**< Delta Time (seconds) between plots  */
#define ISTEP      ((int)      3600)                  /**< Number of Steps per Plot            */

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

/* ========================================================================================== */
/* **                              P R O P R I E T A R Y                                   ** */
/* =========================================================================== END FILE ===== */
