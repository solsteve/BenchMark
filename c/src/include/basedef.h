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
/**   \file basedef.h
 *    \brief Header file.
 *     Provides a common set of macros.
 */
/* ========================================================================================== */

#ifndef __BASEDEF_H
#define __BASEDEF_H

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#ifndef MAX_PATH
# ifndef MAXPATH
#  define MAX_PATH 128
# else
#  define MAX_PATH MAXPATH
# endif
#endif

/* ------------------------------------------------------------------------------------------ */
/** Arbitray Root.
 *  Find the \a d root of \a x using the log identity.
 *  \param x number.
 *  \param d root.
 *  \return the \a d root of \a x.
 */
/* ------------------------------------------------------------------------------------------ */
#define ROOT(x,d)  exp(log(x)/(d))

/* ------------------------------------------------------------------------------------------ */
/** Arbitray Power.
 *  Find the \a d power of \a x using the log identity.
 *  \param x base.
 *  \param d power.
 *  \return the \a d power of \a x.
 */
/* ------------------------------------------------------------------------------------------ */
#define POWER(x,d) exp(log(x)*(d))

/* ------------------------------------------------------------------------------------------ */
/** Safe Divide.
 *  Division that does not result in NaN if both the numerator and the denominator are zero.
 *  \param n numerator.
 *  \param d denominator.
 *  \return the \a n divided by \a d.
 */
/* ------------------------------------------------------------------------------------------ */
#define SAFEDIV(n,d)   (((n)<0.0)?((n)/(d)):(((n)>0.0)?((n)/(d)):(0.0)))

/* ------------------------------------------------------------------------------------------ */
/** Message.
 *  Display a common formated message to the STDERR. The format consists of the source file
 *  name, the line number, and a user supplied string.
 * \param a user supplied string.
 */
/* ------------------------------------------------------------------------------------------ */
#define MSG(a) fprintf(stderr, "%s: %d: %s\n", __FILE__, __LINE__, (a));

/* ------------------------------------------------------------------------------------------ */
/** Debug Marker.
 *  Use message for displaying a debug position marker.
 */
/* ------------------------------------------------------------------------------------------ */
#define MARK MSG("<<========================================>>")

/* ------------------------------------------------------------------------------------------ */

#define N_2PI ((double) 6.28318530717959)          /**< 2 times Pi                            */
#define N_G   ((double) 6.6742e-11)                /**< Newton's Gravitational Constant       */

#endif

/* =========================================================================== END FILE ===== */
