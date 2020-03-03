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
/* **  Date   2012-09-30                                                                   ** */
/* **                                                                                      ** */
/**   @file common.h
 *    @brief Loop timing for various math functions.
 */
/* ========================================================================================== */

#ifndef __COMMON_H
#define __COMMON_H

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define MSG(a) fprintf( stderr, "%s: %d: %s.\n", __FILE__, __LINE__, a )
#define MARK MSG("<<========================================>>")

#define SHOW(a) fprintf( stderr, "%s = %g\n", #a, (double)a )

int version( void );

#endif

/* =========================================================================== END FILE ===== */
