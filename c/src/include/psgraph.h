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
/**   \file psgraph.h
 *    \brief Header file.
 *     Provides the interface for wrappers that allow the user to draw in PostScript.
 */
/* ========================================================================================== */

#ifndef __PSGRAPH_H
#define __PSGRAPH_H

#include <basedef.h>

/* ========================================================================================== */
/** A struct type.
 *  A stucture to hold the state of the graphics class.
 *
 */
/* ------------------------------------------------------------------------------------------ */
struct __psgraph {
  /* ---------------------------------------------------------------------------------------- */
  double  minX;  /**< minimum X coordinate        */
  double  minY;  /**< minimum Y coordinate        */
  double  difX;  /**< width                       */
  double  difY;  /**< height                      */
  FILE   *fp;    /**< output file handle          */
  char   *fspc;  /**< output postscript file name */
};
  
typedef struct __psgraph PSGraph;

/* ========================================================================================== */

PSGraph *PSG_new          ( char *fspc );
int      PSG_del          ( PSGraph *G );

int      PSG_setWorldCo   ( PSGraph *G, double x0, double y0, double x1, double y1 );
int      PSG_initGraphics ( PSGraph *G );
int      PSG_drawLine     ( PSGraph *G, double x0, double y0, double x1, double y1 );
int      PSG_drawCircle   ( PSGraph *G, double xc, double yc, double r );

#endif

/* =========================================================================== END FILE ===== */
