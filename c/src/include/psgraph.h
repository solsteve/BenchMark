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

/* ========================================================================================== */
/* **                              P R O P R I E T A R Y                                   ** */
/* =========================================================================== END FILE ===== */
