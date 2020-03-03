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
/**   \file ptest.c
 *    \brief Test Graphics.
 *     This program tests the functionality of \a PSGraph
 */
/* ========================================================================================== */

#include <psgraph.h>

#define STEP 7                                       /**< number of edges                     */
#define SemiMajorAxis ((double) 9000)                /**< Semi major axis of the test ellipse */
#define SemiMinorAxis ((double) 7500)                /**< Semi minor axis of the test ellipse */

/* ========================================================================================== */
/** Entry Point.
 *  Standard C entry point.
 * \param argc number of command line arguments.
 * \param argv list of command line fields.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
int main( int argc, char *argv[] ) {
  /* ---------------------------------------------------------------------------------------- */
  char *progName = argv[0];
  char *fspc     = argv[1];

  PSGraph *G;

  int i;

  double t, dt, x0, y0, x1, y1;

  double maxr = (((SemiMinorAxis)>(SemiMajorAxis)) ? (SemiMinorAxis) : (SemiMajorAxis));

  if (argc != 2) {
    fprintf( stderr, "USAGE: %s test.ps\n\n", progName );
    return 1;
  }

  maxr *= 1.05;

  G = PSG_new( fspc );

  PSG_setWorldCo( G, -maxr, -maxr, maxr, maxr );
  PSG_initGraphics ( G );

  t = 0.0;
  dt = N_2PI / ((double)STEP);

  x0 = SemiMajorAxis * cos(t);
  y0 = SemiMinorAxis * sin(t);
  for (i=0; i<STEP; i++) {
    t += dt;
    x1 = SemiMajorAxis * cos(t);
    y1 = SemiMinorAxis * sin(t);

    PSG_drawLine( G, x0, y0, x1, y1 );

    x0 = x1;
    y0 = y1;
  }

  PSG_drawCircle( G, 0.0, 0.0, maxr*0.8 );
    
  PSG_del( G );

  return 0;
}

/* =========================================================================== END FILE ===== */
