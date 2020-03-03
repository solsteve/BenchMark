// ===== BEGIN FILE =========================================================================
// **                              P R O P R I E T A R Y                                   **
// ==========================================================================================
// **                                                                                      **
// **  Copyright (c) 2006, Stephen W. Soliday                                              **
// **                      stephen@soliday.com                                             **
// **                      http://www.soliday.com/stephen                                  **
// **                                                                                      **
// **  This file, and the associated algorithms, are not free software; you may not        **
// **  redistribute them and/or modify them. These algorithms were developed and           **
// **  implemented for the purpose of an internal assessment and have, as yet, not been    **
// **  publicly distributed. Development of these algorithms have been at the sole cost    **
// **  in both time and funding by their author. Until such a public release is made,      **
// **  the author retains ALL RIGHTS to these algorithms. It is expected that if this      **
// **  program or any of the algorithms contained herein are deemed releasable they will   **
// **  be released under the GNU Public license for non-commercial use and/or with         **
// **  restricted rights for government use. At that time each source file will contain    **
// **  either/both the standard GPL statement/disclaimer, and/or                           **
// **  the DFARS Restricted Rights Legend.                                                 **
// **                                                                                      **
// **  These algorithms exists at the present time WITHOUT ANY WARRANTY; without even      **
// **  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        **
// **  As you are not supposed to be in possession of this file if you use it,             **
// **  you do so AT YOUR OWN RISK.                                                         **
// **                                                                                      **
// **  ----- Modification History -------------------------------------------------------  **
// **                                                                                      **
// **  Author Stephen W. Soliday                                                           **
// **  Date   2006-11-24                                                                   **
// **                                                                                      **
/**   \file ptest.cc
 *    \brief Test Graphics.
 *     This program tests the functionality of \a PSGraph
 */
// ==========================================================================================

#include <psgraph.hh>

#define STEP 7                                    /**< number of edges                     */
#define SemiMajorAxis ((double) 9000)             /**< Semi major axis of the test ellipse */
#define SemiMinorAxis ((double) 7500)             /**< Semi minor axis of the test ellipse */

// ==========================================================================================
/** Entry Point.
 *  Standard C entry point.
 * \param argc number of command line arguments.
 * \param argv list of command line fields.
 * \return success=0, failure=non-zero
 */
// ------------------------------------------------------------------------------------------
int main( int argc, char *argv[] ) {
  // ----------------------------------------------------------------------------------------
  char *progName = argv[0];
  char *fspc     = argv[1];

  if (argc != 2) {
    fprintf( stderr, "USAGE: %s test.ps\n\n", progName );
    return 1;
  }

  PSGraph *PSG = new PSGraph( fspc );

  double t, dt, x0, y0, x1, y1;

  double maxr = (((SemiMinorAxis)>(SemiMajorAxis)) ? (SemiMinorAxis) : (SemiMajorAxis));

  maxr *= 1.05;

  PSG->setWorldCo( -maxr, -maxr, maxr, maxr );
  PSG->initGraphics();

  t = 0.0;
  dt = N_2PI / ((double)STEP);

  x0 = SemiMajorAxis * cos(t);
  y0 = SemiMinorAxis * sin(t);

  for (int i=0; i<STEP; i++) {
    t += dt;
    x1 = SemiMajorAxis * cos(t);
    y1 = SemiMinorAxis * sin(t);

    PSG->drawLine( x0, y0, x1, y1 );

    x0 = x1;
    y0 = y1;
  }

  PSG->drawCircle( 0.0, 0.0, maxr*0.8 );
    
  delete PSG;

  return 0;
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
