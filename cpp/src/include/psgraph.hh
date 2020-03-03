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
/**   \file psgraph.hh
 *    \brief Header file.
 *     Provides the interface for wrappers that allow the user to draw in PostScript.
 */
// ==========================================================================================

#ifndef __PSGRAPH_HH
#define __PSGRAPH_HH

#include <basedef.hh>
#include <fstream>

// ==========================================================================================
// ------------------------------------------------------------------------------------------
class PSGraph {
  // ----------------------------------------------------------------------------------------
private:
  double         minX;  /**< minimum X coordinate        */
  double         minY;  /**< minimum Y coordinate        */
  double         difX;  /**< width                       */
  double         difY;  /**< height                      */
  std::ofstream  outf;  /**< output file handle          */
  char          *fspc;  /**< output postscript file name */

  int write_ps_head( void );
  int write_ps_tail( void );

  // ----------------------------------------------------------------------------------------
public:
  PSGraph    ( char *fspc );

  PSGraph    (const PSGraph &RP);
  PSGraph & operator = (const PSGraph &RP);

  void copy  (const PSGraph &RP);

  ~PSGraph ( void );

  int  setWorldCo   ( double x0, double y0, double x1, double y1 );
  int  initGraphics ( void );
  void drawLine     ( double x0, double y0, double x1, double y1 );
  void drawCircle   ( double xc, double yc, double r );
};

// ==========================================================================================
/** Deep Copy.
 *  Make a deep copy of this class.
 * \param that reference to the source copy.
 */
// ------------------------------------------------------------------------------------------
inline void PSGraph::copy(const PSGraph &that) {
  // ------------------------------------------------------------------------------------------
  this->minX = that.minX;
  this->minY = that.minY;
  this->difX = that.difX;
  this->difY = that.difY;
  //  this->outf = that.outf;
  ::copy(this->fspc, that.fspc);
}

// ==========================================================================================
/** Draw Line.
 *  Draw a line between two points.
 * \param x0 starting X coordinate.
 * \param y0 starting Y coordinate.
 * \param x1 ending X coordinate.
 * \param y1 ending Y coordinate.
 */
// ------------------------------------------------------------------------------------------
inline void PSGraph::drawLine( double x0, double y0,  double x1, double y1 ) {
  // ----------------------------------------------------------------------------------------
  outf << x0 << " " << y0 << " " << x1 << " " << y1 << " dl" << std::endl;
}

// ==========================================================================================
/** Draw Circle.
 *  Draw a circle with an arbitrary radius centered on a point.
 * \param xc center X coordinate.
 * \param yc center Y coordinate.
 * \param r radius of the circle.
 */
// ------------------------------------------------------------------------------------------
inline void PSGraph::drawCircle( double xc, double yc, double r ) {
  // ----------------------------------------------------------------------------------------
  outf << xc << " " << yc << " " << r << " dc" << std::endl;
}

#endif

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
