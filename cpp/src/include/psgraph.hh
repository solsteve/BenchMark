// ===== BEGIN FILE =========================================================================
// **                                                                                      **
// **  Copyright (c) 2006, Stephen W. Soliday                                              **
// **                      stephen@soliday.com                                             **
// **                      http://www.soliday.com/stephen                                  **
// **                                                                                      **
// **  This program is free software: you can redistribute it and/or modify it under       **
// **  the terms of the GNU General Public License as published by the Free Software       **
// **  Foundation, either version 3 of the License, or (at your option)                    **
// **  any later version.                                                                  **
// **                                                                                      **
// **  This program is distributed in the hope that it will be useful, but WITHOUT         **
// **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS       **
// **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.      **
// **                                                                                      **
// **  You should have received a copy of the GNU General Public License along with        **
// **  this program. If not, see <http://www.gnu.org/licenses/>.                           **
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

// =========================================================================== END FILE =====
