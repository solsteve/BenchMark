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
// **  Date   2006-11-27                                                                   **
// **                                                                                      **
/**   \file psgraph.cc
 *    \brief Implementation.
 *     Provides the implementation for the wrapers for the PostScript graphing utility.
 */
// ==========================================================================================

#include <psgraph.hh>
#include <string.h>

/** Number of pixels (72/inch) in the printable box. */
#define BOX ((double) 540) 

// ==========================================================================================
/** PostScript header.
 *  Write a standard version 1 PostScript header. Scale and translation are based on the
 *  world coordinates provided in the initialization of the graphics state data structure.
 * \return success=0, failure=non-zero
 */
// ------------------------------------------------------------------------------------------
int PSGraph::write_ps_head( void ) {
  // ----------------------------------------------------------------------------------------
  outf << "%!PS-Adobe-3.0"                                      << std::endl;
  outf << "%%Title: " << fspc                                   << std::endl;
  outf << "%%Creator: PSGraph"                                  << std::endl;
  outf << "%%Orientation: Landscape"                            << std::endl;
  outf << "%%%%Pages: 1"                                        << std::endl;
  outf << "%%BoundingBox: 0 0 612 792"                          << std::endl;
  outf << "%%DocumentPaperSizes: Letter"                        << std::endl;
  outf << "%%BeginSetup"                                        << std::endl;
  outf << "[{"                                                  << std::endl;
  outf << "%%BeginFeature: *PageRegion Letter"                  << std::endl;
  outf << "<</PageSize [612 792]>> setpagedevice"               << std::endl;
  outf << "%%EndFeature"                                        << std::endl;
  outf << "} stopped cleartomark"                               << std::endl;
  outf << "%%EndSetup"                                          << std::endl;
  outf << "%%Magnification: 1.0000"                             << std::endl;
  outf << "%%EndComments"                                       << std::endl << std::endl;

  outf << "%%BeginProlog"                                       << std::endl;
  outf << "/dl { newpath moveto lineto stroke } bind def"       << std::endl;
  outf << "/dr { newpath rectstroke } bind def"                 << std::endl;
  outf << "/dc { newpath 0 360 arc closepath stroke } bind def" << std::endl;
  outf << "%EndProlog"                                          << std::endl << std::endl;

  outf << "%%Page: 1 1"                                         << std::endl  << std::endl;

  outf << "gsave"                                               << std::endl << std::endl;

  outf << "0 setlinewidth"                                      << std::endl << std::endl;

  outf << "576 162 translate 90 rotate"                         << std::endl << std::endl;

  outf << "0 0 " << BOX << " " << BOX << " dr"                  << std::endl << std::endl;

  outf << "" << BOX << " " << difX << " div " << BOX << " " << difY
       << " div scale " << (-minX) << " " << (-minY) << " translate" << std::endl << std::endl;

  return 0;
}

// ==========================================================================================
/** PostScript Trailer.
 *  Write a standard version 1 PostScript trailer.
 * \return success=0, failure=non-zero
 */
// ------------------------------------------------------------------------------------------
int PSGraph::write_ps_tail( void ) {
  // ----------------------------------------------------------------------------------------
  outf << "grestore" << std::endl;
  outf << "showpage" << std::endl << std::endl;

  outf << "%%Trailer" << std::endl;
  outf << "%%EOF" << std::endl;

  return 0;
}

// ==========================================================================================
/** Constructor.
 *  Initialize the graphics state data structure. Allocate dynamic memory for the structure.
 * \param path file path for postscript output.
 */
// ------------------------------------------------------------------------------------------
PSGraph::PSGraph( char *path ) : 
  minX(0.0), minY(0.0), difX(0.0), difY(0.0), outf(0), fspc(0) {
  // ----------------------------------------------------------------------------------------
  outf.open(path);

  fspc = new char[MAX_PATH];

  ::copy(fspc, path, MAX_PATH-1 );
}

// ==========================================================================================
/** Constructor.
 *  Initialize the graphics state data structure. Allocate dynamic memory for the structure.
 * \param RP reference to a \a PSGraph to copy from.
 */
// ------------------------------------------------------------------------------------------
PSGraph::PSGraph(const PSGraph &RP) : 
  minX(0.0), minY(0.0), difX(0.0), difY(0.0), outf(0), fspc(0) {
  // ----------------------------------------------------------------------------------------
  this->fspc = new char[MAX_PATH];
  this->copy(RP);
}

// ==========================================================================================
/** Copy.
 *  Perform a deep copy.
 * \param RP reference to a \a PSGraph to copy from.
 */
// ------------------------------------------------------------------------------------------
PSGraph & PSGraph::operator = (const PSGraph &RP) {
  // ----------------------------------------------------------------------------------------
  this->copy(RP);
  return *this;
}

// ==========================================================================================
/** Destructor.
 *  Long description.
 */
// ------------------------------------------------------------------------------------------
PSGraph::~PSGraph( void ) {
  // ----------------------------------------------------------------------------------------

  write_ps_tail();

  outf.close();
  
  delete fspc;
}

// ==========================================================================================
/** Sets the world coordinates of the structure \a G.
 *  Coordinates are set in device and world coordinates. This function is used
 *  to define the world coordinate window.
 *  \param x0 minimum X coordinate
 *  \param y0 minimum Y coordinate
 *  \param x1 maximum X coordinate
 *  \param y1 maximum Y coordinate
 *  \return success=0, failure=1. 
 */
// ------------------------------------------------------------------------------------------
int PSGraph::setWorldCo( double x0, double y0,  double x1, double y1 ) {
  // ----------------------------------------------------------------------------------------
  minX = x0;
  minY = y0;
  difX = (x1-x0);
  difY = (y1-y0);
  return 0;
}

// ==========================================================================================
/** Start Graphics.
 *  Begin the capture of graphics commands.
 * \return success=0, failure=non-zero
 */
// ------------------------------------------------------------------------------------------
int PSGraph::initGraphics( void ) {
  // ----------------------------------------------------------------------------------------
  return write_ps_head();
}

// =========================================================================== END FILE =====
