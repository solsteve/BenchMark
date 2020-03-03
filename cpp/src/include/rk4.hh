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
/**   \file rk4.hh
 *    \brief Header file.
 *     Provides the interface for the Runge-Kutta numerical integrator.
 *     The user must supply the implementation for DIFEQ and CHECK.
 */
// ==========================================================================================

#ifndef __RK4_HH
#define __RK4_HH

#include <basedef.hh>

// ==========================================================================================
class RK4 {
  // ----------------------------------------------------------------------------------------
private:
  int     dim;  /**< number of coupled first order equations */
  double *A;    /**< stage one state vector                  */
  double *B;    /**< stage two state vector                  */
  double *C;    /**< stage three state vector                */
  double *D;    /**< stage four state vector                 */
  double *W;    /**< DIFEQ input vector                      */

public:
  RK4( int n );
  virtual ~RK4( void );

  RK4    (const RK4 &RP);
  RK4 & operator = (const RK4 &RP);

  void copy  (const RK4 &RP);

  virtual  void   DIFEQ     ( double *Qd, double *Q, double t, double *P ) = 0;
  virtual  int    CHECK     (             double *Q, double t, double *P ) = 0;

  double integrate ( double *Q,  double t0, double t1, int step, double *P );
};

#endif

// =========================================================================== END FILE =====
