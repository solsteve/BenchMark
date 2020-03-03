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

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
