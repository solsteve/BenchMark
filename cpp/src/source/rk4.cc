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
/**   \file rk4.cc
 *    \brief Implementation.
 *     Provides the implementation for the fourth order Runge-Kutta numerical integrator.
 */
// ==========================================================================================

#include <rk4.hh>

// ==========================================================================================
/** Constructor.
 *  Allocate work vectors for integration.
 * \param n number of coupled first-order differential equations.
 */
// ------------------------------------------------------------------------------------------
RK4::RK4( int n ) : dim(n), A(0), B(0), C(0), D(0), W(0) {
  // ------------------------------------------------------------------------------------------
  A = new double[dim];
  B = new double[dim];
  C = new double[dim];
  D = new double[dim];
  W = new double[dim];
}

// ==========================================================================================
/** Constructor.
 *  Allocate work vectors for integration.
 * \param RP reference to the template class.
 */
// ------------------------------------------------------------------------------------------
RK4::RK4 (const RK4 &RP) : dim(RP.dim), A(0), B(0), C(0), D(0), W(0) {
  // ----------------------------------------------------------------------------------------
  A = new double[dim];
  B = new double[dim];
  C = new double[dim];
  D = new double[dim];
  W = new double[dim];
  copy(RP);
}

// ==========================================================================================
/** Destructor.
 *  Free work vectors for integration.
 */
// ------------------------------------------------------------------------------------------
RK4::~RK4( void ) {
  // ----------------------------------------------------------------------------------------
  delete A;
  delete B;
  delete C;
  delete D;
  delete W;
}

// ==========================================================================================
/** Copy.
 *  Overload the equal sign and perform a deep copy.
 * \param RP reference to a \a RK4 to copy from.
 */
// ------------------------------------------------------------------------------------------
RK4 & RK4::operator = (const RK4 &RP) {
  // ----------------------------------------------------------------------------------------
  copy(RP);
  return *this;
}

// ==========================================================================================
/** Copy.
 *  Perform a deep copy.
 * \param that reference to a \a RK4 to copy from.
 */
// ------------------------------------------------------------------------------------------
void RK4::copy(const RK4 &that) {
  // ----------------------------------------------------------------------------------------
  for (int i=0; i<dim; i++) {
    this->A[i] = that.A[i];
    this->B[i] = that.B[i];
    this->C[i] = that.C[i];
    this->D[i] = that.D[i];
    this->W[i] = that.W[i];
  }
}

// ==========================================================================================
/** Fourth order Runge-Kutta.
 *  Provides the implementation for a fourth order Runge-Kutta numerical integrator with 
 *  uniform step sizes.
 * \param Q real vector containing the state.
 * \param t0 initial time.
 * \param t1 final time.
 * \param step number of steps between current time \a t0 and final time \a t1.
 * \param P vector containing fixed parameters.
 * \return new time (\a t1).
 */
// ------------------------------------------------------------------------------------------
double RK4::integrate( double *Q, double t0, double t1, int step, double *P ) {
  // ----------------------------------------------------------------------------------------
  double h  = (t1 - t0) / ((double) step);
  double t  = t0;
  double h2 = h / 2.0;

  for (int k=0; k<step; k++) {

    int v = CHECK( Q, t, P );
    if (0 != v) {
      std::cout << "Impact at " << t << " with " << v << std::endl;
      return t;
    }

    for (int j=0; j<dim; j++) {
      W[j] = Q[j];
    }
    DIFEQ(A, W, t, P);
    
    for (int j=0; j<dim; j++) {
      W[j] = Q[j] + (A[j] * h2);
    }
    DIFEQ(B, W, t+h2, P);
    
    for (int j=0; j<dim; j++) {
      W[j] = Q[j] + (B[j] * h2);
    }
    DIFEQ(C, W, t+h2, P);
    
    for (int j=0; j<dim; j++) {
      W[j] = Q[j] + (C[j] * h);
    }
    DIFEQ(D, W, t+h,  P);

    for (int j=0; j<dim; j++) {
      Q[j] += (h*(A[j] + 2.0*(B[j] + C[j]) + D[j])/6.0);
    }

    t += h;
  }

  return t;
}

// =========================================================================== END FILE =====
