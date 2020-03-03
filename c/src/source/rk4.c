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
/**   \file rk4.c
 *    \brief Implementation.
 *     Provides the implementation for the fourth order Runge-Kutta numerical integrator.
 */
/* ========================================================================================== */

#include <rk4.h>

static double A[64]; /**< stage one state vector */
static double B[64]; /**< stage two state vector */
static double C[64]; /**< stage three state vector */
static double D[64]; /**< stage four state vector */

static double W[64]; /**< DIFEQ input vector */

/* ========================================================================================== */
/** Fourth order Runge-Kutta.
 *  Provides the implementation for a fourth order Runge-Kutta numerical integrator with 
 *  uniform step sizes.
 * \param Q real vector containing the state.
 * \param n number of elements in the state vector.
 * \param t0 initial time.
 * \param t1 final time.
 * \param step number of steps between current time \a t0 and final time \a t1.
 * \param P vector containing fixed parameters.
 * \return new time (\a t1).
 */
/* ------------------------------------------------------------------------------------------ */
double rk4( double *Q, int n, double t0, double t1, int step, double *P ) {
  /* ---------------------------------------------------------------------------------------- */

  double h = (t1 - t0) / ((double) step);

  double t = t0;

  double h2 = h/2.0;

  int k, j;

  for (k=0; k<step; k++) {

    int r = CHECK( Q, t, P );

    if (0 < r) { 
      fprintf( stderr, "Check Failed\n" );
      return t;
    }

    for (j=0; j<n; j++) { W[j] = Q[j];               } DIFEQ(A, W, t,    P);
    for (j=0; j<n; j++) { W[j] = Q[j] + (A[j] * h2); } DIFEQ(B, W, t+h2, P);
    for (j=0; j<n; j++) { W[j] = Q[j] + (B[j] * h2); } DIFEQ(C, W, t+h2, P);
    for (j=0; j<n; j++) { W[j] = Q[j] + (C[j] * h);  } DIFEQ(D, W, t+h,  P);

    for (j=0; j<n; j++) { Q[j] += (h*(A[j] + 2.0*(B[j] + C[j]) + D[j])/6.0); }

    t += h;
  }

  return t;
}

/* =========================================================================== END FILE ===== */
