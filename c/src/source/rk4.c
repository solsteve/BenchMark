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

/* ========================================================================================== */
/* **                              P R O P R I E T A R Y                                   ** */
/* =========================================================================== END FILE ===== */
