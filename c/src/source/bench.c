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
/**   \file bench.c
 *    \brief Implementation.
 *     Provides the implementation for the primary bench marking program in C.
 */
/* ========================================================================================== */

#include <rk4.h>
#include <psgraph.h>
#include <4body.h>
#include <time.h>

static int done = 0;

#define iVX1  0
#define iVY1  1
#define  iX1  2
#define  iY1  3
#define iVX2  4
#define iVY2  5
#define  iX2  6
#define  iY2  7
#define iVX3  8
#define iVY3  9
#define  iX3 10
#define  iY3 11
#define iVX4 12
#define iVY4 13
#define  iX4 14
#define  iY4 15

#define   jG 0
#define  jM1 1
#define  jR1 2
#define  jM2 3
#define  jR2 4
#define  jM3 5
#define  jR3 6
#define  jM4 7
#define  jR4 8

#define NN 4

static long   iCount = 0;
static double tCount = 0.0;

static double Acc    = (DELTATIME / ((double) ISTEP))*9.81*7;

/* ========================================================================================== */
/** Check State.
 *  Determine if any two bodies are closer than the sum of thier radii.
 *  At a predetermined time, add velocity to the smallest body.
 * \param Q current state vector.
 * \param t current time.
 * \param P parameter vector.
 * \return success=0, failure=non-zero.
 */
/* ------------------------------------------------------------------------------------------ */
int CHECK( double *Q, double t, double *P ) {
  /* ---------------------------------------------------------------------------------------- */
  double x1 = Q[iX1];   double y1 = Q[iY1];
  double x2 = Q[iX2];   double y2 = Q[iY2];
  double x3 = Q[iX3];   double y3 = Q[iY3];
  double x4 = Q[iX4];   double y4 = Q[iY4];
  
  double rad1 = P[jR1];
  double rad2 = P[jR2];
  double rad3 = P[jR3];
  double rad4 = P[jR4];
 
  double r12 = rad1 + rad2;
  double r13 = rad1 + rad3;
  double r14 = rad1 + rad4;
  double r23 = rad2 + rad3;
  double r24 = rad2 + rad4;
  double r34 = rad3 + rad4;

  double dx12 = x1 - x2;
  double dy12 = y1 - y2;

  double dx13 = x1 - x3;
  double dy13 = y1 - y3;

  double dx14 = x1 - x4;
  double dy14 = y1 - y4;

  double dx23 = x2 - x3;
  double dy23 = y2 - y3;

  double dx24 = x2 - x4;
  double dy24 = y2 - y4;

  double dx34 = x3 - x4;
  double dy34 = y3 - y4;

  if (t > 21912800.0) {
    double v2 = (Q[iVX4]*Q[iVX4]) + (Q[iVY4]*Q[iVY4]);
    if (v2 < 1.0e+9) {
      double cc = sqrt(v2);
      fprintf( stdout, "%13.6e %13.6e %13.6e %13.6e\n", Q[iX4], Q[iY4], Q[iVX4], Q[iVY4]);
      Q[iVX4] += Acc*Q[iVX4]/cc;
      Q[iVY4] += Acc*Q[iVY4]/cc;
      iCount++;
      tCount += (DELTATIME / ((double) ISTEP));
    }
  }

  if ((r12*r12) > ((dx12*dx12)+(dy12*dy12))) { return 1; }
  if ((r13*r13) > ((dx13*dx13)+(dy13*dy13))) { return 2; }
  if ((r14*r14) > ((dx14*dx14)+(dy14*dy14))) { return 3; }
  if ((r23*r23) > ((dx23*dx23)+(dy23*dy23))) { return 4; }
  if ((r24*r24) > ((dx24*dx24)+(dy24*dy24))) { return 5; }
  if ((r34*r34) > ((dx34*dx34)+(dy34*dy34))) { return 6; }

  return 0;
}

/* ========================================================================================== */
/** Integrate.
 *  Integrate for time \a t0 to \a t1. Using a fourth order Runge-Kutta numerical integrator.
 *  The equations of mothion describe a generic N body gravitational problem.
 * \param Qd first time derivative of the current state vector.
 * \param Q current state vector.
 * \param t current time.
 * \param P parameter vector.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
void DIFEQ( double *Qd, double *Q, double t, double *P ) {
  /* ---------------------------------------------------------------------------------------- */
  static double dx[NN][NN];
  static double dy[NN][NN];

  static double DEN[NN][NN];

  static double xdd[NN];
  static double ydd[NN];

  static double x[NN];
  static double y[NN];

  static double m[NN];

  int i,k;

  if (!done) {
    double G = P[jG];

    x[0]  = Q[iX1];    y[0]  = Q[iY1];
    x[1]  = Q[iX2];    y[1]  = Q[iY2];
    x[2]  = Q[iX3];    y[2]  = Q[iY3];
    x[3]  = Q[iX4];    y[3]  = Q[iY4];

    m[0] = P[jM1];
    m[1] = P[jM2];
    m[2] = P[jM3];
    m[3] = P[jM4];

    for (k=1; k<NN; k++) {
      for (i=0; i<k; i++) {
	dx[i][k] = -(dx[k][i] = x[k] - x[i]);
	dy[i][k] = -(dy[k][i] = y[k] - y[i]);
      }
    }

    for (k=1; k<NN; k++) {
      for (i=0; i<k; i++) {
	double tt =  (dx[k][i]*dx[k][i]) + (dy[k][i]*dy[k][i]);
	DEN[i][k] = (DEN[k][i] = POWER(tt,-1.5));
      }
    }

    for (i=0; i<NN; i++) {
      double sumx = 0.0;
      double sumy = 0.0;
      for (k=0; k<NN; k++) {
	if (k!=i) {
	  sumx += (m[k]*dx[k][i]*DEN[k][i]);
	  sumy += (m[k]*dy[k][i]*DEN[k][i]);
	}
      }
      xdd[i] = G*sumx;
      ydd[i] = G*sumy;
    }

    Qd[iVX1] = xdd[0];  Qd[iVY1] = ydd[0];    Qd[iX1] = Q[iVX1];  Qd[iY1] = Q[iVY1];
    Qd[iVX2] = xdd[1];  Qd[iVY2] = ydd[1];    Qd[iX2] = Q[iVX2];  Qd[iY2] = Q[iVY2];
    Qd[iVX3] = xdd[2];  Qd[iVY3] = ydd[2];    Qd[iX3] = Q[iVX3];  Qd[iY3] = Q[iVY3];
    Qd[iVX4] = xdd[3];  Qd[iVY4] = ydd[3];    Qd[iX4] = Q[iVX4];  Qd[iY4] = Q[iVY4];
  }
}

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

  clock_t start_time, elapsed;

  char *progName = argv[0];
  char *fspc     = argv[1];

  PSGraph *PS;

  double Xc  = 3.0e+11;
  double Yc  = 3.5e+11;
  double win = 6.0e+11;
  double kk;
  double param[16];
  double state[32];
  double t  = 0.0;
  double maxTime = (MAXT * 86400.0);

  double x1 = X1;  double y1 = Y1;
  double x2 = X2;  double y2 = Y2;
  double x3 = X3;  double y3 = Y3;
  double x4 = X4;  double y4 = Y4;

  param[jG]  = N_G;
  param[jM1] = M1;   param[jR1] = R1;
  param[jM2] = M2;   param[jR2] = R2;
  param[jM3] = M3;   param[jR3] = R3;
  param[jM4] = M4;   param[jR4] = R4;

  state[iVX1] = VX1;  state[iVY1] = VY1;    state[iX1] = X1;  state[iY1] = Y1;
  state[iVX2] = VX2;  state[iVY2] = VY2;    state[iX2] = X2;  state[iY2] = Y2;
  state[iVX3] = VX3;  state[iVY3] = VY3;    state[iX3] = X3;  state[iY3] = Y3;
  state[iVX4] = VX4;  state[iVY4] = VY4;    state[iX4] = X4;  state[iY4] = Y4;

  if (argc != 2) {
    fprintf( stderr, "USAGE: %s test.ps\n\n", progName );
    return 1;
  }

  PS = PSG_new( fspc );

  PSG_setWorldCo( PS, Xc-win, Yc-win, Xc+win, Yc+win );

  PSG_initGraphics ( PS );

  start_time = clock();

  while(t < maxTime) {

    t = rk4( state, 16, t, t+DELTATIME, ISTEP, param );

    PSG_drawLine( PS, x1, y1, state[iX1],  state[iY1]);
    PSG_drawLine( PS, x2, y2, state[iX2],  state[iY2]);
    PSG_drawLine( PS, x3, y3, state[iX3],  state[iY3]);
    PSG_drawLine( PS, x4, y4, state[iX4],  state[iY4]);

    x1 = state[iX1]; y1 = state[iY1];
    x2 = state[iX2]; y2 = state[iY2];
    x3 = state[iX3]; y3 = state[iY3];
    x4 = state[iX4]; y4 = state[iY4];

    if (done) { break; }
  }
  elapsed = clock() - start_time;
  
  PSG_drawCircle( PS, state[iX1],  state[iY1],  param[jR1] );
  PSG_drawCircle( PS, state[iX2],  state[iY2],  param[jR2] );
  PSG_drawCircle( PS, state[iX3],  state[iY3],  param[jR3] );
  PSG_drawCircle( PS, state[iX4],  state[iY4],  param[jR4] );

  PSG_del( PS );

  kk = sqrt((state[iVX4]*state[iVX4])+(state[iVY4]*state[iVY4]));

  fprintf( stdout, "Time    %13.6e\n", t );
  fprintf( stdout, "Sun     %13.6e %13.6e %13.6e %13.6e\n", state[iX1], state[iY1], state[iVX1], state[iVY1] );
  fprintf( stdout, "Jupiter %13.6e %13.6e %13.6e %13.6e\n", state[iX2], state[iY2], state[iVX2], state[iVY2] );
  fprintf( stdout, "Earth   %13.6e %13.6e %13.6e %13.6e\n", state[iX3], state[iY3], state[iVX3], state[iVY3] );
  fprintf( stdout, "Moon    %13.6e %13.6e %13.6e %13.6e %13.6e\n", state[iX4], state[iY4], state[iVX4], state[iVY4], kk );
  fprintf( stdout, "Count   %ld\n", iCount );
  fprintf( stdout, "Burn    %13.6e seconds\n", tCount );

  fprintf( stderr, "\nClocks = %ld\n\n", elapsed );

  return 0;
}

/* ========================================================================================== */
/* **                              P R O P R I E T A R Y                                   ** */
/* =========================================================================== END FILE ===== */
