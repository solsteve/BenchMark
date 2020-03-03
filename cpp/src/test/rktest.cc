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
/**   \file rktest.cc
 *    \brief Test Numerical Integrator.
 *     This program tests the functionality of \a RK4. \a RK4 is a fourth order Runge-Kutta
 *     numerical integrator.
 */
// ==========================================================================================

#include <rk4.hh>
#include <psgraph.hh>
#include <2body.h>

// ==========================================================================================
// ------------------------------------------------------------------------------------------
class Motion :public RK4 {
  // ----------------------------------------------------------------------------------------
public:
  int done;

  Motion( void ) :  RK4(8), done(0) {};
  virtual ~Motion( void ) { };

  virtual void DIFEQ ( double *Qd, double *Q, double t, double *P );
  virtual int  CHECK (             double *Q, double t, double *P );
};

// ==========================================================================================
/** Check State.
 *  Determine if any two bodies are closer than the sum of thier radii.
 *  At a predetermined time, add velocity to the smallest body.
 * \param Q current state vector.
 * \param t current time.
 * \param P parameter vector.
 * \return success=0, failure=non-zero.
 */
// ------------------------------------------------------------------------------------------
int Motion::CHECK( double *Q, double t, double *P ) {
  // ----------------------------------------------------------------------------------------
  double dx = Q[6] - Q[4];
  double dy = Q[7] - Q[5];
  double r2 = P[1] + P[3];

  return (((r2*r2) < ((dx*dx)+(dy*dy))) ? (0) : (1));
}

// ==========================================================================================
/** Integrate.
 *  Integrate for time \a t0 to \a t1. Using a fourth order Runge-Kutta numerical integrator.
 *  The equations of mothion describe a generic two body gravitational problem.
 * \param Qd first time derivative of the current state vector.
 * \param Q current state vector.
 * \param t current time.
 * \param P parameter vector.
 * \return success=0, failure=non-zero
 */
// ------------------------------------------------------------------------------------------
void Motion::DIFEQ( double *Qd, double *Q, double t, double *P ) {
  // ----------------------------------------------------------------------------------------
  if (done == 0) {

    double xd1 = Q[0];  double x1 = Q[4];
    double yd1 = Q[1];  double y1 = Q[5];
    double xd2 = Q[2];  double x2 = Q[6];
    double yd2 = Q[3];  double y2 = Q[7];

    double m1   = P[0];
    double m2   = P[2];

    double dx21 = x2 - x1;
    double dy21 = y2 - y1;
    double dx12 = x1 - x2;
    double dy12 = y1 - y2;

    double r12sq = (dx12*dx12) + (dy12*dy12);
    double den   = POWER(r12sq,1.5);

    if (den > 0.0) {} else { fprintf( stderr, "INFINITY\n" ); }

    double a1 = N_G*m2*dx21;
    double a2 = N_G*m2*dy21;
    double a3 = N_G*m1*dx12;
    double a4 = N_G*m1*dy12;

    Qd[0] = SAFEDIV(a1,den);
    Qd[1] = SAFEDIV(a2,den);
    Qd[2] = SAFEDIV(a3,den);
    Qd[3] = SAFEDIV(a4,den);
    Qd[4] = xd1;
    Qd[5] = yd1;
    Qd[6] = xd2;
    Qd[7] = yd2;
  }
}

// ==========================================================================================
/** Entry Point.
 *  Standard CPP entry point.
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
    std::cerr << "USAGE: " << progName << " test.ps" << std::endl << std::endl;
    return 1;
  }

  double param[4];
  double state[8];
  double t  = 0.0;
  double dt = (MAXT * 86400.0) / ((double) ITER);

  param[0] = Me;
  param[1] = Re;
  param[2] = Mm;
  param[3] = Rm;

  state[0] = VXe;       state[4] = Xe;
  state[1] = VYe;       state[5] = Ye;
  state[2] = VXm;       state[6] = Xm;
  state[3] = VYm;       state[7] = Ym;

  double x1 = state[4];
  double y1 = state[5];
  double x2 = state[6];
  double y2 = state[7];

  PSGraph *PSG = new PSGraph( fspc );

  PSG->setWorldCo( -maxr, -maxr, maxr, maxr );
  PSG->initGraphics();

  Motion *MM = new Motion();

  for (int i=0; i<ITER; i++) {

    t = MM->integrate( state, t, t+dt, ISTEP, param );

    PSG->drawLine( x1, y1, state[4], state[5]);
    PSG->drawLine( x2, y2, state[6], state[7] );
 
    x1 = state[4];
    y1 = state[5];
    x2 = state[6];
    y2 = state[7];

    if (MM->done == 1) { break; }
  }

  PSG->drawCircle( state[4], state[5], param[1] );
  PSG->drawCircle( state[6], state[7], param[3] );
  
  delete PSG;

  return 0;
}

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
