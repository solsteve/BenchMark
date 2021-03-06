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
/* **  Date   2012-09-30                                                                   ** */
/* **                                                                                      ** */
/**   @file sincos.c
 *    @brief Loop timing for various math functions.
 */
/* ========================================================================================== */

#include <common.h>

#include <complex.h>

typedef double (*time_test_f)(size_t n);

void LOOPTEST( time_test_f func, const char* name, size_t loop_count );
double CEXP_1( size_t n );
double CEXP_2( size_t n );
double CEXP_3( size_t n );

/* ========================================================================================== */
/* ------------------------------------------------------------------------------------------ */
double CEXP_1( size_t n ) {
  /* ---------------------------------------------------------------------------------------- */
  double x0 = -1.0e0;
  double x1 =  1.0e0;
  double dx = (x1-x0) / (double)n;
  double x  = x0;
  double A  = 0.0e0;
  double B  = 0.0e0;
  size_t i;
  for (i=0; i<n; i++) {
    A += cos(x);
    B += sin(x);
    x += dx;
  }

  return sqrt(A*A + B*B);
}

/* ========================================================================================== */
/* ------------------------------------------------------------------------------------------ */
double CEXP_2( size_t n ) {
  /* ---------------------------------------------------------------------------------------- */
  double x0 = -1.0e0;
  double x1 =  1.0e0;
  double dx = (x1-x0) / (double)n;
  double x  = x0;
  double a,A  = 0.0e0;
  double b,B  = 0.0e0;
  size_t i;
  for (i=0; i<n; i++) {
    sincos( x, &b, &a );
    A += a;
    B += b;
    x += dx;
  }

  return sqrt(A*A + B*B);
}

/* ========================================================================================== */
/* ------------------------------------------------------------------------------------------ */
double CEXP_3( size_t n ) {
  /* ---------------------------------------------------------------------------------------- */
  double x0 = -1.0e0;
  double x1 =  1.0e0;
  double dx = (x1-x0) / (double)n;
  double x  = x0;

  double complex z = 0.0e0 + 0.0e0*I;

  size_t i;
  for (i=0; i<n; i++) {
    z += cexp(I*x);
    x += dx;    
  }

  return cabs(z);
}

/* ========================================================================================== */
/* ------------------------------------------------------------------------------------------ */
void LOOPTEST( time_test_f func, const char* name, size_t loop_count ) {
  /* ---------------------------------------------------------------------------------------- */
  clock_t start_time;
  clock_t stop_time;
  double  elapsed_time, x;

  fprintf( stdout, "\nTest %s: loop count = %lu\n", name, loop_count );

  start_time = clock();

  x = (*func) (loop_count);  

  stop_time  = clock();

  elapsed_time = (double)(stop_time - start_time) / (double) CLOCKS_PER_SEC;

  fprintf( stdout, "%g seconds.\n", elapsed_time);
  fprintf( stdout, "%g = results.\n\n", x );
}


/* ========================================================================================== */
/* ------------------------------------------------------------------------------------------ */
int main( void ) {
  /* ---------------------------------------------------------------------------------------- */
  LOOPTEST( CEXP_1, "Individual Sine-Cosine", 512*512*20*40 );
  LOOPTEST( CEXP_2, "Combined   Sine-Cosine", 512*512*20*40 );
  LOOPTEST( CEXP_3, "Combined   Sine-Cosine", 512*512*20*40 );

  return 0;
}

/* =========================================================================== END FILE ===== */
