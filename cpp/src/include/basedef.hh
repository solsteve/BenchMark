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
/**   \file basedef.hh
 *    \brief Header file.
 *     Provides a common set of macros.
 */
// ==========================================================================================

#ifndef __BASEDEF_HH
#define __BASEDEF_HH

#include <iostream>
#include <math.h>

#ifndef MAX_PATH
# ifndef MAXPATH
#  define MAX_PATH 128
# else
#  define MAX_PATH MAXPATH
# endif
#endif

// ==========================================================================================
/** Arbitray Root.
 *  Find the \a d root of \a x using the log identity.
 *  \param x number.
 *  \param d root.
 *  \return the \a d root of \a x.
 */
// ------------------------------------------------------------------------------------------
inline double ROOT( double x, double d ) {
  // ----------------------------------------------------------------------------------------
  return exp(log(x)/(d));
}

// ==========================================================================================
/** Arbitray Power.
 *  Find the \a d power of \a x using the log identity.
 *  \param x base.
 *  \param d power.
 *  \return the \a d power of \a x.
 */
// ------------------------------------------------------------------------------------------
inline double POWER( double x, double d ) {
  // ----------------------------------------------------------------------------------------
  return exp(log(x)*(d));
}

// ==========================================================================================
/** Safe Divide.
 *  Division that does not result in NaN if both the numerator and the denominator are zero.
 *  \param n numerator.
 *  \param d denominator.
 *  \return the \a n divided by \a d.
 */
// ------------------------------------------------------------------------------------------
inline double SAFEDIV( double n, double d ) {
  // ----------------------------------------------------------------------------------------
  return (((n)<0.0)?((n)/(d)):(((n)>0.0)?((n)/(d)):(0.0)));
}

// ==========================================================================================
/** Clone string.
 *  Clone a string. If \a n is non-NULL the size of the string will be return.
 * \param src pointer to the source string.
 * \param n pointer to return the size of the string (if \a n is non NULL).
 * \return pointer to the newly allocated string.
 */
// ------------------------------------------------------------------------------------------
inline char* clone( const char *src, size_t *n = (size_t)0 ) {
  // ----------------------------------------------------------------------------------------
  size_t count = 0;
  while(src[count]) {
    count++;
  }
  char *dst = new char[count+1];
  for (size_t i=0; i<count; i++) {
    dst[i] = src[i];
  }
  dst[count] = (char)0;
  if (n) {
    *n = count;
  }
  return dst;
}

// ==========================================================================================
/** Copy string.
 *  Copy a string. If \a n is supplied only \a n character plus the NULL terminator will be
 *  copied.
 * \param dst pointer to the destination string.
 * \param src pointer to the source string.
 * \param n number of characters to copy (if supplied)
 * \return pointer to the destination string.
 */
// ------------------------------------------------------------------------------------------
inline char* copy( char* dst, const char *src, size_t n = 0 ) {
  // ----------------------------------------------------------------------------------------
  if (n>0) {
    for (size_t i=0; i<n; i++) {
      dst[i] = src[i];
    }
    dst[n] = (char)0;
  } else {
    size_t idx = 0;
    while(src[idx]) {
      dst[idx] = src[idx];
      idx++;
    }
    dst[idx] = (char)0;
  }
  return dst;
}

// ------------------------------------------------------------------------------------------
/** Message.
 *  Display a common formated message to the STD::CERR. The format consists of the source
 *  filename, the line number, and a user supplied string.
 * \param a user supplied string.
 */
// ------------------------------------------------------------------------------------------
#define MSG(a) std::cerr << __FILE__ << ": " << __LINE__ << ": " << a << std::endl;

// ------------------------------------------------------------------------------------------
/** Debug Marker.
 *  Use message for displaying a debug position marker.
 */
// ------------------------------------------------------------------------------------------
#define MARK MSG("<<========================================>>")

// ------------------------------------------------------------------------------------------

#define N_2PI ((double) 6.28318530717959)          /**< 2 times Pi                         */
#define N_G   ((double) 6.6742e-11)                /**< Newton's Gravitational Constant    */

#endif

// ==========================================================================================
// **                              P R O P R I E T A R Y                                   **
// =========================================================================== END FILE =====
