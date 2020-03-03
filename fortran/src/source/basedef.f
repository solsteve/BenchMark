C ===== BEGIN FILE =============================================================
C **                                                                          **
C **  COPYRIGHT (C) 2007, STEPHEN W. SOLIDAY                                  **
C **                      STEPHEN@SOLIDAY.COM                                 **
C **                      HTTP://WWW.SOLIDAY.COM/STEPHEN                      **
C **                                                                          **
C **  THIS PROGRAM IS FREE SOFTWARE: YOU CAN REDISTRIBUTE IT AND/OR MODIFY    **
C **  IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY    **
C **  THE FREE SOFTWARE FOUNDATION, EITHER VERSION 3 OF THE LICENSE, OR       **
C **  (AT YOUR OPTION) ANY LATER VERSION                                      **
C **                                                                          **
C **  THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT     **
C **  WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF              **
C **  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE            **
C **  GNU GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            **
C **                                                                          **
C **  YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE       **
C **  ALONG WITH THIS PROGRAM. IF NOT, SEE <HTTP://WWW.GNU.ORG/LICENSES/>.    **
C **                                                                          **
C **  ----- MODIFICATION HISTORY -------------------------------------------- **
C **                                                                          **
C **  AUTHOR STEPHEN W. SOLIDAY                                               **
C **  DATE   2002-02-18                                                       **
C **                                                                          **
C ==============================================================================

C----+==================================================================--------
      BLOCK DATA BASEDEF
C----+==================================================================--------
      DOUBLE PRECISION N2PI,NG
      COMMON /CONSTANTS/N2PI,NG
C     ------------------------------------------------------------------
      DATA N2PI/6.28318530717959/
      DATA NG/6.6742D-11/
C     ------------------------------------------------------------------
      END

C----+==================================================================--------
      FUNCTION SAFEDIV(A,B)
      DOUBLE PRECISION SAFEDIV
C----+==================================================================--------
      DOUBLE PRECISION A,B
      SAFEDIV = 0.0D0
      IF(0.0.GT.A) SAFEDIV=A/B
      IF(0.0.LT.A) SAFEDIV=A/B
      RETURN
      END

C =============================================================== END FILE =====
