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
C **  DATE   2007-02-18                                                       **
C **                                                                          **
C ==============================================================================

C----+==================================================================--------
      PROGRAM PTEST
C----+==================================================================--------
      DOUBLE PRECISION SEMIMAJORAXIS,SEMIMINORAXIS
      INTEGER STEP
      PARAMETER (SEMIMAJORAXIS=9000.0,SEMIMINORAXIS=7500.0)
      PARAMETER (STEP=7)
C     ------------------------------------------------------------------
      DOUBLE PRECISION N2PI,NG
      COMMON /CONSTANTS/N2PI,NG
C     ------------------------------------------------------------------
      DOUBLE PRECISION T,DT,X0,Y0,X1,Y1,MAXR
      INTEGER I

      MAXR=SEMIMAJORAXIS
      IF(SEMIMINORAXIS.GT.SEMIMAJORAXIS) THEN MAXR=SEMIMINORAXIS

      MAXR=MAXR*1.05

      CALL SETWORLDCO(-MAXR,-MAXR,MAXR,MAXR)
      CALL INITGRAPHICS

      T=0.0
      DT=N2PI / DBLE(STEP)

      X0=SEMIMAJORAXIS*DCOS(T)
      Y0=SEMIMINORAXIS*DSIN(T)

      DO 10 I=1,STEP
         T=T+DT
         X1=SEMIMAJORAXIS*DCOS(T)
         Y1=SEMIMINORAXIS*DSIN(T)
         CALL DRAWLINE(X0,Y0,X1,Y1)
         X0=X1
         Y0=Y1
 10   CONTINUE

      CALL DRAWCIRCLE( 0.0, 0.0, MAXR*0.8 );

      CALL PRINTGRAPHICS

C     ------------------------------------------------------------------
      STOP
      END

C =============================================================== END FILE =====
