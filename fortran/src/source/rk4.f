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
C **  PROVIDES THE IMPLEMENTATION FOR THE FOURTH ORDER RUNGE-KUTTA NUMERICAL  **
C **  INTEGRATOR.                                                             **
C **                                                                          **
C ==============================================================================

C----+==================================================================--------
      BLOCK DATA RK4BD
C----+==================================================================--------
      DOUBLE PRECISION A,B,C,D,W
      DIMENSION A(64),B(64),C(64),D(64),W(64)
      COMMON /RK4PARS/A,B,C,D,W
C     ------------------------------------------------------------------
      END

C----+==================================================================--------
C     FOURTH ORDER RUNGE-KUTTA.
C     PROVIDES THE IMPLEMENTATION FOR A FOURTH ORDER RUNGE-KUTTA NUMERICAL
C     INTEGRATOR WITH UNIFORM STEP SIZES.
C    \PARAM Q REAL VECTOR CONTAINING THE STATE.
C    \PARAM N NUMBER OF ELEMENTS IN THE STATE VECTOR.
C    \PARAM T0 INITIAL TIME.
C    \PARAM T1 FINAL TIME.
C    \PARAM STEP NUMBER OF STEPS BETWEEN CURRENT TIME A T0 AND FINAL TIME A T1.
C    \PARAM P VECTOR CONTAINING FIXED PARAMETERS.
C    \RETURN NEW TIME (\A T1).
C----+==================================================================--------
      FUNCTION RK4(Q,N,T0,T1,STEP,P)
      DOUBLE PRECISION RK4
C----+==================================================================--------
      DOUBLE PRECISION Q(*)
      INTEGER N
      DOUBLE PRECISION T0,T1
      INTEGER STEP
      DOUBLE PRECISION P(*)
C     ------------------------------------------------------------------
      DOUBLE PRECISION A,B,C,D,W
      DIMENSION A(64),B(64),C(64),D(64),W(64)
      COMMON /RK4PARS/A,B,C,D,W
C     ------------------------------------------------------------------
      DOUBLE PRECISION H,H2,T
      INTEGER J,K,R, CHECK
C     ------------------------------------------------------------------
      H = (T1-T0)/DBLE(STEP)
      T=T0
      H2=H/2.0

      DO 10 K=1,STEP
         R = CHECK(Q,T,P)
         IF(0.LT.R) THEN
            WRITE(0,*) '2 CHECK FAILED'         
            GOTO 20
         END IF
C        --------------------------------------
         DO 1 J=1,N
            W(J)=Q(J)
 1       CONTINUE
         CALL DIFEQ(A,W,T,P)
C        --------------------------------------
         DO 2 J=1,N
            W(J)=Q(J)+(A(J)*H2)
 2       CONTINUE
         CALL DIFEQ(B,W,T+H2,P)
C        --------------------------------------
         DO 3 J=1,N
            W(J)=Q(J)+(B(J)*H2)
 3       CONTINUE
         CALL DIFEQ(C,W,T+H2,P)
C        --------------------------------------
         DO 4 J=1,N
            W(J)=Q(J)+(C(J)*H)
 4       CONTINUE
         CALL DIFEQ(D,W,T+H,P)
C        --------------------------------------
         DO 5 J=1,N
            Q(J)= Q(J)+(H*(A(J)+2.0*(B(J)+C(J))+D(J))/6.0)
 5          CONTINUE
C        --------------------------------------
         T=T+H
C        --------------------------------------
 10   CONTINUE
 20   CONTINUE
      RK4=T
C     ------------------------------------------------------------------
      RETURN
      END

C =============================================================== END FILE =====
