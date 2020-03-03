C ===== BEGIN FILE =============================================================
C **                        P R O P R I E T A R Y                             **
C ==============================================================================
C **                                                                          **
C **  COPYRIGHT (C) 2007, STEPHEN W. SOLIDAY                                  **
C **                      STEPHEN@SOLIDAY.COM                                 **
C **                      HTTP://WWW.SOLIDAY.COM/STEPHEN                      **
C **                                                                          **
C **  THIS FILE, AND THE ASSOCIATED ALGORITHMS, ARE NOT FREE SOFTWARE; YOU    **
C **  MAY NOT REDISTRIBUTE THEM AND/OR MODIFY THEM. THESE ALGORITHMS WERE     **
C **  DEVELOPED AND IMPLEMENTED FOR THE PURPOSE OF AN INTERNAL ASSESSMENT AND **
C **  HAVE, AS YET, NOT BEEN PUBLICLY DISTRIBUTED. DEVELOPMENT OF THESE       **
C **  ALGORITHMS HAVE BEEN AT THE SOLE COST IN BOTH TIME AND FUNDING BY THEIR **
C **  AUTHOR. UNTIL SUCH A PUBLIC RELEASE IS MADE, THE AUTHOR RETAINS ALL     **
C **  RIGHTS TO THESE ALGORITHMS. IT IS EXPECTED THAT IF THIS PROGRAM OR ANY  **
C **  OF THE ALGORITHMS CONTAINED HEREIN ARE DEEMED RELEASABLE THEY WILL BE   **
C **  RELEASED UNDER THE GNU PUBLIC LICENSE FOR NON-COMMERCIAL USE AND/OR     **
C **  WITH RESTRICTED RIGHTS FOR GOVERNMENT USE. AT THAT TIME EACH SOURCE     **
C **  FILE WILL CONTAIN EITHER/BOTH THE STANDARD GPL STATEMENT/DISCLAIMER,    **
C **  AND/OR THE DFARS RESTRICTED RIGHTS LEGEND.                              **
C **                                                                          **
C **  THESE ALGORITHMS EXISTS AT THE PRESENT TIME WITHOUT ANY WARRANTY;       **
C **  WITHOUT EVEN THE IMPLIED WARRANTY OF MERCHANTABILITY OR FITNESS FOR A   **
C **  PARTICULAR PURPOSE. AS YOU ARE NOT SUPPOSED TO BE IN POSSESSION OF THIS **
C **  FILE IF YOU USE IT, YOU DO SO AT YOUR OWN RISK.                         **
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
C    \PARAM STEP NUMBER OF STEPS BETWEEN CURRENT TIME \A T0 AND FINAL TIME \A T1.
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

C ==============================================================================
C **                        P R O P R I E T A R Y                             **
C =============================================================== END FILE =====
