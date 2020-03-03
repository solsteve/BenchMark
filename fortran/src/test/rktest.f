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
C **  DATE   2007-02-18                                                       **
C **                                                                          **
C ==============================================================================

C----+==================================================================--------
      BLOCK DATA GLOB01
C----+==================================================================--------
      INTEGER DONE
      COMMON /TPARS/DONE
C     ------------------------------------------------------------------
      DATA DONE/0/
      END

C----+==================================================================--------
      BLOCK DATA TWOBODY
C----+==================================================================--------
      INTEGER ITER,ISTEP
      DOUBLE PRECISION MAXR,MAXT,ME,MM,RE,RM,XE,YE,XM,YM,
     1     VXE,VYE,VXM,VYM
C     ------------------------------------------------------------------
      COMMON /TBPARS/ITER,ISTEP,MAXR,MAXT,ME,MM,RE,RM,XE,YE,XM,YM,
     1     VXE,VYE,VXM,VYM
C     ------------------------------------------------------------------
      DATA ITER/10000/,ISTEP/60/
      DATA MAXR/4.6D+8/
      DATA MAXT/29.0/
      DATA ME/5.9742D+24/
      DATA MM/7.347673D+22/
      DATA RE/6370996.0/
      DATA RM/1737146.0/
      DATA XE/-4.41156D+6/
      DATA YE/0.0/
      DATA XM/3.58692D+8/
      DATA YM/0.0/
      DATA VXE/0.0/
      DATA VYE/-11.7422/
      DATA VXM/0.0/
      DATA VYM/1082.0/
C     ------------------------------------------------------------------
      END

C----+==================================================================--------
C     CHECK STATE.
C     DETERMINE IF ANY TWO BODIES ARE CLOSER THAN THE SUM OF THIER RADII.
C     AT A PREDETERMINED TIME, ADD VELOCITY TO THE SMALLEST BODY.
C    \PARAM Q CURRENT STATE VECTOR.
C    \PARAM T CURRENT TIME.
C    \PARAM P PARAMETER VECTOR.
C    \RETURN SUCCESS=0, FAILURE=NON-ZERO.
C----+==================================================================--------
      FUNCTION CHECK(Q,T,P)
      INTEGER CHECK
C----+==================================================================--------
      DOUBLE PRECISION Q(*),T,P(*)
C     ------------------------------------------------------------------
      DOUBLE PRECISION DX,DY,DR,S
C     ------------------------------------------------------------------
      INTEGER DONE
      COMMON /TPARS/DONE
C     ------------------------------------------------------------------
      DX=Q(7)-Q(5)
      DY=Q(8)-Q(6)
      DR=P(2)+P(4)

      S=(DX*DX)+(DY*DY)-(DR*DR)

      IF(S.LT.0.0) THEN
         DONE=1
         CHECK=1
      ELSE           
         CHECK=0
      END IF
C     ------------------------------------------------------------------
      RETURN
      END

C----+==================================================================--------
C     INTEGRATE FOR TIME \A T0 TO \A T1. USING A FOURTH ORDER RUNGE-KUTTA 
C     NUMERICAL INTEGRATOR. THE EQUATIONS OF MOTION DESCRIBE A GENERIC TWO BODY
C     GRAVITATIONAL PROBLEM.
C    \PARAM QD FIRST TIME DERIVATIVE OF THE CURRENT STATE VECTOR.
C    \PARAM Q CURRENT STATE VECTOR.
C    \PARAM T CURRENT TIME.
C    \PARAM P PARAMETER VECTOR.
C----+==================================================================--------
      SUBROUTINE DIFEQ(QD,Q,T,P)
C----+==================================================================--------
      DOUBLE PRECISION QD(*),Q(*),T,P(*)
C     ------------------------------------------------------------------
      DOUBLE PRECISION N2PI,NG
      COMMON /CONSTANTS/N2PI,NG
C     ------------------------------------------------------------------
      INTEGER DONE
      COMMON /TPARS/DONE
C     ------------------------------------------------------------------
      DOUBLE PRECISION XD1,YD1,XD2,YD2,M1,M2,DX21,DY21,DX12,DY12
      DOUBLE PRECISION R12SQ,DEN,A1,A2,A3,A4,X1,Y1,X2,Y2
C     ------------------------------------------------------------------
      IF(1.EQ.DONE) GOTO 99

      XD1   = Q(1)
      YD1   = Q(2)
      XD2   = Q(3)
      YD2   = Q(4)
      X1    = Q(5)
      Y1    = Q(6)
      X2    = Q(7)
      Y2    = Q(8)

      M1    = P(1)
      M2    = P(3)

      DX21  = X2-X1
      DY21  = Y2-Y1
      DX12  = X1-X2
      DY12  = Y1-Y2

      R12SQ = (DX12*DX12) + (DY12*DY12);

      DEN   = R12SQ**1.5
      
      A1 = NG*M2*DX21;
      A2 = NG*M2*DY21;
      A3 = NG*M1*DX12;
      A4 = NG*M1*DY12;
      

      IF (DEN.LE.0.0) THEN
         WRITE(0,*) 'INFINITY'
      END IF

      QD(1) = SAFEDIV(A1,DEN)
      QD(2) = SAFEDIV(A2,DEN)
      QD(3) = SAFEDIV(A3,DEN)
      QD(4) = SAFEDIV(A4,DEN)

      QD(5) = XD1
      QD(6) = YD1
      QD(7) = XD2
      QD(8) = YD2

 99   CONTINUE
C     ------------------------------------------------------------------
      RETURN
      END

C----+==================================================================--------
C     ENTRY POINT.
C----+==================================================================--------
      PROGRAM RKTEST
C----+==================================================================--------
      INTEGER DONE
      COMMON /TPARS/DONE
C     ------------------------------------------------------------------
      DOUBLE PRECISION N2PI,NG
      COMMON /CONSTANTS/N2PI,NG
C     ------------------------------------------------------------------
      INTEGER ITER,ISTEP
      DOUBLE PRECISION MAXR,MAXT,ME,MM,RE,RM,XE,YE,XM,YM,
     1     VXE,VYE,VXM,VYM
C     ------------------------------------------------------------------
      COMMON /TBPARS/ITER,ISTEP,MAXR,MAXT,ME,MM,RE,RM,XE,YE,XM,YM,
     1     VXE,VYE,VXM,VYM
C     ------------------------------------------------------------------
      DOUBLE PRECISION PARAM, STATE
      DIMENSION PARAM(4), STATE(8)
      INTEGER I
      DOUBLE PRECISION T,DT,X1,X2,Y1,Y2
C     ------------------------------------------------------------------
      T  = 0.0
      DT = (MAXT*86400.0) / DBLE(ITER)

      X1 = XE
      Y1 = YE
      X2 = XM
      Y2 = YM

      PARAM(1) = ME
      PARAM(2) = RE
      PARAM(3) = MM
      PARAM(4) = RM
      
      STATE(1) = VXE
      STATE(2) = VYE
      STATE(3) = VXM
      STATE(4) = VYM
      STATE(5) = XE
      STATE(6) = YE
      STATE(7) = XM
      STATE(8) = YM

      CALL SETWORLDCO(-MAXR,-MAXR,MAXR,MAXR)
      CALL INITGRAPHICS

      DONE = 0

      DO 10 I=1,ITER
         T = RK4(STATE,8,T,T+DT,ISTEP,PARAM)

         CALL DRAWLINE(X1,Y1,STATE(5),STATE(6))
         CALL DRAWLINE(X2,Y2,STATE(7),STATE(8))
         
         X1 = STATE(5)
         Y1 = STATE(6)
         X2 = STATE(7)
         Y2 = STATE(8)

         IF(1.EQ.DONE) GOTO 20
 10   CONTINUE
 20   CONTINUE
      
      CALL DRAWCIRCLE(STATE(5),STATE(6),PARAM(2))
      CALL DRAWCIRCLE(STATE(7),STATE(8),PARAM(4))
  
      CALL PRINTGRAPHICS

C     ------------------------------------------------------------------
      STOP
      END

C ==============================================================================
C **                        P R O P R I E T A R Y                             **
C =============================================================== END FILE =====
