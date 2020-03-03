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
      INTEGER DONE,ICOUNT
      DOUBLE PRECISION TCOUNT
      COMMON /TPARS/DONE,ICOUNT,TCOUNT
C     ------------------------------------------------------------------
      DATA DONE/0/
      DATA ICOUNT/0/
      DATA TCOUNT/0.0/
      END

C----+==================================================================--------
      BLOCK DATA FOURBODY
C----+==================================================================--------
      DOUBLE PRECISION MAXT,DELTATIME,ACC
      DOUBLE PRECISION M1,M2,M3,M4,R1,R2,R3,R4
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,X4,Y4
      DOUBLE PRECISION VX1,VY1,VX2,VY2,VX3,VY3,VX4,VY4
      INTEGER ISTEP
C     ------------------------------------------------------------------
      COMMON /FBPARS1/MAXT,DELTATIME,ACC,ISTEP
C     ------------------------------------------------------------------
      COMMON /FBPARS2/M1,M2,M3,M4,R1,R2,R3,R4,X1,Y1,
     1     X2,Y2,X3,Y3,X4,Y4,VX1,VY1,VX2,VY2,VX3,VY3,VX4,VY4
C     ------------------------------------------------------------------
      DATA MAXT/1200.0/
      DATA DELTATIME/21600.0/
      DATA ISTEP/360/
      DATA M1/1.988435D+30/
      DATA M2/1.899D+27/
      DATA M3/5.9742D+24/
      DATA M4/7.347673D+22/
      DATA R1/695000000.0/
      DATA R2/69924754.6/
      DATA R3/6370996.16/
      DATA R4/1737146.5/
      DATA X1/-7.0675082353D+8/
      DATA Y1/0.0/
      DATA X2/7.40035847176D+11/
      DATA Y2/0.0/
      DATA X3/1.47102485561D+11/
      DATA Y3/0.0/
      DATA X4/1.46739381561D+11/
      DATA Y4/0.0/
      DATA VX1/0.0/
      DATA VY1/-11.861/
      DATA VX2/0.0/
      DATA VY2/13712.0/
      DATA VX3/0.0/
      DATA VY3/30287.0/
      DATA VX4/0.0/
      DATA VY4/29205.0/
      DATA ACC/1.0/
C     ------------------------------------------------------------------
      END

C----+==================================================================--------
      BLOCK DATA INDEX
C----+==================================================================--------
      INTEGER IVX1,IVY1,IX1,IY1,IVX2,IVY2,IX2,IY2,IVX3,IVY3,IX3,IY3
      INTEGER IVX4,IVY4,IX4,IY4,JG,JM1,JR1,JM2,JR2,JM3,JR3,JM4,JR4,NN
      PARAMETER (IVX1=1,IVY1=2,IX1=3,IY1=4,IVX2=5,IVY2=6,IX2=7,IY2=8,
     1     IVX3=9,IVY3=10,IX3=11,IY3=12,IVX4=13,IVY4=14,IX4=15,IY4=16,
     2     JG=1,JM1=2,JR1=3,JM2=4,JR2=5,JM3=6,JR3=7,JM4=8,JR4=9,NN=4)
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
      DOUBLE PRECISION X1,X2,X3,X4,Y1,Y2,Y3,Y4
      DOUBLE PRECISION RAD1,RAD2,RAD3,RAD4
      DOUBLE PRECISION R12,R13,R14,R23,R24,R34
      DOUBLE PRECISION DX12,DY12,DX13,DY13,DX14,DY14
      DOUBLE PRECISION DX23,DY23,DX24,DY24,DX34,DY34
      DOUBLE PRECISION V2,CC
C     ------------------------------------------------------------------
      INTEGER IVX1,IVY1,IX1,IY1,IVX2,IVY2,IX2,IY2,IVX3,IVY3,IX3,IY3
      INTEGER IVX4,IVY4,IX4,IY4,JG,JM1,JR1,JM2,JR2,JM3,JR3,JM4,JR4,NN
      PARAMETER (IVX1=1,IVY1=2,IX1=3,IY1=4,IVX2=5,IVY2=6,IX2=7,IY2=8,
     1     IVX3=9,IVY3=10,IX3=11,IY3=12,IVX4=13,IVY4=14,IX4=15,IY4=16,
     2     JG=1,JM1=2,JR1=3,JM2=4,JR2=5,JM3=6,JR3=7,JM4=8,JR4=9,NN=4)
C     ------------------------------------------------------------------
      DOUBLE PRECISION MAXT,DELTATIME,ACC
      INTEGER ISTEP
      COMMON /FBPARS1/MAXT,DELTATIME,ACC,ISTEP
C     ------------------------------------------------------------------
      INTEGER DONE,ICOUNT
      DOUBLE PRECISION TCOUNT
      COMMON /TPARS/DONE,ICOUNT,TCOUNT
C     ------------------------------------------------------------------
      CHECK=0
C
      X1=Q(IX1)
      Y1=Q(IY1)
      X2=Q(IX2)
      Y2=Q(IY2)
      X3=Q(IX3)
      Y3=Q(IY3)
      X4=Q(IX4)
      Y4=Q(IY4)
C
      RAD1=P(JR1)
      RAD2=P(JR2)
      RAD3=P(JR3)
      RAD4=P(JR4)
C
      R12=RAD1+RAD2
      R13=RAD1+RAD3
      R14=RAD1+RAD4
      R23=RAD2+RAD3
      R24=RAD2+RAD4
      R34=RAD3+RAD4
C
      DX12=X1-X2
      DY12=Y1-Y2
      DX13=X1-X3
      DY13=Y1-Y3
      DX14=X1-X4
      DY14=Y1-Y4
      DX23=X2-X3
      DY23=Y2-Y3
      DX24=X2-X4
      DY24=Y2-Y4
      DX34=X3-X4
      DY34=Y3-Y4
C
      IF(T.GT.21912800.0) THEN
         V2=(Q(IVX4)*Q(IVX4))+(Q(IVY4)*Q(IVY4))
         IF (V2.LT.1.0D+9) THEN
            CC=DSQRT(V2)
C           WRITE(0,*) Q(IX4),Q(IY4),Q(IVX4),Q(IVY4)
            Q(IVX4)=Q(IVX4)+(ACC*Q(IVX4)/CC)
            Q(IVY4)=Q(IVY4)+(ACC*Q(IVY4)/CC)
            ICOUNT=ICOUNT+1
            TCOUNT=TCOUNT+(DELTATIME / dble(ISTEP))
         END IF
      END IF
C
      AA12=(R12*R12)
      BB12=(DX12*DX12)+(DY12*DY12)
      IF(AA12.GT.BB12) THEN
         CHECK=1
         DONE=1
         GOTO 99
      END IF
C
      AA13=(R13*R13)
      BB13=(DX13*DX13)+(DY13*DY13)
      IF(AA13.GT.BB13) THEN
         CHECK=2
         DONE=1
         GOTO 99
      END IF
C
      AA14=(R14*R14)
      BB14=(DX14*DX14)+(DY14*DY14)
      IF(AA14.GT.BB14) THEN
         CHECK=3
         DONE=1
         GOTO 99
      END IF
C
      AA23=(R23*R23)
      BB23=(DX23*DX23)+(DY23*DY23)
      IF(AA23.GT.BB23) THEN
         CHECK=4
         DONE=1
         GOTO 99
      END IF
C
      AA24=(R24*R24)
      BB24=(DX24*DX24)+(DY24*DY24)
      IF(AA24.GT.BB24) THEN
         CHECK=5
         DONE=1
         GOTO 99
      END IF
C
      AA34=(R34*R34)
      BB34=(DX34*DX34)+(DY34*DY34)
      IF(AA34.GT.BB34) THEN
         CHECK=6
         DONE=1
      END IF
C
 99   CONTINUE
      RETURN
C     ------------------------------------------------------------------
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
      DOUBLE PRECISION DX,DY,DEN,XDD,YDD,X,Y,M
      COMMON /DEQSAVE/DX(4,4),DY(4,4),DEN(4,4),XDD(4),YDD(4),
     1     X(4),Y(4),M(4)
C     ------------------------------------------------------------------
      INTEGER I,K
      DOUBLE PRECISION G,TT,SUMX,SUMY
C     ------------------------------------------------------------------
      INTEGER IVX1,IVY1,IX1,IY1,IVX2,IVY2,IX2,IY2,IVX3,IVY3,IX3,IY3
      INTEGER IVX4,IVY4,IX4,IY4,JG,JM1,JR1,JM2,JR2,JM3,JR3,JM4,JR4,NN
      PARAMETER (IVX1=1,IVY1=2,IX1=3,IY1=4,IVX2=5,IVY2=6,IX2=7,IY2=8,
     1     IVX3=9,IVY3=10,IX3=11,IY3=12,IVX4=13,IVY4=14,IX4=15,IY4=16,
     2     JG=1,JM1=2,JR1=3,JM2=4,JR2=5,JM3=6,JR3=7,JM4=8,JR4=9,NN=4)
C     ------------------------------------------------------------------
      INTEGER DONE,ICOUNT
      DOUBLE PRECISION TCOUNT
      COMMON /TPARS/DONE,ICOUNT,TCOUNT
C     ------------------------------------------------------------------
      IF(0.NE.DONE) GOTO 99
C
      G=P(JG)

      X(1)=Q(IX1)
      Y(1)=Q(IY1)
      X(2)=Q(IX2)
      Y(2)=Q(IY2)
      X(3)=Q(IX3)
      Y(3)=Q(IY3)
      X(4)=Q(IX4)
      Y(4)=Q(IY4)

      M(1)=P(JM1)
      M(2)=P(JM2)
      M(3)=P(JM3)
      M(4)=P(JM4)

C     ------------------------------------------------------------------
      DO 20 K=2,NN
         DO 10 I=1,K-1
            DX(K,I)=X(K)-X(I)
            DX(I,K)=-DX(K,I)
            DY(K,I)=Y(K)-Y(I)
            DY(I,K)=-DY(K,I)
 10      CONTINUE
 20   CONTINUE
C     ------------------------------------------------------------------
      DO 40 K=2,NN
         DO 30 I=1,K-1
         TT= (DX(K,I)*DX(K,I)) + (DY(K,I)*DY(K,I))
         DEN(K,I)=TT**(-1.5)
         DEN(I,K)=DEN(K,I)
 30      CONTINUE
 40   CONTINUE
C     ------------------------------------------------------------------
      DO 60 I=1,NN
         SUMX=0.0
         SUMY=0.0
         DO 50 K=1,NN
            IF(I.NE.K) THEN
               SUMX=SUMX+(M(K)*DX(K,I)*DEN(K,I))
               SUMY=SUMY+(M(K)*DY(K,I)*DEN(K,I))
            END IF
 50      CONTINUE
         XDD(I)=G*SUMX
         YDD(I)=G*SUMY
 60   CONTINUE
C     ------------------------------------------------------------------
      QD(IVX1)=XDD(1)
      QD(IVY1)=YDD(1)
      QD(IVX2)=XDD(2)
      QD(IVY2)=YDD(2)
      QD(IVX3)=XDD(3)
      QD(IVY3)=YDD(3)
      QD(IVX4)=XDD(4)
      QD(IVY4)=YDD(4)
C
      QD(IX1)=Q(IVX1)
      QD(IY1)=Q(IVY1)
      QD(IX2)=Q(IVX2)
      QD(IY2)=Q(IVY2)
      QD(IX3)=Q(IVX3)
      QD(IY3)=Q(IVY3)
      QD(IX4)=Q(IVX4)
      QD(IY4)=Q(IVY4)
C     ------------------------------------------------------------------
 99   CONTINUE
      RETURN
      END

C----+==================================================================--------
      PROGRAM BENCH
C----+==================================================================--------
      DOUBLE PRECISION XC,YC,WIN,KK,T,MAXTIME,PARAM,STATE
      DOUBLE PRECISION PX1,PX2,PX3,PX4,PY1,PY2,PY3,PY4
      DIMENSION PARAM(16),STATE(32)
C     ------------------------------------------------------------------
      INTEGER IVX1,IVY1,IX1,IY1,IVX2,IVY2,IX2,IY2,IVX3,IVY3,IX3,IY3
      INTEGER IVX4,IVY4,IX4,IY4,JG,JM1,JR1,JM2,JR2,JM3,JR3,JM4,JR4,NN
      PARAMETER (IVX1=1,IVY1=2,IX1=3,IY1=4,IVX2=5,IVY2=6,IX2=7,IY2=8,
     1     IVX3=9,IVY3=10,IX3=11,IY3=12,IVX4=13,IVY4=14,IX4=15,IY4=16,
     2     JG=1,JM1=2,JR1=3,JM2=4,JR2=5,JM3=6,JR3=7,JM4=8,JR4=9,NN=4)
C     ------------------------------------------------------------------
      DOUBLE PRECISION MAXT,DELTATIME,ACC
      DOUBLE PRECISION M1,M2,M3,M4,R1,R2,R3,R4
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,X4,Y4
      DOUBLE PRECISION VX1,VY1,VX2,VY2,VX3,VY3,VX4,VY4
      INTEGER ISTEP
C     ------------------------------------------------------------------
      INTEGER DONE,ICOUNT
      DOUBLE PRECISION TCOUNT
      COMMON /TPARS/DONE,ICOUNT,TCOUNT
C     ------------------------------------------------------------------
      COMMON /FBPARS1/MAXT,DELTATIME,ACC,ISTEP
C     ------------------------------------------------------------------
      COMMON /FBPARS2/M1,M2,M3,M4,R1,R2,R3,R4,X1,Y1,
     1     X2,Y2,X3,Y3,X4,Y4,VX1,VY1,VX2,VY2,VX3,VY3,VX4,VY4
C     ------------------------------------------------------------------
      DOUBLE PRECISION N2PI,NG
      COMMON /CONSTANTS/N2PI,NG
C     ------------------------------------------------------------------
      XC     =3.0D+11
      YC     =3.5D+11
      WIN    =6.0D+11
      T      =0.0
      MAXTIME=MAXT*86400.0
      ACC    =DELTATIME*9.81*7.0/DBLE(ISTEP)

      PX1=X1
      PY1=Y1
      PX2=X2
      PY2=Y2
      PX3=X3
      PY3=Y3
      PX4=X4
      PY4=Y4

      PARAM(JG)  =NG
      PARAM(JM1) =M1
      PARAM(JR1) =R1
      PARAM(JM2) =M2
      PARAM(JR2) =R2
      PARAM(JM3) =M3
      PARAM(JR3) =R3
      PARAM(JM4) =M4
      PARAM(JR4) =R4

      STATE(IVX1)=VX1
      STATE(IVY1)=VY1
      STATE(IVX2)=VX2
      STATE(IVY2)=VY2
      STATE(IVX3)=VX3
      STATE(IVY3)=VY3
      STATE(IVX4)=VX4
      STATE(IVY4)=VY4

      STATE(IX1) =X1
      STATE(IY1) =Y1
      STATE(IX2) =X2
      STATE(IY2) =Y2
      STATE(IX3) =X3
      STATE(IY3) =Y3
      STATE(IX4) =X4
      STATE(IY4) =Y4

      CALL SETWORLDCO(XC-WIN,YC-WIN,XC+WIN,YC+WIN)
      CALL INITGRAPHICS

 10   CONTINUE
      IF(T.GT.MAXTIME) GOTO 99
      IF(0.NE.DONE)    GOTO 99

      T=RK4(STATE,16,T,T+DELTATIME,ISTEP,PARAM)

      CALL DRAWLINE(X1,Y1,STATE(IX1),STATE(IY1))
      CALL DRAWLINE(X2,Y2,STATE(IX2),STATE(IY2))
      CALL DRAWLINE(X3,Y3,STATE(IX3),STATE(IY3))
      CALL DRAWLINE(X4,Y4,STATE(IX4),STATE(IY4))

      X1=STATE(IX1)
      Y1=STATE(IY1)
      X2=STATE(IX2)
      Y2=STATE(IY2)
      X3=STATE(IX3)
      Y3=STATE(IY3)
      X4=STATE(IX4)
      Y4=STATE(IY4)
      GOTO 10
 99   CONTINUE

      CALL DRAWCIRCLE(STATE(IX1),STATE(IY1),PARAM(JR1))
      CALL DRAWCIRCLE(STATE(IX2),STATE(IY2),PARAM(JR2))
      CALL DRAWCIRCLE(STATE(IX3),STATE(IY3),PARAM(JR3))
      CALL DRAWCIRCLE(STATE(IX4),STATE(IY4),PARAM(JR4))

      CALL PRINTGRAPHICS

C     ------------------------------------------------------------------
      KK=DSQRT((STATE(IVX4)*STATE(IVX4))+(STATE(IVY4)*STATE(IVY4)))

      WRITE (0,1010) T
      WRITE (0,1020) STATE(IX1),STATE(IY1),STATE(IVX1),STATE(IVY1)
      WRITE (0,1030) STATE(IX2),STATE(IY2),STATE(IVX2),STATE(IVY2)
      WRITE (0,1040) STATE(IX3),STATE(IY3),STATE(IVX3),STATE(IVY3)
      WRITE (0,1050) STATE(IX4),STATE(IY4),STATE(IVX4),STATE(IVY4),KK
      WRITE (0,1060) ICOUNT
      WRITE (0,1070) TCOUNT
      STOP
C     ------------------------------------------------------------------
 1010 FORMAT('TIME    ',D13.6)
 1020 FORMAT('SUN    ',4(1X,D13.6))
 1030 FORMAT('JUPITER',4(1X,D13.6))
 1040 FORMAT('EARTH  ',4(1X,D13.6))
 1050 FORMAT('MOON   ',4(1X,D13.6))
 1060 FORMAT('COUNT   ',I6)
 1070 FORMAT('BURN    ',D13.6)
C     ------------------------------------------------------------------
      END
C ==============================================================================
C **                        P R O P R I E T A R Y                             **
C =============================================================== END FILE =====
