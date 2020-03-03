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

C ==============================================================================
C **                        P R O P R I E T A R Y                             **
C =============================================================== END FILE =====
