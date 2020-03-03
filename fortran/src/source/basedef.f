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

C ==============================================================================
C **                        P R O P R I E T A R Y                             **
C =============================================================== END FILE =====
