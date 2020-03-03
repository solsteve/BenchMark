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
C **  PROVIDES THE IMPLEMENTATION FOR THE WRAPPERS FOR THE POSTSCRIPT         **
C **  GRAPHING UTILITY                                                        **
C **                                                                          **
C ==============================================================================

C----+==================================================================--------
      BLOCK DATA PSGRAPH
C----+==================================================================--------
      DOUBLE PRECISION MINX,MINY,DIFX,DIFY
      INTEGER BOX,FU
      COMMON /PSGPARS/MINX,MINY,DIFX,DIFY,BOX,FU
C     ------------------------------------------------------------------
      DATA MINX/-1.0D0/
      DATA MINY/-1.0D0/
      DATA DIFX/2.0D0/
      DATA DIFY/2.0D0/
      DATA BOX/540/
      DATA FU/6/
C     ------------------------------------------------------------------
      END

C----+==================================================================--------
C     POSTSCRIPT HEADER.
C     WRITE A STANDARD VERSION 1 POSTSCRIPT HEADER. SCALE AND TRANSLATION ARE
C     BASED ON THE WORLD COORDINATES PROVIDED IN THE INITIALIZATION OF THE
C     GRAPHICS STATE DATA STRUCTURE.
C    \PARAM FP FILE HANDLE FOR POSTSCRIPT OUTPUT.
C    \PARAM FN FILE NAME FOR POSTSCRIPT OUTPUT.
C    \PARAM X MINIMUM X WORLD-COORDINATE.
C    \PARAM Y MINIMUM Y WORLD-COORDINATE.
C    \PARAM DX DIFFERENCE BETWEEN MAXIMUM AND MINIMUM X WORLD-COORDINATES.
C    \PARAM DY DIFFERENCE BETWEEN MAXIMUM AND MINIMUM Y WORLD-COORDINATES.
C----+==================================================================--------
      SUBROUTINE WRITEPSHEAD
C----+==================================================================--------
      DOUBLE PRECISION MINX,MINY,DIFX,DIFY
      INTEGER BOX,FU
      COMMON /PSGPARS/MINX,MINY,DIFX,DIFY,BOX,FU
C     ------------------------------------------------------------------
      WRITE(FU,*) '%!PS-Adobe-3.0'
      WRITE(FU,*) '%%Title: FPS.ps'
      WRITE(FU,*) '%%Creator: PSGraph'
      WRITE(FU,*) '%%Orientation: Landscape'
      WRITE(FU,*) '%%Pages: 1'
      WRITE(FU,*) '%%BoundingBox: 0 0 612 792'
      WRITE(FU,*) '%%DocumentPaperSizes: Letter'
      WRITE(FU,*) '%%BeginSetup'
      WRITE(FU,*) '[{'
      WRITE(FU,*) '%BeginFeature: *PageRegion Letter'
      WRITE(FU,*) '<</PageSize [612 792]>> setpagedevice'
      WRITE(FU,*) '%EndFeature'
      WRITE(FU,*) '} stopped cleartomark'
      WRITE(FU,*) '%EndSetup'
      WRITE(FU,*) '%%Magnification: 1.0000'
      WRITE(FU,*) '%%EndComments\n'
      WRITE(FU,*) ''
      WRITE(FU,*) '%%BeginProlog'
      WRITE(FU,*) '/np { newpath   } bind def'
      WRITE(FU,*) '/mt { moveto    } bind def'
      WRITE(FU,*) '/lt { lineto    } bind def'
      WRITE(FU,*) '/cp { closepath } bind def'
      WRITE(FU,*) '/st { stroke    } bind def'
      WRITE(FU,*) ' '
      WRITE(FU,*) '/dl { np mt lt st } bind def'
      WRITE(FU,*) '/dr { np rectstroke } bind def'
      WRITE(FU,*) '/dc { np 0 360 arc cp st } bind def'
      WRITE(FU,*) '%EndProlog'
      WRITE(FU,*) ' '
      WRITE(FU,*) '%%Page: 1 1'
      WRITE(FU,*) ' '
      WRITE(FU,*) 'gsave'
      WRITE(FU,*) ' '
      WRITE(FU,*) '0 setlinewidth'
      WRITE(FU,*) ' '
      WRITE(FU,*) '576 162 translate 90 rotate'
      WRITE(FU,*) ' '
      WRITE(FU,*) '0 0 ', BOX, BOX, 'dr'
      WRITE(FU,*) ' '
      WRITE(FU,*) BOX,DIFX,'div',BOX,DIFY,'div scale',
     1     -MINX,-MINY,'translate'    
C     ------------------------------------------------------------------
      RETURN
      END

C----+==================================================================--------
C     POSTSCRIPT TRAILER.
C     WRITE A STANDARD VERSION 1 POSTSCRIPT TRAILER.
C----+==================================================================--------
      SUBROUTINE WRITEPSTAIL
C----+==================================================================--------
      DOUBLE PRECISION MINX,MINY,DIFX,DIFY
      INTEGER BOX,FU
      COMMON /PSGPARS/MINX,MINY,DIFX,DIFY,BOX,FU
C     ------------------------------------------------------------------
      WRITE(FU,*) 'grestore'
      WRITE(FU,*) 'showpage'
      WRITE(FU,*) ' '
      WRITE(FU,*) '%%Trailer'
      WRITE(FU,*) '%%EOF'
C     ------------------------------------------------------------------
      RETURN
      END

C----+==================================================================--------
C     SETS THE WORLD COORDINATES OF THE STRUCTURE \A G.
C     COORDINATES ARE SET IN DEVICE AND WORLD COORDINATES. THIS FUNCTION IS USED
C     TO DEFINE THE WORLD COORDINATE WINDOW.
C    \PARAM X0 MINIMUM X COORDINATE
C    \PARAM Y0 MINIMUM Y COORDINATE
C    \PARAM X1 MAXIMUM X COORDINATE
C    \PARAM Y1 MAXIMUM Y COORDINATE
C----+==================================================================--------
      SUBROUTINE SETWORLDCO(X0,Y0,X1,Y1)
C----+==================================================================--------
      DOUBLE PRECISION X0,Y0,X1,Y1
C     ------------------------------------------------------------------
      DOUBLE PRECISION MINX,MINY,DIFX,DIFY
      INTEGER BOX,FU
      COMMON /PSGPARS/MINX,MINY,DIFX,DIFY,BOX,FU
C     ------------------------------------------------------------------
      MINX=X0
      MINY=Y0
      DIFX=X1-X0
      DIFY=Y1-Y0
C     ------------------------------------------------------------------
      RETURN
      END

C----+==================================================================--------
C     START GRAPHICS.
C     BEGIN THE CAPTURE OF GRAPHICS COMMANDS.
C----+==================================================================--------
      SUBROUTINE INITGRAPHICS
C----+==================================================================--------
      CALL WRITEPSHEAD
C     ------------------------------------------------------------------
      RETURN
      END

C----+==================================================================--------
C     BRIEF DESCRIPTION.
C     LONG DESCRIPTION.
C----+==================================================================--------
      SUBROUTINE PRINTGRAPHICS
C----+==================================================================--------
      DOUBLE PRECISION MINX,MINY,DIFX,DIFY
      INTEGER BOX,FU
      COMMON /PSGPARS/MINX,MINY,DIFX,DIFY,BOX,FU
C     ------------------------------------------------------------------
      CALL WRITEPSTAIL
      IF(6.EQ.FU) GOTO 20
      CLOSE(FU,ERR=10)
      GOTO 20
 10   CONTINUE
      WRITE(0,1000) FU
 20   CONTINUE
C     ------------------------------------------------------------------
      RETURN
C     ------------------------------------------------------------------
 1000 FORMAT(' ERROR CLOSEING FILE = ',I3)
C     ------------------------------------------------------------------
      END

C----+==================================================================--------
C     DRAW A LINE BETWEEN TWO POINTS.
C    \PARAM X0 STARTING X COORDINATE.
C    \PARAM Y0 STARTING Y COORDINATE.
C    \PARAM X1 ENDING X COORDINATE.
C    \PARAM Y1 ENDING Y COORDINATE.
C----+==================================================================--------
      SUBROUTINE DRAWLINE(X0,Y0,X1,Y1)
C----+==================================================================--------
      DOUBLE PRECISION X0,Y0,X1,Y1
C     ------------------------------------------------------------------
      DOUBLE PRECISION MINX,MINY,DIFX,DIFY
      INTEGER BOX,FU
      COMMON /PSGPARS/MINX,MINY,DIFX,DIFY,BOX,FU
C     ------------------------------------------------------------------
      WRITE(FU,*) X0,Y0,X1,Y1,'dl'
C     ------------------------------------------------------------------
      RETURN
      END

C----+==================================================================--------
C     DRAW A CIRCLE WITH AN ARBITRARY RADIUS CENTERED ON A POINT.
C    \PARAM XC CENTER X COORDINATE.
C    \PARAM YC CENTER Y COORDINATE.
C    \PARAM R RADIUS OF THE CIRCLE.
C----+==================================================================--------
      SUBROUTINE DRAWCIRCLE(XC,YC,R)
C----+==================================================================--------
      DOUBLE PRECISION XC,YC,R
C     ------------------------------------------------------------------
      DOUBLE PRECISION MINX,MINY,DIFX,DIFY
      INTEGER BOX,FU
      COMMON /PSGPARS/MINX,MINY,DIFX,DIFY,BOX,FU
C     ------------------------------------------------------------------
      WRITE(FU,*) XC,YC,R,'dc'
C     ------------------------------------------------------------------
      RETURN
      END

C =============================================================== END FILE =====
