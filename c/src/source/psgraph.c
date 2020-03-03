/* ===== BEGIN FILE ========================================================================= */
/* **                              P R O P R I E T A R Y                                   ** */
/* ========================================================================================== */
/* **                                                                                      ** */
/* **  Copyright (c) 2006, Stephen W. Soliday                                              ** */
/* **                      stephen@soliday.com                                             ** */
/* **                      http://www.soliday.com/stephen                                  ** */
/* **                                                                                      ** */
/* **  This file, and the associated algorithms, are not free software; you may not        ** */
/* **  redistribute them and/or modify them. These algorithms were developed and           ** */
/* **  implemented for the purpose of an internal assessment and have, as yet, not been    ** */
/* **  publicly distributed. Development of these algorithms have been at the sole cost    ** */
/* **  in both time and funding by their author. Until such a public release is made,      ** */
/* **  the author retains ALL RIGHTS to these algorithms. It is expected that if this      ** */
/* **  program or any of the algorithms contained herein are deemed releasable they will   ** */
/* **  be released under the GNU Public license for non-commercial use and/or with         ** */
/* **  restricted rights for government use. At that time each source file will contain    ** */
/* **  either/both the standard GPL statement/disclaimer, and/or                           ** */
/* **  the DFARS Restricted Rights Legend.                                                 ** */
/* **                                                                                      ** */
/* **  These algorithms exists at the present time WITHOUT ANY WARRANTY; without even      ** */
/* **  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        ** */
/* **  As you are not supposed to be in possession of this file if you use it,             ** */
/* **  you do so AT YOUR OWN RISK.                                                         ** */
/* **                                                                                      ** */
/* **  ----- Modification History -------------------------------------------------------  ** */
/* **                                                                                      ** */
/* **  Author Stephen W. Soliday                                                           ** */
/* **  Date   2006-11-24                                                                   ** */
/* **                                                                                      ** */
/**   \file psgraph.c
 *    \brief Implementation.
 *     Provides the implementation for the wrapers for the PostScript graphing utility.
 */
/* ========================================================================================== */

#include <psgraph.h>
#include <string.h>

/** Number of pixels (72/inch) in the printable box. */
#define BOX ((double) 540) 

int write_ps_head( FILE *fp, char *fn, double x, double y, double dx, double dy );
int write_ps_tail( FILE *fp );

/* ========================================================================================== */
/** PostScript header.
 *  Write a standard version 1 PostScript header. Scale and translation are based on the
 *  world coordinates provided in the initialization of the graphics state data structure.
 * \param fp file handle for postscript output.
 * \param fn file name for postscript output.
 * \param x minimum X world-coordinate.
 * \param y minimum Y world-coordinate.
 * \param dx difference between maximum and minimum X world-coordinates.
 * \param dy difference between maximum and minimum Y world-coordinates.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
int write_ps_head( FILE *fp, char *fn,
		   double x, double y, double dx, double dy ) {
  /* ---------------------------------------------------------------------------------------- */
  fprintf(fp, "%%!PS-Adobe-3.0\n");
  fprintf(fp, "%%%%Title: %s\n", fn);
  fprintf(fp, "%%%%Creator: PSGraph\n");
  fprintf(fp, "%%%%Orientation: Landscape\n");
  fprintf(fp, "%%%%Pages: 1\n");
  fprintf(fp, "%%%%BoundingBox: 0 0 612 792\n");
  fprintf(fp, "%%%%DocumentPaperSizes: Letter\n");
  fprintf(fp, "%%%%BeginSetup\n");
  fprintf(fp, "[{\n");
  fprintf(fp, "%%BeginFeature: *PageRegion Letter\n");
  fprintf(fp, "<</PageSize [612 792]>> setpagedevice\n");
  fprintf(fp, "%%EndFeature\n");
  fprintf(fp, "} stopped cleartomark\n");
  fprintf(fp, "%%EndSetup\n");
  fprintf(fp, "%%%%Magnification: 1.0000\n");
  fprintf(fp, "%%%%EndComments\n\n");

  fprintf(fp, "%%%%BeginProlog\n");
  fprintf(fp, "/dl { newpath moveto lineto stroke } bind def\n");
  fprintf(fp, "/dr { newpath rectstroke } bind def\n");
  fprintf(fp, "/dc { newpath 0 360 arc closepath stroke } bind def\n");
  fprintf(fp, "%%EndProlog\n\n");

  fprintf(fp, "%%%%Page: 1 1\n\n");

  fprintf(fp, "gsave\n\n");

  fprintf(fp, "0 setlinewidth\n\n");

  fprintf(fp, "576 162 translate 90 rotate\n\n");

  fprintf(fp, "0 0 %g %g dr\n\n", BOX, BOX);

  fprintf( fp,
	   "%g %g div %g %g div scale %g %g translate\n\n",
	   BOX, dx, BOX, dy, -x, -y
	   );

  return 0;
}

/* ========================================================================================== */
/** PostScript Trailer.
 *  Write a standard version 1 PostScript trailer.
 * \param fp file handle for postscript output.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
int write_ps_tail( FILE *fp ) {
  /* ---------------------------------------------------------------------------------------- */
  fprintf( fp, "grestore\n\n");
  fprintf( fp, "showpage\n\n");

  fprintf( fp, "%%%%Trailer\n");
  fprintf( fp, "%%%%EOF\n\n");

  return 0;
}

/* ========================================================================================== */
/** Graphics initialzation.
 *  Initialize the graphics state data structure. Allocate dynamic memory for the structure.
 * \param fspc file path for postscript output.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
PSGraph *PSG_new( char *fspc ) {
  /* ---------------------------------------------------------------------------------------- */
  PSGraph *temp = (PSGraph*) malloc(sizeof(PSGraph));

  temp->minX = 0.0;
  temp->minY = 0.0;
  temp->difX = 0.0;
  temp->difY = 0.0;

  temp->fp   = fopen(fspc, "w");;
  temp->fspc = (char *) malloc(MAX_PATH);

  strncpy(temp->fspc, fspc, MAX_PATH-1 );

  return temp;
}

/* ========================================================================================== */
/** Brief description.
 *  Long description.
 * \param PS pointer to the graphics state data structure.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
int PSG_del( PSGraph *PS ) {
  /* ---------------------------------------------------------------------------------------- */
  write_ps_tail( PS->fp );
  fclose(PS->fp);
  free(PS->fspc);
  free(PS);
  return 0;
}

/* ========================================================================================== */
/** Sets the world coordinates of the structure \a G.
 *  Coordinates are set in device and world coordinates. This function is used
 *  to define the world coordinate window.
 * \param PS pointer to the graphics state data structure.
 *  \param x0 minimum X coordinate
 *  \param y0 minimum Y coordinate
 *  \param x1 maximum X coordinate
 *  \param y1 maximum Y coordinate
 *  \return success=0, failure=1. 
 */
/* ------------------------------------------------------------------------------------------ */
int PSG_setWorldCo( PSGraph *PS, double x0, double y0,  double x1, double y1 ) {
  /* ---------------------------------------------------------------------------------------- */
  PS->minX = x0;
  PS->minY = y0;
  PS->difX = (x1-x0);
  PS->difY = (y1-y0);
  return 0;
}

/* ========================================================================================== */
/** Start Graphics.
 *  Begin the capture of graphics commands.
 * \param PS pointer to the graphics state data structure.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
int PSG_initGraphics( PSGraph *PS ) {
  /* ---------------------------------------------------------------------------------------- */
  return write_ps_head( PS->fp, PS->fspc, PS->minX, PS->minY, PS->difX, PS->difY );
}

/* ========================================================================================== */
/** Draw Line.
 *  Draw a line between two points.
 * \param PS pointer to the graphics state data structure.
 * \param x0 starting X coordinate.
 * \param y0 starting Y coordinate.
 * \param x1 ending X coordinate.
 * \param y1 ending Y coordinate.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
int PSG_drawLine( PSGraph *PS, double x0, double y0,  double x1, double y1 ) {
  /* ---------------------------------------------------------------------------------------- */
  fprintf( PS->fp, "%g %g %g %g dl\n", x0, y0, x1, y1 );
  return 0;
}

/* ========================================================================================== */
/** Draw Circle.
 *  Draw a circle with an arbitrary radius centered on a point.
 * \param PS pointer to the graphics state data structure.
 * \param xc center X coordinate.
 * \param yc center Y coordinate.
 * \param r radius of the circle.
 * \return success=0, failure=non-zero
 */
/* ------------------------------------------------------------------------------------------ */
int PSG_drawCircle( PSGraph *PS, double xc, double yc, double r ) {
  /* ---------------------------------------------------------------------------------------- */
  fprintf( PS->fp, "%g %g %g dc\n", xc, yc, r );
  return 0;
}

/* ================================================================================ */
int PSG_setRGB( PSGraph *G, double r, double g, double b ) {
  /* ------------------------------------------------------------------------------ */
  fprintf(G->fp,"%g %g %g setrgbcolor\n", r, g, b);
  return 0;
}

/* ========================================================================================== */
/* **                              P R O P R I E T A R Y                                   ** */
/* =========================================================================== END FILE ===== */
