#!/usr/bin/python
#/ ===== BEGIN FILE =========================================================================
#/ **                                 P R O P R I E T A R Y                                **
#/ ==========================================================================================
#/ **                                                                                      **
#/ **  Copyright (c) 2007, Stephen W. Soliday                                              **
#/ **                      stephen@soliday.com                                             **
#/ **                      http://www.soliday.com/stephen                                  **
#/ **                                                                                      **
#/ **  This file, and the associated algorithms, are not free software; you may not        **
#/ **  redistribute them and/or modify them. These algorithms were developed and           **
#/ **  implemented for the purpose of an internal assessment and have, as yet, not been    **
#/ **  publicly distributed. Development of these algorithms have been at the sole cost    **
#/ **  in both time and funding by their author. Until such a public release is made,      **
#/ **  the author retains ALL RIGHTS to these algorithms. It is expected that if this      **
#/ **  program or any of the algorithms contained herein are deemed releasable they will   **
#/ **  be released under the GNU Public license for non-commercial use and/or with         **
#/ **  restricted rights for government use. At that time each source file will contain    **
#/ **  either/both the standard GPL statement/disclaimer, and/or                           **
#/ **  the DFARS Restricted Rights Legend.                                                 **
#/ **                                                                                      **
#/ **  These algorithms exists at the present time WITHOUT ANY WARRANTY; without even      **
#/ **  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.        **
#/ **  As you are not supposed to be in possession of this file if you use it,             **
#/ **  you do so AT YOUR OWN RISK.                                                         **
#/ **                                                                                      **
#/ **  ----- Modification History -------------------------------------------------------  **
#/ **                                                                                      **
#/ **  Author Stephen W. Soliday                                                           **
#/ **  Date   2007-04-30                                                                   **
#/ **                                                                                      **
#/ **  $Id$                                                                                **
#/ **  $Log$                                                                               **
#/ **                                                                                      **
#/ ==========================================================================================

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
class PSGraph(object):
	#/ ----------------------------------------------------------------------------------
	minX = 0.0
	minY = 0.0
	difX = 1.0
	difY = 1.0
	fspc = "psgraph.ps"
	outf = 0

	#/ ==================================================================================
	#/ Constructor.
	#/ Initialize the graphics state data structure. Allocate dynamic memory for
	#/ the structure.
	#/ path file path for postscript output.
	#/ ----------------------------------------------------------------------------------
	def __init__(self,fileName):
		#/ --------------------------------------------------------------------------
		self.fspc = fileName
		self.outf = open(fileName,'w')
		
	#/ ==================================================================================
	#/ Sets the world coordinates of the structure \a G.
	#/ Coordinates are set in device and world coordinates. This function is used
	#/ to define the world coordinate window.
	#/ x0 minimum X coordinate
	#/ y0 minimum Y coordinate
	#/ x1 maximum X coordinate
	#/ y1 maximum Y coordinate
	#/ return success=0, failure=1
	#/ ----------------------------------------------------------------------------------
	def setWorldCo(self,x0,y0,x1,y1):
		#/ --------------------------------------------------------------------------
		self.minX = x0;
		self.minY = y0;
		self.difX = (x1-x0);
		self.difY = (y1-y0);

	#/ ==================================================================================
	#/ Start Graphics.
	#/ Begin the capture of graphics commands.
	#/ return success=0, failure=non-zero
	#/ ----------------------------------------------------------------------------------
	def initGraphics(self):
		#/ --------------------------------------------------------------------------
		self.write_ps_head()
	
	#/ ==================================================================================
	#/ Draw Line.
	#/ Draw a line between two points.
	#/ x0 starting X coordinate.
	#/ y0 starting Y coordinate.
	#/ x1 ending X coordinate.
	#/ y1 ending Y coordinate.
	#/ ----------------------------------------------------------------------------------
	def drawLine(self,x0,y0,x1,y1):
		#/ --------------------------------------------------------------------------
		s = str(x0)+' '+str(y0)+' '+str(x1)+' '+str(y1)+' dl\n'
		self.outf.write(s)

	#/ ==================================================================================
	#/ Draw Circle.
	#/ Draw a circle with an arbitrary radius centered on a point.
	#/ xc center X coordinate.
	#/ yc center Y coordinate.
	#/ r radius of the circle.
	#/ ----------------------------------------------------------------------------------
	def drawCircle(self,xc,yc,r):
		#/ --------------------------------------------------------------------------
		s = str(xc)+' '+str(yc)+' '+str(r)+' dc\n'
		self.outf.write(s)

	#/ ==================================================================================
	#/ PostScript header.
	#/ Write a standard version 1 PostScript header. Scale and translation are based on
	#/ the world coordinates provided in the initialization of the graphics state data
	#/ structure.
	#/ return success=0, failure=non-zero
	#/ ----------------------------------------------------------------------------------
	def write_ps_head(self):
		#/ --------------------------------------------------------------------------
		BOX="540"

		self.outf.write("%!PS-Adobe-3.0\n")
		self.outf.write("%%Title: "+self.fspc+"\n")
		self.outf.write("%%Creator: PSGraph\n")
		self.outf.write("%%Orientation: Landscape\n")
		self.outf.write("%%%%Pages: 1\n")
		self.outf.write("%%BoundingBox: 0 0 612 792\n")
		self.outf.write("%%DocumentPaperSizes: Letter\n")
		self.outf.write("%%BeginSetup\n")
		self.outf.write("[{\n")
		self.outf.write("%%BeginFeature: *PageRegion Letter\n")
		self.outf.write("<</PageSize [612 792]>> setpagedevice\n")
		self.outf.write("%%EndFeature\n")
		self.outf.write("} stopped cleartomark\n")
		self.outf.write("%%EndSetup\n")
		self.outf.write("%%Magnification: 1.0000\n")
		self.outf.write("%%EndComments\n\n")

		self.outf.write("%%BeginProlog\n")
		self.outf.write("/dl { newpath moveto lineto stroke } bind def\n")
		self.outf.write("/dr { newpath rectstroke } bind def\n")
		self.outf.write("/dc { newpath 0 360 arc closepath stroke } bind def\n")
		self.outf.write("%EndProlog\n\n")

		self.outf.write("%%Page: 1 1\n\n")

		self.outf.write("gsave\n\n")

		self.outf.write("0 setlinewidth\n\n")

		self.outf.write("576 162 translate 90 rotate\n\n")

		self.outf.write("0 0 "+BOX+" "+BOX+" dr\n\n")

		self.outf.write(BOX+" "+str(self.difX)+" div "+BOX+" "+str(self.difY)+
			" div scale "+str(-self.minX)+" "+str(-self.minY)+" translate\n")

	#/ ==================================================================================
	#/ PostScript Trailer.
	#/ Write a standard version 1 PostScript trailer.
	#/ return success=0, failure=non-zero
	#/ ----------------------------------------------------------------------------------
	def write_ps_tail(self):
		#/ --------------------------------------------------------------------------
		self.outf.write("grestore\n")
		self.outf.write("showpage\n")
		self.outf.write("\n")
		self.outf.write("%%Trailer\n")
		self.outf.write("%%EOF\n")

	#/ ==================================================================================
	#/ Destructor.
	#/ Long description.
	#/ ----------------------------------------------------------------------------------
	def done(self):
		#/ --------------------------------------------------------------------------
		self.write_ps_tail()
		self.outf.close()

#/ ==========================================================================================
#/ **                                 P R O P R I E T A R Y                                **
#/ =========================================================================== END FILE =====
