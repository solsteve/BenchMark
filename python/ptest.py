#!/usr/bin/python
#/ ===== BEGIN FILE =========================================================================
#/ **                                                                                      **
#/ **  Copyright (c) 2007, Stephen W. Soliday                                              **
#/ **                      stephen@soliday.com                                             **
#/ **                      http://www.soliday.com/stephen                                  **
#/ **                                                                                      **
#/ **  This program is free software: you can redistribute it and/or modify it under    **
#/ **  the terms of the GNU General Public License as published by the Free Software    **
#/ **  Foundation, either version 3 of the License, or (at your option)                 **
#/ **  any later version.                                                               **
#/ **                                                                                   **
#/ **  This program is distributed in the hope that it will be useful, but WITHOUT      **
#/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
#/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
#/ **                                                                                   **
#/ **  You should have received a copy of the GNU General Public License along with     **
#/ **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
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

import sys
import PSGraph
import Math, math

SemiMajorAxis = 9000.0
SemiMinorAxis = 7500.0
STEP          = 7

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
def usage(progName):
	#/ ----------------------------------------------------------------------------------
	print "Usage:",progName,"fileName"
	sys.exit(1)

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
def main():
	#/ ----------------------------------------------------------------------------------
	argc = len(sys.argv)

	if (2 != argc):
		usage(sys.argv[0])

	PSG = PSGraph.PSGraph(sys.argv[1])

	maxr = SemiMajorAxis
	if (SemiMinorAxis > SemiMajorAxis):
		maxr = SemiMinorAxis

	maxr *= 1.05

	PSG.setWorldCo( -maxr, -maxr, maxr, maxr )
	PSG.initGraphics()

	t = 0.0
	dt = Math.N_2PI / STEP

	x0 = SemiMajorAxis * math.cos(t)
	y0 = SemiMinorAxis * math.sin(t)

	for i in range(0,STEP):
		t += dt
		x1 = SemiMajorAxis * math.cos(t)
		y1 = SemiMinorAxis * math.sin(t)

		PSG.drawLine( x0, y0, x1, y1 )

		x0 = x1
		y0 = y1

	PSG.drawCircle( 0.0, 0.0, maxr*0.8 )

	PSG.done()

#/ ==========================================================================================
if __name__ == "__main__":
    main()

#/ =========================================================================== END FILE =====
