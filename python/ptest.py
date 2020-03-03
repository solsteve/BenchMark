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

#/ ==========================================================================================
#/ **                                 P R O P R I E T A R Y                                **
#/ =========================================================================== END FILE =====
