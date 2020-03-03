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
#/ **  Date   2007-05-05                                                                   **
#/ **                                                                                      **
#/ **  $Id$                                                                                **
#/ **  $Log$                                                                               **
#/ **                                                                                      **
#/ ==========================================================================================

import sys
import PSGraph
import RK4
import Math, math
import TwoBody

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
class Motion(RK4.RK4):
	#/ ----------------------------------------------------------------------------------
	done=0
	
        #/ ==================================================================================
        #/ ----------------------------------------------------------------------------------
	def __init__(self):
		#/ --------------------------------------------------------------------------
		super(Motion,self).__init__(8)
		self.done=0

	#/ ==================================================================================
	#/ ----------------------------------------------------------------------------------
	def CHECK(self,Q,t,P):
		#/ --------------------------------------------------------------------------
		dx = Q[6] - Q[4]
		dy = Q[7] - Q[5]
		r2 = P[1] + P[3]

		rv = 1
		if (r2*r2) < ((dx*dx)+(dy*dy)): rv = 0

		return rv

	#/ ==================================================================================
	#/ ----------------------------------------------------------------------------------
	def DIFEQ(self,Qd,Q,t,P):
		#/ --------------------------------------------------------------------------
		if 0==self.done:
			xd1 = Q[0]
			x1  = Q[4]
			yd1 = Q[1]
			y1  = Q[5]
			xd2 = Q[2]
			x2  = Q[6]
			yd2 = Q[3]
			y2  = Q[7]

			m1  = P[0]
			m2  = P[2]

			dx21 = x2 - x1
			dy21 = y2 - y1
			dx12 = x1 - x2
			dy12 = y1 - y2

			r12sq = (dx12*dx12) + (dy12*dy12)
			den   = Math.POWER(r12sq,1.5)

			if den > 0.0:
				pass
			else:
				print "INFINITY\n"

			a1 = Math.N_G*m2*dx21
			a2 = Math.N_G*m2*dy21
			a3 = Math.N_G*m1*dx12
			a4 = Math.N_G*m1*dy12

			Qd[0] = Math.SAFEDIV(a1,den)
			Qd[1] = Math.SAFEDIV(a2,den)
			Qd[2] = Math.SAFEDIV(a3,den)
			Qd[3] = Math.SAFEDIV(a4,den)
			Qd[4] = xd1
			Qd[5] = yd1
			Qd[6] = xd2
			Qd[7] = yd2

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

	param = [0.0 for i in range(0,4)]
	state = [0.0 for i in range(0,8)]
   
	t  = 0.0
	dt = (TwoBody.MAXT * 86400.0) / TwoBody.ITER

	param[0] = TwoBody.Me
	param[1] = TwoBody.Re
	param[2] = TwoBody.Mm
	param[3] = TwoBody.Rm

	state[0] = TwoBody.VXe
	state[1] = TwoBody.VYe
	state[2] = TwoBody.VXm
	state[3] = TwoBody.VYm
	state[4] = TwoBody.Xe
	state[5] = TwoBody.Ye
	state[6] = TwoBody.Xm
	state[7] = TwoBody.Ym

	x1 = state[4]
	y1 = state[5]
	x2 = state[6]
	y2 = state[7]

	PSG = PSGraph.PSGraph( sys.argv[1] )

	PSG.setWorldCo( -TwoBody.MAXR, -TwoBody.MAXR, TwoBody.MAXR, TwoBody.MAXR )
	PSG.initGraphics()

	MM = Motion()

	for i in range(0,TwoBody.ITER):
		t = MM.integrate( state, t, t+dt, TwoBody.ISTEP, param )

		PSG.drawLine(x1, y1, state[4], state[5])
		PSG.drawLine(x2, y2, state[6], state[7])
 
		x1 = state[4]
		y1 = state[5]
		x2 = state[6]
		y2 = state[7]

		if 1==MM.done:
			break

	PSG.drawCircle(state[4], state[5], param[1])
	PSG.drawCircle(state[6], state[7], param[3])
  
	PSG.done()

#/ ==========================================================================================
if __name__ == "__main__":
    main()

#/ ==========================================================================================
#/ **                                 P R O P R I E T A R Y                                **
#/ =========================================================================== END FILE =====
