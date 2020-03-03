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
import FourBody
import fpformat

iVX1 =  0
iVY1 =  1
iX1  =  2
iY1  =  3
iVX2 =  4
iVY2 =  5
iX2  =  6
iY2  =  7
iVX3 =  8
iVY3 =  9
iX3  = 10
iY3  = 11
iVX4 = 12
iVY4 = 13
iX4  = 14
iY4  = 15

jG   = 0
jM1  = 1
jR1  = 2
jM2  = 3
jR2  = 4
jM3  = 5
jR3  = 6
jM4  = 7
jR4  = 8

NN   = 4

AccInc = FourBody.DELTATIME / float(FourBody.ISTEP)
Acc    = AccInc*9.81*7.0

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
class Motion(RK4.RK4):
	#/ ----------------------------------------------------------------------------------
	done   = 0
	iCount = 0
	tCount = 0.0


	dx  = [[0.0 for i in range(0,NN)] for j in range(0,NN)]
	dy  = [[0.0 for i in range(0,NN)] for j in range(0,NN)]
	DEN = [[0.0 for i in range(0,NN)] for j in range(0,NN)]

	xdd = [0.0 for i in range(0,NN)]
	ydd = [0.0 for i in range(0,NN)]
	x   = [0.0 for i in range(0,NN)]
	y   = [0.0 for i in range(0,NN)]
	m   = [0.0 for i in range(0,NN)]

        #/ ==================================================================================
        #/ ----------------------------------------------------------------------------------
	def __init__(self):
		#/ --------------------------------------------------------------------------
		super(Motion,self).__init__(4*NN)
		self.done=0

		print NN,AccInc,Acc

	#/ ==================================================================================
	#/ Check State.
	#/ Determine if any two bodies are closer than the sum of thier radii.
	#/ At a predetermined time, add velocity to the smallest body.
	#/ Q current state vector.
	#/ t current time.
	#/ P parameter vector.
	#/ return success=0, failure=non-zero.
	#/ ----------------------------------------------------------------------------------
	def CHECK(self,Q,t,P):
		#/ --------------------------------------------------------------------------
		x1 = Q[iX1]
		y1 = Q[iY1]
		x2 = Q[iX2]
		y2 = Q[iY2]
		x3 = Q[iX3]
		y3 = Q[iY3]
		x4 = Q[iX4]
		y4 = Q[iY4]
  
		rad1 = P[jR1]
		rad2 = P[jR2]
		rad3 = P[jR3]
		rad4 = P[jR4]
 
		r12 = rad1 + rad2
		r13 = rad1 + rad3
		r14 = rad1 + rad4
		r23 = rad2 + rad3
		r24 = rad2 + rad4
		r34 = rad3 + rad4

		dx12 = x1 - x2
		dy12 = y1 - y2

		dx13 = x1 - x3
		dy13 = y1 - y3

		dx14 = x1 - x4
		dy14 = y1 - y4

		dx23 = x2 - x3
		dy23 = y2 - y3

		dx24 = x2 - x4
		dy24 = y2 - y4

		dx34 = x3 - x4
		dy34 = y3 - y4

		if (t > 21912800.0):
			v2 = (Q[iVX4]*Q[iVX4]) + (Q[iVY4]*Q[iVY4])
			if (v2 < 1.0e+9):
				cc       = math.sqrt(v2)
				Q[iVX4] += Acc*Q[iVX4]/cc
				Q[iVY4] += Acc*Q[iVY4]/cc
				self.iCount  += 1
				self.tCount  += AccInc
				print fpformat.sci(Q[iX4],5), fpformat.sci(Q[iY4],5), fpformat.sci(Q[iVX4],5), fpformat.sci(Q[iVY4],5), fpformat.sci(Acc,5), fpformat.sci(cc,5)

		rv = 0
		if   ((r12*r12) > ((dx12*dx12)+(dy12*dy12))): rv = 1
		elif ((r13*r13) > ((dx13*dx13)+(dy13*dy13))): rv = 2
		elif ((r14*r14) > ((dx14*dx14)+(dy14*dy14))): rv = 3
		elif ((r23*r23) > ((dx23*dx23)+(dy23*dy23))): rv = 4
		elif ((r24*r24) > ((dx24*dx24)+(dy24*dy24))): rv = 5
		elif ((r34*r34) > ((dx34*dx34)+(dy34*dy34))): rv = 6

		return rv

	#/ ==================================================================================
	#/ Integrate.
	#/ Integrate for time t0 to t1.
	#/ Using a fourth order Runge-Kutta numerical integrator.
	#/ The equations of mothion describe a generic N body gravitational problem.
	#/ Qd first time derivative of the current state vector.
	#/ Q current state vector.
	#/ t current time.
	#/ parameter vector.
	#/ return success=0, failure=non-zero
	#/ ----------------------------------------------------------------------------------
	def DIFEQ(self,Qd,Q,t,P):
		#/ --------------------------------------------------------------------------
		if (0==self.done):
			G    = P[jG]

			self.x[0] = Q[iX1]
			self.y[0] = Q[iY1]
			self.x[1] = Q[iX2]
			self.y[1] = Q[iY2]
			self.x[2] = Q[iX3]
			self.y[2] = Q[iY3]
			self.x[3] = Q[iX4]
			self.y[3] = Q[iY4]

			self.m[0] = P[jM1]
			self.m[1] = P[jM2]
			self.m[2] = P[jM3]
			self.m[3] = P[jM4]

			for k in range(1,NN):
				for i in range(0,k):
					self.dx[k][i] = self.x[k] - self.x[i]
					self.dy[k][i] = self.y[k] - self.y[i]
					self.dx[i][k] = -self.dx[k][i]
					self.dy[i][k] = -self.dy[k][i]

			for k in range(1,NN):
				for i in range(0,k):
					tt = (self.dx[k][i]*self.dx[k][i]) + (self.dy[k][i]*self.dy[k][i])
					self.DEN[k][i] = Math.POWER(tt,-1.5)
					self.DEN[i][k] = self.DEN[k][i]

			for i in range(0,NN):
				sumx = 0.0
				sumy = 0.0
				for k in range(0,NN):
					if k != i:
						tk = self.m[k]*self.DEN[k][i]
						sumx += (tk*self.dx[k][i])
						sumy += (tk*self.dy[k][i])
				self.xdd[i] = G*sumx
				self.ydd[i] = G*sumy


			Qd[iVX1] = self.xdd[0]
			Qd[iVY1] = self.ydd[0]
			Qd[iX1]  = Q[iVX1]
			Qd[iY1]  = Q[iVY1]
			Qd[iVX2] = self.xdd[1]
			Qd[iVY2] = self.ydd[1]
			Qd[iX2]  = Q[iVX2]
			Qd[iY2]  = Q[iVY2]
			Qd[iVX3] = self.xdd[2]
			Qd[iVY3] = self.ydd[2]
			Qd[iX3]  = Q[iVX3]
			Qd[iY3]  = Q[iVY3]
			Qd[iVX4] = self.xdd[3]
			Qd[iVY4] = self.ydd[3]
			Qd[iX4]  = Q[iVX4]
			Qd[iY4]  = Q[iVY4]

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
def usage(progName):
	#/ ----------------------------------------------------------------------------------
	print "Usage:",progName,"fileName"
	sys.exit(1)

#/ ==========================================================================================
#/ Entry Point.
#/ Standard C entry point.
#/ argc number of command line arguments.
#/ argv list of command line fields.
#/ return success=0, failure=non-zero
#/ ------------------------------------------------------------------------------------------
def main():
	#/ ----------------------------------------------------------------------------------
	argc = len(sys.argv)

	if (2 != argc):
		usage(sys.argv[0])

	param = [0.0 for i in range(0,16)]
	state = [0.0 for i in range(0,32)]

	t  = 0.0
	maxTime = FourBody.MAXT * 86400.0

	param[jG]   = Math.N_G
	param[jM1]  = FourBody.M1
	param[jR1]  = FourBody.R1
	param[jM2]  = FourBody.M2
	param[jR2]  = FourBody.R2
	param[jM3]  = FourBody.M3
	param[jR3]  = FourBody.R3
	param[jM4]  = FourBody.M4
	param[jR4]  = FourBody.R4

	state[iVX1] = FourBody.VX1
	state[iVY1] = FourBody.VY1
	state[iX1]  = FourBody.X1
	state[iY1]  = FourBody.Y1
	state[iVX2] = FourBody.VX2
	state[iVY2] = FourBody.VY2
	state[iX2]  = FourBody.X2
	state[iY2]  = FourBody.Y2
	state[iVX3] = FourBody.VX3
	state[iVY3] = FourBody.VY3
	state[iX3]  = FourBody.X3
	state[iY3]  = FourBody.Y3
	state[iVX4] = FourBody.VX4
	state[iVY4] = FourBody.VY4
	state[iX4]  = FourBody.X4
	state[iY4]  = FourBody.Y4

	x1 = FourBody.X1
	y1 = FourBody.Y1
	x2 = FourBody.X2
	y2 = FourBody.Y2
	x3 = FourBody.X3
	y3 = FourBody.Y3
	x4 = FourBody.X4
	y4 = FourBody.Y4

	PSG = PSGraph.PSGraph( sys.argv[1] )

	Xc  = 3.0e+11
	Yc  = 3.5e+11
	win = 6.0e+11
 
	PSG.setWorldCo( Xc-win, Yc-win, Xc+win, Yc+win )

	PSG.initGraphics()

	MM = Motion()

	while(t < maxTime):
		t = MM.integrate( state, t, t+FourBody.DELTATIME, FourBody.ISTEP, param )

		PSG.drawLine( x1, y1, state[iX1],  state[iY1])
		PSG.drawLine( x2, y2, state[iX2],  state[iY2])
		PSG.drawLine( x3, y3, state[iX3],  state[iY3])
		PSG.drawLine( x4, y4, state[iX4],  state[iY4])

		x1 = state[iX1]
		y1 = state[iY1]
		x2 = state[iX2]
		y2 = state[iY2]
		x3 = state[iX3]
		y3 = state[iY3]
		x4 = state[iX4]
		y4 = state[iY4]

		if 1==MM.done:
			break

	PSG.drawCircle( state[iX1],  state[iY1],  param[jR1] )
	PSG.drawCircle( state[iX2],  state[iY2],  param[jR2] )
	PSG.drawCircle( state[iX3],  state[iY3],  param[jR3] )
	PSG.drawCircle( state[iX4],  state[iY4],  param[jR4] )

	PSG.done()

	kk = math.sqrt((state[iVX4]*state[iVX4])+(state[iVY4]*state[iVY4]))

	print "Time    "+str(t)
	print "Sun     "+str(state[iX1])+"\t"+str(state[iY1])+"\t"+str(state[iVX1])+"\t"+str(state[iVY1])
	print "Jupiter "+str(state[iX2])+"\t"+str(state[iY2])+"\t"+str(state[iVX2])+"\t"+str(state[iVY2])+"\n"
	print "Earth   "+str(state[iX3])+"\t"+str(state[iY3])+"\t"+str(state[iVX3])+"\t"+str(state[iVY3])+"\n"
	print "Moon    "+str(state[iX4])+"\t"+str(state[iY4])+"\t"+str(state[iVX4])+"\t"+str(state[iVY4])+"\t"+str(kk)+"\n"
	print "Count   "+str(MM.iCount)
	print "Burn    "+str(MM.tCount)+" seconds"

	return 0

#/ ==========================================================================================
if __name__ == "__main__":
    main()

#/ =========================================================================== END FILE =====
