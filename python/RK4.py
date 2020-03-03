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

#/ ==========================================================================================
#/ Provides the interface for the Runge-Kutta numerical integrator.
#/ The user must supply the implementation for DIFEQ and CHECK.
#/ ------------------------------------------------------------------------------------------
class RK4(object):
	#/ ----------------------------------------------------------------------------------
	dim = 0  # number of coupled first order equations
	A = []   # stage one state vector
	B = []   # stage two state vector
	C = []   # stage three state vector
	D = []   # stage four state vector
	W = []   # DIFEQ input vector

	#/ ==================================================================================
        #/ Constructor.
        #/ Allocate work vectors for integration.
        #/ n - number of coupled first-order differential equations.
	#/ ----------------------------------------------------------------------------------
	def __init__(self,n):
		#/ --------------------------------------------------------------------------
		self.dim = n
		self.A = [0 for i in range(0,n)]
		self.B = [0 for i in range(0,n)]
		self.C = [0 for i in range(0,n)]
		self.D = [0 for i in range(0,n)]
		self.W = [0 for i in range(0,n)]

	#/ ==================================================================================
	#/ ----------------------------------------------------------------------------------
	def CHECK(self,Q,t,P):
		#/ --------------------------------------------------------------------------
		return 0

	#/ ==================================================================================
	#/ ----------------------------------------------------------------------------------
	def DIFEQ(self,Qd,Q,t,P):
		#/ --------------------------------------------------------------------------
		pass

	#/ ==================================================================================
	#/ Fourth order Runge-Kutta.
	#/ Provides the implementation for a fourth order Runge-Kutta numerical integrator
	#/ with uniform step sizes.
	#/ Q    - real vector containing the state.
	#/ t0   - initial time.
	#/ t1   - final time.
	#/ step - number of steps between current time t0 and final time t1.
	#/ P    - vector containing fixed parameters.
	#/ return new time (t1).
	#/ ----------------------------------------------------------------------------------
	def integrate(self,Q,t0,t1,step,P ):
		#/ --------------------------------------------------------------------------
		h = (t1 - t0) / step
		t = t0
		h2 = h / 2.0

		for k in range(0,step):
			rv = self.CHECK(Q,t,P)
			if rv != 0:
				break
			#/ -------------------------------------
			for j in range(0,self.dim):
				self.W[j] = Q[j]
			self.DIFEQ(self.A,self.W,t,P)
			#/ -------------------------------------
			for j in range(0,self.dim):
				self.W[j] = Q[j] + (self.A[j] * h2)
			self.DIFEQ(self.B,self.W,t+h2,P)
			#/ -------------------------------------
			for j in range(0,self.dim):
				self.W[j] = Q[j] + (self.B[j] * h2)
			self.DIFEQ(self.C,self.W,t+h2,P)
			#/ -------------------------------------
			for j in range(0,self.dim):
				self.W[j] = Q[j] + (self.C[j] * h)
			self.DIFEQ(self.D,self.W,t+h,P)
			#/ -------------------------------------
			
			for j in range(0,self.dim):
				Q[j] += (h*(self.A[j]+2.0*(self.B[j]+self.C[j])+self.D[j])/6.0)
			t += h

		
		return t

#/ =========================================================================== END FILE =====
