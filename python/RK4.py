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

#/ ==========================================================================================
#/ **                                 P R O P R I E T A R Y                                **
#/ =========================================================================== END FILE =====
