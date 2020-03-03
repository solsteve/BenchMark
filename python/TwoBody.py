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

MAXR  =  4.6e+8          # Newton's Gravitational Constant
ITER  =  10000           # Number of Plot Iterations
ISTEP =  60              # Number of Integration Steps per Plot

MAXT  =  29.0            # Time in days of plot

#/ ------------------------------------------------------------------------------------------

Me    =  5.9742e+24      # Mass of the Earth
Mm    =  7.347673e+22    # Mass of the Moon

Re    =  6370996.0       # Average Radius of the Earth
Rm    =  1737146.0       # Average Radius of the Moon

#/ ------------------------------------------------------------------------------------------

Xe    = -4.41156e+6      # Initial X Pos of the Earth
Ye    =  0.0             # Initial Y Pos of the Earth
Xm    =  3.58692e+8      # Initial X Pos of the Moon
Ym    =  0.0             # Initial Y Pos of the Moon

#/ ------------------------------------------------------------------------------------------

VXe   =  0.0             # Initial X Velocity of the Earth
VYe   = -11.7422         # Initial Y Velocity of the Earth
VXm   =  0.0             # Initial X Velocity of the Moon
VYm   =  1082.0          # Initial Y Velocity of the Moon

#/ =========================================================================== END FILE =====
