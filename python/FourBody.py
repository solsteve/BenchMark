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

MAXT       = 1200.0                 # Time in days of plot
DELTATIME  = 21600.0                # Delta Time (seconds between plots
ISTEP      = 360                    # Number of Steps per Plot

#/ ------------------------------------------------------------------------------------------

M1  =  1.988435e+30                 # Mass of the SUN
M2  =  1.899e+27                    # Mass of the JUPITER
M3  =  5.9742e+24                   # Mass of the EARTH
M4  =  7.347673e+22                 # Mass of the MOON

R1  =  695000000.0                  # Average Radius of the SUN
R2  =  69924754.6                   # Average Radius of the JUPITER
R3  =  6370996.16                   # Average Radius of the EARTH
R4  =  1737146.5                    # Average Radius of the MOON

#/ ------------------------------------------------------------------------------------------

X1  =  -7.0675082353e+8             # Initial Y Pos of the SUN
Y1  =  0.0                          # Initial Y Pos of the SUN
X2  =  7.40035847176e+11            # Initial X Pos of the JUPITER
Y2  =  0.0                          # Initial Y Pos of the JUPITER
X3  =  1.47102485561e+11            # Initial X Pos of the EARTH
Y3  =  0.0                          # Initial Y Pos of the EARTH
X4  =  1.46739381561e+11            # Initial X Pos of the MOON
Y4  =  0.0                          # Initial Y Pos of the MOON

#/ ------------------------------------------------------------------------------------------

VX1 =  0.0                          # Init X Velocity of the SUN
VY1 =  -11.861                      # Init Y Velocity of the SUN
VX2 =  0.0                          # Init X Velocity of the JUPITER
VY2 =  13712.0                      # Init Y Velocity of the JUPITER
VX3 =  0.0                          # Init X Velocity of the EARTH
VY3 =  30287.0                      # Init Y Velocity of the EARTH
VX4 =  0.0                          # Init X Velocity of the MOON
VY4 =  29205.0                      # Init Y Velocity of the MOON

#/ =========================================================================== END FILE =====
