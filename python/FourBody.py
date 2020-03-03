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

#/ ==========================================================================================
#/ **                                 P R O P R I E T A R Y                                **
#/ =========================================================================== END FILE =====
