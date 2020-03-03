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

#/ ==========================================================================================
#/ **                                 P R O P R I E T A R Y                                **
#/ =========================================================================== END FILE =====
