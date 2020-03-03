#/ ===== BEGIN FILE =========================================================================
#/ **                                                                                   **
#/ **  Copyright (c) 2012, Stephen W. Soliday                                           **
#/ **                      stephen.soliday@trncmp.org                                   **
#/ **                      http://research.trncmp.org                                   **
#/ **                                                                                   **
#/ **  -------------------------------------------------------------------------------  **
#/ **                                                                                   **
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
#/ **  Date   2006-12-16                                                                   **
#/ **                                                                                      **
#/ **  $Id$                                                                                **
#/ **  $Log$                                                                               **
#/ **                                                                                      **
#/ ==========================================================================================

package Math2;
use strict;
use warnings;
use diagnostics -verbose;
enable diagnostics;

$Math2::N_2PI = 6.28318530717959;     # 2 times Pi
$Math2::N_G   = 6.6742e-11;           # Newton's Gravitational Constant

#/ ==========================================================================================
sub POWER {
#/ ------------------------------------------------------------------------------------------
    my ($base, $expon) = @_;
    return exp(log($base)*($expon));
}

#/ ==========================================================================================
sub SAFEDIV {
#/ ------------------------------------------------------------------------------------------
    my ($n, $d) = @_;
    return ((($n)<0.0)?(($n)/($d)):((($n)>0.0)?(($n)/($d)):(0.0)));
}

1;

#/ =========================================================================== END FILE =====
