#!/usr/bin/perl
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

use strict;
use warnings;
use diagnostics -verbose;
enable diagnostics;

require util::Math2;
require util::PSGraph;

my $STEP          = 7;         # number of edges
my $SemiMajorAxis = 9000.0;    # Semi major axis of the test ellipse
my $SemiMinorAxis = 7500.0;    # Semi minor axis of the test ellipse

exit(main());

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
sub main {
#/ ------------------------------------------------------------------------------------------
    my $psg  = PSGraph->new();

    my $maxr = ((($SemiMinorAxis)>($SemiMajorAxis)) ? ($SemiMinorAxis) : ($SemiMajorAxis));

    $maxr *= 1.05;

    $psg->setWorldCo( -$maxr, -$maxr, $maxr, $maxr );
    $psg->initGraphics();


    my $t  = 0.0;
    my $dt = $Math2::N_2PI / $STEP;

    my $x0 = $SemiMajorAxis * cos($t);
    my $y0 = $SemiMinorAxis * sin($t);

    for (my $i=0; $i<$STEP; $i++) {
	$t += $dt;
	my $x1 = $SemiMajorAxis * cos($t);
	my $y1 = $SemiMinorAxis * sin($t);

	$psg->drawLine( $x0, $y0, $x1, $y1 );

	$x0 = $x1;
	$y0 = $y1;
    }

    $psg->drawCircle( 0.0, 0.0, $maxr*0.8 );

    $psg->printGraphics();

    return 0;
}

#/ =========================================================================== END FILE =====
