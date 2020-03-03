#!/usr/bin/perl
#/ ===== BEGIN FILE =========================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ ==========================================================================================
#/ **                                                                                      **
#/ **  Copyright (c) 2006, Stephen W. Soliday                                              **
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

#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
