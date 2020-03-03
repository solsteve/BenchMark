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
require util::RK4;
require util::PSGraph;
require TwoBody;

exit(main());

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
sub main {
#/ ------------------------------------------------------------------------------------------

  my @param = (0,0,0,0);
  my @state = (0,0,0,0,0,0,0,0);
  my $t     = 0.0;
  my $dt    = ($TwoBody::MAXT * 86400.0) / $TwoBody::ITER;

  $param[0] = $TwoBody::Me;
  $param[1] = $TwoBody::Re;
  $param[2] = $TwoBody::Mm;
  $param[3] = $TwoBody::Rm;

  $state[0] = $TwoBody::VXe;       $state[4] = $TwoBody::Xe;
  $state[1] = $TwoBody::VYe;       $state[5] = $TwoBody::Ye;
  $state[2] = $TwoBody::VXm;       $state[6] = $TwoBody::Xm;
  $state[3] = $TwoBody::VYm;       $state[7] = $TwoBody::Ym;

  my $x1 = $state[4];
  my $y1 = $state[5];
  my $x2 = $state[6];
  my $y2 = $state[7];

#/ ------------------------------------------------------------------------------------------

  my $psg  = PSGraph->new();

  $psg->setWorldCo( -$TwoBody::maxr, -$TwoBody::maxr, $TwoBody::maxr, $TwoBody::maxr );
  $psg->initGraphics();

  my $mm = TwoBody->new();

  for (my $i=0; $i<$TwoBody::ITER; $i++) {

    $t = $mm->integrate( \@state, $t, $t+$dt, $TwoBody::ISTEP, \@param );

    $psg->drawLine( $x1, $y1, $state[4], $state[5] );
    $psg->drawLine( $x2, $y2, $state[6], $state[7] );

    $x1 = $state[4];
    $y1 = $state[5];
    $x2 = $state[6];
    $y2 = $state[7];

    if ($mm->{done} == 1) { goto OVER; }
  }

OVER:

    $psg->drawCircle( $state[4], $state[5], $param[1] );
    $psg->drawCircle( $state[6], $state[7], $param[3] );

    $psg->printGraphics();

    return 0;
}

#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
