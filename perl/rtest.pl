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

#/ =========================================================================== END FILE =====
