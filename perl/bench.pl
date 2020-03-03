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
require FourBody;

exit(main());

#/ ==========================================================================================
#/ ------------------------------------------------------------------------------------------
sub main {
#/ ------------------------------------------------------------------------------------------

  my @param = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  my @state = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  my $t     = 0.0;
  my $maxTime = ($FourBody::MAXT * 86400.0);

  $param[$FourBody::jG]  = 6.6742e-11;
  $param[$FourBody::jM1] = $FourBody::M1;   $param[$FourBody::jR1] = $FourBody::R1;
  $param[$FourBody::jM2] = $FourBody::M2;   $param[$FourBody::jR2] = $FourBody::R2;
  $param[$FourBody::jM3] = $FourBody::M3;   $param[$FourBody::jR3] = $FourBody::R3;
  $param[$FourBody::jM4] = $FourBody::M4;   $param[$FourBody::jR4] = $FourBody::R4;

  $state[$FourBody::iVX1] = $FourBody::VX1;  $state[$FourBody::iVY1] = $FourBody::VY1;    $state[$FourBody::iX1] = $FourBody::X1;  $state[$FourBody::iY1] = $FourBody::Y1;
  $state[$FourBody::iVX2] = $FourBody::VX2;  $state[$FourBody::iVY2] = $FourBody::VY2;    $state[$FourBody::iX2] = $FourBody::X2;  $state[$FourBody::iY2] = $FourBody::Y2;
  $state[$FourBody::iVX3] = $FourBody::VX3;  $state[$FourBody::iVY3] = $FourBody::VY3;    $state[$FourBody::iX3] = $FourBody::X3;  $state[$FourBody::iY3] = $FourBody::Y3;
  $state[$FourBody::iVX4] = $FourBody::VX4;  $state[$FourBody::iVY4] = $FourBody::VY4;    $state[$FourBody::iX4] = $FourBody::X4;  $state[$FourBody::iY4] = $FourBody::Y4;

  my $x1 = $FourBody::X1;  my $y1 = $FourBody::Y1;
  my $x2 = $FourBody::X2;  my $y2 = $FourBody::Y2;
  my $x3 = $FourBody::X3;  my $y3 = $FourBody::Y3;
  my $x4 = $FourBody::X4;  my $y4 = $FourBody::Y4;
  
  my $psg  = PSGraph->new();

  my $Xc  = 3.0e+11;
  my $Yc  = 3.5e+11;
  my $win = 6.0e+11;
 
  $psg->setWorldCo( $Xc-$win, $Yc-$win, $Xc+$win, $Yc+$win );
  $psg->initGraphics();


  my $mm = FourBody->new();


  while($t < $maxTime) {

    $t = $mm->integrate( \@state, $t, $t+$FourBody::DELTATIME, $FourBody::ISTEP, \@param );

    $psg->drawLine( $x1, $y1, $state[$FourBody::iX1], $state[$FourBody::iY1] );
    $psg->drawLine( $x2, $y2, $state[$FourBody::iX2], $state[$FourBody::iY2] );
    $psg->drawLine( $x3, $y3, $state[$FourBody::iX3], $state[$FourBody::iY3] );
    $psg->drawLine( $x4, $y4, $state[$FourBody::iX4], $state[$FourBody::iY4] );

    $x1 = $state[$FourBody::iX1]; $y1 = $state[$FourBody::iY1];
    $x2 = $state[$FourBody::iX2]; $y2 = $state[$FourBody::iY2];
    $x3 = $state[$FourBody::iX3]; $y3 = $state[$FourBody::iY3];
    $x4 = $state[$FourBody::iX4]; $y4 = $state[$FourBody::iY4];

    print STDERR $t . "/" . $maxTime . "                  \r";

    if ($mm->{done} == 1) { goto OVER; }
  }
  
OVER:

  $psg->drawCircle( $state[$FourBody::iX1],  $state[$FourBody::iY1],  $param[$FourBody::jR1] );
  $psg->drawCircle( $state[$FourBody::iX2],  $state[$FourBody::iY2],  $param[$FourBody::jR2] );
  $psg->drawCircle( $state[$FourBody::iX3],  $state[$FourBody::iY3],  $param[$FourBody::jR3] );
  $psg->drawCircle( $state[$FourBody::iX4],  $state[$FourBody::iY4],  $param[$FourBody::jR4] );

  $psg->printGraphics();

  my $kk = sqrt(($state[$FourBody::iVX4]*$state[$FourBody::iVX4])+($state[$FourBody::iVY4]*$state[$FourBody::iVY4]));

  print STDERR "Time    " . $t . "\n";
  print STDERR "Sun     " . $state[$FourBody::iX1] . "\t" . $state[$FourBody::iY1] . "\t" . $state[$FourBody::iVX1] . "\t" . $state[$FourBody::iVY1] . "\n";
  print STDERR "Jupiter " . $state[$FourBody::iX2] . "\t" . $state[$FourBody::iY2] . "\t" . $state[$FourBody::iVX2] . "\t" . $state[$FourBody::iVY2] .  "\n";
  print STDERR "Earth   " . $state[$FourBody::iX3] . "\t" . $state[$FourBody::iY3] . "\t" . $state[$FourBody::iVX3] . "\t" . $state[$FourBody::iVY3] .  "\n";
  print STDERR "Moon    " . $state[$FourBody::iX4] . "\t" . $state[$FourBody::iY4] . "\t" . $state[$FourBody::iVX4] . "\t" . $state[$FourBody::iVY4] . "\t" . $kk .  "\n";
  print STDERR "Count   " . $FourBody::iCount . "\n";
  print STDERR "Burn    " . $FourBody::tCount . " seconds" . "\n";

  return 0;
}

#/ =========================================================================== END FILE =====
