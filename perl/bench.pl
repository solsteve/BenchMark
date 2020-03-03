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

#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
