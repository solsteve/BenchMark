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

package FourBody;
@ISA = ("RK4");

use strict;
use warnings;
use diagnostics -verbose;
enable diagnostics;

require util::Math2;

$FourBody::MAXT       =  1200.0;           # Time in days of plot
$FourBody::DELTATIME  = 21600.0;           # Delta Time (seconds) between plots
$FourBody::ISTEP      =   360;             # Number of Steps per Plot

$FourBody::M1         = 1.988435e+30;      # Mass of the SUN
$FourBody::M2         = 1.899e+27;         # Mass of the JUPITER
$FourBody::M3         = 5.9742e+24;        # Mass of the EARTH
$FourBody::M4         = 7.347673e+22;      # Mass of the MOON

$FourBody::R1         = 695000000.0;       # Average Radius of the SUN
$FourBody::R2         = 69924754.6;        # Average Radius of the JUPITER
$FourBody::R3         = 6370996.16;        # Average Radius of the EARTH
$FourBody::R4         = 1737146.5;         # Average Radius of the MOON

$FourBody::X1         = -7.0675082353e+8;  # Initial Y Pos of the SUN
$FourBody::Y1         = 0.0;               # Initial Y Pos of the SUN
$FourBody::X2         = 7.40035847176e+11; # Initial X Pos of the JUPITER
$FourBody::Y2         = 0.0;               # Initial Y Pos of the JUPITER
$FourBody::X3         = 1.47102485561e+11; # Initial X Pos of the EARTH
$FourBody::Y3         = 0.0;               # Initial Y Pos of the EARTH
$FourBody::X4         = 1.46739381561e+11; # Initial X Pos of the MOON
$FourBody::Y4         = 0.0;               # Initial Y Pos of the MOON

$FourBody::VX1        = 0.0;               # Init X Velocity of the SUN
$FourBody::VY1        = -11.861;           # Init Y Velocity of the SUN
$FourBody::VX2        = 0.0;               # Init X Velocity of the JUPITER
$FourBody::VY2        = 13712.0;           # Init Y Velocity of the JUPITER
$FourBody::VX3        = 0.0;               # Init X Velocity of the EARTH
$FourBody::VY3        = 30287.0;           # Init Y Velocity of the EARTH
$FourBody::VX4        = 0.0;               # Init X Velocity of the MOON
$FourBody::VY4        = 29205.0;           # Init Y Velocity of the MOON

 $FourBody::iVX1 =  0;
 $FourBody::iVY1 =  1;
  $FourBody::iX1 =  2;
  $FourBody::iY1 =  3;
 $FourBody::iVX2 =  4;
 $FourBody::iVY2 =  5;
  $FourBody::iX2 =  6;
  $FourBody::iY2 =  7;
 $FourBody::iVX3 =  8;
 $FourBody::iVY3 =  9;
  $FourBody::iX3 = 10;
  $FourBody::iY3 = 11;
 $FourBody::iVX4 = 12;
 $FourBody::iVY4 = 13;
  $FourBody::iX4 = 14;
  $FourBody::iY4 = 15;

   $FourBody::jG = 0;
  $FourBody::jM1 = 1;
  $FourBody::jR1 = 2;
  $FourBody::jM2 = 3;
  $FourBody::jR2 = 4;
  $FourBody::jM3 = 5;
  $FourBody::jR3 = 6;
  $FourBody::jM4 = 7;
  $FourBody::jR4 = 8;

  $FourBody::NN  = 4;

 $FourBody::iCount = 0;
 $FourBody::tCount = 0.0;
 $FourBody::Acc    = ($FourBody::DELTATIME / $FourBody::ISTEP) * 9.81 * 7.0;


#/ ==========================================================================================
sub new {
#/ ------------------------------------------------------------------------------------------
    my $pkg = shift;
    my $obj = $pkg->SUPER::new(4*$FourBody::NN);
    $obj; 
}

#/ ==========================================================================================
#/ Check State.
#/ Determine if any two bodies are closer than the sum of thier radii.
#/ At a predetermined time, add velocity to the smallest body.
#/ ------------------------------------------------------------------------------------------
sub CHECKEQ {
#/ ------------------------------------------------------------------------------------------
    my $self = shift; #
    my $Q    = shift; # current state vector
    my $t    = shift; # current time
    my $P    = shift; # parameter vector
    #/ --------------------------------------------------------------------------------------

    my $x1 = $$Q[$FourBody::iX1];
    my $x2 = $$Q[$FourBody::iX2];
    my $x3 = $$Q[$FourBody::iX3];
    my $x4 = $$Q[$FourBody::iX4];

    my $y1 = $$Q[$FourBody::iY1];
    my $y2 = $$Q[$FourBody::iY2];
    my $y3 = $$Q[$FourBody::iY3];
    my $y4 = $$Q[$FourBody::iY4];

    my $rad1 = $$P[$FourBody::jR1];
    my $rad2 = $$P[$FourBody::jR2];
    my $rad3 = $$P[$FourBody::jR3];
    my $rad4 = $$P[$FourBody::jR4];
 
    my $r12 = $rad1 + $rad2;
    my $r13 = $rad1 + $rad3;
    my $r14 = $rad1 + $rad4;
    my $r23 = $rad2 + $rad3;
    my $r24 = $rad2 + $rad4;
    my $r34 = $rad3 + $rad4;

    my $dx12 = $x1 - $x2;
    my $dy12 = $y1 - $y2;

    my $dx13 = $x1 - $x3;
    my $dy13 = $y1 - $y3;

    my $dx14 = $x1 - $x4;
    my $dy14 = $y1 - $y4;

    my $dx23 = $x2 - $x3;
    my $dy23 = $y2 - $y3;

    my $dx24 = $x2 - $x4;
    my $dy24 = $y2 - $y4;

    my $dx34 = $x3 - $x4;
    my $dy34 = $y3 - $y4;

    if ($t > 21912800.0) {
	my $v2 = ($$Q[$FourBody::iVX4]*$$Q[$FourBody::iVX4]) + ($$Q[$FourBody::iVY4]*$$Q[$FourBody::iVY4]);
	if ($v2 < 1.0e+9) {
	    
	    print STDERR $$Q[$FourBody::iX4] . " " . $$Q[$FourBody::iY4] . "\n";
	    
	    my $cc = sqrt($v2);
	    $$Q[$FourBody::iVX4] += $FourBody::Acc*$$Q[$FourBody::iVX4]/$cc;
	    $$Q[$FourBody::iVY4] += $FourBody::Acc*$$Q[$FourBody::iVY4]/$cc;
	    $FourBody::iCount++;
	    $FourBody::tCount += ($FourBody::DELTATIME / $FourBody::ISTEP);
	}
    }

    if (($r12*$r12) > (($dx12*$dx12)+($dy12*$dy12))) { return 1; }
    if (($r13*$r13) > (($dx13*$dx13)+($dy13*$dy13))) { return 2; }
    if (($r14*$r14) > (($dx14*$dx14)+($dy14*$dy14))) { return 3; }
    if (($r23*$r23) > (($dx23*$dx23)+($dy23*$dy23))) { return 4; }
    if (($r24*$r24) > (($dx24*$dx24)+($dy24*$dy24))) { return 5; }
    if (($r34*$r34) > (($dx34*$dx34)+($dy34*$dy34))) { return 6; }
    
    return 0;
}


my @dx;
my @dy;
my @DEN;
my @xdd;
my @ydd;
my @x;
my @y;
my @m;

#/ ==========================================================================================
#/ Integrate.
#/ Integrate for time \a t0 to \a t1. Using a fourth order Runge-Kutta numerical integrator.
#/ The equations of mothion describe a generic two body gravitational problem.
#/ param Qd first time derivative of the current state vector.
#/ param Q current state vector.
#/ param t current time.
#/ param P parameter vector.
#/ return success=0, failure=non-zero
#/ ------------------------------------------------------------------------------------------
sub DIFEQ {
#/ ------------------------------------------------------------------------------------------
    my $self = shift; #
    my $Qd   = shift; # first time derivative of the current state vector
    my $Q    = shift; # current state vector
    my $t    = shift; # current time
    my $P    = shift; # parameter vector
    #/ --------------------------------------------------------------------------------------
    if ($self->{done} == 0) {
	my $G = $$P[$FourBody::jG];

	$x[0] = $$Q[$FourBody::iX1];    $y[0]  = $$Q[$FourBody::iY1];
	$x[1] = $$Q[$FourBody::iX2];    $y[1]  = $$Q[$FourBody::iY2];
	$x[2] = $$Q[$FourBody::iX3];    $y[2]  = $$Q[$FourBody::iY3];
	$x[3] = $$Q[$FourBody::iX4];    $y[3]  = $$Q[$FourBody::iY4];

	$m[0] = $$P[$FourBody::jM1];
	$m[1] = $$P[$FourBody::jM2];
	$m[2] = $$P[$FourBody::jM3];
	$m[3] = $$P[$FourBody::jM4];

	for (my $k=1; $k<$FourBody::NN; $k++) {
	    for (my $i=0; $i<$k; $i++) {
		$dx[$i][$k] = -($dx[$k][$i] = $x[$k] - $x[$i]);
		$dy[$i][$k] = -($dy[$k][$i] = $y[$k] - $y[$i]);
	    }
	}

	for (my $k=1; $k<$FourBody::NN; $k++) {
	    for (my $i=0; $i<$k; $i++) {
		my $tt =  ($dx[$k][$i]*$dx[$k][$i]) + ($dy[$k][$i]*$dy[$k][$i]);
		$DEN[$i][$k] = ($DEN[$k][$i] = Math2::POWER($tt,-1.5));
	    }
	}

	for (my $i=0; $i<$FourBody::NN; $i++) {
	    my $sumx = 0.0;
	    my $sumy = 0.0;
	    for (my $k=0; $k<$FourBody::NN; $k++) {
		if ($k!=$i) {
		    $sumx += ($m[$k]*$dx[$k][$i]*$DEN[$k][$i]);
		    $sumy += ($m[$k]*$dy[$k][$i]*$DEN[$k][$i]);
		}
	    }
	    $xdd[$i] = $G*$sumx;
	    $ydd[$i] = $G*$sumy;
	}

	$$Qd[$FourBody::iVX1] = $xdd[0];  $$Qd[$FourBody::iVY1] = $ydd[0];    $$Qd[$FourBody::iX1] = $$Q[$FourBody::iVX1];  $$Qd[$FourBody::iY1] = $$Q[$FourBody::iVY1];

	$$Qd[$FourBody::iVX2] = $xdd[1];  $$Qd[$FourBody::iVY2] = $ydd[1];    $$Qd[$FourBody::iX2] = $$Q[$FourBody::iVX2];  $$Qd[$FourBody::iY2] = $$Q[$FourBody::iVY2];

	$$Qd[$FourBody::iVX3] = $xdd[2];  $$Qd[$FourBody::iVY3] = $ydd[2];    $$Qd[$FourBody::iX3] = $$Q[$FourBody::iVX3];  $$Qd[$FourBody::iY3] = $$Q[$FourBody::iVY3];

	$$Qd[$FourBody::iVX4] = $xdd[3];  $$Qd[$FourBody::iVY4] = $ydd[3];    $$Qd[$FourBody::iX4] = $$Q[$FourBody::iVX4];  $$Qd[$FourBody::iY4] = $$Q[$FourBody::iVY4];
    }
}

1;

#/ =========================================================================== END FILE =====
