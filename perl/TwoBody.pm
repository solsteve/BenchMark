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

package TwoBody;
@ISA = ("RK4");

use strict;
use warnings;
use diagnostics -verbose;
enable diagnostics;

require util::Math2;

$TwoBody::maxr  = 4.6e+8;          # Newton's Gravitational Constant
$TwoBody::ITER  = 1000;            # Number of Plot Iterations
$TwoBody::ISTEP = 60;              # Number of Integration Steps per Plot
$TwoBody::MAXT  = 29.0;            # Time in days of plot
$TwoBody::Me    =  5.9742e+24;     # Mass of the Earth
$TwoBody::Mm    =  7.347673e+22;   # Mass of the Moon
$TwoBody::Re    =  6370996.0;      # Average Radius of the Earth
$TwoBody::Rm    =  1737146.0;      # Average Radius of the Moon
$TwoBody::Xe    = -4.41156e+6;     # Initial X Pos of the Earth
$TwoBody::Ye    =  0.0;            # Initial Y Pos of the Earth
$TwoBody::Xm    =  3.58692e+8;     # Initial X Pos of the Moon
$TwoBody::Ym    =  0.0;            # Initial Y Pos of the Moon
$TwoBody::VXe   =  0.0;            # Initial X Velocity of the Earth
$TwoBody::VYe   = -11.7422;        # Initial Y Velocity of the Earth
$TwoBody::VXm   =  0.0;            # Initial X Velocity of the Moon
$TwoBody::VYm   =  1082.0;         # Initial Y Velocity of the Moon        


#/ ==========================================================================================
sub new {
#/ ------------------------------------------------------------------------------------------
    my $pkg = shift;
    my $obj = $pkg->SUPER::new(8);
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
    my $dx = $$Q[6] - $$Q[4];
    my $dy = $$Q[7] - $$Q[5];
    my $r2 = $$P[1] + $$P[3];

    return ((($r2*$r2) < (($dx*$dx)+($dy*$dy))) ? (0) : (1));
}

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
	
	my $xd1 = $$Q[0];  my $x1 = $$Q[4];
	my $yd1 = $$Q[1];  my $y1 = $$Q[5];
	my $xd2 = $$Q[2];  my $x2 = $$Q[6];
	my $yd2 = $$Q[3];  my $y2 = $$Q[7];
	
	my $m1   = $$P[0];
	my $m2   = $$P[2];
	
	my $dx21 = $x2 - $x1;
	my $dy21 = $y2 - $y1;
	my $dx12 = $x1 - $x2;
	my $dy12 = $y1 - $y2;

	my $r12sq = ($dx12*$dx12) + ($dy12*$dy12);
	my $den   = Math2::POWER($r12sq, 1.5);

	if ($den > 0.0) {} else { print STDERR "INFINITY\n"; }

	my $a1 = $Math2::N_G*$m2*$dx21;
	my $a2 = $Math2::N_G*$m2*$dy21;
	my $a3 = $Math2::N_G*$m1*$dx12;
	my $a4 = $Math2::N_G*$m1*$dy12;

	$$Qd[0] = Math2::SAFEDIV($a1,$den);
	$$Qd[1] = Math2::SAFEDIV($a2,$den);
	$$Qd[2] = Math2::SAFEDIV($a3,$den);
	$$Qd[3] = Math2::SAFEDIV($a4,$den);
	$$Qd[4] = $xd1;
	$$Qd[5] = $yd1;
	$$Qd[6] = $xd2;
	$$Qd[7] = $yd2;
    }
}

1;

#/ =========================================================================== END FILE =====
