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

package RK4;

use strict;
use warnings;
use diagnostics -verbose;
enable diagnostics;

my @A = ();
my @B = ();
my @C = ();
my @D = ();
my @W = ();


#/ ==========================================================================================
sub new {
#/ ------------------------------------------------------------------------------------------
    my ($proto, $n) = @_;
    my $class = ref($proto) || $proto;

    for (my $i=0; $i<$n; $i++) {
	$A[$i] = 0.0;
	$B[$i] = 0.0;
	$C[$i] = 0.0;
	$D[$i] = 0.0;
	$W[$i] = 0.0;
    }

    my $self = {};
    $self->{dim}  = $n;    # number of coupled first order equations
    $self->{done} = 0;     # DIFEQ input vector
    bless ($self, $class);

    return $self;
}

#/ ==========================================================================================
#/ Fourth order Runge-Kutta.
#/ Provides the implementation for a fourth order Runge-Kutta numerical integrator with 
#/ uniform step sizes.
#/ return new time (\a t1).
#/ ------------------------------------------------------------------------------------------
sub integrate {
#/ ----------------------------------------------------------------------------------------
    my $self = shift;
    my $Q    = shift; # real vector containing the state.
    my $t0   = shift; # initial time.
    my $t1   = shift; # final time.
    my $step = shift; # number of steps between current time \a t0 and final time \a t1.
    my $P    = shift; # vector containing fixed parameters.
#/ ----------------------------------------------------------------------------------------
    my $h  = ($t1 - $t0) / $step;
    my $t  = $t0;
    my $h2 = $h / 2.0;

    my $n = $self->{dim};

    for (my $k=0; $k<$step; $k++) {
	
	if ($self->CHECKEQ( $Q, $t, $P ) != 0) { return $t; }
	
	for (my $j=0; $j<$n; $j++) { $W[$j] = $$Q[$j];                  } $self->DIFEQ(\@A, \@W, $t,     $P);
	for (my $j=0; $j<$n; $j++) { $W[$j] = $$Q[$j] + ($A[$j] * $h2); } $self->DIFEQ(\@B, \@W, $t+$h2, $P);
	for (my $j=0; $j<$n; $j++) { $W[$j] = $$Q[$j] + ($B[$j] * $h2); } $self->DIFEQ(\@C, \@W, $t+$h2, $P);
	for (my $j=0; $j<$n; $j++) { $W[$j] = $$Q[$j] + ($C[$j] * $h);  } $self->DIFEQ(\@D, \@W, $t+$h,  $P);

	for (my $j=0; $j<$n; $j++) { $$Q[$j] += ($h*($A[$j] + 2.0*($B[$j] + $C[$j]) + $D[$j])/6.0); }
	
	$t += $h;
    }
    
    return $t;
}

1;

#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
