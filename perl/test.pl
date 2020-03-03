#!/usr/bin/perl

use strict;

mytest( "Family",
	[
	 "Jacob"    => 6,
	 "Rebekah"  => 9,
	 "Adrienne" => 22,
	 "Amber"    => 23,
	 "Heather"  => 37,
	 "Stephen"  => 42
	 ]
	);

exit(0);




sub mytest {
    my $name = shift;
    my %db;
    $db{'Jacob'}    = 6;
    $db{'Rebekah'}  = 9;
    $db{'Adrienne'} = 22;
    $db{'Amber'}    = 23;
    $db{'Heather'}  = 37;
    $db{'Stephen'}  = 42;

    foreach my $k (keys %db) {
	print $k . "\n";
    }

}
