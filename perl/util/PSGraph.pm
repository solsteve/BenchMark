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

package PSGraph;

use strict;
use warnings;
use diagnostics -verbose;
enable diagnostics;

#/ ==========================================================================================
sub new {
#/ ------------------------------------------------------------------------------------------
    my ($proto, %param) = @_;
    my $class = ref($proto) || $proto;

    my $self = {};
    $self->{fspc} = $param{fspc} || "psgraph.ps";  # output postscript file name
    $self->{BOX}  = $param{BOX}  || 540;
    $self->{difX} = $param{difX} || 0.0;           # width
    $self->{difY} = $param{difY} || 0.0;           # height
    $self->{minX} = $param{minX} || 0.0;           # minimum X coordinate
    $self->{minY} = $param{minY} || 0.0;           # minimum Y coordinate
    bless ($self, $class);

    open PS, ">$self->{fspc}";

    return $self;
}

#/ ==========================================================================================
#/ PostScript header.
#/ Write a standard version 1 PostScript header. Scale and translation are based on the
#/ world coordinates provided in the initialization of the graphics state data structure.
#/ ------------------------------------------------------------------------------------------
sub write_ps_head {
#/ ------------------------------------------------------------------------------------------
    my $self = shift;
    
    print PS "%!PS-Adobe-3.0\n";
    print PS "%%Title: " . $self->{fspc} . "\n";
    print PS "%%Creator: PSGraph\n";
    print PS "%%Orientation: Landscape\n";
    print PS "%%%%Pages: 1\n";
    print PS "%%BoundingBox: 0 0 612 792\n";
    print PS "%%DocumentPaperSizes: Letter\n";
    print PS "%%BeginSetup\n";
    print PS "[{\n";
    print PS "%%BeginFeature: *PageRegion Letter\n";
    print PS "<</PageSize [612 792]>> setpagedevice\n";
    print PS "%%EndFeature\n";
    print PS "} stopped cleartomark\n";
    print PS "%%EndSetup\n";
    print PS "%%Magnification: 1.0000\n";
    print PS "%%EndComments\n\n";

    print PS "%%BeginProlog\n";
    print PS "/dl { newpath moveto lineto stroke } bind def\n";
    print PS "/dr { newpath rectstroke } bind def\n";
    print PS "/dc { newpath 0 360 arc closepath stroke } bind def\n";
    print PS "%EndProlog\n\n";

    print PS "%%Page: 1 1\n\n" ;

    print PS "gsave\n\n";

    print PS "0 setlinewidth\n\n";

    print PS "576 162 translate 90 rotate\n\n";

    print PS "0 0 " . $self->{BOX} . " " . $self->{BOX} . " dr\n\n";

    print PS $self->{BOX} . " " . $self->{difX} . " div " . $self->{BOX} . " " .
	$self->{difY} . " div scale " . (-$self->{minX}) . " " . (-$self->{minY}) .
	" translate\n\n";
}

#/ ==========================================================================================
#/ PostScript Trailer.
#/ Write a standard version 1 PostScript trailer.
#/ ------------------------------------------------------------------------------------------
sub write_ps_tail {
#/ ------------------------------------------------------------------------------------------
    my $self = shift;
    
    print PS "grestore\n";
    print PS "showpage\n\n";

    print PS "%%Trailer\n";
    print PS "%%EOF\n";
}

#/ ==========================================================================================
#/ PostScript Trailer.
#/ Write a standard version 1 PostScript trailer.
#/ ------------------------------------------------------------------------------------------
sub display {
#/ ------------------------------------------------------------------------------------------
    my $self = shift;
    
    print STDOUT "FSPC:\t" . $self->{fspc} . "\n";
    print STDOUT "BOX:\t"  . $self->{BOX}  . "\n";
    print STDOUT "DifX:\t" . $self->{difX} . "\n";
    print STDOUT "DifY:\t" . $self->{difY} . "\n";
    print STDOUT "MinX:\t" . $self->{minX} . "\n";
    print STDOUT "MinY:\t" . $self->{minY} . "\n";
}

#/ ==========================================================================================
#/ Sets the world coordinates of the structure \a G.
#/ Coordinates are set in device and world coordinates. This function is used
#/ to define the world coordinate window.
#/ ------------------------------------------------------------------------------------------
sub setWorldCo { # double x0, double y0,  double x1, double y1
#/ ----------------------------------------------------------------------------------------
    my $self = shift;        
    my $x0   = shift;	     # minimum X coordinate
    my $y0   = shift;	     # minimum Y coordinate
    my $x1   = shift;	     # maximum X coordinate
    my $y1   = shift;	     # maximum Y coordinate

    $self->{minX} = $x0;
    $self->{minY} = $y0;
    $self->{difX} = ($x1-$x0);
    $self->{difY} = ($y1-$y0);
}

#/ ==========================================================================================
#/ Start Graphics.
#/ Begin the capture of graphics commands.
#/ ------------------------------------------------------------------------------------------
sub initGraphics {
#/ ----------------------------------------------------------------------------------------
    my $self = shift;        
    $self->write_ps_head();
}

#/ ==========================================================================================
#/ End Graphics.
#/ End the graphics capture
#/ ------------------------------------------------------------------------------------------
sub printGraphics {
#/ ----------------------------------------------------------------------------------------
    my $self = shift;        
    $self->write_ps_tail();
    close PS;
}

#/ ==========================================================================================
#/ Draw Line.
#/ Draw a line between two points.
#/ ------------------------------------------------------------------------------------------
sub drawLine {
#/ ----------------------------------------------------------------------------------------
    my $self = shift;
    my $x0 = shift;       # starting X coordinate.
    my $y0 = shift;       # starting Y coordinate.
    my $x1 = shift;       # ending X coordinate.
    my $y1 = shift;       # ending Y coordinate.

    print PS $x0 . " " . $y0 . " " . $x1 . " " . $y1 . " dl\n";
}

#/ ==========================================================================================
#/ Draw Circle.
#/ Draw a circle with an arbitrary radius centered on a point.
#/ ------------------------------------------------------------------------------------------
sub drawCircle {
#/ ----------------------------------------------------------------------------------------
    my $self = shift;
    my $xc = shift;         # center X coordinate.
    my $yc = shift;         # center Y coordinate.
    my $r  = shift;         # radius of the circle.

    print PS $xc . " " . $yc . " " . $r . " dc\n";
}

1;

#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
