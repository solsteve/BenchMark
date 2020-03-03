#/ ===== BEGIN FILE =========================================================================
#/ **                                                                                      **
#/ **  Copyright (c) 2006, Stephen W. Soliday                                              **
#/ **                      stephen@soliday.com                                             **
#/ **                      http://www.soliday.com/stephen                                  **
#/ **                                                                                      **
#/ **  This program is free software: you can redistribute it and/or modify it under       **
#/ **  the terms of the GNU General Public License as published by the Free Software       **
#/ **  Foundation, either version 3 of the License, or (at your option)                    **
#/ **  any later version.                                                                  **
#/ **                                                                                      **
#/ **  This program is distributed in the hope that it will be useful, but WITHOUT         **
#/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS       **
#/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.      **
#/ **                                                                                      **
#/ **  You should have received a copy of the GNU General Public License along with        **
#/ **  this program. If not, see <http://www.gnu.org/licenses/>.                           **
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

SHELL      = /bin/sh

#/ ==========================================================================================

all:
	ls -lpa

#/ ------------------------------------------------------------------------------------------
clean:
	rm -f core
	rm -f *.ps

#/ ------------------------------------------------------------------------------------------
fullclean: clean
	rm -f *~
	rm -f util/*~
	rm -f math/*~

#/ ------------------------------------------------------------------------------------------
ptest:
	perl -I. ptest.pl
	PSView psgraph.ps

rtest:
	perl -I. rtest.pl
	PSView psgraph.ps


test:
	time perl -I. bench.pl
	mv psgraph.ps ../test-perl.ps
	PSView ../test-perl.ps

bench:
	@sync
	@date
	@time perl -I. bench.pl > /dev/null 2> /dev/null
	@sync
	@date



#/ =========================================================================== END FILE =====
