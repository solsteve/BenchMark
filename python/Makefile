#/ ===== BEGIN FILE =========================================================================
#/ **                                                                                      **
#/ **  Copyright (c) 2007, Stephen W. Soliday                                              **
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
#/ **  Date   2007-05-05                                                                   **
#/ **                                                                                      **
#/ **  $Id$                                                                                **
#/ **  $Log$                                                                               **
#/ **                                                                                      **
#/ ==========================================================================================

SHELL      = /bin/sh

PYTH=python2.7


#/ ==========================================================================================

all:
	ls -lpa

#/ ------------------------------------------------------------------------------------------

clean:
	rm -f core
	rm -f *.pyc
	rm -f *.pyo
	rm -f *.ps

#/ ------------------------------------------------------------------------------------------

fullclean: clean
	rm -f *~

#/ ------------------------------------------------------------------------------------------

ptest:
	$(PYTH) -O ptest.py ptest.ps
	PSView ptest.ps

rtest:
	$(PYTH) rktest.py rktest.ps
	PSView rktest.ps


test:
	$(PYTH) -O bench.py ../test-python.ps
	PSView ../test-python.ps

bench1:
	@sync
	@date
	@time $(PYTH) bench.py psgraph1.ps > /dev/null 2> /dev/null
	@sync
	@date

bench2:
	@sync
	@date
	@time $(PYTH) ctest.py psgraph2.ps > /dev/null 2> /dev/null
	@sync
	@date

bench3:
	@sync
	@date
	@time $(PYTH) -O ctest.py psgraph3.ps > /dev/null 2> /dev/null
	@sync
	@date

bench:
	@sync
	@date
	@time $(PYTH) bench.py psgraph.ps > /dev/null 2> /dev/null
	@sync
	@date



#/ =========================================================================== END FILE =====
