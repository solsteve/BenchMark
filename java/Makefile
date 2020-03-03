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
#/ **  Date   2006-11-24                                                                   **
#/ **                                                                                      **
#/ **  $Id$                                                                                **
#/ **  $Log$                                                                               **
#/ **                                                                                      **
#/ ==========================================================================================

SHELL = /bin/sh

#/ ------------------------------------------------------------------------------------------
all:
	ant compile
	@echo ""
	@echo ""
	@cat ALIAS
	@echo ""
	@echo ""

#/ ------------------------------------------------------------------------------------------
makedoc:
	ant javadoc
	konqueror doc/api/index.html

#/ ------------------------------------------------------------------------------------------
commonclean:
	rm -f test*.*ps
	rm -f *~

clean: commonclean
	./clean.sh clean

#/ ------------------------------------------------------------------------------------------
fullclean: commonclean
	./clean.sh fullclean
	find . -name "*~" | xargs rm -f

#/ ------------------------------------------------------------------------------------------
ptest: all
	java -cp ./build/lib/Benchmark.jar com.soliday.lib.util.PSGraph test.ps
	PSView test.ps

#/ ------------------------------------------------------------------------------------------
rtest: all
	time java -cp ./build/lib/Benchmark.jar com.soliday.apps.benchmark.test.RKTest test.ps
	PSView test.ps

#/ ------------------------------------------------------------------------------------------
test: all
	time java -cp ./build/lib/Benchmark.jar com.soliday.apps.benchmark.Bench ../test-java.ps
	PSView ../test-java.ps

bench: all
	@sync
	@date
	@time java -cp ./build/lib/Benchmark.jar com.soliday.apps.benchmark.Bench /dev/null > /dev/null 2> /dev/null
	@sync
	@date




#/ =========================================================================== END FILE =====
