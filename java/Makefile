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
	time java -cp ./build/lib/Benchmark.jar com.soliday.apps.benchmark.Bench test.ps
	PSView test.ps

bench: all
	@sync
	@date
	@time java -cp ./build/lib/Benchmark.jar com.soliday.apps.benchmark.Bench /dev/null > /dev/null 2> /dev/null
	@sync
	@date




#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
