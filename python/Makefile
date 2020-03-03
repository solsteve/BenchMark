#/ ===== BEGIN FILE =========================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ ==========================================================================================
#/ **                                                                                      **
#/ **  Copyright (c) 2007, Stephen W. Soliday                                              **
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
#/ **  Date   2007-05-05                                                                   **
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
	rm -f *.pyc
	rm -f *.pyo
	rm -f *.ps

#/ ------------------------------------------------------------------------------------------

fullclean: clean
	rm -f *~

#/ ------------------------------------------------------------------------------------------

ptest:
	python -O ptest.py ptest.ps
	PSView ptest.ps

rtest:
	python rktest.py rktest.ps
	PSView rktest.ps


test:
	python -O bench.py psgraph.ps
	PSView psgraph.ps

bench1:
	@sync
	@date
	@time python bench.py psgraph1.ps > /dev/null 2> /dev/null
	@sync
	@date

bench2:
	@sync
	@date
	@time python ctest.py psgraph2.ps > /dev/null 2> /dev/null
	@sync
	@date

bench3:
	@sync
	@date
	@time python -O ctest.py psgraph3.ps > /dev/null 2> /dev/null
	@sync
	@date

bench:
	@sync
	@date
	@time python bench.py psgraph.ps > /dev/null 2> /dev/null
	@sync
	@date



#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
