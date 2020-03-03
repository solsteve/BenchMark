#/ ====================================================================== BEGIN FILE =====
#/ **                                  M A K E F I L E                                  **
#/ =======================================================================================
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
#/ **                                                                                   **
#/ ----- Modification History ------------------------------------------------------------
#/
#/ @brief   Build environment.
#/
#/ @details Provides a common build environment.
#/
#/ @author  Stephen W. Soliday
#/ @date    2012-Aug-10
#/
#/ =======================================================================================

all:
	make -C c       $@
	make -C cpp     $@
	make -C fortran $@
	make -C java    $@
	make -C perl    $@
	make -C python  $@
#	make -C math    $@

#/ --------------------------------------------------------------------------

bench:
	make -C c       $@
	make -C cpp     $@
	make -C fortran $@
	make -C java    $@
	make -C perl    $@
	make -C python  $@
#	make -C math    $@

#/ --------------------------------------------------------------------------
clean:
	make -C c       $@
	make -C cpp     $@
	make -C fortran $@
	make -C java    $@
	make -C perl    $@
	make -C python  $@
	make -C math    $@

#/ --------------------------------------------------------------------------
fullclean: clean
	make -C c       $@
	make -C cpp     $@
	make -C fortran $@
	make -C java    $@
	make -C perl    $@
	make -C python  $@
	make -C math    $@
	rm -f test-*.ps *~ data/*~

#/ ==========================================================================
