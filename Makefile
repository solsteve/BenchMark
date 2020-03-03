#/ ==========================================================================
#/ --------------------------------------------------------------------------
#/ ==========================================================================

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
