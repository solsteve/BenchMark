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

SHELL      = /bin/sh
CHECKER    = ON
DEBUGTRACE = OFF

#/ ==========================================================================================

BIN = ./bin
OBJ = ./obj
INC = ./src/include
SRC = ./src/source
TST = ./src/test
MAN = ./DOX
DOC = ./src/doc
TMP = /tmp

#/ ------------------------------------------------------------------------------------------

include ../compdef.mak

#/ ------------------------------------------------------------------------------------------

$(OBJ)/%.o: $(SRC)/%.c
	@echo ""
	@echo "----- compile object code $@ ----------"
	@echo ""
	$(CC) -c $(CFLAGS) -I$(INC) -I$(SRC) $< -o $@
	@echo ""

$(OBJ)/%.o: $(TST)/%.c
	@echo ""
	@echo "----- compile test object code $@ ----------"
	@echo ""
	$(CC) -c $(CFLAGS) -I$(INC) -I$(TST) $< -o $@
	@echo ""

#/ --------------------------------------------------------------------------
TESTS = $(BIN)/rktest $(BIN)/ptest
PROGS = $(BIN)/bench $(TESTS)

OBJ01T = $(OBJ)/rktest.o $(OBJ)/rk4.o $(OBJ)/psgraph.o
OBJ02T = $(OBJ)/ptest.o               $(OBJ)/psgraph.o
OBJ01  = $(OBJ)/bench.o  $(OBJ)/rk4.o $(OBJ)/psgraph.o

all: $(PROGS)

#/ ------------------------------------------------------------------------------------------

$(BIN)/rktest: $(OBJ01T)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(LD) -o $@ $(LDFLAG) $^ $(CLIBS)
	@echo ""

$(BIN)/ptest: $(OBJ02T)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(LD) -o $@ $(LDFLAG) $^ $(CLIBS)
	@echo ""

$(BIN)/bench: $(OBJ01)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(LD) -o $@ $(LDFLAG) $^ $(CLIBS)
	@echo ""

#/ ==========================================================================================
clean:
	rm -f $(PROGS) $(OBJ)/*
	rm -f *.ps
	rm -rf $(MAN)

fullclean: clean
	rm -f core
	rm -f $(DOC)/*~ $(SRC)/*~ $(TST)/*~ $(INC)/*~
	rm -f *~
	rm -f $(TMP)/Soliday.*

#/ ==========================================================================================

LOGO = Soliday.jpg

makedoc: ./bench.dox $(MAN)/html/$(LOGO)
	doxygen $<
	cp $(DOC)/report.tex $(MAN)/latex
	konqueror $(MAN)/html/index.html

$(MAN)/html/Soliday.jpg: $(MAN)/html $(OBJ)/Soliday.jpg
	cp $(OBJ)/Soliday.jpg $@

$(MAN)/html: $(MAN)
	mkdir $@

$(MAN):
	mkdir $@

PFLAGS = +FP +w256 +h256 +x +d0 -v +sp16 +P +L/usr/local/src/povray-3.6/include
$(TMP)/Soliday.ppm: $(TMP)/Soliday.pov
	povray +O$@ $(PFLAGS) -i$<

$(OBJ)/Soliday.jpg: $(TMP)/Soliday.ppm
	pnmscale 0.25 $< | ppmtojpeg --quality=100 > $@

$(TMP)/Soliday.pov: $(DOC)/Soliday.pov
	cp $< $@

#/ ==========================================================================================

$(OBJ)/rk4.o:     $(SRC)/rk4.c     $(INC)/rk4.h $(INC)/psgraph.h $(INC)/basedef.h
$(OBJ)/psgraph.o: $(SRC)/psgraph.c              $(INC)/psgraph.h $(INC)/basedef.h

$(OBJ)/rktest.o:  $(TST)/rktest.c  $(INC)/rk4.h $(INC)/psgraph.h $(INC)/basedef.h
$(OBJ)/ptest.o:   $(TST)/ptest.c                $(INC)/psgraph.h $(INC)/basedef.h
$(OBJ)/bench.o:   $(SRC)/bench.c   $(INC)/rk4.h $(INC)/psgraph.h $(INC)/basedef.h

#/ ==========================================================================================

test: ../test-c.ps
	PSView $<

../test-c.ps: $(BIN)/bench
	time $< $@

test3: test3.ps
	PSView $<

test3.ps: $(BIN)/rktest
	$< $@

test2: test2.ps
	PSView $<

test2.ps: $(BIN)/ptest
	$< $@

bench: $(BIN)/bench
	@sync
	@date
	@time ($< /dev/null > /dev/null)
	@sync
	@date

#/ ==========================================================================================

show:
	@echo "CC              = $(CC)"
	@echo "CFLAGS          = $(CFLAGS)"
	@echo "LD              = $(LD)"
	@echo "LDFLAGS         = $(LDFLAGS)"
	@echo "CLIBS           = $(CLIBS)"
	@echo ""
	@echo "CXX             = $(CXX)"
	@echo "CXXFLAGS        = $(CXXFLAGS)"
	@echo "LDXX            = $(LDXX)"
	@echo "LDXXFLAGS       = $(LDXXFLAGS)"
	@echo "CXXLIBS         = $(CXXLIBS)"
	@echo ""
	@echo "CO              = $(CO)"
	@echo "COFLAGS         = $(COFLAGS)"
	@echo ""
	@echo "AR              = $(AR)"
	@echo "RM              = $(RM)"
	@echo "ARFLAGS         = $(ARFLAGS)"
	@echo "RANLIB          = $(RANLIB)"

#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
