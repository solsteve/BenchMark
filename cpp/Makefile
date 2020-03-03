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

$(OBJ)/%.o: $(SRC)/%.cc
	@echo ""
	@echo "----- compile object code $@ ----------"
	@echo ""
	$(CXX) -c $(CXXFLAGS) -I$(INC) -I$(SRC) $< -o $@
	@echo ""

$(OBJ)/%.o: $(TST)/%.cc
	@echo ""
	@echo "----- compile test object code $@ ----------"
	@echo ""
	$(CXX) -c $(CXXFLAGS) -I$(INC) -I$(TST) $< -o $@
	@echo ""

#/ ------------------------------------------------------------------------------------------

TESTS = $(BIN)/ptest $(BIN)/rktest
PROGS = $(TESTS) $(BIN)/bench

OBJ01T = $(OBJ)/rktest.o $(OBJ)/rk4.o $(OBJ)/psgraph.o
OBJ02T = $(OBJ)/ptest.o               $(OBJ)/psgraph.o
OBJ01  = $(OBJ)/bench.o  $(OBJ)/rk4.o $(OBJ)/psgraph.o

all: $(PROGS)

#/ ------------------------------------------------------------------------------------------

$(BIN)/rktest: $(OBJ01T)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(LDXX) -o $@ $(LDXXFLAG) $^ $(CXXLIBS)
	@echo ""

$(BIN)/ptest: $(OBJ02T)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(LDXX) -o $@ $(LDXXFLAG) $^ $(CXXLIBS)
	@echo ""

$(BIN)/bench: $(OBJ01)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(LDXX) -o $@ $(LDXXFLAG) $^ $(CXXLIBS)
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

$(OBJ)/rk4.o:     $(SRC)/rk4.cc     $(INC)/rk4.hh $(INC)/psgraph.hh $(INC)/basedef.hh
$(OBJ)/psgraph.o: $(SRC)/psgraph.cc               $(INC)/psgraph.hh $(INC)/basedef.hh

$(OBJ)/rktest.o:  $(TST)/rktest.cc  $(INC)/rk4.hh $(INC)/psgraph.hh $(INC)/basedef.hh
$(OBJ)/ptest.o:   $(TST)/ptest.cc                 $(INC)/psgraph.hh $(INC)/basedef.hh
$(OBJ)/bench.o:   $(SRC)/bench.cc   $(INC)/rk4.hh $(INC)/psgraph.hh $(INC)/basedef.hh

#/ ==========================================================================================

test: test.ps
	PSView $<

test.ps: $(BIN)/bench
	time $< $@

test3: test3.ps
	PSView $<

test3.ps: $(BIN)/rktest
	time $< $@

test2: test2.ps
	PSView $<

test2.ps: $(BIN)/ptest
	$< $@

bench: $(BIN)/bench
	@sync
	@date
	@time $< /dev/null > /dev/null 2> /dev/null
	@sync
	@date

#/ ==========================================================================================
#/ **                              P R O P R I E T A R Y                                   **
#/ =========================================================================== END FILE =====
