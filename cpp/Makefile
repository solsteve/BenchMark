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

test: ../test-cpp.ps
	PSView $<

../test-cpp.ps: $(BIN)/bench
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

#/ =========================================================================== END FILE =====
