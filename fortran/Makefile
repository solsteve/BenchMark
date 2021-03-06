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
INC = ./src/source
SRC = ./src/source
TST = ./src/test
MAN = ./DOX
DOC = ./src/doc
TMP = /tmp

#/ ------------------------------------------------------------------------------------------

include ../compdef.mak

#/ ------------------------------------------------------------------------------------------

$(OBJ)/%.o: $(SRC)/%.f
	@echo ""
	@echo "----- compile object code $@ ----------"
	@echo ""
	$(F77) -c $(F77FLAGS) -I$(INC) -I$(SRC) $< -o $@
	@echo ""

$(OBJ)/%.o: $(TST)/%.f
	@echo ""
	@echo "----- compile test object code $@ ----------"
	@echo ""
	$(F77) -c $(F77FLAGS) -I$(INC) -I$(TST) $< -o $@
	@echo ""

#/ --------------------------------------------------------------------------
TESTS = $(BIN)/rktest $(BIN)/ptest
PROGS = $(BIN)/bench $(TESTS)

OBJ01T = $(OBJ)/rktest.o $(OBJ)/rk4.o $(OBJ)/psgraph.o $(OBJ)/basedef.o
OBJ02T = $(OBJ)/ptest.o               $(OBJ)/psgraph.o $(OBJ)/basedef.o
OBJ01  = $(OBJ)/bench.o  $(OBJ)/rk4.o $(OBJ)/psgraph.o $(OBJ)/basedef.o

all: $(PROGS)

#/ ------------------------------------------------------------------------------------------

$(BIN)/rktest: $(OBJ01T)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(F77LD) -o $@ $(F77LDFLAG) $^ $(F77LIBS)
	@echo ""

$(BIN)/ptest: $(OBJ02T)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(F77LD) -o $@ $(F77LDFLAG) $^ $(F77LIBS)
	@echo ""

$(BIN)/bench: $(OBJ01)
	@echo ""
	@echo "----- link test binary $@ ----------"
	@echo ""
	$(F77LD) -o $@ $(F77LDFLAG) $^ $(F77LIBS)
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

$(OBJ)/rk4.o:     $(SRC)/rk4.f     $(INC)/rk4.f $(INC)/basedef.f
$(OBJ)/psgraph.o: $(SRC)/psgraph.f              $(INC)/basedef.f

$(OBJ)/rktest.o:  $(TST)/rktest.f  $(INC)/rk4.f $(INC)/basedef.f
$(OBJ)/ptest.o:   $(TST)/ptest.f                $(INC)/basedef.f
$(OBJ)/bench.o:   $(SRC)/bench.f   $(INC)/rk4.f $(INC)/basedef.f

#/ ==========================================================================================

test: ../test-f77.ps
	PSView $<

../test-f77.ps: $(BIN)/bench
	time $< > $@

test3: test3.ps
	PSView $<

test3.ps: $(BIN)/rktest
	$< > $@

test2: test2.ps
	PSView $<

test2.ps: $(BIN)/ptest
	$< > $@

bench: $(BIN)/bench
	@sync
	@date
	@time $< /dev/null > /dev/null 2> /dev/null
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

#/ =========================================================================== END FILE =====
