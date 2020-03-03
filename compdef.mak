CARCH_NONE = \
-fverbose-asm \
-ansi \
-fsigned-char \
-funsigned-bitfields \
-fshort-enums \
#

CARCH_I7 = \
-fverbose-asm \
-ansi \
-fsigned-char \
-funsigned-bitfields \
-fshort-enums \
-ffast-math \
-funroll-loops \
-march=corei7 \
-mtune=corei7 \
#

CARCH_MP = \
-fverbose-asm \
-ansi \
-fsigned-char \
-funsigned-bitfields \
-fshort-enums \
-ffast-math \
-funroll-loops \
-march=corei7 \
-mtune=corei7 \
-fopenmp -msse2 -ftree-vectorizer-verbose=0 -fopt-info-optimized
#

CARCH=$(CARCH_MP)

WCOMMON = \
-W \
-Wall \
-Wextra \
-pedantic \
-Wfloat-equal \
-Wshadow \
-Wcast-align \
-Wredundant-decls \
-Wundef \
-Wpointer-arith \
-Wsign-compare \
-Wconversion \
-Wpacked \
-Wno-multichar \
-Wmissing-noreturn \
-Wunused-macros \
-Wendif-labels \
-Wpadded \
-Waggregate-return \
-Wformat \
-Wformat-security \
-Wswitch-default \
-Wswitch-enum

#/ ==========================================================================================

CWARNS = $(WCOMMON) \
-Wdeclaration-after-statement \
-Wstrict-prototypes \
-Wmissing-prototypes \
-Wnested-externs

CC = gcc

CFLAGS  = -O3 $(CWARNS) $(CARCH)

LD = gcc

LDFLAGS =

CLIBS   = -lgomp -lpthread -lm

#/ ==========================================================================================

CXXARCH = $(CARCH)

CXXWARNS = $(WCOMMON) \
-Wsynth \
-Wreorder \
-Winline \
-Weffc++ \
-Woverloaded-virtual \
-ffor-scope \
-fcheck-new \
-Wsign-promo

CXX = $(CC) -xc++

CXXFLAGS = -g -O2 $(CXXWARNS) $(CXXARCH)

LDXX = $(LD)

LDXXFLAGS =

CXXLIBS  = -lstdc++ $(CLIBS)

#/ ==========================================================================================

F77WARNS=\
-W    \
-Wall  \
-Wextra \
-Wshadow \
-Wpacked  \
-pedantic   \
-Wunderflow  \
-ffixed-form  \
-Wcast-align   \
-Wconversion    \
-fdefault-real-8  \
-Waggregate-return  \
-Wmissing-noreturn   \
-fdefault-double-8    \
-ffixed-line-length-72

FARCH_NOOPT=-frecursive


FARCH_I7=-frecursive\
         -ffast-math -funroll-loops -march=corei7 -mtune=corei7

FARCH_OMP=-frecursive\
          -ffast-math -funroll-loops -march=corei7 -mtune=corei7 \
          -fopenmp -msse2 -ftree-vectorizer-verbose=0 -fopt-info-optimized

FARCH=$(FARCH_NOOPT)

F77 = gfortran

F77FLAGS = -O3 $(F77WARNS) $(FARCH)

F77LD = gfortran

F77LDFLAGS =

F77LIBS  = -lgomp -lpthread 

#/ ==========================================================================================
