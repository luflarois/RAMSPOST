# ==================================
#    Makefile RAMSPOST version 5.0
# ==================================

include ./include_ramspost.mk
include ./objects.mk

# Compiler commands
F_COMMAND = $(F_COMP) -c $(F_OPTS)
C_COMMAND  = $(C_COMP) -c $(C_OPTS) -D$(CMACH)

# RAMSPOST root directory.
RAMSPOST=./src

# Source directories.
ALL_INCS=./include
INCLUDES=-I$(ALL_INCS)


# Executable definitions ============================

# Archive and executable names.
BASE=ramspost_60
EXE=$(BASE)
ARC=$(BASE).a

# Executable targets
all: all_lib $(EXE)
	@echo ""
	@echo ========== Finished building executable $(EXE)
	@echo ""

$(EXE): $(ARC_LIB) $(ARC)
	$(LOADER) -o $(EXE) $(LOADER_OPTS) $(ARC) $(ARC_LIB)
###	ln -fs $(EXE) $(BASE)
	rm -f r*.o

$(ARC): $(F_SRC)
	@echo ""
	@echo ========== Building executable $(EXE) ...
	@echo ""
	$(F_COMMAND) $(INCLUDES) $(?)
	$(ARCHIVE) $(ARC) *.o
	$(F_COMMAND) $(INCLUDES) $(RAMSPOST)/ramspost_A.f90
#	rm -f *.o

install:
#	ln -fs `pwd`/$(EXE) ../run/$(BASE)
#	ln -fs `pwd`/$(EXE) ../test/$(BASE)

clean:
	@echo ""
	@echo ========== Cleaning compiled and executable files
	@echo ""
	rm -f $(EXE) *.o *.mod *.a


# LIB definitions  ==================================

# LIB source directories
INCLUDES_LIB=$(INCLUDES)
RCIO=./src/LIB/RCIO/

# LIB archives
BASE_LIB=libutils
ARC_LIB=./libutils-ramspost.a

# LIB rules
.f90.a:
	@echo ""
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(INCLUDES_LIB) $(<F:.f90=.f90)
	$(ARCHIVE) $@ $(<F:.f90=.o)
	rm -f $(<F:.f90=.f90) $(<F:.f90=.o)

# for non-IBM
.F90.a:
	@echo ""
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) $(INCLUDES_LIB) -D$(CMACH) $(<F:.F90=.F90)
	$(ARCHIVE) $@ $(<F:.F90=.o)
	rm -f $(<F:.F90=.F90) $(<F:.F90=.o)

# For IBM
#.F90.a:
#	@echo ""
#	$(C_COMMAND) $(INCLUDES_LIB) $(C_PP_OPTS) $<
#	mv $(<F:.F90=.i) $(<F:.F90=.f)
#	$(F_COMMAND) $(<F:.F90=.f)
#	$(ARCHIVE) $@ $(<F:.F90=.o)
#	rm -f $(<F:.F90=.f) $(<F:.F90=.o)

.c.a:
	@echo ""
	$(C_COMMAND) $(INCLUDES_LIB) $<
	$(ARCHIVE) $@ $(<F:.c=.o)
	rm -f $(<F:.c=.o)


# LIB targets
all_lib:  $(ARC_LIB)

$(ARC_LIB): $(OBJ_LIB)
	@echo ""
	@echo ========== LIB generated: $(ARC_LIB)
	@echo ""

# TODO ====================================
check: FORCE
	@echo ""
	@echo ========== Checking ...
	@echo ""
	check

FORCE:
