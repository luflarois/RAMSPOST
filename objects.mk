# Executable
# Sources.
F_SRC = \
        $(RAMSPOST)/anheader.f90   \
			  $(RAMSPOST)/ramspost_A.f90 \
        $(RAMSPOST)/ramspost_B.f90 \
        $(RAMSPOST)/ramspost_D.f90 \
        $(RAMSPOST)/ramspost_C.f90

# LIB
# Sources
.SUFFIXES:
.SUFFIXES: .f90 .F90 .f .c .o .a

# objects
OBJ_LIB = $(ARC_LIB)($(RCIO)/vformat_brams3.3.o) \
      $(ARC_LIB)($(RCIO)/charutils.o)  \
      $(ARC_LIB)($(RCIO)/error_mess.o) \
      $(ARC_LIB)($(RCIO)/polarst.o)	 \
      $(ARC_LIB)($(RCIO)/dateutils.o) \
      $(ARC_LIB)($(RCIO)/modrconfig.o) \
      $(ARC_LIB)($(RCIO)/rcio.o)  	 \
      $(ARC_LIB)($(RCIO)/interp_lib.o) \
      $(ARC_LIB)($(RCIO)/rnumr.o)	 \
      $(ARC_LIB)($(RCIO)/rutil.o)      \
      $(ARC_LIB)($(RCIO)/rfvec.o)       \
      $(ARC_LIB)($(RCIO)/therm_lib.o)       \
      \
      $(ARC_LIB)($(RCIO)/dted.o) \
      $(ARC_LIB)($(RCIO)/parlib.o) \
      $(ARC_LIB)($(RCIO)/tmpname.o) \
      $(ARC_LIB)($(RCIO)/utils_c.o) \
      $(ARC_LIB)($(RCIO)/eenviron.o)
