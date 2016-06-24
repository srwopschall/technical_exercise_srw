# makefile
# Purpose: Create the executable or the static library file for MAXENT
#
include makefile.inc

LIBPATH = -L../json-fortran-master/lib/

MODPATH = -I../json-fortran-master/lib/

EXT_LIBS  = $(LIBPATH) -ljsonfortran

EXEC = te_srw.x

SRCEXEC = cut_cost_m.f te_srw.f 

SRC   = cut_cost_m.f

OBJS = ${SRC:.f=.o}

OBJSEXEC = ${SRCEXEC:.f=.o}

MOD  = ${SRC:.f=.mod}

cut_cost:
	@echo "Compiling $(G95) $(FFLAGS) $(SRC)"
	@$(G95) $(FFLAGS) $(SRCEXEC) $(MODPATH)
	@echo "Linking...$(OBJSEXEC)"
	@$(G95) -o $(EXEC) $(OBJSEXEC) $(EXT_LIBS)

clean   :
	@echo "Removing *.o, *.mod"
	@$(RM) $(OBJSEXEC)
	@$(RM) $(MOD)
