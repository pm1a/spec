FC = gfortran
#FC = ifort
OPT = -O3

MODS = typy.mod mtx.mod datasetup.mod solvers.mod
OBJS = typy.o mtx.o datasetup.o solvers.o

Release : $(MODS) $(OBJS)
	$(FC) $(OPT) -o spec $(OBJS) main.f90

typy.mod : typy.f90
	$(FC) $(OPT) -c typy.f90
typy.o : typy.mod
	$(FC) $(OPT) -c typy.f90

mtx.mod : typy.mod mtx.f90
	$(FC) $(OPT) -c mtx.f90
mtx.o : typy.mod mtx.mod
	$(FC) $(OPT) -c mtx.f90

datasetup.mod : typy.mod mtx.mod datasetup.f90
	$(FC) $(OPT) -c datasetup.f90
datasetup.o : typy.mod mtx.mod datasetup.mod
	$(FC) $(OPT) -c datasetup.f90

solvers.mod : typy.mod mtx.mod solvers.f90
	$(FC) $(OPT) -c solvers.f90
solvers.o : typy.mod mtx.mod solvers.mod
	$(FC) $(OPT) -c solvers.f90

doc :
	doxygen spec.sfg
clean :
	rm *.o *.mod

