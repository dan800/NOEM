GOAL = snoedrvr
OBJ = snoe.mod.o snoedrvr.o

FC = gfortran 
FFLAGS= -c -I/usr/local/include
.SUFFIXES : .F90 .o

.F90.o :
	$(FC) $(FFLAGS) $<

#  link with netcdf libraries
program : $(OBJ)
	$(FC) $(LFLAGS) -o $(GOAL) $(OBJ) \
	   -lnetcdf 

clean:
	rm -v *.o *.mod $(GOAL)

