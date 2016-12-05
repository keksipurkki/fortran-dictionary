FC=gfortran
FFLAGS=-g -D_DEBUG
PROG=dict
FORTRAN_FILES=dictentry.F90\
	      dictionary.F90

OBJ := $(patsubst %.F90, %.o, $(FORTRAN_FILES))

all: $(PROG)

$(PROG): $(OBJ)
	$(FC) $(FFLAGS) $(OBJ) -o $(PROG)

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f *.mod *.o
