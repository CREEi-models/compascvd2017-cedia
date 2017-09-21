F90 = ifort
OBJECTS = statamod.o
INCLUDES = readstata.inc writestata1.inc writestata2.inc
OUTPUTDIR = ../modules-dev
FFLAGS = -O3 -fPIC -gen-interfaces -stand f03 -warn all -module $(OUTPUTDIR)
#FFLAGS = -O0 -g -debug -fPIC -traceback -gen-interfaces -stand f03 -warn all -module $(OUTPUTDIR)

all:$(OBJECTS)
	@ar rc $(OUTPUTDIR)/statamod.a $(OBJECTS)

statamod.o:statamod.f90 $(INCLUDES)
	$(F90) $(FFLAGS) -c statamod.f90

clean:
	@rm -f $(OBJECTS)
	@cd $(OUTPUTDIR) && rm -f $(OBJECTS:.o=.mod) statamod.a

