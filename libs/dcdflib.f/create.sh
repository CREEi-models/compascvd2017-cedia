# used to create library dcdflib.a
gfortran -c src/*.f 
ar r ../dcdflib.a *.o
rm *.o
