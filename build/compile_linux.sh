#Identify Stata location
locate /stata-se > ../params/stata_path.dat
read stata < ../params/stata_path.dat
echo $stata
echo "character(len=*), parameter :: STATA = '$stata'">../src/stata_path.f95

# program to compile compas model
if [ -z $1 ]
 then
gfortran   -fopenmp -O3 -fbounds-check -o ../runtime/compas ../libs/statamod/statamod.f90  ../src/random.f95 ../src/compas.f95   ../libs/dcdflib.a  ../libs/ranlib.a
if [[ $? -ne 0 ]] ; then
    echo 'Compilation fail - exit script'
    exit 1
else
    echo Successful compilation in optimization mode
fi
#ALL
elif [ $1 = "all" ]
	then
for FILE in `ls -1 ../libs/ranlib/*.f90`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit 1
  fi
done
#
rm ranlib.a
ar qc ranlib.a *.o
rm *.o
#
mv ranlib.a ../libs/
#
echo "Library installed as compas/libs/ranlib.a."
for FILE in `ls -1 ../libs/dcdflib.f/src/*.f`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit 1
  fi
done
rm dcdflib.a
ar qc dcdflib.a *.o
rm *.o
#
mv dcdflib.a ../libs/
for FILE in `ls -1 ../libs/compaslib/*.f90`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit 1
  fi
done
#
rm compaslib.a
ar qc compaslib.a *.o
rm *.o
mv compaslib.a ../libs/

echo "Library installed as compas/libs/compaslib.a."

gfortran   -fopenmp -O3 -fbounds-check -o ../runtime/compas ../libs/statamod/statamod.f90 ../src/random.f95 ../src/compas.f95  ../libs/dcdflib.a  ../libs/ranlib.a
if [[ $? -ne 0 ]] ; then
    echo 'Compilation fail - exit script'
    exit 1
else
  echo Successful compilation in production mode and all library
fi

elif [ $1 = "debug" ]
then
# Debug
	gfortran -fopenmp -O0 -g -fbounds-check -o ../runtime/compas ../libs/statamod/statamod.f90  ../src/random.f95../src/random.f95 ../src/compas.f95  ../libs/dcdflib.a  ../libs/ranlib.a
  if [[ $? -ne 0 ]] ; then
      echo 'Compilation fail - exit script'
      exit 1
  else
    echo Successful compilation in debug mode
  fi
# No multithread
elif [ $1 = "nomp" ]
	then
   gfortran  -O3 -fbounds-check -o ../runtime/compas ../libs/statamod/statamod.f90 ../src/random.f95 ../src/compas.f95   ../libs/dcdflib.a  ../libs/ranlib.a
  if [[ $? -ne 0 ]] ; then
      echo 'Compilation fail - exit script'
      exit 1
  else
  echo Successful compilation in no open-mp
  fi
else
ECHO 'Wrong argument. Chose between -all- -debug- -nomp- or none for optimize compilation'
fi
