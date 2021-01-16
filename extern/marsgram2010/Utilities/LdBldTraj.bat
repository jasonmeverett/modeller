gfortran -fbacktrace -std=f95 -Wextra -Wall -pedantic -fbounds-check bldtraj.f90 -o bldtraj.exe
Pause
erase *.o
erase *.mod
pause
