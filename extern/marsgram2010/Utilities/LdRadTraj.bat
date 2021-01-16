gfortran -fbacktrace -std=f95 -Wextra -Wall -pedantic -fbounds-check radtraj.f90 -o radtraj.exe
Pause
erase *.o
erase *.mod
pause
