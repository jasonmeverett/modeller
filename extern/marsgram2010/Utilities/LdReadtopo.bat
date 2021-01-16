gfortran -fbacktrace -std=f95 -Wextra -Wall -pedantic -fbounds-check readtopo.f90 -o readtopo.exe
Pause
erase *.o
erase *.mod
pause
