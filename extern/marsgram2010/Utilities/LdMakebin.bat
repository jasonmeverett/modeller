gfortran -fbacktrace -std=f95 -Wextra -Wall -pedantic -fbounds-check makebin.f90 -o makebin.exe
Pause
erase *.o
erase *.mod
pause
