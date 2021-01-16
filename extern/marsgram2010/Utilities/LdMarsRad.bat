gfortran -fbacktrace -std=f95 -Wextra -Wall -pedantic -fbounds-check marsrad.f90 -o marsrad.exe
Pause
erase *.o
erase *.mod
pause
