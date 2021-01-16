gfortran -fbacktrace -std=f95 -Wextra -Wall -pedantic -fbounds-check finddate.f90 -o finddate.exe
Pause
erase *.o
erase *.mod
pause
