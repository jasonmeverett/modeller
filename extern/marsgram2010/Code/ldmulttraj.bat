gfortran -c -Werror -fbacktrace -std=f95 -Wextra -Wall -pedantic-errors -fbounds-check Cfiles_M10_C.F90
gfortran -c -Werror -fbacktrace -std=f95 -Wextra -Wall -pedantic-errors -fbounds-check Ifiles_M10_I.F90
gfortran -c -Werror -fbacktrace -std=f95 -Wextra -Wall -pedantic-errors -fbounds-check MARSsubs_M10.F90
gfortran -c -Werror -fbacktrace -std=f95 -Wextra -Wall -pedantic-errors -fbounds-check TESsubs_M10.F90
gfortran -c -Werror -fbacktrace -std=f95 -Wextra -Wall -pedantic-errors -fbounds-check SETUP_M10.F90
gfortran -c -Werror -fbacktrace -std=f95 -Wextra -Wall -pedantic-errors -fbounds-check wrapper_M10.F90
pause
gfortran  multtraj_M10.f90 Cfiles_M10_C.o Ifiles_M10_I.o MARSSUBS_M10.o TESsubs_M10.o SETUP_M10.o wrapper_M10.o -o multtraj_M10.exe
erase *.o
erase *.mod
copy multtraj_M10.exe D:\MARS\Mars2010\Release1.0_Nov10\Executables
pause
erase multtraj_M10.exe
pause

