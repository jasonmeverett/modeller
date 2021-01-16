Dear Mars-GRAM user,

Mars-GRAM 2010 is now publicly available from the NASA Marshall Space Flight 
Center (see contact information, below).

The Mars-GRAM 2010 Version 1.0 directories contain the following directories and files:


Directory Code: SOURCE CODE FOR MARS-GRAM STAND-ALONE AND EXAMPLE TRAJECTORY PROGRAMS


  Cfiles_M10_C.f90 - Mars-GRAM common block modules

  Ifiles_M10_I.f90 - Mars-GRAM interface modules

  marsgram_M10.f90 - source code for the "stand-alone" version main program

  marssubs_M10.f90 - subroutines used by marsgram_M10, dumytraj_M10 and multtraj_M10

  setup_M10.f90    - setup routines used by marsgram_M10, dumytraj_M10 and multtraj_M10

  TESsubs_M10.f90  - subroutines to read and interpolate TES mapping year 1 & 2
                     data; used by marsgram_M10, dumytraj_M10, and multtraj_M10

  dumytraj_M10.f90 - Source code for main driver of dumytraj example trajectory program.
                     To be compiled with marssubs_M10.f90, TESsubs_M10.f90,setup_M10.f90, 
                     and wrapper_M10.f90, this program illustrates the use of Mars-GRAM as a 
                     subroutine in trajectory programs or orbit propagator programs.

  multtraj_M10.f90 - Source code for main driver of multtraj example trajectory program
                     that computes multiple trajectories during one run.  This is
                     to be compiled with marssubs_M10.f90, TESsubs_M10.f90, setup_M10.f90, 
                     and wrapper_M10.f90, this program illustrates the use of Mars-GRAM as 
                     a subroutine in trajectory programs or orbit propagator 
                     programs that compute positions of multiple spacecraft during 
                     one run.

  wrapper_M10.f90  - Wrapper routine (subroutine marstraj), called by dumytraj_M10 
                     and multtraj_M10, for including in user's trajectory code
                     (along with marssubs, TESsubs, and setup routines, called by wrapper)

  ldMarsGRAM.bat   - Example macro to compile and link the Mars-GRAM stand-alone program

  lddumytraj.bat   - Example macro to compile and link the dummy trajectory program dumytraj_M10

  ldmulttraj.bat   - Example macro to compile and link the dummy trajectory program multtraj_M10


Directory Utilities: SOURCE CODE FOR MARS-GRAM UTILITY PROGRAMS 


  makebin.f90    - program to read ASCII version MGCM and MTGCM data files 
                   and write out binary version (for faster reading on user 
                   machine). Reads near-surface MGCM data (ground surface, 5 
                   m, and 30 m above surface) for both Mars-GRAM 2001 data
                   (dust optical depths 0.3, 1, and 3) and new TES mapping
                   year 1 and 2 data.  Reads 0 to 80 km Mars-GRAM 2001 MGCM 
                   data and -5 to 80 km TES mapping year 1& 2 MGCM data.
                   Reads 80 to 170 km Mars-GRAM 2001 MTGCM data and 80 to
                   240 km TES mapping year 1 & 2 MTGCM data. Also reads 
                   optical depth file (tau versus lat-lon-Ls) for TES mapping 
                   years 1 & 2.  Binary conversion process is required (once) 
                   before initial running of Mars-GRAM. See file README3.txt 
                   for file descriptions.

  readalb.f90    - program to read ASCII albedos and convert to binary

  READTOPO.f90   - program to read ASCII MOLA topography and convert to binary

  BLDTRAJ.f90    - program to build pseudo-trajectory file for use in Mars-
                   GRAM to compute output for maps or cross-sections

  finddate.f90   - utility to find Earth date/time for desired Ls or Mars time

  julday.f90     - utility to find Julian day from year, month, day input

  marsrad.f90    - uses Mars-GRAM output to compute various solar (shortwave)
                   and thermal (longwave) fluxes at the surface and the top of
                   the atmosphere

  radtraj.f90    - special "trajectory" building program to compute vertical
                   profiles at lat-lon, lat-time, or lon-time cross sections,
                   for input to Mars-GRAM runs to produce output for input
                   to marsrad radiation calculations

  LdBldTraj.bat  - Example macro to compile and link program BLDTRAJ

  LdFindDate.bat - Example macro to compile and link program finddate

  LdJulDay.bat   - Example macro to compile and link program julday

  LdMakebin.bat  - Example macro to compile and link program makebin

  LdMarsRad.bat  - Example macro to compile and link program marsrad

  LdRadTraj.bat  - Example macro to compile and link program radtraj

  LdReadalb.bat  - Example macro to compile and link program readalb

  LdReadtopo.bat - Example macro to compile and link program READTOPO


Directory Executables:  PC EXECUTABLE FILES


PC executables for Mars-GRAM standalone program, example trajectory programs
and the various Utilities programs.  Executables for other platforms must be
compiled from the source code.


Directory txtFiles: TEXT FORMAT MGCM AND MTGCM DATA FILES


  Data files (ASCII format) for MGCM near-surface data:


    sfc00xxy.txt - MGCM boundary layer data at ground surface for Mars-GRAM 
                   2001 data at dust optical depths 0.3, 1.0, and 3.0 (xx = 
                   03, 10, 30) and for TES mapping years 1 & 2 (xx =
                   y1, y2), and version number y

    sfc05xxy.txt - MGCM boundary layer data at 5 m height for Mars-GRAM 2001
                   data at dust optical depths 0.3, 1.0, and 3.0 (xx = 03, 
                   10, 30) and for TES mapping years 1 & 2 (xx = y1,
                   y2), and version number y

    sfc30xxy.txt - MGCM boundary layer data at 30 m height for Mars-GRAM 2001
                   data at dust optical depths 0.3, 1.0, and 3.0 (xx = 03, 
                   10, 30) and for TES mapping years 1 & 2 (xx = y1,
                   y2), and version number y


  MGCM data files (ASCII format) for 0 to 80 km or -5 to 80 km:


    tpdloxxy.txt - Mars-GRAM 2001 MGCM 0-80 km temperature, pressure, and  
                   density data for 3 dust optical depths (xx = 03, 10, 30), 
                   version number y, or TES mapping year 1 & 2 data (xx =
                   y1, y2) for -5 to 80 km, version number y

    uvloxxy.txt  - Mars-GRAM 2001 MGCM 0-80 km EW wind and NS wind data
                   for 3 dust optical depths (xx = 03, 10, 30), version number 
                   y, or TES mapping year 1 & 2 data (xx = y1, y2) for -5 
                   to 80 km, version number y
  

  Data files (ASCII format) for MTGCM 80-170 (or 240) km data:


    tpdlsxxy.txt - Mars-GRAM 2001 MTGCM 80-170 km temperature, pressure, and 
                   density data for 3 dust optical depths (xx = 03, 10, 30),  
                   version number y, for solar activity F10.7 = 70; or MTGCM
                   data for 80 to 240 km for TES mapping years 1 & 2 (xx = 
                   y1, y2)
  
    tpdmsxxy.txt - Mars-GRAM 2001 MTGCM 80-170 km temperature, pressure, and 
                   density data for 3 dust optical depths (xx = 03, 10, 30),  
                   version number y, for solar activity F10.7 = 130; or MTGCM
                   data for 80 to 240 km for TES mapping years 1 & 2 (xx = 
                   y1, y2)
  
    tpdhsxxy.txt - MTGCM 80 to 240 km temperature, pressure, and density data 
                   for TES mapping years 1 & 2 (xx = y1, y2), version number 
                   y, for solar activity F10.7 = 200 

    uvlsxxy.txt  - MTGCM 80-170 km EW wind and NS wind data for 3 dust optical
                   depths xx, version number y, for solar activity F10.7 = 70;
                   or 80 to 240 km for TES mapping years 1 & 2 (xx = y1, y2)

    uvmsxxy.txt  - MTGCM 80-170 km EW wind and NS wind data for 3 dust optical
                   depths xx, version number y, for solar activity F10.7 = 130;
                   or 80 to 240 km for TES mapping years 1 & 2 (xx = y1, y2)

    uvhsxxy.txt  - MTGCM 80 to 240 km EW wind and NS wind data for TES mapping 
                   years 1 & 2 (xx = y1, y2) version number y, for solar 
                   activity F10.7 = 200


  Other text files, which must be converted to binary format
 

    albedo1.txt  - global surface albedo at 1 by 1 degree lat-lon resolution;
                   must be converted to binary with program readalb.f90

    MOLATOPH.TXT - MOLA areoid and surface topography at 1/2 by 1/2 degree
                   lat-lon resolution; must be converted to binary with 
                   program READTOPO.f90 

    TESdust1.txt - TES dust optical depth data versus Ls, lat, lon for TES mapping
                   years 1 and 2;  must be converted to binary with program
                   makebin.f90
  

Directory binFiles:  BINARY FORMAT MGCM AND MTGCM DATA FILES PLUS OTHER FILES


  sfc*.bin     - Binary format MGCM surface data files 

  tpdlo*.bin   - Binary format MGCM thermodynamic files
 
  uvlo*.bin    - Binary format MGCM wind files 

  tpdxs*.bin   - Binary format MTGCM thermodynamic files (x=l,m,or h for low,
                 medium, or high solar activity)

  uvxs*.bin    - Binary format MTGCM wind files (x=l,m,or h for low, medium, 
                 or high solar activity)

  albedo1.bin  - Binary format albedo file

  molatoph.bin - Binary format topography file 

  TESdust1.bin - Binary format dust file

  COSPAR2.DAT  - Text format data file for COSPAR reference model atmosphere
  
  hgtoffst.dat - Text format height offset file

  zfhtlsy.txt  - Height ZF of 1.26 nbar level for all dust optical depths,
                 version number y, for solar activity F10.7 = 70 for Mars-
                 GRAM 2001 MTGCM data

  zfhtmsy.txt  - Height ZF of 1.26 nbar level for all dust optical depths,
                 version number y, for solar activity F10.7 = 130 for Mars-
                 GRAM 2001 MTGCM data

  zfTESlsy.txt - Height ZF of 1.26 nbar level for TES mapping years 1 & 2,
                 MTGCM data version y, for solar activity F10.7 = 70

  zfTESmsy.txt - Height ZF of 1.26 nbar level for TES mapping years 1 & 2,
                 MTGCM data version y, for solar activity F10.7 = 130
  
  zfTEShsy.txt - Height ZF of 1.26 nbar level for TES mapping years 1 & 2,
                 MTGCM data version y, for solar activity F10.7 = 200

Note:  For PC users, binary version MGCM files, MTGCM files, and albedo1.bin, 
       molatoph.bin, and TESdust1.bin are supplied and the readalb, readtopo, and
       makebin programs do not have to be run.  For more details, see README3.txt.


Directory IOfiles: SAMPLE INPUT AND OUTPUT FILES


  inputstd0.txt   - commented test input file for reference case using Mapping
                    year 0 MGCM and MTGCM input data 

  inputstd1.txt   - commented test input file for reference case using TES 
                    mapping year 1 MGCM and MTGCM input data
 
  inputstd2.txt   - commented test input file for reference case using TES 
                    mapping year 2 MGCM and MTGCM input data
   
  ListMapYr0.txt  - list output file for inputstd0.txt reference case

  ListMapYr1.txt  - list output file for inputstd1.txt reference case

  ListMapYr2.txt  - list output file for inputstd2.txt reference case

  profiledata.txt - Sample auxiliary profile

  dumytraj.bat    - Example execute macro for dummy trajectory program dumytraj_M10

  marsgram0.bat   - Example execute macro for Mapping year 0 reference input

  marsgram1.bat   - Example execute macro for Mapping year 1 reference input

  marsgram2.bat   - Example execute macro for Mapping year 2 reference input

  multtraj.bat    - Example execute macro for dummy trajectory program multtraj_M10


Directory Documentation: TEXT AND PDF FILES DOCUMENTING SPECIAL PROGRAM FEATURES


  headers.txt         - list of output files and file header definitions

  marsfix.html        - list of code changes since Mars-GRAM 2010 Version 1.0 (Nov 2010)

  marshist.txt        - history file summarizing various Mars-GRAM versions and changes

  Mars2000.pdf        - A PDF version of the Mars-GRAM 2000 user guide

  Mars2001.pdf        - A PDF version of the Mars-GRAM 2001 user guide

  Mars2010.pdf        - A PDF Version of the Mars-GRAM 2010 user guide (NASA/TM—2014–217499)
   
  MarsEnvironment.pdf - A PDF version of the Mars Transportation Environment 
                        Definition Document (NASA/TM-2001-210935)
                 
  README1.txt         - this general program introduction file

  README2.txt         - discussion of dumytraj_M10.f90 dummy trajectory program

  README3.txt         - discussion of MGCM and MTGCM input data files, including
                        programs to convert ASCII files (provided) into binary 
                        files (for faster running of Mars-GRAM)

  README4.txt         - discussion of auxiliary programs provided, including 
                        marsrad solar and thermal radiative transfer program
  
  README5.txt         - discussion of Mars-GRAM sample input and output files,
                        including reference test case

  README6.txt         - description of some special program features not fully 
                        covered in other README files

  xycodes.txt         - list of x-y plot codes (also given below)


Plotable output files can be generated with data given versus
several selected parameters.  Generation of LIST file output and
plotable output files is controlled by the value of iup on input.
For Mars-GRAM 2010, a number of plotable output files are generated,
each containing several parameters suitable for plotting.  These 
plotable files have headers to help identify parameters in the files.
File names and definitions of headers are given in the file headers.txt.

Plotable x and y parameters and their code values (set by input variables
NVARX and NVARY) are as follows:

 Code              Parameter
 ----  -------------------------------------------------------------------
  1    Planeto-Centric Height (above local MOLA areoid, km)
  2    Planeto-Centric Height (above local MOLA topographic surface, km)
  3    Planeto-Centric Latitude (deg.)
  4    Longitude (deg.) West+ if LonEW = 0, East+ if LonEW = 1
  5    Time from start (Earth seconds)
  6    Time from start (Martian Sols)
  7    Areocentric Longitude of Sun, Ls (deg.)
  8    Local True Solar Time (Mars hours)
  9    Pressure (mb)
 10    Pressure Height (km) [-H*log(Pres/PresSurf) = -H*log(sigma)]
 11    Sigma coordinate [sigma = Pressure/(Pressure at Surface)]
 12    Planeto-Centric Height (km) above reference ellipsoid
 13    Planeto-Graphic Height (km) above reference ellipsoid
 14    Planeto-Graphic Latitude (deg.)
 15    Longitude in range -180 to +180 (East or West, controlled by LonEW)

 
To set up Mars-GRAM from the distribution CD, copy files to the selected
drive/directory. Modify values of input namelist variables DATADIR and GCMDIR
in files inputstd0.txt, inputstd1.txt, and inputstd2.txt, to reflect the 
selected path to the binary file subdirectory 'binFiles'. 

 
To compile marsgram_M10.f90, dumytraj_M10.f90, and multtraj_M10.f90 under UNIX, to 
produce executable files marsgram_M10.x, dumytraj_M10.x, and multtraj_M10.exe, 
you can use the commands:

f90 Cfiles_M10_C.f90 Ifiles_M10_I.f90 marsgram_M10.x marsgram_M10.f90 marssubs_M10.f90 /
 setup_M10.f90 TESsubs_M10.f90
mv a.out marsgram_M10.x
erase *.o
erase *.mod
 
f90 Cfiles_M10_C.f90 Ifiles_M10_I.f90 dumytraj_M10.f90 marssubs_M10.f90 setup_M10.f90 /
 TESsubs_M10.f90 wrapper_M10.f90
mv a.out dumytraj_M10.x
erase *.o
erase *.mod
 
f90 Cfiles_M10_C.f90 Ifiles_M10_I.f90 multtraj_M10.f90 marssubs_M10.f90 setup_M10.f90 /
 TESsubs_M10.f90 wrapper_M10.f90
mv a.out multtraj_M10.x
erase *.o
erase *.mod


To compile marsgram_M10.f90, dumytraj_M10.f90, and multtraj_M10.f90 under PC-DOS (for 
example, with gfortran), to produce executable files marsgram_M10.exe, dumytraj_M10.exe, 
and multtraj_M10.exe, you can use the example compile-and-link macros in the Code 
director.


To compile the auxiliary programs bldtraj.f90, finddate.f90, marsrad.f90,
julday.f90, or radtraj.f90, or the binary conversion programs makebin.f90, 
readalb.f90, or readtopo.f90 under UNIX, just use the FORTRAN compile statement for 
the specific auxiliary program source code file, i.e.

f90 -o auxiliary.x auxiliary.f90

or for PC-DOS, use the compile-and-link macros in the Utilities directory.


For technical, programmatic or policy questions, please contact

Hilary Justh
NASA MSFC EV44
Marshall Space Flight Center, AL 35812
phone: (256)-544-3694
fax:   (256)-544-3060
e-mail: hilary.l.justh@nasa.gov
