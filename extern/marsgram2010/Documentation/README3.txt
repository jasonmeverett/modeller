                          Input Data Files for:
                          
             NASA Ames Mars General Circulation Model (MGCM),
University of Michigan Mars Thermospheric General Circulation Model (MTGCM),
       Mars Orbiter Laser Altimeter (MOLA) Areoid and Topography Data,
          Global Surface Albedo, MGCM-MTGCM Height Offset Data,
         TES Dust Optical Depth Data, and COSPAR Reference Data


TIME-OF-DAY VARIATION OF MGCM AND MTGCM DATA

ASCII format MGCM and MTGCM data files are provided, each having values for 
amplitudes and phases of diurnal (period = 24 Mars hours) and semi-diurnal 
(period = 12 Mars hours) components.  Generically, the amplitudes and phases 
are:

    A0   = Diurnal mean value of the given parameter

    A1   = Amplitude of the diurnal tide component

    phi1 = Phase (local time in Mars hours) of the diurnal component

    A2   = Amplitude of the semi-diurnal tide component

    phi2 = phase (local time in Mars hours) of the semi-diurnal component

For temperature, wind components, and height of 1.26 nbar level (ZF), data 
files give amplitudes in the same units as those of the parameter (K for 
temperature, m/s for wind, or km for ZF).  For pressure and density, data files 
give amplitudes in units of percent of the mean value (A0).  A0 units for 
pressure are N/m**2, while density A0 units are kg/m**3.

Three slightly different functions for tidal variation versus time of
day are used.  For MGCM and MTGCM temperatures and winds and MTGCM ZF heights, 
function TideX is used, where

    TideX = A0 + A1*Cos((pi/12)*(time - phi1)) + A2*Cos((pi/6)*(time - phi2))
 
and time is the local solar time in Mars hours [Note that units for A1 and A2 are 
same as those for A0 in this function form].  For Mars-GRAM 2001 MGCM and
MTGCM pressure and density data (which have A1 and A2 in units of percent of A0), 
function TideY is used, where
        
    TideY = A0*(1 + 0.01*A1*Cos((pi/12)*(t - phi1)) + 0.01*A2*Cos((pi/6)*(t - phi2)))

TES MTGCM data extend to higher altitude (240 km versus 170 km for Mars-GRAM
2001 MTGCM data).  Consequently tidal amplitudes for density and pressure for
the new data grow to larger values than for the Mars-GRAM 2001 data. To accommodate 
this situation, an alternate model is adopted for pressure and density variation
with time of day, whereby it is the log of pressure and density that are assumed 
to vary as cosine of time of day.  Namely TES year 1 and 2 MTGCM pressure and 
density tides are computed by

    tTideY = A0*((1.0d0 + 0.01d0*A1)**c1)*((1.0d0 + 0.01d0*A2)**c2)
     
where exponents c1 and c2 are given by
    
    c1 = Cos((pi/12)*(time - phi1))        c2 = Cos((pi/6)*(t - phi2))   



NEAR-SURFACE MGCM DATA

Near-surface MGCM data files are provided as follows (in ASCII format):

    sfc00xxy.txt - MGCM temperature data at topographic surface

    sfc05xxy.txt - MGCM temperature and wind data at 5 m height above the surface
 
    sfc30xxy.txt - MGCM temperature and wind data at 30 m height above the surface 

Naming convention for these files is: 

    xx = 03, 10, 30 for Mars-GRAM 2001 data at globally-uniform dust optical 
         depths of 0.3, 1.0, and 3.0 

    xx = y1, y2 for data at time-and space-variable dust optical depths
         as observed during TES mapping years 1 and 2

    y  = version number
                
Each record of these files contains Ls value, latitude, longitude, and tidal
coefficients (A0, A1, phi1, A2, phi2) for temperature, and for Eastward and
Northward wind components (except for ground surface data files, which contain
only temperature information).


MGCM DATA UP TO 80 km ALTITUDE

Files of zonally-averaged MGCM data up to 80 km altitude (above MOLA areoid)
are provided as follows (in ASCII format):

    tpdloxxy.txt - MGCM temperature, pressure, and density data

    uvloxxy.txt  - MGCM Eastward and Northward wind data 

Naming convention for these files is:

    xx = 03, 10, 30 for Mars-GRAM 2001 data at globally-uniform dust optical 
                    depths of 0.3, 1.0, and 3.0.  These data are at 5 km intervals 
                    from 0 km to 80 km above MOLA

    xx = y1, y2 for new data at time-and space-variable dust optical depths
                as observed during TES mapping years 1 and 2.  These data are at 1
                km interval from -5 km to + 10 km above MOLA, and 5 km interval 
                from 10 km to 80 km above MOLA

    y  = version number


For the Mars-GRAM 2001 data sets, each record of the tpdloxx.txt files contains 
Ls value, height, latitude, and tidal coefficients for temperature and pressure.  
Only the A0 coefficient is given for density.  Tidal variations in density are 
computed from those for pressure and temperature by the perfect gas law 
relation.  For the TES mapping year 1 & 2 data, each record of the tpdloxx.txt 
files contains Ls value, height, latitude, and tidal coefficients for
temperature and density.  Only the A0 coefficient is given for pressure.  Tidal 
variations in pressure are computed from those for density and temperature by 
the perfect gas law relation.  

For both Mars-GRAM 2001 and TES mapping year 1 & 2 data, each record of the 
uvloxx.txt files contains Ls value, height, latitude, and tidal coefficients 
for the Eastward and Northward wind components.


MTGCM DATA UP TO 170 or 240 km ALTITUDE

Zonally-averaged MTGCM data are provided (in ASCII format) at 5 km height 
resolution for the altitude range 80 - 170 km for Mars-GRAM 2001 data and 
80 - 240 km for TES mapping year 1 & 2 data. Data sets are provided for different 
levels of solar activity (as characterized by 10.7 cm solar flux, F10.7, as
measured at 1 AU):

    tpdlsxxy.txt - MTGCM temperature, pressure, and density data for low solar
                   activity (F10.7 = 70)

    tpdmsxxy.txt - MTGCM temperature, pressure, and density data for moderate
                   solar activity (F10.7 = 130)

    tpdhsxxy.txt - MTGCM temperature, pressure, and density data for high
                   solar activity (F10.7 = 200; available for TES mapping years 
                   1 & 2 only)

    uvlsxxy.txt  - MTGCM Eastward and Northward wind data for F10.7 = 70 

    uvmsxxy.txt  - MTGCM Eastward and Northward wind data for F10.7 = 130 

    uvhsxxy.txt  - MTGCM Eastward and Northward wind data for F10.7 = 200
                  (available for TES mapping years 1 & 2 only)
                 
Naming convention for these files is:

    xx = 03, 10, 30 for Mars-GRAM 2001 data at globally-uniform dust optical 
                    depths of 0.3, 1.0, and 3.0.

    xx = y1, y2     for data at time-and space-variable dust optical depths
                    as observed during TES mapping years 1 and 2.

    y  = version number

Each record of the tpdlsxxy.txt, tpdmsxxy.txt and tpdhsxxy.txt files contains 
Ls value, height, latitude, and tidal coefficients for temperature, pressure, 
and density.  Because of height variations in molecular weight, tidal
coefficients are retained for all three of these thermodynamic components.
Each record of the uvlsxxy.txt, uvmsxxy.txt and uvhsxxy.txt files contains Ls 
value, height, latitude, and tidal coefficients for the Eastward and Northward
wind components.

Files zfhtlsy.txt, and zfhtmsy (Mars-GRAM 2001 data, F10.7 = 70 or 130) and
zfTESlsy.txt, zfTESmsy.txt, and zfTEShsy.txt (TES mapping years 1 and 2, F10.7
= 70, 130, or 200) provide tidal coefficient information for altitude ZF, the
height of the 1.26 nbar level.  Each record of the ZF files contains dust 
optical depth (or TES mapping year), Ls, latitude, and tidal coefficient values 
(mean ZF value and tidal amplitudes are in km).


MOLA AREOID AND TOPOGRAPHY DATA

MOLA areoid and topography data at 1/2 by 1/2 degree lat-lon resolution
is provided in ASCII file MOLATOPH.TXT  Each line of this file contains
(1) East longitude and (2) latitude at the center of the 1/2 by 1/2 degree grid 
box, (3) grid-box-average radius (meters) to topographic surface, (4) areoid 
radius (meters) = radius to reference constant potential surface, evaluated at 
the center of the grid box, (5) topography = grid-box-average difference 
(meters) between local planetary radius and areoid, analogous to local terrain 
height above sea level for Earth, and (6) Num = number of laser shots averaged 
over the grid box.  MOLA latitudes are planeto-centric.  Longitudes in the MOLA 
input file are with respect to IAU 1991 prime meridian.  A shift (of about 0.24 
degrees) is made automatically within Mars-GRAM 2010, in order to convert to 
longitudes relative to the IAU 2000 prime meridian.


SURFACE ALBEDO DATA

Global surface albedo at 1 by 1 degree lat-lon resolution is given in file 
albedo1.txt.  Each line of this file contains (1) latitude, and (2) West 
longitude at the center of the 1 by 1 degree grid box, and (3) grid-box-average
surface albedo (ratio of surface-reflected solar flux to that incident on the
surface).


GLOBAL MEAN MGCM-MTGCM HEIGHT OFFSET DATA

Height offsets can be used to control the smoothness of the transition at
80 km altitude between MGCM data and MTGCM data.  Height offset options are 
controlled by input parameters zoffset and ibougher (see file README6.txt for
more details about height offsets and these input parameters).  Option ibougher
= 2 causes height offsets to be evaluated from global average height offset 
data given in file hgtoffst.dat.  In the first part of this file, global- 
average offsets (km) are given for Mars-GRAM 2001 data as a function of Ls and  
dust optical depth (0.3, 1.0, and 3.0).  In the second part of this file, 
global-average offsets are given versus Ls for TES mapping years 1 and 2 (y1 or 
y2).


TES DUST OPTICAL DEPTH DATA

Observed average dust optical depth from TES mapping years 1 and 2 are provided
in file TESdust1.txt.  Based on definitions used in doing the MGCM model runs
that provided TES mapping year 1 and 2 data, these periods span from March, 
1999 through February, 2001 and from February, 2001 through December, 2002, 
respectively. In terms of areocentric longitude of the Sun (Ls), TES
mapping years 1 and 2 cover from Ls = 115, through Ls = 360/0, and back to Ls
= 115 the following Mars year. (Conventional Mars years run from Ls = 0 to Ls 
= 360).  There were no global-scale dust storms during TES mapping year 1. 
However, a very intense, global-scale dust storm began near the end of June, 
2001, during TES mapping year 2. Each line of file TESdust1.txt contains TES 
mapping year (1 or 2), Ls, latitude, West longitude, and average TES optical 
depth (expressed as visible-wavelength optical depth, approximately twice the 
optical depth values measured by TES at its 9 micron observing wavelength).  
Data in this file are at a resolution of 5 degrees in Ls, 7.5 degrees in 
latitude, and 9 degrees in longitude.


COSPAR REFERENCE DATA

COSPAR Northern hemisphere mean reference data, as given in Pitts et al., "The 
Mars Atmosphere: Observations and Model Profiles for Mars Missions", NASA 
JSC-24455, 1990, are provided in file COSPAR2.DAT.  Each line of this file 
contains height (km), temperature (K), pressure (mbar), and density (gm/cm**3).


CONVERSION OF ASCII DATA TO BINARY

Source code is provided for a program (makebin.f90) to read the ASCII format 
MGCM and MTGCM data files and the TES dust optical depth data file and write 
these out in binary format.  After this ASCII-to-binary conversion is once 
completed, subsequent reading of the binary format files significantly 
shortens the time required to initialize Mars-GRAM on each run.

Source code is also provided for the ASCII-to-binary conversion programs 
(READTOPO.f90 and readalb.f90) to covert MOLA topography data and albedo data
files.

ZF data files, height offset file, and COSPAR reference data file are 
sufficiently small that they can be read in ASCII format, so no conversion 
to binary is required for these files.

Note:  For PC users, all necessary binary version files are supplied and
       the ASCII-to-binary conversion programs (readalb, readtopo, and makebin)
       do not have to be run. 

To run Mars-GRAM, the binary-version MGCM and MTGCM files, together with the
ASCII-format ZF data files, must be in the directory whose pathname is given
by input parameter GCMDIR in the NAMELIST input file.  The binary version 
albedo, MOLA topography, and TES dust optical depth data files, together with
ASCII-format files COSPAR2.DAT and hgtoffst.dat must be in the directory 
whose pathname is given by input parameter DATADIR in the NAMELIST input file.
In this distribution of Mars-GRAM, the pathname for DATADIR and GCMDIR are the
same, with all necessary files together in the same directory (binFiles).  


OPTIONAL TRAJECTORY INPUT FILES

If the number of positions to be calculated (NPOS) is set to 0, an optional 
trajectory input file is read from a file with the name given by the input parameter 
TRAJFILE. Each line of the trajectory file consists of: (1) time, in
seconds past the start time specified in the NAMELIST input, (2) height, in
km, (3) latitude in degrees, and (4) longitude in degrees.  Heights are
relative to MOLA areoid or reference ellipsoid, as set by the input parameter
MOLAhgts. Latitudes are planeto-centric or planeto-graphic, as set by the input
parameter ipclat.  Longitudes are East or West, as set by the input parameter
LonEW.  Any additional reference information included on each line (e.g. 
orbit number, measured density, etc.) is ignored.  Trajectory positions
in these files do not have to be at small time or space steps.  For example,
a trajectory file may consist of successive periapsis times and positions
for a simulated (or observed) aerobraking operation (see README5.txt).  
Trajectory files may also contain arrays of locations used for computing 
height-latitude cross sections or latitude-longitude cross sections.  Such
trajectory input files can be as built by program BLDTRAJ.f90 (see README4.txt).


OPTIONAL AUXILIARY PROFILE INPUT FILE

As an option, data read from an auxiliary profile may be used to replace
data from the MGCM and MTGCM data files.  This option is controlled by
setting the input parameters profile, profnear, and proffar in the NAMELIST input
file (see further explanation in README6.txt file). Each line of the 
auxiliary profile input file consists of: (1) height, in km, (2) latitude,
in degrees, (3) longitude, in degrees, (4) temperature, in K, (5) pressure, 
in N/m**2, (6) density, in kg/m**3, (7) Eastward wind, in m/s, and (8)
Northward wind, in m/s.  Heights are relative to MOLA areoid or reference 
ellipsoid, as set by the input parameter MOLAhgts. Latitudes are planeto-
centric or planeto-graphic, as set by the input parameter ipclat.  Longitudes 
are East or West, as set by the input parameter LonEW. MGCM/MTGCM temperature,
pressure, and density data are used if any of the profile inputs for 
temperature, pressure, or density are 0.  MGCM/MTGCM winds are used if 
BOTH wind components are 0 on the profile file. A sample auxiliary profile
file (named profiledata.txt) is provided in the IO files directory.
