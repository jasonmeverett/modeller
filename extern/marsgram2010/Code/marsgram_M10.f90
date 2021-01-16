      PROGRAM MarsGRAM_M10                                                        !MGRM  1
!-----------------------------------------------                                  !MGRM  2
!   M o d u l e s                                                                 !MGRM  3
!-----------------------------------------------                                  !MGRM  4
      USE vast_kind_param, ONLY:  DOUBLE                                          !MGRM  5
!     Mars Global Reference Atmospheric Model 2010                                !MGRM  6
!                  (Version 1.0) - Nov, 2010                                      !MGRM  7
!                                                                                 !MGRM  8
!     A program to evaluate density, temperature, pressure and wind               !MGRM  9
!     components at any given time and position in the atmosphere                 !MGRM 10
!     of Mars                                                                     !MGRM 11
!                                                                                 !MGRM 12
!     Mars-GRAM 2010 is now publicly available from the NASA Marshall             !MGRM 16
!     Space Flight Center.                                                        !MGRM 17
!                                                                                 !MGRM 18
!.....................................................................            !MGRM 19
!                                                                                 !MGRM 20
!...Switches:                                                                     !MGRM 22
!-----------------------------------------------                                  !MGRM 23
!   I n t e r f a c e   B l o c k s                                               !MGRM 24
!-----------------------------------------------                                  !MGRM 25
      USE setup_M10_I                                                             !MGRM 26
      USE randinit_M10_I                                                          !MGRM 27
      USE datastep_M10_I                                                          !MGRM 28
      IMPLICIT NONE                                                               !MGRM 29
!-----------------------------------------------                                  !MGRM 30
!   L o c a l   V a r i a b l e s                                                 !MGRM 31
!-----------------------------------------------                                  !MGRM 32
      INTEGER :: EOF, IUSTDOUT, IUSTDIN, MAXNUM, NR1, NMONTE, LONEW, IULIST,    & !MGRM 33
         IERT, IUTC, NPROF, IUPDATE, J, NUMWAVE, I                                !MGRM 34
      REAL(DOUBLE) :: DAY0, CORLIM, CORLMIN, PERTSTEP, MARSAU, ALS, SZA, OWLT,  & !MGRM 35
         SUNLAT, SUNLON, NSWIND, NSPERT, CHGT, CLAT, CLON, CSEC, RHOD, RHOU,    & !MGRM 36
         RHOV, RHOW, DELHGT, DELLAT, DELLON, DELTIME, HGTASFC, PROFNEAR,        & !MGRM 37
         PROFFAR, FHGT, FLAT, FLON, FSEC, FDHGT, FDLAT, FDLON, FDTIME, TEMP,    & !MGRM 38
         PRES, DENSLO, DENS, DENSHI, DENSP, EWWIND, EWPERT, VWPERT, HRHO, HPRES & !MGRM 39
         , DENSTOT, TLOCAL                                                        !MGRM 40
      CHARACTER(LEN=99) :: INPUTFL                                                !MGRM 41
!-----------------------------------------------                                  !MGRM 42
!...  Set unit numbers for standard (screen) input and output                     !MGRM 43
      IUSTDOUT = 6                                                                !MGRM 44
      IUSTDIN = 5                                                                 !MGRM 45
!...  Read file name for NAMELIST format input file                               !MGRM 46
      WRITE (IUSTDOUT, *) ' Enter file name for NAMELIST input'                   !MGRM 47
      READ (IUSTDIN, 10) INPUTFL                                                  !MGRM 48
   10 FORMAT(A)                                                                   !MGRM 49
!                                                                                 !MGRM 50
!.....................................................................            !MGRM 51
!     Input is read by Setup_M10 routine in NAMELIST form.  Example:              !MGRM 52
!                                                                                 !MGRM 53
! $INPUT_M10                                                                      !MGRM 54
!  LSTFL    = 'LIST.txt'               ! List file name (CON for                  !MGRM 55
!                                      !   console listing)                       !MGRM 56
!  OUTFL    = 'OUTPUT.txt'             ! Output file name                         !MGRM 57
!  TRAJFL   = 'TRAJDATA.txt'           ! (Optional) Trajectory input              !MGRM 58
!                                      !   file name                              !MGRM 59
!  profile  = 'null'                   ! (Optional) auxiliary profile             !MGRM 60
!                                      !   input file name                        !MGRM 61
!  WaveFile = 'null'                   ! (Optional) file for time-                !MGRM 62
!                                      !   dependent wave model data              !MGRM 63
!  DATADIR  = 'C:\Mars\Mars2010\'      ! Directory for COSPAR data and            !MGRM 64
!                                      !   topographic height data                !MGRM 65
!  GCMDIR   = 'C:\Mars\Mars2010\'      ! Directory for GCM binary data            !MGRM 66
!                                      !   files                                  !MGRM 67
!  IERT     = 1        ! 1 for time input as Earth-Receive time (ERT)             !MGRM 68
!                      !   or 0 Mars-event time (MET)                             !MGRM 69
!  IUTC     = 1        ! 1 for time input as Coordinated Universal Time           !MGRM 70
!                      !   (UTC), or 0 for Terrestrial (Dynamical) Time           !MGRM 71
!                      !   (TT)                                                   !MGRM 72
!  MONTH    = 7        ! month of year                                            !MGRM 73
!  MDAY     = 20       ! day of month                                             !MGRM 74
!  MYEAR    = 76       ! year (4-digit; 1970-2069 can be 2-digit)                 !MGRM 75
!  NPOS     = 21       ! max # positions to evaluate (0 = read data               !MGRM 76
!                      !                   from trajectory input file)            !MGRM 77
!  IHR       = 0       ! Hour of day (ERT or MET, controlled by IERT              !MGRM 78
!                      !   and UTC or TT, controlled by IUTC)                     !MGRM 79
!  IMIN      = 0       ! minute of hour (meaning controlled by IERT and           !MGRM 80
!                      !   IUTC)                                                  !MGRM 81
!  SEC       = 0.0     ! seconds of minute (meaning controlled by IERT            !MGRM 82
!                      !   and IUTC).  IHR:IMIN:SEC is time for initial           !MGRM 83
!                      ! position to be evaluated                                 !MGRM 84
!  LonEW    = 0        ! 0 for input and output West longitudes                   !MGRM 85
!                      !   positive; 1 for East longitudes positive               !MGRM 86
!  Dusttau  = 0.3      ! Optical depth of background dust level (no               !MGRM 87
!                      !   time-developing dust storm, just uniformly             !MGRM 88
!                      !   mixed dust), 0.1 to 3.0, or use 0 for                  !MGRM 89
!                      !   assumed seasonal variation of background               !MGRM 90
!                      !   dust                                                   !MGRM 91
!  Dustmin  = 0.3      ! Minimum seasonal dust tau if input Dusttau=0             !MGRM 92
!                      !   (>=0.1)                                                !MGRM 93
!  Dustmax  = 1.0      ! Maximum seasonal dust tau if input Dusttau=0             !MGRM 94
!                      !   (<=1.0)                                                !MGRM 95
!  Dustnu   = 0.003    ! Parameter for vertical distribution of dust              !MGRM 96
!                      !   density (Haberle et al., J. Geophys. Res.,             !MGRM 97
!                      !   104, 8957, 1999)                                       !MGRM 98
!  Dustdiam = 5.0      ! Dust particle diameter (micrometers, assumed             !MGRM 99
!                      !   monodisperse)                                          !MGRM100
!  Dustdens = 3000.    ! Dust particle density (kg/m**3)                          !MGRM101
!  ALS0     = 0.0      ! starting Ls value (degrees) for dust storm               !MGRM102
!                      !                                   (0 = none)             !MGRM103
!  INTENS   = 0.0      ! dust storm intensity (0.0 - 3.0). Storm                  !MGRM104
!                      !   intensity (>0) is added to Dusttau.                    !MGRM105
!  RADMAX   = 0.0      ! max. radius (km) of dust storm (0 or                     !MGRM106
!                      !                             >10000 = global)             !MGRM107
!  DUSTLAT  = 0.0      ! Latitude (degrees) for center of dust storm              !MGRM108
!  DUSTLON  = 0.0      ! Longitude (degrees) (West positive if LonEW =            !MGRM109
!                      !   0 ,or East positive if LonEW = 1) for center           !MGRM110
!                      !   of dust storm                                          !MGRM111
!  MapYear  = 1        ! 1 or 2 for TES mapping year 1,2 GCM input data           !MGRM112
!                      !   or 0 for Mars-GRAM 2001 GCM input data sets            !MGRM113
!  F107     = 68.0     ! 10.7 cm solar flux (10**-22 W/cm**2 at 1 AU)             !MGRM114
!  NR1      = 1001     ! starting random number (0 < NR1 < 30000)                 !MGRM117
!  NVARX    = 1        ! x-code for plotable output (1=hgt above MOLA             !MGRM118
!                      !   areoid). See file xycodes.txt                          !MGRM119
!  NVARY    = 0        ! y-code for 2-D plotable output (0 for 1-D                !MGRM120
!                      !                                       plots)             !MGRM121
!  LOGSCALE = 0        ! 0=regular SI units, 1=log-base-10 scale,                 !MGRM122
!                      !    2=percentage deviations from COSPAR model,            !MGRM123
!                      !    3=SI units with density in kg/km**3                   !MGRM124
!  FLAT     = 22.0     ! initial latitude (N positive), degrees                   !MGRM125
!  FLON     = 48.0     ! initial longitude (West positive if LowEW = 0            !MGRM126
!                      !   or East positive if LonEW = 1), degrees                !MGRM127
!  FHGT     = -5.0     ! initial height (km) (<=-10 means use surface)            !MGRM128
!  MOLAhgts = 1        ! 1 for input heights relative to MOLA areoid;             !MGRM129
!                      !   otherwise input heights are relative to                !MGRM130
!                      !   reference ellipsoid                                    !MGRM131
!  hgtasfcm = 0.0      ! height above surface (0-4500 m); use if FHGT             !MGRM132
!                      !    <= -10. km                                            !MGRM133
!  zoffset  = 5.0      ! constant height offset (km) for MTGCM data               !MGRM134
!                      !  or constant part of Ls-dependent (Bougher)              !MGRM135
!                      !  height offset (0.0 means no constant offset).           !MGRM136
!                      !  Positive offset increases density, negative             !MGRM137
!                      !  offset decreases density.                               !MGRM138
!  ibougher = 2        ! 0 for no Ls-dependent (Bougher) height offset            !MGRM139
!                      !  term; 1 means add Ls-dependent (Bougher)                !MGRM140
!                      !  term, -A*Sin(Ls) (km), to constant term                 !MGRM141
!                      !  (zoffset) [A=2.5 for MapYear=0 or 0.5 for               !MGRM142
!                      !  MapYear>0]; 2 means use global mean height              !MGRM143
!                      !  offset from data file hgtoffst.dat; 3 means             !MGRM144
!                      !  use daily average height offset at local                !MGRM145
!                      !  position; 4 means use height offset at                  !MGRM146
!                      !  current time and local position. Value of               !MGRM147
!                      !  zoffset is ignored if ibougher = 2, 3, or 4             !MGRM148
!  DELHGT   = 10.0     ! height increment (km) between steps                      !MGRM149
!  DELLAT   = 0.0      ! latitude increment (deg) between steps                   !MGRM150
!  DELLON   = 0.0      ! Longitude increment (deg) between steps (West            !MGRM151
!                      !   positive if LonEW = 0, East positive if                !MGRM152
!                      !   LonEW = 1)                                             !MGRM153
!  DELTIME  = 0.0      ! time increment (sec) between steps                       !MGRM154
!  deltaTEX = 0.0      ! adjustment for exospheric temperature (K)                !MGRM155
!  profnear = 0.0      ! Lat-lon radius within which weight for                   !MGRM156
!                      !   auxiliary profile is 1.0                               !MGRM157
!  proffar  = 0.0      ! Lat-lon radius beyond which weight for                   !MGRM158
!                      !   auxiliary profile is 0.0                               !MGRM159
!  rpscale  = 1.0      ! random density perturbation scale factor (0-2)           !MGRM160
!  rwscale  = 1.0      ! random wind perturbation scale factor (>=0)              !MGRM161
!  wlscale  = 1.0      ! scale factor for perturbation wavelengths                !MGRM162
!                      !   (0.1-10)                                               !MGRM163
!  wmscale  = 1.0      ! scale factor for mean winds                              !MGRM164
!  blwinfac = 1.0      ! scale factor for boundary layer slope winds              !MGRM165
!                      !   (0 = none)                                             !MGRM166
!  NMONTE   = 1        ! number of Monte Carlo runs                               !MGRM167
!  iup      = 11       ! 0 for no LIST and graphics output, unit number           !MGRM168
!                      !  for LIST file otherwise                                 !MGRM169
!  corlmin   = 0.0     ! Minimum relative step size for perturbations             !MGRM170
!                      !  (0.0 - 1.0)                                             !MGRM171
!  WaveA0   = 1.0      ! Mean term of longitude-dependent wave                    !MGRM172
!                      !   multiplier for density                                 !MGRM173
!  WaveDate = 0.0      ! Julian day for (primary) peak(s) of wave                 !MGRM174
!                      !   (0 for no traveling component)                         !MGRM175
!  WaveA1   = 0.0      ! Amplitude of wave-1 component of longitude-              !MGRM176
!                      !   dependent wave multiplier for density                  !MGRM177
!  Wavephi1 = 0.0      ! Phase of wave-1 component of longitude-                  !MGRM178
!                      !   dependent wave multiplier (longitude, with             !MGRM179
!                      !   West positive if LonEW = 0, East positive              !MGRM180
!                      !   if LonEW = 1)                                          !MGRM181
!  phi1dot  = 0.0      ! Rate of longitude movement (degrees per day)             !MGRM182
!                      !   for wave-1 component (Westward positive if             !MGRM183
!                      !   LonEW = 0, Eastward positive if LonEW = 1)             !MGRM184
!  WaveA2   = 0.0      ! Amplitude of wave-2 component of longitude-              !MGRM185
!                      !   dependent wave multiplier for density                  !MGRM186
!  Wavephi2 = 0.0      ! Phase of wave-2 component of longitude-                  !MGRM187
!                      !   dependent wave multiplier (longitude, with             !MGRM188
!                      !   West positive if LonEW = 0, East positive              !MGRM189
!                      !   if LonEW = 1)                                          !MGRM190
!  phi2dot  = 0.0      ! Rate of longitude movement (degrees per day)             !MGRM191
!                      !   for wave-2 component (Westward positive if             !MGRM192
!                      !   LonEW = 0, Eastward positive if LonEW = 1)             !MGRM193
!  WaveA3   = 0.0      ! Amplitude of wave-3 component of longitude-              !MGRM194
!                      !   dependent wave multiplier for density                  !MGRM195
!  Wavephi3 = 0.0      ! Phase of wave-3 component of longitude-                  !MGRM196
!                      !   dependent wave multiplier (longitude, with             !MGRM197
!                      !   West positive if LonEW = 0, East positive              !MGRM198
!                      !   if LonEW = 1)                                          !MGRM199
!  phi3dot  = 0.0      ! Rate of longitude movement (degrees per day)             !MGRM200
!                      !   for wave-3 component (Westward positive if             !MGRM201
!                      !   LonEW = 0, Eastward positive if LonEW = 1)             !MGRM202
!  iuwave   = 0        ! Unit number for (Optional) time-dependent wave           !MGRM203
!                      !    coefficient data file (or 0 for none)                 !MGRM204
!  Wscale   = 20.      ! Vertical scale (km) of longitude-dependent               !MGRM205
!                      !    wave damping at altitudes below 100 km                !MGRM206
!                      !    (10<=Wscale<=10,000 km)                               !MGRM207
!  ipclat   = 1        ! 1 = Planeto-centric latitude and height input,           !MGRM208
!                      ! 0 = Planeto-graphic latitude and height input            !MGRM209
!  requa    = 3396.19  ! Equatorial radius (km) for reference ellipsoid           !MGRM210
!  rpole    = 3376.20  ! Polar radius (km) for reference ellipsoid                !MGRM211
!  idaydata = 1        ! 1 = output daily max/min data; 0 = none                  !MGRM212
! $END                                                                            !MGRM213
!.....................................................................            !MGRM214
!                                                                                 !MGRM215
!...  Setup_M10 information for start of run                                      !MGRM216
      CALL SETUP_M10 (CHGT, CLAT, CLON, CSEC, DAY0, RHOD, RHOU, RHOV, RHOW,    &  !MGRM217
         DELHGT, DELLAT, DELLON, DELTIME, MAXNUM, NR1, NMONTE, 0.0D0, 0.0D0,   &  !MGRM218
         0.0D0, LONEW, INPUTFL, IUSTDOUT, IULIST, HGTASFC, IERT, IUTC, CORLMIN &  !MGRM219
         , PROFNEAR, PROFFAR, NPROF)                                              !MGRM220
!                                                                                 !MGRM221
!...  Save initial position, date, and position displacement values               !MGRM222
      FHGT = CHGT                                                                 !MGRM223
      FLAT = CLAT                                                                 !MGRM224
      FLON = CLON                                                                 !MGRM225
      FSEC = CSEC                                                                 !MGRM226
      FDHGT = DELHGT                                                              !MGRM227
      FDLAT = DELLAT                                                              !MGRM228
      FDLON = DELLON                                                              !MGRM229
      FDTIME = DELTIME                                                            !MGRM230
!...  Initialize total perturbation step                                          !MGRM231
      PERTSTEP = 0.0D0                                                            !MGRM232
      IUPDATE = 0                                                                 !MGRM233
!...  Step through number of Monte Carlo runs                                     !MGRM234
      L910: DO J = 1, NMONTE                                                      !MGRM235
!...  Initialize number for wave coefficients                                     !MGRM236
         NUMWAVE = 0                                                              !MGRM237
!...  Re-initialize random number, position and time for each run                 !MGRM238
         IF (J > 1) THEN                                                          !MGRM239
            CALL RANDINIT_M10 (J, NR1, RHOD, RHOU, RHOV, RHOW, IULIST, IUSTDOUT & !MGRM240
               )                                                                  !MGRM241
            CHGT = FHGT                                                           !MGRM242
            CLAT = FLAT                                                           !MGRM243
            CLON = FLON                                                           !MGRM244
            CSEC = FSEC                                                           !MGRM245
            DELHGT = FDHGT                                                        !MGRM246
            DELLAT = FDLAT                                                        !MGRM247
            DELLON = FDLON                                                        !MGRM248
            DELTIME = FDTIME                                                      !MGRM249
!...      Re-initialize total perturbation step                                   !MGRM250
            PERTSTEP = 0.0D0                                                      !MGRM251
            IUPDATE = 0                                                           !MGRM252
         ENDIF                                                                    !MGRM253
!...  Step through max Number of points for each Monte Carlo run                  !MGRM254
         DO I = 0, MAXNUM                                                         !MGRM255
!                                                                                 !MGRM256
            CALL DATASTEP_M10 (I, CHGT, CLAT, CLON, CSEC, DAY0, RHOD, RHOU,     & !MGRM257
               RHOV, RHOW, EOF, DELHGT, DELLAT, DELLON, DELTIME, TEMP, PRES,    & !MGRM258
               DENSLO, DENS, DENSHI, DENSP, EWWIND, EWPERT, NSWIND, NSPERT,     & !MGRM259
               VWPERT, HRHO, HPRES, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, LONEW,   & !MGRM260
               CORLIM, DENSTOT, NUMWAVE, HGTASFC, IERT, IUTC, PERTSTEP, CORLMIN & !MGRM261
               , IUPDATE, ALS, SZA, OWLT, SUNLAT, SUNLON, MARSAU, TLOCAL,       & !MGRM262
               PROFNEAR, PROFFAR, NPROF)                                          !MGRM263
!                                                                                 !MGRM264
!.....................................................................            !MGRM265
!                                                                                 !MGRM266
!....   Parameters passed as output from Datastep_M10 are:                        !MGRM267
!       TEMP   = temperature (K)                                                  !MGRM268
!       PRES   = pressure (N/m**2)                                                !MGRM269
!       DENSLO = nominal low density (kg/m**3), approx. -1 sigma                  !MGRM270
!       DENS   = mean density (kg/m**3)                                           !MGRM271
!       DENSHI = nominal high density (kg/m**3), approx. +1 sigma                 !MGRM272
!       DENSP  = density perturbation (% of unperturbed mean)                     !MGRM273
!       EWWIND = mean eastward wind component (m/s)                               !MGRM274
!       EWpert = eastward wind component perturbation (m/s)                       !MGRM275
!       NSWIND = mean northward wind component (m/s)                              !MGRM276
!       NSpert = northward wind component perturbation (m/s)                      !MGRM277
!       Hrho   = density scale height (km)                                        !MGRM278
!       Hpres  = pressure scale height (km)                                       !MGRM279
!       corlim = ratio of step size to minimum step size for assured              !MGRM280
!                accuracy in perturbations (should be >= 1)                       !MGRM281
!       DENSTOT= total density (mean plus perturbed), kg/m**3                     !MGRM282
!       iupdate  = 1 if perturbations updated, 0 if perturbations not             !MGRM283
!                     updated but perturbation step updated, -1 if                !MGRM284
!                     neither perturbations nor step updated                      !MGRM285
!       ALS    = Planeto-centric longitude of Sun (Ls, degrees)                   !MGRM286
!       SZA    = Solar zenith angle (degrees)                                     !MGRM287
!       owlt   = Mars-Earth one-way light time (minutes)                          !MGRM288
!       sunlat = Sub-solar latitude (degrees)                                     !MGRM289
!       sunlon = Sub-solar longitude (degrees)                                    !MGRM290
!       MarsAU = Mars orbital radius (AU)                                         !MGRM291
!       TLOCAL = Local Solar rime (Mars hours)                                    !MGRM292
!                                                                                 !MGRM293
!       In addition to being passed to other routines, these                      !MGRM294
!       parameters may also be written out here.                                  !MGRM295
!                                                                                 !MGRM296
!       Optional high resolution ephemeris inputs are:                            !MGRM297
!         dsunlat = latitude of sub-solar point (deg)                             !MGRM298
!         dsunlon = longitude of sub-solar point (deg)                            !MGRM299
!         dsunLs  = solar Ls angle (deg)                                          !MGRM300
!         dradau  = Mars orbital radius (AU)                                      !MGRM301
!         dowlt   = Earth-Mars one-way light-time (minutes)                       !MGRM302
!       Values of 0.0D0, used here, cause use of the internal epheremis           !MGRM303
!       subroutine to compute these parameters                                    !MGRM304
!.....................................................................            !MGRM305
!                                                                                 !MGRM306
!       Go to next Monte Carlo run if EOF=1                                       !MGRM307
            IF (EOF /= 1) CYCLE                                                   !MGRM308
            CYCLE  L910                                                           !MGRM309
         END DO                                                                   !MGRM310
      END DO L910                                                                 !MGRM311
      STOP ' Normal Termination'                                                  !MGRM312
      END PROGRAM MarsGRAM_M10                                                    !MGRM313
!----------------------------------------------------------------------           !MGRM314
