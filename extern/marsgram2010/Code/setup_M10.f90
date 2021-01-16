      SUBROUTINE SETUP_M10(CHGT, CLAT, CLON, CSEC, DAY0, RHOD, RHOU, RHOV, RHOW & !SETU  1
         , DHGT, DLAT, DLON, DTIME, MAXNUM, NRN1, NMCR1, DSUNLS, DRADAU, DOWLT  & !SETU  2
         , LNEW, INPUTFL, IUSTDOUT, IULIST, HGTASFC, INERT, INUTC, STEPMIN,     & !SETU  3
         PROFNR, PROFFR, NPROF)                                                   !SETU  4
!-----------------------------------------------                                  !SETU  5
!   M o d u l e s                                                                 !SETU  6
!-----------------------------------------------                                  !SETU  7
      USE vast_kind_param, ONLY:  DOUBLE                                          !SETU  8
      USE FILENAME_M10_C                                                          !SETU  9
      USE RANDCOM_M10_C                                                           !SETU 10
      USE TERHGT_M10_C                                                            !SETU 11
      USE TGCMOFFSET_M10_C                                                        !SETU 12
      USE COSPARNH_M10_C                                                          !SETU 13
      USE THERM_M10_C                                                             !SETU 14
      USE DATACOM_M10_C                                                           !SETU 15
      USE MGCMPARM_M10_C                                                          !SETU 16
      USE WAVECOEF_M10_C                                                          !SETU 17
!-----------------------------------------------                                  !SETU 18
!                                                                                 !SETU 19
!...  Set parameter values for number of MOLA topography lat-lons,                !SETU 20
!     number of albedo lat-lons, number of boundary layer levels,                 !SETU 21
!     number of dust optical depths, and number of F10 values                     !SETU 22
!...                                                                              !SETU 23
!...Switches:                                                                     !SETU 24
!-----------------------------------------------                                  !SETU 25
!   I n t e r f a c e   B l o c k s                                               !SETU 26
!-----------------------------------------------                                  !SETU 27
      USE rdprof_M10_I                                                            !SETU 28
      USE readtes_M10_I                                                           !SETU 29
      USE readsurf_M10_I                                                          !SETU 30
      USE readmgcm_M10_I                                                          !SETU 31
      USE readtgcm_M10_I                                                          !SETU 32
      USE rdtessrf_M10_I                                                          !SETU 33
      USE rdtesmgcm_M10_I                                                         !SETU 34
      USE rdtestgcm_M10_I                                                         !SETU 35
      USE chkdt_M10_I                                                             !SETU 36
      USE caltojul_M10_I                                                          !SETU 37
      USE marsephm_M10_I                                                          !SETU 38
      USE random_M10_I                                                            !SETU 39
      USE ppnd_M10_I                                                              !SETU 40
      IMPLICIT NONE                                                               !SETU 41
!-----------------------------------------------                                  !SETU 42
!   D u m m y   A r g u m e n t s                                                 !SETU 43
!-----------------------------------------------                                  !SETU 44
      INTEGER , INTENT(INOUT) :: MAXNUM                                           !SETU 45
      INTEGER , INTENT(OUT) :: NRN1                                               !SETU 46
      INTEGER , INTENT(OUT) :: NMCR1                                              !SETU 47
      INTEGER , INTENT(OUT) :: LNEW                                               !SETU 48
      INTEGER , INTENT(IN) :: IUSTDOUT                                            !SETU 49
      INTEGER , INTENT(OUT) :: IULIST                                             !SETU 50
      INTEGER , INTENT(OUT) :: INERT                                              !SETU 51
      INTEGER , INTENT(OUT) :: INUTC                                              !SETU 52
      INTEGER , INTENT(INOUT)  :: NPROF                                           !SETU 53
      REAL(DOUBLE) , INTENT(OUT) :: CHGT                                          !SETU 54
      REAL(DOUBLE) , INTENT(OUT) :: CLAT                                          !SETU 55
      REAL(DOUBLE) , INTENT(OUT) :: CLON                                          !SETU 56
      REAL(DOUBLE) , INTENT(OUT) :: CSEC                                          !SETU 57
      REAL(DOUBLE) , INTENT(OUT) :: DAY0                                          !SETU 58
      REAL(DOUBLE) , INTENT(OUT) :: RHOD                                          !SETU 59
      REAL(DOUBLE) , INTENT(OUT) :: RHOU                                          !SETU 60
      REAL(DOUBLE) , INTENT(OUT) :: RHOV                                          !SETU 61
      REAL(DOUBLE) , INTENT(OUT) :: RHOW                                          !SETU 62
      REAL(DOUBLE) , INTENT(OUT) :: DHGT                                          !SETU 63
      REAL(DOUBLE) , INTENT(OUT) :: DLAT                                          !SETU 64
      REAL(DOUBLE) , INTENT(OUT) :: DLON                                          !SETU 65
      REAL(DOUBLE) , INTENT(OUT) :: DTIME                                         !SETU 66
      REAL(DOUBLE) , INTENT(IN) :: DSUNLS                                         !SETU 67
      REAL(DOUBLE) , INTENT(IN) :: DRADAU                                         !SETU 68
      REAL(DOUBLE) , INTENT(IN) :: DOWLT                                          !SETU 69
      REAL(DOUBLE) , INTENT(OUT) :: HGTASFC                                       !SETU 70
      REAL(DOUBLE) , INTENT(OUT) :: STEPMIN                                       !SETU 71
      REAL(DOUBLE) , INTENT(OUT) :: PROFNR                                        !SETU 72
      REAL(DOUBLE) , INTENT(OUT) :: PROFFR                                        !SETU 73
      CHARACTER(LEN=99) , INTENT(IN) :: INPUTFL                                   !SETU 74
!-----------------------------------------------                                  !SETU 75
!   L o c a l   P a r a m e t e r s                                               !SETU 76
!-----------------------------------------------                                  !SETU 77
      CHARACTER, PARAMETER :: VERSION = '1'                                       !SETU 78
!-----------------------------------------------                                  !SETU 79
!   L o c a l   V a r i a b l e s                                                 !SETU 80
!-----------------------------------------------                                  !SETU 81
      INTEGER , DIMENSION(13) :: IERR                                             !SETU 82
      INTEGER :: IERR1, IERR2, IERR3, IERR4, IERR5, IERR6, IERR7, IERR8, IERR9  & !SETU 83
         , IERR10, IERR11, IERR12, IERR13, MONTH, MDAY, MYEAR, IHR, IMIN, IERT  & !SETU 84
         , IUTC, LONEW, NR1, NMONTE, IOERR, I, LENDIR, J, LS, IDTERR,           & !SETU 85
         L                                                                        !SETU 86
      REAL(DOUBLE) :: MARSAU, SEC, FLAT, FLON, FHGT, DELHGT, DELLAT, DELLON,    & !SETU 87
         DELTIME, HGTASFCM, PROFNEAR, PROFFAR, CORLMIN,                         & !SETU 88
         ALS, OWLT, TTSEC, DT, SUNLAT, SUNLON, EOT, Z1                            !SETU 89
      CHARACTER(LEN=99) :: EWLON, DUMMY, DATADIR, GCMDIR, PROFILE, TRAJFL    ,  & !SETU 90
         WAVEFILE                                                                 !SETU 91
      CHARACTER(LEN=99) , DIMENSION(13) :: FILES                                  !SETU 92
      CHARACTER(LEN=7) :: HGTLBL                                                  !SETU 93
      CHARACTER(LEN=5) :: LATLBL                                                  !SETU 93a
      CHARACTER(LEN=8) :: DENSLBL                                                 !SETU 93b
                                                                                  !SETU 94
      SAVE EWLON, DUMMY, DATADIR, GCMDIR, PROFILE, TRAJFL, WAVEFILE, MARSAU,    & !SETU 95
         IERR, FILES, HGTLBL, LATLBL, DENSLBL, IERR1, IERR2, IERR3, IERR4,      & !SETU 96
         IERR5, IERR6, IERR7, IERR8, IERR9, IERR10, IERR11, IERR12, IERR13,     & !SETU 97
         MONTH, MDAY, MYEAR, IHR, IMIN, SEC, FLAT, FLON, FHGT, DELHGT, DELLAT,  & !SETU 98
         DELLON, DELTIME, IERT, IUTC, LONEW, NR1, HGTASFCM,                     & !SETU 99
         PROFNEAR, PROFFAR, NMONTE, CORLMIN, IOERR, I                           & !SETU100
         , LENDIR, J, LS, IDTERR, ALS, OWLT, TTSEC, DT, SUNLAT, SUNLON, EOT, Z1 & !SETU101
         , L                                                                      !SETU102
!-----------------------------------------------                                  !SETU103
!                                                                                 !SETU104
!     If output to the list file, output file, and plotable files                 !SETU105
!     is not desired, the following statements may be removed                     !SETU106
!                                                                                 !SETU107
      EQUIVALENCE (IERR1, IERR(1)), (IERR2, IERR(2)), (IERR3, IERR(3)),         & !SETU108
         (IERR4, IERR(4)), (IERR5, IERR(5)), (IERR6, IERR(6)), (IERR7,          & !SETU109
         IERR(7)), (IERR8, IERR(8)), (IERR9, IERR(9)), (IERR10, IERR(10)),      & !SETU110
         (IERR11, IERR(11)), (IERR12, IERR(12)), (IERR13, IERR(13))               !SETU111
      DATA FILES/ 'LIST.txt', 'Density.txt', 'Perturb.txt', 'Winds.txt',        & !SETU112
         'TPresHgt.txt', 'DayData.txt', 'ThrmData.txt', 'molatoph.bin',         & !SETU113
         'COSPAR2.DAT', 'OUTPUT.txt', 'hgtoffst.dat', 'albedo1.bin',            & !SETU114
         'MarsRad.txt'/                                                           !SETU115
!......................................................................           !SETU116
!...  Establish default values for input parameters                               !SETU117
      DATA TRAJFL/ 'TRAJDATA.txt'/                                                !SETU118
      DATA DATADIR, GCMDIR, PROFILE/ 'null', 'null', 'null'/                      !SETU119
      DATA WAVEFILE/ 'null'/                                                      !SETU120
!...  Default time = Viking 1 Lander                                              !SETU121
      DATA MONTH, MDAY, MYEAR, IHR, IMIN, SEC/ 7, 20, 76, 12, 30, 0.0D0/          !SETU122
!...  Default position = Viking 1 Lander Site, height = surface                   !SETU123
      DATA FLAT, FLON, FHGT, DELHGT, DELLAT, DELLON, DELTIME/ 22.0D0, 48.0D0,   & !SETU124
         -0.5D0, 10.0D0, 3*0.0D0/                                                 !SETU125
!......................................................................           !SETU126
!     Definition of the Namelist input data                                       !SETU127
      NAMELIST /INPUT_M10/LSTFL, OUTFL, TRAJFL, PROFILE, WAVEFILE, DATADIR,     & !SETU128
         IERT,IUTC, GCMDIR, MONTH, MDAY, MYEAR, NPOS, IHR, IMIN, SEC, LONEW,    & !SETU129
         DUSTTAU, DUSTMIN, DUSTMAX, DUSTNU, DUSTDIAM, DUSTDENS, ALS0, ALSDUR,   & !SETU130
         INTENS, RADMAX, DUSTLAT, DUSTLON, F107, NR1, NVARX, NVARY,             & !SETU131
         LOGSCALE, FLAT, FLON, FHGT, MOLAHGTS, HGTASFCM, ZOFFSET, IBOUGHER,     & !SETU132
         DELHGT, DELLAT, DELLON, DELTIME, DELTATEX, PROFNEAR, PROFFAR, RPSCALE, & !SETU133
         RWSCALE, WLSCALE, WMSCALE, BLWINFAC, NMONTE, IUP, WAVEA0, WAVEDATE,    & !SETU134
         WAVEA1, WAVEPHI1, PHI1DOT, WAVEA2, WAVEPHI2, PHI2DOT, WAVEA3,          & !SETU135
         WAVEPHI3, PHI3DOT, IUWAVE, WSCALE, CORLMIN, IPCLAT, REQUA, RPOLE,      & !SETU136
         MAPYEAR, IDAYDATA                                                        !SETU137
!.....................................................................            !SETU138
    1 FORMAT(' Mars-GRAM 2010 (Version ',A1,'.0) - Nov 2010')                     !SETU139
!                                                                                 !SETU140
!...  Default deltaTEX to zero                                                    !SETU141
      DELTATEX = 0.0                                                              !SETU142
!...  Default random perturbation scale factor                                    !SETU143
      RPSCALE =  1.0D0                                                            !SETU144
!...  Default Mapping Year = 1                                                    !SETU145
      MAPYEAR = 1                                                                 !SETU146
!...  Default profile parameters                                                  !SETU147
      PROFNEAR = 0.0D0                                                            !SETU148
      PROFFAR = 0.0D0                                                             !SETU149
!...  Default daily max/min data                                                  !SETU150
      IDAYDATA = 1                                                                !SETU151
!...  Default perturbation factors                                                !SETU152
      RWSCALE = 1.0D0                                                             !SETU153
      WLSCALE = 1.0D0                                                             !SETU154
      BLWINFAC = 1.0D0                                                            !SETU155
!...  Default mean wind scale factor                                              !SETU156
      WMSCALE = 1.0D0                                                             !SETU157
!...  Default corlmin value                                                       !SETU158
      CORLMIN = 0.0D0                                                             !SETU159
!...  Default time options IERT = 1 for Earth-receive time and IUTC =             !SETU160
!     1 for UTC time (not Terrestrial Dynamical Time)                             !SETU161
      IERT = 1                                                                    !SETU162
      IUTC = 1                                                                    !SETU163
!...  Default planetry radii and lat/height input                                 !SETU164
      REQUA = 3396.19D0                                                           !SETU165
      RPOLE = 3376.20D0                                                           !SETU166
      IPCLAT = 1                                                                  !SETU167
!...  Default with no wave model modification                                     !SETU168
      WAVEA0 = 1.0D0                                                              !SETU169
      WAVEDATE = 0.0D0                                                            !SETU170
      WAVEA1 = 0.0D0                                                              !SETU171
      WAVEPHI1 = 0.0D0                                                            !SETU172
      PHI1DOT = 0.0D0                                                             !SETU173
      WAVEA2 = 0.0D0                                                              !SETU174
      WAVEPHI2 = 0.0D0                                                            !SETU175
      PHI2DOT = 0.0D0                                                             !SETU176
      WAVEA3 = 0.0D0                                                              !SETU177
      WAVEPHI3 = 0.0D0                                                            !SETU178
      PHI3DOT = 0.0D0                                                             !SETU179
      IUWAVE = 0                                                                  !SETU180
      WSCALE = 20.0D0                                                             !SETU181
!...  Set dust optical depths for GCM data                                        !SETU182
      DUST(1) = 0.3D0                                                             !SETU183
      DUST(2) = 1.0D0                                                             !SETU184
      DUST(3) = 3.0D0                                                             !SETU185
      DUSTC(1) = '03'                                                             !SETU186
      DUSTC(2) = '10'                                                             !SETU187
      DUSTC(3) = '30'                                                             !SETU188
!...  Set heights for GCM boundary layer data                                     !SETU189
      DZBL(1) = 0.0D0                                                             !SETU190
      DZBL(2) = 0.005D0                                                           !SETU191
      DZBL(3) = 0.030D0                                                           !SETU192
!...  Set surface roughness parameter (m).  Value 1 cm (0.01 m) is                !SETU193
!     consistent with NASA Ames MGCM boundary layer model                         !SETU194
      ZWSFC = 0.01D0                                                              !SETU195
!...  Set solar activity values (F10.7 at 1AU)                                    !SETU196
      F10VAL(1) = 70.0D0                                                          !SETU197
      F10VAL(2) = 130.0D0                                                         !SETU198
      SOLACT(1) = 'ls'                                                            !SETU199
      SOLACT(2) = 'ms'                                                            !SETU200
!...  Set values for TES year 1 and year 2 data files                             !SETU201
      TESYR(1) = 'y1'                                                             !SETU202
      TESYR(2) = 'y2'                                                             !SETU203
      SOLTES(1) = 'ls'                                                            !SETU204
      SOLTES(2) = 'ms'                                                            !SETU205
      SOLTES(3) = 'hs'                                                            !SETU206
      F10TES(1) = 70.0D0                                                          !SETU207
      F10TES(2) = 130.0D0                                                         !SETU208
      F10TES(3) = 200.0D0                                                         !SETU209
!...  Set unit number for screen I/O and pass it into Common                      !SETU210
      IU0 = IUSTDOUT                                                              !SETU211
!...  default list and output files                                               !SETU212
      LSTFL = 'LIST.txt'                                                          !SETU213
      OUTFL = 'OUTPUT.txt'                                                        !SETU214
!...  Default number of positions                                                 !SETU215
      NPOS = 21                                                                   !SETU216
!...  Default use West Longitude positive (USGS Convention)                       !SETU217
      LONEW = 0                                                                   !SETU218
!...  Default background dust optical depth, and min & max                        !SETU219
      DUSTTAU = 0.3D0                                                             !SETU220
      DUSTMIN = 0.3D0                                                             !SETU221
      DUSTMAX = 1.0D0                                                             !SETU222
!...  Default dust particle density parameters                                    !SETU223
!     See Fig. 2 of Haberle et al., J. geophys. Res., vol 104, p. 8957            !SETU224
!     (1999) and Haberle et al., Icarus, vol 50, p. 322 (1982)                    !SETU225
      DUSTNU = 0.003D0                                                            !SETU226
      DUSTDIAM = 5.0D0                                                            !SETU227
      DUSTDENS = 3000.0D0                                                         !SETU228
!...  Default no dust storm                                                       !SETU229
      ALS0 = 0.0D0                                                                !SETU230
      ALSDUR = 48.0D0                                                             !SETU231
      INTENS = 0.0D0                                                              !SETU232
      RADMAX = 0.0D0                                                              !SETU233
      DUSTLAT = 0.0D0                                                             !SETU234
      DUSTLON = 0.0D0                                                             !SETU235
!...  Default Solar Flux parameters                                               !SETU236
      F107 = 68.0D0                                                               !SETU237
      STDL = 0.0D0                                                                !SETU238
!...  Default plot variable = height above MOLA areoid                            !SETU239
      NVARX = 1                                                                   !SETU240
      NVARY = 0                                                                   !SETU241
!...  Default to regular linear scale                                             !SETU242
      LOGSCALE = 0                                                                !SETU243
!...  Default random number seed and Number of Monte Carlo runs                   !SETU244
      NR1 = 1001                                                                  !SETU245
      NMONTE = 1                                                                  !SETU246
!...  Default unit number for print output data file                              !SETU247
      IUP = 13                                                                    !SETU248
!...  Set length of Mars day                                                      !SETU249
      DAY = 24.622962D0                                                           !SETU250
!...  Default to input heights above MOLA areoid                                  !SETU251
      MOLAHGTS = 1                                                                !SETU252
!...  Default height above surface                                                !SETU253
      HGTASFCM = 0.0D0                                                            !SETU254
!...  Default (global) MTGCM height offset                                        !SETU255
      ZOFFSET = 3.25D0                                                            !SETU256
      IBOUGHER = 2                                                                !SETU257
!...  Open Namelist data file                                                     !SETU258
      OPEN(8, FILE=INPUTFL, STATUS='old', IOSTAT=IOERR, POSITION='asis')          !SETU259
      IF (IOERR /= 0) THEN                                                        !SETU260
         WRITE (IUSTDOUT, *) ' Error opening NAMELIST input file'                 !SETU261
         STOP                                                                     !SETU262
      ENDIF                                                                       !SETU263
!...  Read Namelist data                                                          !SETU264
      READ (8, NML=INPUT_M10)                                                     !SETU265
      CLOSE(8)                                                                    !SETU266
!.....................................................................            !SETU267
!     For compilers not supporting the NAMELIST input mode, the                   !SETU268
!     previous Read statement may be replaced by:                                 !SETU269
!                                                                                 !SETU270
!     Read(8,10)LSTFL                                                             !SETU271
!     Read(8,10)OUTFL                                                             !SETU272
!     Read(8,10)TRAJFL                                                            !SETU273
!     Read(8,10)WaveFile                                                          !SETU274
!     Read(8,10)DATADIR                                                           !SETU275
!     Read(8,10)GCMDIR                                                            !SETU276
!     Read(8,10)profile                                                           !SETU277
! 10  Format(A)                                                                   !SETU278
!     Read(8,*)IERT,IUTC,MONTH,MDAY,MYEAR,NPOS,IHR,IMIN,SEC,LonEW,       &        !SETU279
!      Dusttau,Dustmin,Dustmax,Dustnu,Dustdiam,Dustdens,ALS0,ALSDUR,     &        !SETU280
!      INTENS,RADMAX,DUSTLAT,DUSTLON,F107,NR1,NVARX,NVARY,               &        !SETU281
!      LOGSCALE,FLAT,FLON,FHGT,MOLAhgts,hgtasfcm,zoffset,ibougher,       &        !SETU282
!      DELHGT,DELLAT,DELLON,DELTIME,deltaTEX,profnear,proffar,rpscale,   &        !SETU283
!      rwscale,wlscale,wmscale,blwinfac,NMONTE,iup,WaveA0,WaveDate,      &        !SETU284
!      WaveA1,Wavephi1,phi1dot,WaveA2,Wavephi2,phi2dot,WaveA3,           &        !SETU285
!      Wavephi3,phi3dot,iuwave,Wscale,corlmin,ipclat,requa,rpole,        &        !SETU286
!      MapYear,idaydata                                                           !SETU287
!                                                                                 !SETU288
!     and the NAMELIST file INPUT_M10 may be modified to contain free-            !SETU289
!     field input data as in the above list.                                      !SETU290
!.....................................................................            !SETU291
!...  Check that unit iup is in allowable range                                   !SETU292
      IF (IUP>=5 .AND. IUP<=12 .OR. IUP>=21 .AND. IUP<=29) STOP          &        !SETU293
         ' Unit iup conflict with another file'                                   !SETU294
      IF (FLAT<(-90.0D0) .OR. FLAT>90.0D0) THEN                                   !SETU295
         WRITE (IU0, 382)                                                         !SETU296
  382    FORMAT(' Error in first latitude or longitude.')                         !SETU297
         GO TO 9998                                                               !SETU298
      ENDIF                                                                       !SETU299
      IF (FLON < 0.0D0) FLON = FLON + 360.0D0                                     !SETU300
      IF (FLON<0.0D0 .OR. FLON>360.0D0) THEN                                      !SETU301
         WRITE (IU0, 382)                                                         !SETU302
         GO TO 9998                                                               !SETU303
      ENDIF                                                                       !SETU304
!...  Store values for output arguments                                           !SETU305
      PROFNR = PROFNEAR                                                           !SETU306
      PROFFR = PROFFAR                                                            !SETU307
      LNEW = LONEW                                                                !SETU308
!...  Test profnear and proffar.  Read profile data if profnear > 0               !SETU309
      IF (PROFNEAR > 0.0D0) THEN                                                  !SETU310
         IF (PROFFAR <= PROFNEAR) STOP ' proffar must be > profnear'              !SETU311
         CALL RDPROF_M10 (PROFILE, NPROF, LONEW)                                  !SETU312
      ENDIF                                                                       !SETU313
!...  Must use planeto-centric height and latitude if MOLAhgts = 1                !SETU314
      IF (MOLAHGTS == 1) IPCLAT = 1                                               !SETU315
      IULIST = IUP                                                                !SETU316
!...  Convert Height above surface to km and insure proper range                  !SETU317
      HGTASFCM = DMAX1(0.0D0,HGTASFCM)                                            !SETU318
      HGTASFCM = MIN(4500.0D0,HGTASFCM)                                           !SETU319
      HGTASFC = HGTASFCM/1000.0D0                                                 !SETU320
!...  Insure Wscale within proper range                                           !SETU321
      WSCALE = DMAX1(10.0D0,WSCALE)                                               !SETU322
      WSCALE = MIN(10000.0D0,WSCALE)                                              !SETU323
!...  Set traveling wave parameters if WaveDate le 0                              !SETU324
      IF (WAVEDATE <= 0.0D0) THEN                                                 !SETU325
         WAVEDATE = 0.0D0                                                         !SETU326
         PHI1DOT = 0.0D0                                                          !SETU327
         PHI2DOT = 0.0D0                                                          !SETU328
         PHI3DOT = 0.0D0                                                          !SETU329
      ENDIF                                                                       !SETU330
!...  Open and read WaveFile and load wavedata array if iuwave>0                  !SETU331
      IF (IUWAVE > 0) THEN                                                        !SETU332
         OPEN(IUWAVE, FILE=WAVEFILE, IOSTAT=IERR1, POSITION='asis')               !SETU333
         IF (IERR1 /= 0) STOP '  Error opening WaveFile'                          !SETU334
         NWAVE = 1                                                                !SETU335
!...    Each WaveFile record contains: (1) time at which wave model               !SETU336
!         coefficients first apply (1st record must start at time=0),             !SETU337
!         (2) WaveA0, (3) WaveDate, (4) WaveA1, (5) Wavephi1, (6)                 !SETU338
!         phi1dot, (6), WaveA2, (7)Wavephi2, (8) phi2dot, (9) WaveA3,             !SETU339
!         (10) Wavephi3, (11) phi3dot.  Times for wave model                      !SETU340
!          coefficients must be in ascending order.                               !SETU341
    5    CONTINUE                                                                 !SETU342
         READ (IUWAVE, *, END=6) WAVETIME(NWAVE), (WAVEDATA(NWAVE,I),I=1,11)      !SETU343
         IF (NWAVE == 1) THEN                                                     !SETU344
            IF (WAVETIME(1) > 0.0D0) STOP                                    &    !SETU345
               ' First wave data in file must be at time 0'                       !SETU346
         ELSE                                                                     !SETU347
            IF (WAVETIME(NWAVE) <= WAVETIME(NWAVE-1)) STOP                   &    !SETU348
               ' Wave data in file must in increasing order by time'              !SETU349
         ENDIF                                                                    !SETU350
         IF (WAVEDATA(NWAVE,1)<0.1D0 .OR. WAVEDATA(NWAVE,1)>12.0D0) STOP     &    !SETU351
            ' WaveA0 from input file is out of range'                             !SETU352
         NWAVE = NWAVE + 1                                                        !SETU353
         GO TO 5                                                                  !SETU354
!...    Close wave data file                                                      !SETU355
    6    CONTINUE                                                                 !SETU356
         CLOSE(IUWAVE)                                                            !SETU357
!...    Re-set nwave to number of wave data                                       !SETU358
         NWAVE = NWAVE - 1                                                        !SETU359
!...    Re-initialize wave coefficients                                           !SETU360
         WAVEA0 = WAVEDATA(1,1)                                                   !SETU361
         WAVEDATE = WAVEDATA(1,2)                                                 !SETU362
         WAVEA1 = WAVEDATA(1,3)                                                   !SETU363
         WAVEPHI1 = WAVEDATA(1,4)                                                 !SETU364
         PHI1DOT = WAVEDATA(1,5)                                                  !SETU365
         WAVEA2 = WAVEDATA(1,6)                                                   !SETU366
         WAVEPHI2 = WAVEDATA(1,7)                                                 !SETU367
         PHI2DOT = WAVEDATA(1,8)                                                  !SETU368
         WAVEA3 = WAVEDATA(1,9)                                                   !SETU369
         WAVEPHI3 = WAVEDATA(1,10)                                                !SETU370
         PHI3DOT = WAVEDATA(1,11)                                                 !SETU371
      ENDIF                                                                       !SETU372
!...  Open and read TES dust optical depths                                       !SETU373
      CALL READTES_M10 (DATADIR, VERSION)                                         !SETU374
!...  Convert FLON, DUSTLON, DELLON to West if LonEW=1                            !SETU375
      IF (LONEW == 1) THEN                                                        !SETU376
         DUSTLON = 360.0D0 - DUSTLON                                              !SETU377
         FLON = 360.0D0 - FLON                                                    !SETU378
         DELLON = -DELLON                                                         !SETU379
      ENDIF                                                                       !SETU380
!...  Insure corlmin input value within proper bounds                             !SETU381
      CORLMIN = DMAX1(0.0D0,CORLMIN)                                              !SETU382
      CORLMIN = MIN(1.0D0,CORLMIN)                                                !SETU383
!---  RPFACTOR = RPSCALE                                                          !SETU384
      IF (RPSCALE<0.0D0 .OR. RPSCALE>2.0D0) STOP ' Must have 0 <= rpscale <= 2'   !SETU385
!...  Insure Dustmin and Dustmax in proper ranges                                 !SETU386
      DUSTMIN = DMAX1(0.1D0,DUSTMIN)                                              !SETU387
      DUSTMAX = MIN(1.0D0,DUSTMAX)                                                !SETU388
      IF (WAVEA0<=0.1D0 .OR. WAVEA0>12.0D0) STOP ' WaveA0 out of range'           !SETU389
      NMONTE = MAX0(1,NMONTE)                                                     !SETU390
!...  Pass corlmin value to output                                                !SETU391
      STEPMIN = CORLMIN                                                           !SETU392
!...  Pass 1st random number and Number Monte Carlo runs to output                !SETU393
      NRN1 = NR1                                                                  !SETU394
      NMCR1 = NMONTE                                                              !SETU395
!...  Check option values and pass to output or Common DATACOM                    !SETU396
      WLSCALE = DMAX1(0.1D0,WLSCALE)                                              !SETU397
      WLSCALE = MIN(10.0D0,WLSCALE)                                               !SETU398
      BLWINFAC = DMAX1(0.0D0,BLWINFAC)                                            !SETU399
      RWSCALE = DMAX1(0.0D0,RWSCALE)                                              !SETU400
      INERT = IERT                                                                !SETU401
      INUTC = IUTC                                                                !SETU402
!---  INPCLAT = IPCLAT                                                            !SETU403
!...  Insure requa and rpole within bounds                                        !SETU404
      IF (REQUA<3300.0D0 .OR. REQUA>3500.0D0 .OR. RPOLE<3300.0D0 .OR. RPOLE>    & !SETU405
         3500.0D0) STOP ' requa or rpole out of bounds'                           !SETU406
!---  REQUAT = REQUA                                                              !SETU407
!---  RPOLES = RPOLE                                                              !SETU408
!...  Insure MapYear within legal limits                                          !SETU409
      MAPYEAR = MAX0(0,MAPYEAR)                                                   !SETU410
      MAPYEAR = MIN0(2,MAPYEAR)                                                   !SETU411
      DHGT = DELHGT                                                               !SETU412
      DLAT = DELLAT                                                               !SETU413
      DLON = DELLON                                                               !SETU414
      DTIME = DELTIME                                                             !SETU415
!                                                                                 !SETU416
!     If output to the list file, output file, and plotable files                 !SETU417
!     is not desired, the following statements may be removed.                    !SETU418
!     Note, however, that the HEIGHTS.DAT file is required.                       !SETU419
!     Note that output to list and other files can also be suppressed             !SETU420
!     by setting iup to 0 above, near line SETU153                                !SETU421
!                                                                                 !SETU422
      FILES(1) = LSTFL                                                            !SETU423
      FILES(10) = OUTFL                                                           !SETU424
      IF (LSTFL/='CON' .AND. LSTFL/='con' .AND. IUP>0) OPEN(IUP, FILE=LSTFL,   &  !SETU425
         IOSTAT=IERR1, POSITION='asis')                                           !SETU426
!...  Write version number and file names to standard output                      !SETU427
      WRITE (IU0, 1) VERSION                                                      !SETU428
      IF (IUP > 0) THEN                                                           !SETU429
         WRITE (IU0, 14) LSTFL, OUTFL                                             !SETU430
      ELSE                                                                        !SETU431
         WRITE (IU0, 14) 'null', 'null'                                           !SETU432
      ENDIF                                                                       !SETU433
      WRITE (IU0, 15) DATADIR, GCMDIR, MAPYEAR                                    !SETU434
      IF (NPOS == 0) WRITE (IU0, 16) TRAJFL                                       !SETU435
      IF (NPOS <= 0) THEN                                                         !SETU436
!...  If NPOS = 0 is entered, program reads position data from                    !SETU437
!      unit 7, trajectory data file                                               !SETU438
!...  Each trajectory file record contains time (seconds from initial             !SETU439
!      time), height (km), latitude (degrees, North positive), and                !SETU440
!      longitude (degrees, West positive if LonEW=0 or East positive              !SETU441
!      if LonEW=1).                                                               !SETU442
         OPEN(7, FILE=TRAJFL, STATUS='old', IOSTAT=IERR7, POSITION='asis')        !SETU443
         IF (IERR7 /= 0) THEN                                                     !SETU444
            WRITE (IU0, 11)                                                       !SETU445
   11       FORMAT(' Unable to open Trajectory Data file!')                       !SETU446
            GO TO 9998                                                            !SETU447
         ENDIF                                                                    !SETU448
      ENDIF                                                                       !SETU449
      MAXNUM = NPOS - 1                                                           !SETU450
      IF (PROFNEAR > 0.0D0) WRITE (IU0, 13) PROFILE                               !SETU451
      IF (IUWAVE > 0) WRITE (IU0, 17) WAVEFILE                                    !SETU452
      IF (NPOS <= 0) MAXNUM = 99999                                               !SETU453
!...  Write version number and file names to LIST file                            !SETU454
      IF (IUP > 0) THEN                                                           !SETU455
         WRITE (IUP, 1) VERSION                                                   !SETU456
         WRITE (IUP, 14) LSTFL, OUTFL                                             !SETU457
         WRITE (IUP, 24) DATADIR, GCMDIR                                          !SETU458
         IF (NPOS == 0) WRITE (IUP, 16) TRAJFL                                    !SETU459
         IF (PROFNEAR > 0.0D0) WRITE (IUP, 13) PROFILE                            !SETU460
         IF (IUWAVE > 0) WRITE (IUP, 17) WAVEFILE                                 !SETU461
      ENDIF                                                                       !SETU462
   13 FORMAT(' Profile file= ',(A))                                               !SETU463
   14 FORMAT(' LIST file= ',(A),/,' OUTPUT file= ',(A))                           !SETU464
   15 FORMAT(' Data directory= ',(A),/,' GCM  directory= ',(A),/,       &         !SETU465
         ' Input data from Mapping Year = ',I2)                                   !SETU466
   16 FORMAT(' Trajectory file= ',(A))                                            !SETU467
   17 FORMAT(' Wave file= ',(A))                                                  !SETU468
   24 FORMAT(' Data directory= ',(A),/,' GCM  directory= ',(A))                   !SETU469
!...  Files on units 21-27 contain parameters suitable for plotting.              !SETU470
!...  Data are in either of two forms: (1)  X  Y1 Y2 ..., where X                 !SETU471
!...  is the variable to be plotted against (e.g. height), and Y1, Y2,            !SETU472
!...  etc. are variables to be plotted, or (2) X Y Z1 Z2 ..., where X             !SETU473
!...  and Y are two variables (e.g. latitude and height) to provide               !SETU474
!...  position for plotting contour plots of one of the variables                 !SETU475
!...  Z1, Z2, etc.                                                                !SETU476
      IF (IUP > 0) THEN                                                           !SETU477
!...    Unit 21 file = 'Density.txt': Headers for variables are -                 !SETU478
!         DENSLO  =  low (-1 sigma) density                                       !SETU479
!         DENSAV  =  average (mean plus wave-perturbed) density                   !SETU480
!         DENSHI  =  high (+1 sigma) density                                      !SETU481
!         DENSTOT =  total (average+perturbed) density                            !SETU482
!           (density units kg/m**3, log-10 scale, % from COSPAR, or               !SETU483
!             kg/km**3, depending on value of LOGSCALE input parameter)           !SETU484
!         DustOD  =  dust optical depth                                           !SETU485
!         Radius  =  Radial distance from planetary center of mass to             !SETU486
!                    spacecraft position (areoid radius plus altitude)            !SETU487
!         Grav    =  local acceleration of gravity (m/s**2)                       !SETU488
!         RadAU   =  Mars orbital radius (Astronomical Units)                     !SETU489
!        LOGSCALE =  option controlling units of density output                   !SETU490
!        hgtoffset=  local height offset (km) for MTGCM and MGCM data             !SETU491
!        ibougher =  input parameter controlling height offset option             !SETU492
!        MapYear  =  TES mapping year (0 for Mars-GRAM 2001 data)                 !SETU493
!        profwgt  =  Weight factor for auxiliary input profile data               !SETU494
!...                                                                              !SETU495
         OPEN(21, FILE=FILES(2), IOSTAT=IERR2, POSITION='asis')                   !SETU496
         IF (NVARY == 0) THEN                                                     !SETU497
            WRITE (21, 621)                                                       !SETU498
         ELSE                                                                     !SETU499
            WRITE (21, 721)                                                       !SETU500
         ENDIF                                                                    !SETU501
  621    FORMAT('    Var_X        DENSLO     DENSAV     DENSHI',          &       !SETU502
            '    DENSTOT  DustOD  Radius   Grav RadAU LOGSCALE',          &       !SETU503
            ' hgtoffset ibougher MapYear profwgt')                                !SETU504
  721    FORMAT('    Var_X        Var_Y        DENSLO     DENSAV     ',   &       !SETU505
            'DENSHI    DENSTOT  DustOD  Radius   Grav RadAU LOGSCALE',    &       !SETU506
            ' hgtoffset ibougher MapYear profwgt')                                !SETU507
!...    Unit 22 file = 'Perturb.txt': Headers for variables are:                  !SETU508
!         SigD     = Density standard deviation (% of unperturbed mean)           !SETU509
!         DensRand = Random density perturbation (% of unpert. mean)              !SETU510
!         DensWave = Density wave perturbation (% of unperturbed mean)            !SETU511
!         DensP    = Total density perturbation (% of unperturbed mean)           !SETU512
!         corlim   = Ratio of step size to correlation accuracy limit             !SETU513
!                     (ideally should be 1.0 or larger)                           !SETU514
!         SigU     = Standard deviation for wind perturbations (m/s)              !SETU515
!         SigW     = Standard deviation of vertical wind perturbations            !SETU516
!                    (m/s)                                                        !SETU517
!         iupdate  = 1 if perturbations updated, 0 if perturbations not           !SETU518
!                     updated but perturbation step updated, -1 if                !SETU519
!                     neither perturbations nor step updated                      !SETU520
!...                                                                              !SETU521
         OPEN(22, FILE=FILES(3), IOSTAT=IERR3, POSITION='asis')                   !SETU522
         IF (NVARY == 0) THEN                                                     !SETU523
            WRITE (22, 622)                                                       !SETU524
         ELSE                                                                     !SETU525
            WRITE (22, 722)                                                       !SETU526
         ENDIF                                                                    !SETU527
  622    FORMAT('    Var_X      SigD  DensRand  DensWave',               &        !SETU528
            '     DensP    corlim   SigU   SigW iupdate')                         !SETU529
  722    FORMAT('    Var_X        Var_Y      SigD  DensRand',            &        !SETU530
            '  DensWave     DensP    corlim   SigU   SigW iupdate')               !SETU531
!...    Unit 23 file = 'Winds.txt' : Headers for variables are -                  !SETU532
!         EWmean = mean eastward wind component (m/s)                             !SETU533
!         EWpert = perturbation in eastward wind component (m/s)                  !SETU534
!         EWtot  = total (mean + perturbed) eastward wind (m/s)                   !SETU535
!         NSmean = mean northward wind component (m/s)                            !SETU536
!         NSpert = perturbation in northward wind component (m/s)                 !SETU537
!         NStot  = total (mean + perturbed) northward wind (m/s)                  !SETU538
!         VWpert = vertical wind perturbation (m/s)                               !SETU539
!         iupdate  = 1 if perturbations updated, 0 if perturbations not           !SETU540
!                     updated but perturbation step updated, -1 if                !SETU541
!                     neither perturbations nor step updated                      !SETU542
!...                                                                              !SETU543
         OPEN(23, FILE=FILES(4), IOSTAT=IERR4, POSITION='asis')                   !SETU544
         IF (NVARY == 0) THEN                                                     !SETU545
            WRITE (23, 623)                                                       !SETU546
         ELSE                                                                     !SETU547
            WRITE (23, 723)                                                       !SETU548
         ENDIF                                                                    !SETU549
  623    FORMAT('    Var_X      EWmean  EWpert   EWtot  ',              &         !SETU550
            'NSmean  NSpert   NStot  VWpert iupdate')                             !SETU551
  723    FORMAT('    Var_X        Var_Y      EWmean  EWpert   ',        &         !SETU552
            'EWtot  NSmean  NSpert   NStot  VWpert iupdate')                      !SETU553
!...    Unit 24 file = 'TPresHgt.txt' : Headers for variables are -               !SETU554
!         Temp    - Mean temperature (K)                                          !SETU555
!         Pres    - Mean plus wave-perturbed pressure (N/m**2, log-10,            !SETU556
!                    or % from COSPAR, as determined by LOGSCALE)                 !SETU557
!         TdegC   - Mean temperature (degrees C)                                  !SETU558
!         Pres_mb - Mean plus wave-perturbed pressure (mb)                        !SETU559
!         Hrho    - Density scale height (km)                                     !SETU560
!         Hpres   - Pressure scale height (km)                                    !SETU561
!         MolWt   - molecular weight (kg/kg-mole)                                 !SETU562
!         Terhgt  - Altitude of local surface above MOLA 1/2-degree               !SETU563
!                   areoid                                                        !SETU564
!         Tgrnd   - ground surface temperature (K)                                !SETU565
!         Areoid  - local radius (km) of MOLA 1/2-degree areoid                   !SETU566
!         dAreoid - MOLA areoid minus radius of reference                         !SETU567
!                   ellipsoid (km)                                                !SETU568
!         CO2%v   - mole fraction (%) Carbon Dioxide concentration (%             !SETU569
!                   by volume)                                                    !SETU570
!          N2%v   - mole fraction (%) Nitrogen concentration (% by                !SETU571
!                   volume)                                                       !SETU572
!          Ar%v   - mole fraction (%) Argon concentration (% by volume)           !SETU573
!          O2%v   - mole fraction (%) Molecular Oxygen concentration              !SETU574
!                   (% by volume)                                                 !SETU575
!          CO%v   - mole fraction (%) Carbon Monoxide concentration (%            !SETU576
!                   by volume)                                                    !SETU577
!           O%v   - mole fraction (%) Atomic Oxygen concentration (% by           !SETU578
!                   volume)                                                       !SETU579
!          He%v   - mole fraction (%) Helium concentration (% by                  !SETU580
!                   volume)                                                       !SETU581
!          H2%v   - mole fraction (%) Molecular Hydrogen concentration            !SETU582
!                   (% by volume)                                                 !SETU583
!           H%v   - mole fraction (%) Atomic Hydrogen concentration (%            !SETU584
!                   by volume)                                                    !SETU585
!         H2O%v   - mole fraction (%) Water vapor concentration (% by             !SETU586
!                    volume)                                                      !SETU587
!        LOGSCALE - option controlling units of pressure output                   !SETU588
!...                                                                              !SETU589
         OPEN(24, FILE=FILES(5), IOSTAT=IERR5, POSITION='asis')                   !SETU590
         IF (NVARY == 0) THEN                                                     !SETU591
            WRITE (24, 624)                                                       !SETU592
         ELSE                                                                     !SETU593
            WRITE (24, 724)                                                       !SETU594
         ENDIF                                                                    !SETU595
  624    FORMAT('    Var_X       Temp      Pres   TdegC   Pres_mb',       &       !SETU596
            '     Hrho   Hpres MolWt TerHgt Tgrnd  Areoid  dAreoid',      &       !SETU597
            ' CO2%v  N2%v  Ar%v  O2%v  CO%v   O%v  He%v  H2%v   H%v',     &       !SETU598
            ' H2O%v LOGSCALE')                                                    !SETU599
  724    FORMAT('    Var_X        Var_Y       Temp      Pres   TdegC',    &       !SETU600
            '   Pres_mb     Hrho   Hpres MolWt TerHgt Tgrnd  Areoid',     &       !SETU601
            '  dAreoid CO2%v  N2%v  Ar%v  O2%v  CO%v   O%v  He%v  H2%v',  &       !SETU602
            '   H%v H2O%v LOGSCALE')                                              !SETU603
!...                                                                              !SETU604
!...    Unit 25 file = 'DayData.txt' : Headers for variables are -                !SETU605
!         TempDay = Local daily average temperature (K)                           !SETU606
!         PresDay = Local daily average pressure (N/m**2)  (*)                    !SETU607
!         DensDay = Local daily average density (kg/m**3)  (*)                    !SETU608
!         EWwnDay = Local daily average Eastward wind (m/s)                       !SETU609
!         NSwnDay = Local daily average Northward wind (m/s)                      !SETU610
!         Tempmin = Local daily minimum temperature (K)                           !SETU611
!         Tempmax = Local daily maximum temperature (K)                           !SETU612
!         Densmin = Local daily minimum density (kg/m**3)  (*)                    !SETU613
!         Densmax = Local daily maximum density (kg/m**3)  (*)                    !SETU614
!        LOGSCALE = option controlling units of pressure and density              !SETU615
!         DensAv  = Local density (kg/m**3)  (*)                                  !SETU616
!       -----                                                                     !SETU617
!       (*) - or other units, as determined by LOGSCALE                           !SETU618
!...                                                                              !SETU619
         OPEN(25, FILE=FILES(6), IOSTAT=IERR6, POSITION='asis')                   !SETU620
         IF (NVARY == 0) THEN                                                     !SETU621
            WRITE (25, 625)                                                       !SETU622
         ELSE                                                                     !SETU623
            WRITE (25, 725)                                                       !SETU624
         ENDIF                                                                    !SETU625
         IF (PROFNEAR > 0.0) WRITE (25, *) ' No output; profile being used'       !SETU626
  625    FORMAT('    Var_X    TempDay  PresDay    DensDay',                 &     !SETU627
            '   EWwnDay NSwnDay Tempmin Tempmax   Densmin    Densmax',      &     !SETU628
            '  LOGSCALE  DensAV')                                                 !SETU629
  725    FORMAT('    Var_X        Var_Y    TempDay  PresDay',               &     !SETU630
            '    DensDay   EWwnDay NSwnDay Tempmin Tempmax   Densmin',      &     !SETU631
            '    Densmax  LOGSCALE  DensAV')                                      !SETU632
!...                                                                              !SETU633
!...    Unit 26 file = 'ThrmData.txt' : Headers for variables are -               !SETU634
!         Tbase     = temperature at 1.26 nbar level (K)                          !SETU635
!         Zbase     = height of 1.26 nbar level (km)                              !SETU636
!         F1peak    = height of F1 peak layer (km)                                !SETU637
!         MolWgt    = molecular weight (kg/kg mole)                               !SETU638
!         Texos     = exospheric temperature (K)                                  !SETU639
!         hgtoffset = local height offset (km) for MTGCM and MGCM data            !SETU640
!         ibougher  = input parameter controlling height offset option            !SETU641
!...                                                                              !SETU642
         OPEN(26, FILE=FILES(7), IOSTAT=IERR7, POSITION='asis')                   !SETU643
         IF (NVARY == 0) THEN                                                     !SETU644
            WRITE (26, 626)                                                       !SETU645
         ELSE                                                                     !SETU646
            WRITE (26, 726)                                                       !SETU647
         ENDIF                                                                    !SETU648
  626    FORMAT('    Var_X       Tbase   Zbase  F1peak',                   &      !SETU649
            '  MolWgt   Texos  hgtoffset ibougher')                               !SETU650
  726    FORMAT('    Var_X        Var_Y       Tbase   Zbase',              &      !SETU651
            '  F1peak  MolWgt   Texos  hgtoffset ibougher')                       !SETU652
!...                                                                              !SETU653
!...    Unit 27 file = 'MarsRad.txt' : Headers for variables are -                !SETU654
!         alb      = surface albedo                                               !SETU655
!         mu0      = cosine of solar zenith angle                                 !SETU656
!         Dareaden = dust column areal density (kg/m**2)                          !SETU657
!         Dmixrat  = dust mixing ratio (kg dust / kg air)                         !SETU658
!         Dmasden  = dust mass density (micrograms dust / m**3)                   !SETU659
!         Dnumden  = dust number density (number dust particles / m**3)           !SETU660
!         Ice      = surface polar ice indicator (0 = no, 1 = yes)                !SETU661
!                                                                                 !SETU662
!...                                                                              !SETU663
         OPEN(27, FILE=FILES(13), IOSTAT=IERR13, POSITION='asis')                 !SETU664
         IF (NVARY == 0) THEN                                                     !SETU665
            WRITE (27, 627)                                                       !SETU666
         ELSE                                                                     !SETU667
            WRITE (27, 727)                                                       !SETU668
         ENDIF                                                                    !SETU669
  627    FORMAT('    Var_X       alb      mu0 Dareaden  Dmixrat',          &      !SETU670
            '  Dmasden  Dnumden Ice')                                             !SETU671
  727    FORMAT('    Var_X        Var_Y       alb      mu0 Dareaden',      &      !SETU672
            '  Dmixrat  Dmasden  Dnumden Ice')                                    !SETU673
!                                                                                 !SETU674
!...    Unit 29 file = OUTPUT file containing list of variables given             !SETU675
!       in Datastep_M10 routine (Format 800 or Format 810)                        !SETU676
!                                                                                 !SETU677
!       Headers for variables are:                                                !SETU678
!         Time    = time after initial input time (sec)                           !SETU679
!         Height  = planeto-centric height (km) above MOLA areoid                 !SETU680
!                   (Height=HgtMOLA) OR planeto-centric height (km)               !SETU681
!                   above ellipsoid (Height=HgtELPS) OR planeto-centric           !SETU682
!                   height (km) above local MOLA topographic surface              !SETU683
!                   (Height=HgtSFCM) OR planeto-graphic height (km)               !SETU684
!                   above ellipsoid (Height=HgtGRPH), as determined by            !SETU685
!                   input parameters MOLAhgts, NVARX, NVARY, and ipclat           !SETU686
!          Lat    = planeto-centric latitude (Lat=LatPC) or planeto-              !SETU687
!                   graphic latitude (Lat=LatPG) in degrees (North                !SETU688
!                   positive)                                                     !SETU689
!       LonW/LonE = longitude (degrees, West positive or East Positive)           !SETU690
!        Denkgm3  = Average (mean plus wave=perturbed) density                    !SETU691
!                   (kg/m**3) OR "Logkgm3" for Log10(kg/m**3) OR                  !SETU692
!                   "Den%Avg" for percent deviation from COSPAR                   !SETU693
!                   average, OR "Denkgkm3" for kg/km**3, depending on             !SETU694
!                   input value of LOGSCALE                                       !SETU695
!         Temp    = average temperature (K)                                       !SETU696
!        EWind    = eastward wind component (m/s, positive toward East)           !SETU697
!        NWind    = northward wind component (m/s, positive toward                !SETU698
!                   North)                                                        !SETU699
!         sigD    = standard deviation for density perturbations (% of            !SETU700
!                   unperturbed mean)                                             !SETU701
!          Ls     = areocentric longitude of Sun from Mars (degrees)              !SETU702
!         Dust    = dust optical depth                                            !SETU703
!        CO2%m    = Carbon Dioxide mass concentration (% by mass)                 !SETU704
!         N2%m    = Nitrogen mass concentration (% by mass)                       !SETU705
!         Ar%m    = Argon mass concentration (% by mass)                          !SETU706
!         O2%m    = Molecular Oxygen mass concentration (% by mass)               !SETU707
!         CO%m    = Carbon Monoxide mass concentration (% by mass)                !SETU708
!          O%m    = Atomic Oxygen mass concentration (% by mass)                  !SETU709
!         He%m    = Helium mass concentration (% by mass)                         !SETU710
!         H2%m    = Molecular Hydrogen mass concentration (% by mass)             !SETU711
!          H%m    = Atomic Hydrogen mass concentration (% by mass)                !SETU712
!        H2O%m    = Water vapor mass concentration (% by mass)                    !SETU713
!        DensP    = Ratio of perturbed density to mean density                    !SETU713a    
         OPEN(29, FILE=FILES(10), IOSTAT=IERR10, POSITION='asis')                 !SETU714
         EWLON = 'W'                                                              !SETU715
         IF (LONEW == 1) EWLON = 'E'                                              !SETU716
         IF (MOLAHGTS == 1) THEN                                                  !SETU717
            HGTLBL = 'HgtMOLA'                                                    !SETU718
            IF (NVARX==2 .OR. NVARY==2) HGTLBL = 'HgtSFCM'                        !SETU719
            LATLBL = 'LatPC'                                                      !SETU720
         ELSE                                                                     !SETU721
            HGTLBL = 'HgtELPS'                                                    !SETU722
            LATLBL = 'LatPC'                                                      !SETU723
            IF (IPCLAT /= 1) THEN                                                 !SETU724
               HGTLBL = 'HgtGRPH'                                                 !SETU725
               LATLBL = 'LatPG'                                                   !SETU726
            ENDIF                                                                 !SETU727
         ENDIF                                                                    !SETU728
         IF (LOGSCALE == 0) DENSLBL = ' Denkgm3'                                  !SETU729
         IF (LOGSCALE == 1) DENSLBL = ' Logkgm3'                                  !SETU730
         IF (LOGSCALE == 2) DENSLBL = ' Den%Avg'                                  !SETU731
         IF (LOGSCALE == 3) DENSLBL = 'Denkgkm3'                                  !SETU732
         WRITE (29, 629) HGTLBL, LATLBL, EWLON, DENSLBL                           !SETU733
  629    FORMAT('     Time  ',A7,1X,A5,'   Lon',A1,3X,A8,'   Temp',            &  !SETU734
            '  EWind  NWind  sigD  Ls   Dust  LTST CO2%m  N2%m  Ar%m  O2%m',   &  !SETU735
            '  CO%m   O%m  He%m  H2%m   H%m H2O%m  DensP')                        !SETU736
      ENDIF                                                                       !SETU737
!...  Compute character string length of DATADIR path name                        !SETU738
      LENDIR = Len_Trim(DATADIR)                                                  !SETU739
      IF (LENDIR<1 .OR. LENDIR>99) LENDIR = 99                                    !SETU740
!...  Unit 9 molatoph.bin file contains MOLA 1/2 degree resolution                !SETU741
!     areoid radius and topographic heights above the areoid, versus              !SETU742
!     latitude and longitude                                                      !SETU743
      OPEN(9, FILE=DATADIR(1:LENDIR)//FILES(8), STATUS='old', FORM=            &  !SETU744
         'unformatted', IOSTAT=IERR8, POSITION='asis')                            !SETU745
!...  Unit 10 COSPAR2.DAT file contains COSPAR atmosphere data values             !SETU746
      OPEN(10, FILE=DATADIR(1:LENDIR)//FILES(9), STATUS='old', IOSTAT=IERR9,   &  !SETU747
         POSITION='asis')                                                         !SETU748
!...  Unit 11 hgtoffst.dat file with MTGCM height offset array                    !SETU749
      OPEN(11, FILE=DATADIR(1:LENDIR)//FILES(11), STATUS='old', IOSTAT=IERR11, &  !SETU750
         POSITION='asis')                                                         !SETU751
!...  Unit 12 albedo1.bin binary format albedo file                               !SETU752
      OPEN(12, FILE=DATADIR(1:LENDIR)//FILES(12), STATUS='old', FORM=          &  !SETU753
         'unformatted', IOSTAT=IERR12, POSITION='asis')                           !SETU754
!...  Test for file open error condition                                          !SETU755
      DO J = 1, 13                                                                !SETU756
         IF (IERR(J) == 0) CYCLE                                                  !SETU757
         WRITE (IU0, 60) IERR(J), FILES(J)                                        !SETU758
   60    FORMAT(' File open error! Error =',I5,1X,(A))                            !SETU759
         GO TO 9998                                                               !SETU760
      END DO                                                                      !SETU761
!......................................................................           !SETU762
!...  Read COSPAR2.DAT atmosphere data                                            !SETU763
      DO I = 1, 164                                                               !SETU764
         READ (10, *, END=65, ERR=65) ZC(I), TC(I), PC(I), DC(I)                  !SETU765
      END DO                                                                      !SETU766
      GO TO 70                                                                    !SETU767
   65 CONTINUE                                                                    !SETU768
      STOP ' Error or EOF on COSPAR2.DAT file!'                                   !SETU769
!...  Read MTGCM and TES yr1 and yr 2 height offsets                              !SETU770
   70 CONTINUE                                                                    !SETU771
      READ (11, 66) DUMMY                                                         !SETU772
   66 FORMAT(A1)                                                                  !SETU773
      DO I = 0, 12                                                                !SETU774
         READ (11, *, ERR=72) LS, (OFFSETS(I,J),J=1,3)                            !SETU775
         IF (LS == 30*I) CYCLE                                                    !SETU776
   72    CONTINUE                                                                 !SETU777
         STOP ' Error reading MTGCM height offsets'                               !SETU778
      END DO                                                                      !SETU779
      READ (11, 66) DUMMY                                                         !SETU780
      DO I = 0, 12                                                                !SETU781
         READ (11, *, ERR=73) LS, (TOFFSETS(I,J),J=1,2)                           !SETU782
         IF (LS == 30*I) CYCLE                                                    !SETU783
   73    CONTINUE                                                                 !SETU784
         STOP ' Error reading TES Yr1 and Yr2 height offsets'                     !SETU785
      END DO                                                                      !SETU786
!...  Read topographic height data file                                           !SETU787
      WRITE (IU0, *) ' Reading MOLA 1/2 degree areoid and topography'             !SETU788
      READ (9) AREORAD, TOPOMOLA                                                  !SETU789
!...  Read 1 degree resolution albedo file                                        !SETU790
      WRITE (IU0, *) ' Reading 1 degree albedo data'                              !SETU791
      READ (12) ALBEDO                                                            !SETU792
      WRITE (IU0, *) ' Reading Mars GCM surface data files'                       !SETU793
      CALL READSURF_M10 (GCMDIR, VERSION)                                         !SETU794
      WRITE (IU0, *) ' Reading Mars GCM 0-80 km data files'                       !SETU795
      CALL READMGCM_M10 (GCMDIR, VERSION)                                         !SETU796
      WRITE (IU0, *) ' Reading Mars TGCM 80-170 km data files'                    !SETU797
      CALL READTGCM_M10 (GCMDIR, VERSION)                                         !SETU798
      WRITE (IU0, *) ' Reading TES Yr1 & Yr2 MGCM surface data files'             !SETU799
      CALL RDTESSRF_M10 (GCMDIR, VERSION)                                         !SETU800
      WRITE (IU0, *) ' Reading TES Yr1 & Yr2 MGCM -5 to 80 km data files'         !SETU801
      CALL RDTESMGCM_M10 (GCMDIR, VERSION)                                        !SETU802
      WRITE (IU0, *) ' Reading TES Yr1 & Yr2 MTGCM 80-240 km data files'          !SETU803
      CALL RDTESTGCM_M10 (GCMDIR, VERSION)                                        !SETU804
      DTR = DATAN(1.0D0)/45.0D0                                                   !SETU805
!...  Check date; If error,write message and stop                                 !SETU806
      CALL CHKDT_M10 (MYEAR, MONTH, MDAY, IHR, IMIN, SEC, IDTERR)                 !SETU807
      IF (IDTERR < (-6)) THEN                                                     !SETU808
         WRITE (IU0, 92)                                                          !SETU809
      ELSE IF (IDTERR < 0) THEN                                                   !SETU810
         WRITE (IU0, 91)                                                          !SETU811
      ENDIF                                                                       !SETU812
      IF (IDTERR >= 0) THEN                                                       !SETU813
   91    FORMAT(' Input error in month, day or year.')                            !SETU814
   92    FORMAT(' Input error in hour, minute or seconds.')                       !SETU815
!...  Compute Julian day                                                          !SETU816
         CALL CALTOJUL_M10 (MYEAR, MONTH, MDAY, IHR, IMIN, SEC, DAY)              !SETU817
         DAY0 = DAY                                                               !SETU818
         IF (IUP > 0) THEN                                                        !SETU819
            IF (IERT == 1) THEN                                                   !SETU820
               WRITE (IUP, 283)                                                   !SETU821
            ELSE                                                                  !SETU822
               WRITE (IUP, 284)                                                   !SETU823
            ENDIF                                                                 !SETU824
            IF (IUTC == 1) THEN                                                   !SETU825
               WRITE (IUP, 285)                                                   !SETU826
            ELSE                                                                  !SETU827
               WRITE (IUP, 286)                                                   !SETU828
            ENDIF                                                                 !SETU829
  283       FORMAT(' Input time is Earth-Receive Time (ERT)')                     !SETU830
  284       FORMAT(' Input time is Mars-Event Time (MET)')                        !SETU831
  285       FORMAT(' Input time is Coordinated Universal Time (UTC)')             !SETU832
  286       FORMAT(' Input time is Terrestrial (Dynamical) Time (TT)')            !SETU833
            WRITE (IUP, 290) MONTH, MDAY, MYEAR, DAY, IHR, IMIN, SEC              !SETU834
         ENDIF                                                                    !SETU835
  290    FORMAT(' Date = ',I2,'/',I2,'/',I4,'  Julian Day = ',F13.5,'  Time = ' & !SETU836
            ,I2,':',I2,':',F4.1)                                                  !SETU837
!---     DTEX = DELTATEX                                                          !SETU838
         IF (IUP > 0) THEN                                                        !SETU839
            IF (dabs(DELTATEX)>0.0D0) WRITE (IUP, 291) DELTATEX                   !SETU840
  291       FORMAT(' deltaTEX=',F7.1,'K')                                         !SETU841
            IF (MOLAHGTS == 1) THEN                                               !SETU842
               WRITE (IUP, 292)                                                   !SETU843
            ELSE                                                                  !SETU844
               IF (IPCLAT == 1) THEN                                              !SETU845
                  WRITE (IUP, 294)                                                !SETU846
               ELSE                                                               !SETU847
                  WRITE (IUP, 293)                                                !SETU848
               ENDIF                                                              !SETU849
            ENDIF                                                                 !SETU850
  292       FORMAT(' Input heights are planeto-centric, relative to MOLA',      & !SETU851
               ' areoid')                                                         !SETU852
  293       FORMAT(' Input heights are planeto-graphic (relative to ',          & !SETU853
               ' reference ellipsoid)')                                           !SETU854
  294       FORMAT(' Input heights are planeto-centric, relative to ',          & !SETU855
               ' reference ellipsoid')                                            !SETU856
            WRITE (IUP, 297) REQUA, RPOLE                                         !SETU857
  297       FORMAT(' Reference ellipsoid radii (km): Equator =',F8.2,' Pole =', & !SETU858
               F8.2)                                                              !SETU859
            WRITE (IUP, 296)                                                      !SETU860
  296       FORMAT(' Output heights are planeto-centric, except as noted.'/,    & !SETU861
               ' Longitude & ephemeris use IAU 2000 rotational system.')          !SETU862
         ENDIF                                                                    !SETU863
!...  Sun position in Mars latitude (areocentric latitude) and                    !SETU864
!...  longitude. ALS = Ls = areocentric longitude of sun in orbital               !SETU865
!...  position (Ls = 0 at spring equinox). MARSAU = Mars orbital                  !SETU866
!...  radius in Astronomical Units                                                !SETU867
         IF (DRADAU > 0.0D0) THEN                                                 !SETU868
            ALS = DSUNLS                                                          !SETU869
            MARSAU = DRADAU                                                       !SETU870
            OWLT = DOWLT                                                          !SETU871
         ELSE                                                                     !SETU872
!...    Use built-in Mars ephemeris routine                                       !SETU873
!...    Convert to Terrestrial (Dynamical) Time, if necessary                     !SETU874
            TTSEC = 0.0D0                                                         !SETU875
            IF (IUTC == 1) THEN                                                   !SETU876
!...      Get terrestrial dynamical time offset (seconds)                         !SETU877
               DT = (DAY0 - 2451545.0D0)/36525.0D0                                !SETU878
!...      Terrestrial time offset (in seconds) TT = UTC + ttsec                   !SETU879
               TTSEC = (64.184D0 + 95.0D0*DT + 35.0D0*DT**2)/86400.0D0            !SETU880
            ENDIF                                                                 !SETU881
            CALL MARSEPHM_M10 (DAY0 + TTSEC, SUNLAT, SUNLON, ALS, MARSAU, OWLT &  !SETU882
               , EOT)                                                             !SETU883
!...    Convert to Mars-Event Time, if necessary                                  !SETU884
            IF (IERT == 1) CALL MARSEPHM_M10 (DAY0 + TTSEC - OWLT/6050.0D0,    &  !SETU885
               SUNLAT, SUNLON, ALS, MARSAU, OWLT, EOT)                            !SETU886
!...    Convert planetographic sun latitude to planetocentric                     !SETU887
            SUNLAT = DATAN(DTAN(SUNLAT*DTR)/(3396.0D0/3378.32D0)**2)/DTR          !SETU888
         ENDIF                                                                    !SETU889
         IF (ALS0<120.0D0 .OR. ALS0>320.0D0) THEN                                 !SETU890
            IF (ALS0 > 0.0D0) THEN                                                !SETU891
               WRITE (IU0, *) ' ** Ls0 outside range. No dust storm assumed.'     !SETU892
               IF (IUP > 0) WRITE (IUP, *) ' ** Ls0 outside range. ',          &  !SETU893
                  'No dust storm assumed.'                                        !SETU894
            ENDIF                                                                 !SETU895
            ALS0 = -999.0D0                                                       !SETU896
            INTENS = 0.0D0                                                        !SETU897
         ELSE                                                                     !SETU898
            IF (INTENS<0.0D0 .OR. INTENS>3.0D0) THEN                              !SETU899
               WRITE (IU0, *) ' Intensity must be between 0 and 3'                !SETU900
               GO TO 9998                                                         !SETU901
            ENDIF                                                                 !SETU902
            IF (RADMAX<=0.0D0 .OR. RADMAX>10000.0D0) RADMAX = 0.0D0               !SETU903
         ENDIF                                                                    !SETU904
!...  Set ALSDUR within allowable range                                           !SETU905
         ALSDUR = DMAX1(12.0D0,ALSDUR)                                            !SETU906
         ALSDUR = MIN(48.0D0,ALSDUR)                                              !SETU907
         IF (F107<50.0D0 .OR. F107>450.0D0) THEN                                  !SETU908
            WRITE (IU0, *) ' F10.7 must be between 50 and 450'                    !SETU909
            GO TO 9998                                                            !SETU910
         ENDIF                                                                    !SETU911
         IF (NR1<=0 .OR. NR1>=30000) THEN                                         !SETU916
            WRITE (IU0, 298)                                                      !SETU917
  298       FORMAT(' Error in starting random number.')                           !SETU918
            GO TO 9998                                                            !SETU919
         ENDIF                                                                    !SETU920
         IX = NR1                                                                 !SETU921
         IY = 172*MOD(IX,176) - 35*(IX/176)                                       !SETU922
         IZ = 170*MOD(IX,178) - 63*(IX/178)                                       !SETU923
         IF (IY < 0) IY = IY + 30307                                              !SETU924
         IF (IZ < 0) IZ = IZ + 30323                                              !SETU925
         Z1 = RANDOM_M10(L)                                                       !SETU926
         RHOD = PPND_M10(Z1,L)                                                    !SETU927
         Z1 = RANDOM_M10(L)                                                       !SETU928
         RHOU = PPND_M10(Z1,L)                                                    !SETU929
         Z1 = RANDOM_M10(L)                                                       !SETU930
         RHOV = PPND_M10(Z1,L)                                                    !SETU931
         Z1 = RANDOM_M10(L)                                                       !SETU932
         RHOW = PPND_M10(Z1,L)                                                    !SETU933
         IF (L == 1) THEN                                                         !SETU934
            WRITE (IU0, 298)                                                      !SETU935
            GO TO 9998                                                            !SETU936
         ENDIF                                                                    !SETU937
         IF (LSTFL/='CON' .AND. LSTFL/='con' .AND. IUP>0) THEN                    !SETU938
            IF (ALS0 > 0.0D0) THEN                                                !SETU939
               IF (dabs(RADMAX)>0.0D0) THEN                                       !SETU940
                  WRITE (IUP, 363) ALS0, INTENS, ALSDUR, RADMAX, DUSTLAT,       & !SETU941
                     DUSTLON, 360.0D0 - DUSTLON                                   !SETU942
  363             FORMAT(' Local scale dust storm, starting at Ls = ',F5.1,     & !SETU943
                     ' deg.,  Intensity =',F4.1,/,'  with duration = ',F5.1,    & !SETU944
                     ' degrees of Ls angle.'/,' Max. radius = ',F7.1,           & !SETU945
                     ' km,  At Lat-Lon = ',F6.2,' N,  ',F6.2,' W (',F6.2,' E)')   !SETU946
               ELSE                                                               !SETU947
                  WRITE (IUP, 366) ALS0, INTENS, ALSDUR                           !SETU948
  366             FORMAT(' Global scale dust storm, starting at Ls = ',F5.1,    & !SETU949
                     ' deg.,  Intensity =',F4.1,/,'  with duration = ',F5.1,    & !SETU950
                     ' degrees of Ls angle.')                                     !SETU951
               ENDIF                                                              !SETU952
            ENDIF                                                                 !SETU953
            IF (IUP > 0) THEN                                                     !SETU954
               WRITE (IUP, 368) F107, F107/MARSAU**2                              !SETU955
               IF (MAPYEAR == 0) THEN                                             !SETU956
                  WRITE (IUP, 371)                                                !SETU957
               ELSE                                                               !SETU958
                  WRITE (IUP, 372) MAPYEAR                                        !SETU959
               ENDIF                                                              !SETU960
               WRITE (IUP, 369) DUSTNU, DUSTDIAM, DUSTDENS                        !SETU961
            ENDIF                                                                 !SETU962
  368       FORMAT(' F10.7 flux = ',F5.1,' (1 AU)  ',F5.1,' (Mars)')              !SETU963
  369       FORMAT(' Dustnu =',F7.4,'   Dustdiam =',F6.2,                       & !SETU965
               ' E-6 meters   Dustdens =',F8.1,' kg/m**3')                        !SETU966
            IF (IUP > 0) WRITE (IUP, 370) NR1, RPSCALE, CORLMIN, RWSCALE,       & !SETU967
               WLSCALE, WMSCALE, BLWINFAC                                         !SETU968
         ENDIF                                                                    !SETU969
  370    FORMAT('   Random seed =',I6,'  Dens.Pert.Scale Factor =',F5.2,        & !SETU970
            '   corlmin =',F6.3,/,'   Wind.Pert.Scale Factor =',F6.2,           & !SETU971
            '    Wavelength Scale Factor =',F6.2,/,'   Mean Wind Scale',        & !SETU972
            ' Factor =',F6.2,'    Slope Wind Scale Factor =',F6.2)                !SETU973
  371    FORMAT(' Dust optical depth from NAMELIST input')                        !SETU974
  372    FORMAT(' Dust optical depth vs lat and Ls from TES Mapping Year ',I1,  & !SETU975
            ' data')                                                              !SETU976
  380    FORMAT(/,' Select x-code and y-code for plotable output versus',       & !SETU977
            ' desired parameter(s):'/,/,' Code              Parameter'/,        & !SETU978
            ' ----   -------------------------------------------------'/,       & !SETU979
            '   1    Height (above MOLA areoid, km)'/,                          & !SETU980
            '   2    Height (above local MOLA topographic surface, km)'/,       & !SETU981
            '   3    Latitude (deg.)'/,                                         & !SETU982
            '   4    Longitude (deg.) West if LonEW=0, East if LonEW=1'/,       & !SETU983
            '   5    Time from start (Earth seconds)'/,                         & !SETU984
            '   6    Time from start (Martian Sols)'/,                          & !SETU985
            '   7    Areocentric Longitude of Sun, Ls (deg.)'/,                 & !SETU986
            '   8    Local Solar Time (Mars hours = 1/24th sol)'/,              & !SETU987
            '   9    Pressure (mb)'/,                                           & !SETU988
            '  10    Pressure Height (km) [-H*log(Pres/PresSurf) = ',           & !SETU989
                       '-H*log(sigma)]'/,                                       & !SETU990
            '  11    Sigma coordinate [sigma=Pressure/(Surface Pressure)]'/,    & !SETU991
            '  12    Height (km) above reference ellipsoid'/,                   & !SETU992
            '  13    Planeto-Graphic Height (km) above reference ellipsoid'/,   & !SETU993
            '  14    Planeto-Graphic Latitude (deg.)'/,                         & !SETU994
            '  15    Longitude in range -180 to +180 deg. (East or West)'/,/,   & !SETU995
            ' Use y-code = 0 for plotable output vs x-code variable only')        !SETU996
         IF (NVARX<1 .OR. NVARX>15) THEN                                          !SETU997
            WRITE (IU0, 381)                                                      !SETU998
  381       FORMAT(' x-code or y-code input error.')                              !SETU999
            WRITE (IU0, 380)                                                      !SETU1000
            GO TO 9998                                                            !SETU1001
         ENDIF                                                                    !SETU1002
         IF (NVARY<0 .OR. NVARY>15) THEN                                          !SETU1003
            WRITE (IU0, 381)                                                      !SETU1004
            WRITE (IU0, 380)                                                      !SETU1005
            GO TO 9998                                                            !SETU1006
         ENDIF                                                                    !SETU1007
         IF (LOGSCALE<0 .OR. LOGSCALE>3) LOGSCALE = 0                             !SETU1008
!...  Initialize position data                                                    !SETU1009
         CHGT = FHGT                                                              !SETU1010
         CLAT = FLAT                                                              !SETU1011
         CLON = FLON                                                              !SETU1012
         CSEC = 0.0D0                                                             !SETU1013
         WRITE (IU0, *) ' Finished Setup_M10 - Starting computations'             !SETU1014
         RETURN                                                                   !SETU1015
      ENDIF                                                                       !SETU1016
 9998 CONTINUE                                                                    !SETU1017
      STOP ' Error termination! Check the LIST file for messages.'                !SETU1018
      END SUBROUTINE SETUP_M10                                                    !SETU1019
!                                                                                 !SETU1020
!-------------------------------------------------------------------------------  !RNDI  1
      SUBROUTINE RANDINIT_M10(J, NR1, RHOD, RHOU, RHOV, RHOW, IUP, IUSTDOUT)      !RNDI  2
!-----------------------------------------------                                  !RNDI  3
!   M o d u l e s                                                                 !RNDI  4
!-----------------------------------------------                                  !RNDI  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !RNDI  6
!...                                                                              !RNDI  7
!...Switches:                                                                     !RNDI  8
!-----------------------------------------------                                  !RNDI  9
!   I n t e r f a c e   B l o c k s                                               !RNDI 10
!-----------------------------------------------                                  !RNDI 11
      USE random_M10_I                                                            !RNDI 12
      USE ppnd_M10_I                                                              !RNDI 13
      IMPLICIT NONE                                                               !RNDI 14
!-----------------------------------------------                                  !RNDI 15
!   D u m m y   A r g u m e n t s                                                 !RNDI 16
!-----------------------------------------------                                  !RNDI 17
      INTEGER , INTENT(IN) :: J                                                   !RNDI 18
      INTEGER , INTENT(INOUT) :: NR1                                              !RNDI 19
      INTEGER , INTENT(IN) :: IUP                                                 !RNDI 20
      INTEGER , INTENT(IN) :: IUSTDOUT                                            !RNDI 21
      REAL(DOUBLE) , INTENT(OUT)  :: RHOD                                         !RNDI 22
      REAL(DOUBLE) , INTENT(OUT)  :: RHOU                                         !RNDI 23
      REAL(DOUBLE) , INTENT(OUT)  :: RHOV                                         !RNDI 24
      REAL(DOUBLE) , INTENT(OUT)  :: RHOW                                         !RNDI 25
!-----------------------------------------------                                  !RNDI 26
!   C o m m o n   B l o c k s                                                     !RNDI 27
!-----------------------------------------------                                  !RNDI 28
!...  /RANDCOM_M10/                                                               !RNDI 29
      COMMON /RANDCOM_M10/ IX, IY, IZ                                             !RNDI 30
      INTEGER   IX, IY, IZ                                                        !RNDI 31
!-----------------------------------------------                                  !RNDI 32
!   L o c a l   V a r i a b l e s                                                 !RNDI 33
!-----------------------------------------------                                  !RNDI 34
      INTEGER :: L                                                                !RNDI 35
!-----------------------------------------------                                  !RNDI 36
!...  Re-initialize NR1, e.g. by reading from a file, or some algorithm           !RNDI 37
!     from previous NR1 value, or by some computation on index J.                 !RNDI 38
!     Note that it is not necessary to randomly select each seed value            !RNDI 39
!     NR1 in order to get a random sequence of output.  Any regular               !RNDI 40
!     progression of selected NR1 values will do for this process.                !RNDI 41
      NR1 = NR1 + 11                                                              !RNDI 42
      IF (NR1 > 30000) NR1 = MOD(NR1,30000)                                       !RNDI 43
!...  Write random seed value to list file                                        !RNDI 44
      IF (IUP /= 0) THEN                                                          !RNDI 45
         WRITE (IUP, 10) J, NR1                                                   !RNDI 46
         IF (J > 1)WRITE (IUSTDOUT, 10) J, NR1                                    !RNDI 46a
      ELSE                                                                        !RNDI 47
         WRITE (IUSTDOUT, 10) J, NR1                                              !RNDI 48
      ENDIF                                                                       !RNDI 49
   10 FORMAT('   Random seed number',I6,' =',I6)                                  !RNDI 50
      IX = NR1                                                                    !RNDI 51
      IY = 172*MOD(IX,176) - 35*(IX/176)                                          !RNDI 52
      IZ = 170*MOD(IX,178) - 63*(IX/178)                                          !RNDI 53
      IF (IY < 0) IY = IY + 30307                                                 !RNDI 54
      IF (IZ < 0) IZ = IZ + 30323                                                 !RNDI 55
      RHOD = RANDOM_M10(L)                                                        !RNDI 56
      RHOD = PPND_M10(RHOD,L)                                                     !RNDI 57
      RHOU = RANDOM_M10(L)                                                        !RNDI 58
      RHOU = PPND_M10(RHOU,L)                                                     !RNDI 59
      RHOV = RANDOM_M10(L)                                                        !RNDI 60
      RHOV = PPND_M10(RHOV,L)                                                     !RNDI 61
      RHOW = RANDOM_M10(L)                                                        !RNDI 62
      RHOW = PPND_M10(RHOW,L)                                                     !RNDI 63
      RETURN                                                                      !RNDI 64
      END SUBROUTINE RANDINIT_M10                                                 !RNDI 65
!                                                                                 !RNDI 66
!------------------------------------------------------------------------------   !CKDT  1
      subroutine chkdt_M10(myear, month, iday, ihour, minutes, sec, err)          !CKDT  2
!-----------------------------------------------                                  !CKDT  3
!   M o d u l e s                                                                 !CKDT  4
!-----------------------------------------------                                  !CKDT  5
      USE vast_kind_param, ONLY:  double                                          !CKDT  6
!                                                                                 !CKDT  7
!      CHecKs input Date and Time for validity and internal                       !CKDT  8
!      consistency.  Returns error message(s) and prompts                         !CKDT  9
!      user to re-enter inputs.                                                   !CKDT 10
!                                                                                 !CKDT 11
!...Switches:                                                                     !CKDT 13
      implicit none                                                               !CKDT 14
!-----------------------------------------------                                  !CKDT 15
!   D u m m y   A r g u m e n t s                                                 !CKDT 16
!-----------------------------------------------                                  !CKDT 17
      integer , intent(inout) :: myear                                            !CKDT 18
      integer , intent(in) :: month                                               !CKDT 19
      integer , intent(in) :: iday                                                !CKDT 20
      integer , intent(in) :: ihour                                               !CKDT 21
      integer , intent(in) :: minutes                                             !CKDT 22
      integer , intent(out) :: err                                                !CKDT 23
      real(double) , intent(in) :: sec                                            !CKDT 24
!-----------------------------------------------                                  !CKDT 25
!   L o c a l   V a r i a b l e s                                                 !CKDT 26
!-----------------------------------------------                                  !CKDT 27
      logical :: centyear, leapyear                                               !CKDT 28
!-----------------------------------------------                                  !CKDT 29
      err = 0                                                                     !CKDT 30
      centyear = .FALSE.                                                          !CKDT 31
      leapyear = .FALSE.                                                          !CKDT 32
!...   Convert to 4-digit year, if necessary                                      !CKDT 33
      if (myear>=0 .and. myear<=69) myear = myear + 2000                          !CKDT 34
      if (myear>=70 .and. myear<=99) myear = myear + 1900                         !CKDT 35
      if (myear<1970 .or. myear>2069) then                                        !CKDT 36
         write (*, *) ' Year must be 1970-2069'                                   !CKDT 37
         err = -1                                                                 !CKDT 38
      endif                                                                       !CKDT 39
      if (mod(myear,100) == 0) centyear = .TRUE.                                  !CKDT 40
      if (mod(myear,4) == 0) leapyear = .TRUE.                                    !CKDT 41
      if (centyear) then                                                          !CKDT 42
         if (mod(myear,400) /= 0) leapyear = .FALSE.                              !CKDT 43
      endif                                                                       !CKDT 44
!...     Write(*,*)' Month must be 1-12'                                          !CKDT 45
      if (month<1 .or. month>12) err = -2                                         !CKDT 46
      if (month==4 .or. month==6 .or. month==9 .or. month==11) then               !CKDT 47
!...       Write(*,*)' Day of month must be 1-30'                                 !CKDT 48
         if (iday<1 .or. iday>30) err = -3                                        !CKDT 49
      else if (month == 2) then                                                   !CKDT 50
         if (leapyear) then                                                       !CKDT 51
!...         Write(*,*)' Day of month must be 1-29 (Leap Year)'                   !CKDT 52
            if (iday<1 .or. iday>29) err = -4                                     !CKDT 53
         else if (iday<1 .or. iday>28) then                                       !CKDT 54
!...       Write(*,*)' Day of month must be 1-28 (Non-Leap Year)'                 !CKDT 55
            err = -5                                                              !CKDT 56
         endif                                                                    !CKDT 57
      else if (iday<1 .or. iday>31) then                                          !CKDT 58
!...       Write(*,*)' Day of month must be 1-31'                                 !CKDT 59
         err = -6                                                                 !CKDT 60
      endif                                                                       !CKDT 61
!...     Write(*,*)' Hour must be 0-24'                                           !CKDT 62
      if (ihour<0 .or. ihour>24 .or. ihour==24 .and. (minutes/=0 .or. dabs(sec) & !CKDT 63
         > 0.0D0)) err = -7                                                       !CKDT 64
!...     Write(*,*)' Hour must be 23 or Time must be 24:00:00'                    !CKDT 65
      if (minutes<0 .or. minutes>60 .or. minutes==60 .and. dabs(sec)>0.0D0) then  !CKDT 66
!...     Write(*,*)' Minutes must be 0-60'                                        !CKDT 67
         err = -8                                                                 !CKDT 68
      endif                                                                       !CKDT 69
!...     Write(*,*)' Minutes must be 59 or Seconds must be 0'                     !CKDT 70
      if (sec<0.0D0 .or. sec>60.0D0) then                                         !CKDT 71
!...     Write(*,*)' Seconds must be 0.0-60.0'                                    !CKDT 72
         err = -9                                                                 !CKDT 73
      endif                                                                       !CKDT 74
      return                                                                      !CKDT 75
      end subroutine chkdt_M10                                                    !CKDT 76
!----------------------------------------------------------------------           !CKDT 77
