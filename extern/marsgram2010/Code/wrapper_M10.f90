      SUBROUTINE MARSTRAJ_M10(ISETUP, JMONTE, NMONTE, ISTEP, MAXNUM, ZHGT, ZLAT & !MART  1
         , ZLON, ZSEC, PZHGT, PZLAT, PZLON, PZSEC, DAY0, TEMP, PRES, DENSLO,    & !MART  2
         DENS, DENSHI, DENSP, RHOD, RHOU, RHOV, EWWIND, EWPERT, NSWIND, NSPERT  & !MART  3
         , EOF, NR1, DENSTOT, HRHO, HPRES, DSUNLAT, DSUNLON, DSUNLS, DRADAU,    & !MART  4
         DOWLT, CORLIM, IUPDATE, PERTSTEP, INPUTFL, IUSTDOUT, ALS, SZANG, OWLT  & !MART  5
         , SUNLAT, SUNLON, MARSAU, TLOCAL)                                        !MART  6
!-----------------------------------------------                                  !MART  7
!   M o d u l e s                                                                 !MART  8
!-----------------------------------------------                                  !MART  9
      USE vast_kind_param, ONLY:  DOUBLE                                          !MART 10
      USE DATACOM_M10_C                                                           !MART 11
!                                                                                 !MART 12
!     Input argument list variables, supplied from trajectory code:               !MART 13
!                                                                                 !MART 14
!     isetup = 1 for Mars-GRAM setup mode (reads NAMELIST INPUT file)             !MART 15
!              0 for Mars-GRAM evaluation mode (computes atmosphere)              !MART 16
!     jmonte = counter for number of Monte-Carlo cases:                           !MART 17
!              > 0 initializes next Monte-Carlo case                              !MART 18
!              = 0 during trajectory for each Monte-Carlo case                    !MART 19
!     istep  = counter for number of trajectory point steps.                      !MART 20
!              Trajectory computations end when istep=MAXNUM or EOF=1             !MART 21
!     zhgt   = next height at which Mars-GRAM is evaluated                        !MART 22
!              *** NOTE - If height zhgt is above reference ellipsoid,            !MART 23
!              then MOLAhgts should be set to 0 in the NAMELIST                   !MART 24
!              input file (read in by Setup_M10 subroutine) ***                   !MART 25
!     zlat   = next latitude at which Mars-GRAM is evaluated                      !MART 26
!     zlon   = next longitude at which Mars-GRAM is evaluated                     !MART 27
!              *** NOTE - If zlon is positive East, LonEW should be               !MART 28
!              set to 1 in the NAMELIST input file (read in by the                !MART 29
!              Setup_M10 subroutine) ***                                          !MART 30
!     zsec   = next time (sec) at which Mars-GRAM is evaluated                    !MART 31
!     pzhgt  = last height at which Mars-GRAM was evaluated                       !MART 32
!              *** NOTE - If height zhgt is above reference ellipsoid,            !MART 33
!               then MOLAhgts should be set to 0 in the NAMELIST                  !MART 34
!               input file (read in by Setup_M10 subroutine) ***                  !MART 35
!     pzlat  = last latitude at which Mars-GRAM was evaluated                     !MART 36
!     pzlon  = last longitude at which Mars-GRAM was evaluated                    !MART 37
!              *** NOTE - If pzlon is positive East, LonEW should be              !MART 38
!              set to 1 in the NAMELIST input file (read in by the                !MART 39
!              Setup_M10 subroutine) ***                                          !MART 40
!     pzsec  = last time (sec) at which Mars-GRAM was evaluated                   !MART 41
!     DAY0   = Julian day (from date and time in INPUT file or user-              !MART 42
!              provided input). If provided as user input, DAY0                   !MART 43
!              should be consistent with choices used for date/time               !MART 44
!              input parameters.                                                  !MART 45
!     iupdate  = input value >= 0 to update total step for perturbation           !MART 46
!                used; <0 for no update of total step for perturbations           !MART 47
!     INPUTFL  = name for NAMELIST format input file                              !MART 48
!     iustdout = unit number for standard (screen) output                         !MART 49
!     Optional high precision ephemeris inputs are:                               !MART 50
!       dsunlat = high precision latitude of sub-solar point (deg)                !MART 51
!       dsunlon = high precision longitude of sub-solar point (deg)               !MART 52
!       dsunLs  = high precision Ls angle (deg)                                   !MART 53
!       dradau  = high precision Mars orbital radius (AU)                         !MART 54
!       dowlt   = high precision one-way light time (minutes)                     !MART 55
!     If high precision ephemeris data are not used, input 0 values to            !MART 56
!     use the regular ephemeris subroutine of Mars-GRAM                           !MART 57
!                                                                                 !MART 58
!     Output argument list variables, supplied from Mars-GRAM:                    !MART 59
!                                                                                 !MART 60
!     NMONTE = number of Monte-Carlo cases (from INPUT file)                      !MART 61
!     MAXNUM = maximum points in trajectory (from INPUT file)                     !MART 62
!     TEMP   = atmospheric temperature (K)                                        !MART 63
!     PRES   = atmospheric (mean + wave-perturbed) pressure (N/m**2)              !MART 64
!     DENSLO = low (approx. -1 sigma) density (kg/m**3)                           !MART 65
!     DENS   = average (mean + wave-perturbed) density (kg/m**3)                  !MART 66
!     DENSHI = high (approx. +1 sigma) density (kg/m**3)                          !MART 67
!     DENSP  = density perturbation (percent of unperturbed mean)                 !MART 68
!     RHOd   = random part of density perturbation (in sigma units)               !MART 69
!     RHOu   = random part of EW wind perturbation (in sigma units)               !MART 70
!     RHOv   = random part of NS wind perturbation (in sigma units)               !MART 71
!     EWWIND = mean Eastward wind component (m/s)                                 !MART 72
!     EWpert = perturbed part of Eastward wind (m/s)                              !MART 73
!     NSWIND = mean Northward wind component (m/s)                                !MART 74
!     NSpert = perturbed part of Northward wind component (m/s)                   !MART 75
!     EOF    = Parameter to end trajectory calculation (if EOF = 1)               !MART 76
!     NR1    = Starting random number seed (from INPUT file)                      !MART 77
!     DENSTOT= Total (mean plus perturbed) density (kg/m**3)                      !MART 78
!     Hrho   = Density scale height (km)                                          !MART 79
!     Hpres  = Pressure scale height (km)                                         !MART 80
!     corlim = ratio of step size to smallest step size for assured               !MART 81
!              accuracy in perturbation model (should be > 1)                     !MART 82
!     iupdate = output value = 1 if perturbations updated, 0 if not               !MART 83
!               updated but step distance updated, -1 if neither                  !MART 84
!               perturbations nor total step distance updated                     !MART 85
!     pertstep= accumulating step distance, relative to accuracy limit            !MART 86
!     ALS     = Planeto-centric longitude of Sun (Ls, degrees)                    !MART 87
!     szang   = Solar zenith angle (degrees)                                      !MART 88
!     owlt    = Mars-Earth one-way light time (minutes)                           !MART 89
!     sunlat  = Sub-solar latitude (degrees)                                      !MART 90
!     sunlon  = Sub-solar longitude (degrees)                                     !MART 91
!     MarsAU  = Mars orbital radius (AU)                                          !MART 92
!     TLOCAL  = Local Solar time (Mars hours)                                     !MART 93
!                                                                                 !MART 94
!     Other variables from the argument list of subroutine Datastep_M10           !MART 95
!     can be passed to your trajectory program by adding them to the              !MART 96
!     argument list of the Marstraj_M10 subroutine.  See comments below           !MART 97
!     describing argument list variables of the Datastep_M10 routine.             !MART 98
!                                                                                 !MART 99
!...................................................................              !MART100
!...Switches:                                                                     !MART102
!-----------------------------------------------                                  !MART103
!   I n t e r f a c e   B l o c k s                                               !MART104
!-----------------------------------------------                                  !MART105
      USE setup_M10_I                                                             !MART106
      USE randinit_M10_I                                                          !MART107
      USE datastep_M10_I                                                          !MART108
      IMPLICIT NONE                                                               !MART109
!-----------------------------------------------                                  !MART110
!   D u m m y   A r g u m e n t s                                                 !MART111
!-----------------------------------------------                                  !MART112
      INTEGER , INTENT(IN) :: ISETUP                                              !MART113
      INTEGER , INTENT(IN)  :: JMONTE                                             !MART114
      INTEGER , INTENT(INOUT)  :: NMONTE                                          !MART115
      INTEGER , INTENT(IN)  :: ISTEP                                              !MART116
      INTEGER , INTENT(INOUT)  :: MAXNUM                                          !MART117
      INTEGER , INTENT(INOUT)  :: EOF                                             !MART118
      INTEGER , INTENT(INOUT)  :: NR1                                             !MART119
      INTEGER , INTENT(INOUT)  :: IUPDATE                                         !MART120
      INTEGER , INTENT(IN)  :: IUSTDOUT                                           !MART121
      REAL(DOUBLE) , INTENT(IN) :: ZHGT                                           !MART122
      REAL(DOUBLE) , INTENT(IN) :: ZLAT                                           !MART123
      REAL(DOUBLE) , INTENT(IN) :: ZLON                                           !MART124
      REAL(DOUBLE) , INTENT(IN) :: ZSEC                                           !MART125
      REAL(DOUBLE) , INTENT(IN) :: PZHGT                                          !MART126
      REAL(DOUBLE) , INTENT(IN) :: PZLAT                                          !MART127
      REAL(DOUBLE) , INTENT(IN) :: PZLON                                          !MART128
      REAL(DOUBLE) , INTENT(IN) :: PZSEC                                          !MART129
      REAL(DOUBLE) , INTENT(OUT) :: DAY0                                          !MART130
      REAL(DOUBLE) , INTENT(OUT) :: TEMP                                          !MART131
      REAL(DOUBLE) , INTENT(OUT) :: PRES                                          !MART132
      REAL(DOUBLE) , INTENT(OUT) :: DENSLO                                        !MART133
      REAL(DOUBLE) , INTENT(OUT) :: DENS                                          !MART134
      REAL(DOUBLE) , INTENT(OUT) :: DENSHI                                        !MART135
      REAL(DOUBLE) , INTENT(OUT) :: DENSP                                         !MART136
      REAL(DOUBLE) , INTENT(OUT) :: RHOD                                          !MART137
      REAL(DOUBLE) , INTENT(OUT) :: RHOU                                          !MART138
      REAL(DOUBLE) , INTENT(OUT) :: RHOV                                          !MART139
      REAL(DOUBLE) , INTENT(OUT) :: EWWIND                                        !MART140
      REAL(DOUBLE) , INTENT(OUT) :: EWPERT                                        !MART141
      REAL(DOUBLE) , INTENT(OUT) :: NSWIND                                        !MART142
      REAL(DOUBLE) , INTENT(OUT) :: NSPERT                                        !MART143
      REAL(DOUBLE) , INTENT(OUT) :: DENSTOT                                       !MART144
      REAL(DOUBLE) , INTENT(OUT) :: HRHO                                          !MART145
      REAL(DOUBLE) , INTENT(OUT) :: HPRES                                         !MART146
      REAL(DOUBLE) , INTENT(OUT) :: DSUNLAT                                       !MART147
      REAL(DOUBLE) , INTENT(OUT) :: DSUNLON                                       !MART148
      REAL(DOUBLE) , INTENT(OUT) :: DSUNLS                                        !MART149
      REAL(DOUBLE) , INTENT(OUT) :: DRADAU                                        !MART150
      REAL(DOUBLE) , INTENT(OUT) :: DOWLT                                         !MART151
      REAL(DOUBLE) , INTENT(OUT) :: CORLIM                                        !MART152
      REAL(DOUBLE) , INTENT(OUT) :: PERTSTEP                                      !MART153
      REAL(DOUBLE) , INTENT(OUT) :: ALS                                           !MART154
      REAL(DOUBLE) , INTENT(OUT) :: SZANG                                         !MART155
      REAL(DOUBLE) , INTENT(OUT) :: OWLT                                          !MART156
      REAL(DOUBLE) , INTENT(OUT) :: SUNLAT                                        !MART157
      REAL(DOUBLE) , INTENT(OUT) :: SUNLON                                        !MART158
      REAL(DOUBLE) , INTENT(OUT) :: MARSAU                                        !MART159
      REAL(DOUBLE) , INTENT(OUT) :: TLOCAL                                        !MART160
      CHARACTER(LEN=99) , INTENT(IN) :: INPUTFL                                   !MART161
!-----------------------------------------------                                  !MART162
!   L o c a l   V a r i a b l e s                                                 !MART163
!-----------------------------------------------                                  !MART164
      INTEGER :: LONEW, IULIST, IERT, IUTC, NPROF, NUMWAVE                        !MART165
      REAL(DOUBLE) :: CHGT, CLAT, CLON, CSEC, RHOW, DELHGT, DELLAT, DELLON,     & !MART166
         DELTIME, HGTASFC, CORLMIN, PROFNEAR, PROFFAR, VWPERT                     !MART167
                                                                                  !MART168
      SAVE CHGT, CLAT, CLON, CSEC, RHOW, DELHGT, DELLAT, DELLON, DELTIME, LONEW & !MART169
         , IULIST, HGTASFC, IERT, IUTC, CORLMIN, PROFNEAR, PROFFAR, NPROF,      & !MART170
         NUMWAVE, VWPERT                                                          !MART171
!-----------------------------------------------                                  !MART172
      IF (ISETUP == 1) THEN                                                       !MART173
!...    Initialize data and read input file with Setup_M10 subroutine             !MART174
!       See Mars-GRAM code for description of NAMELIST format input               !MART175
!       file                                                                      !MART176
         CALL SETUP_M10 (CHGT, CLAT, CLON, CSEC, DAY0, RHOD, RHOU, RHOV, RHOW,  & !MART177
            DELHGT, DELLAT, DELLON, DELTIME, MAXNUM, NR1, NMONTE, DSUNLS,       & !MART178
            DRADAU, DOWLT, LONEW, INPUTFL, IUSTDOUT, IULIST, HGTASFC, IERT,     & !MART179
            IUTC, CORLMIN, PROFNEAR, PROFFAR, NPROF)                              !MART180
         PERTSTEP = 0.0D0                                                         !MART181
!...    Initialize number of wave coefficients                                    !MART182
         NUMWAVE = 0                                                              !MART183
!...    Make sure NMONTE > 0                                                      !MART184
         NMONTE = MAX0(1,NMONTE)                                                  !MART185
!...    Make sure not to read trajectory from input TRAJDATA file                 !MART186
         IF (MAXNUM == 99999) STOP ' Set NPOS > 0 in INPUT file'                  !MART187
         RETURN                                                                   !MART188
      ENDIF                                                                       !MART189
!...  Store position and time                                                     !MART190
      CSEC = PZSEC                                                                !MART191
      CHGT = PZHGT                                                                !MART192
      CLAT = PZLAT                                                                !MART193
      CLON = PZLON                                                                !MART194
!...  Re-initialize random seed; re-set time for each Monte Carlo run             !MART195
      IF (JMONTE > 1) THEN                                                        !MART196
         CALL RANDINIT_M10 (JMONTE, NR1, RHOD, RHOU, RHOV, RHOW, IUP, IUSTDOUT)   !MART197
         PERTSTEP = 0.0D0                                                         !MART198
!...    Initialize number of wave coefficients                                    !MART199
         NUMWAVE = 0                                                              !MART200
      ENDIF                                                                       !MART201
!...  Store increments of position and time                                       !MART202
      IF (ISTEP > 0) THEN                                                         !MART203
         DELTIME = ZSEC - PZSEC                                                   !MART204
         DELHGT = ZHGT - PZHGT                                                    !MART205
         DELLAT = ZLAT - PZLAT                                                    !MART206
         DELLON = ZLON - PZLON                                                    !MART207
      ENDIF                                                                       !MART208
!     *** NOTE - If height zhgt is above reference ellipsoid,                     !MART209
!     then MOLAhgts should be set to 0 in the NAMELIST input file                 !MART210
!     (read in by the Setup_M10 subroutine) ***                                   !MART211
!...  Convert CLON and DELLON to West longitude if LonEW = 1                      !MART212
!     *** NOTE - If trajectory-calculated longitude zlon is positive              !MART213
!     East, then LonEW should be set to 1 in the NAMELIST input file              !MART214
!     (read in by the Setup_M10 subroutine) ***                                   !MART215
      IF (LONEW == 1) THEN                                                        !MART216
         CLON = 360. - CLON                                                       !MART217
         DELLON = -DELLON                                                         !MART218
      ENDIF                                                                       !MART219
!...  Call Mars-GRAM routine to evaluate atmospheric parameters                   !MART220
      CALL DATASTEP_M10 (ISTEP, CHGT, CLAT, CLON, CSEC, DAY0, RHOD, RHOU, RHOV  & !MART221
         , RHOW, EOF, DELHGT, DELLAT, DELLON, DELTIME, TEMP, PRES, DENSLO, DENS & !MART222
         , DENSHI, DENSP, EWWIND, EWPERT, NSWIND, NSPERT, VWPERT, HRHO, HPRES,  & !MART223
         DSUNLAT, DSUNLON, DSUNLS, DRADAU, DOWLT, LONEW, CORLIM, DENSTOT,       & !MART224
         NUMWAVE, HGTASFC, IERT, IUTC, PERTSTEP, CORLMIN, IUPDATE, ALS, SZANG,  & !MART225
         OWLT, SUNLAT, SUNLON, MARSAU, TLOCAL, PROFNEAR, PROFFAR, NPROF)          !MART226
!.....................................................................            !MART227
!                                                                                 !MART228
!....  Parameters passed as output from Martraj are:                              !MART229
!        TEMP = temperature (K)                                                   !MART230
!        PRES = average (mean plus wave-perturbed) pressure (N/m**2)              !MART231
!        DENSLO = nominal low density (kg/m**3), approx. -1 sigma                 !MART232
!        DENS   = average (mean plus wave-perturbed) density (kg/m**3)            !MART233
!        DENSHI = nominal high density (kg/m**3), approx. +1 sigma                !MART234
!        DENSP  = density perturbation (% of unperturbed mean)                    !MART235
!        EWWIND = mean eastward wind component (m/s)                              !MART236
!        EWpert = perturbation in eastward wind component (m/s)                   !MART237
!        NSWIND = mean northward wind component (m/s)                             !MART238
!        NSpert = perturbation in northward wind component (m/s)                  !MART239
!        Hrho   = density scale height (km)                                       !MART240
!        Hpres  = pressure scale height (km)                                      !MART241
!        corlim = ratio of step size to minimum step size for assured             !MART242
!                 accuracy in perturbations (should be >= 1)                      !MART243
!        iupdate= 1 if perturbations updated, 0 if perturbations not              !MART244
!               updated but step distance updated, -1 if neither                  !MART245
!               perturbations nor total step distance updated                     !MART246
!        DENSTOT= total density (mean plus perturbed), kg/m**3                    !MART247
!                                                                                 !MART248
!      Optional high resolution ephemeris inputs are:                             !MART249
!         dsunlat = latitude of sub-solar point (deg)                             !MART250
!         dsunlon = longitude of sub-solar point (deg)                            !MART251
!         dsunLs  = solar Ls angle (deg)                                          !MART252
!         dradau  = Mars orbital radius (AU)                                      !MART253
!      To use the internal ephemeris subroutine to compute these                  !MART254
!      parameters instead, use input values of 0.0D0                              !MART255
!.....................................................................            !MART256
!                                                                                 !MART257
!     Make sure density doesn't go negative (e.g. from LOGSCALE = 1               !MART258
!     or 2 on INPUT)                                                              !MART259
      IF (DENSTOT <= 0.) STOP ' Negative density!  Set LOGSCALE = 0'              !MART260
      IF (LOGSCALE /= 0) STOP ' LOGSCALE /= 0 on INPUT file'                      !MART261
      RETURN                                                                      !MART262
      END SUBROUTINE MARSTRAJ_M10                                                 !MART263
!----------------------------------------------------------------------           !MART264
