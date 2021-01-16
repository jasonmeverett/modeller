      MODULE marstraj_M10_I                                                       !VAST  1
      INTERFACE                                                                   !VAST  2
!                                                                                 !VAST  3
      SUBROUTINE marstraj_M10 (ISETUP, JMONTE, NMONTE, ISTEP, MAXNUM, ZHGT     &  !VAST  4
         , ZLAT, ZLON, ZSEC, PZHGT, PZLAT, PZLON, PZSEC, DAY0, TEMP, PRES      &  !VAST  5
         , DENSLO, DENS, DENSHI, DENSP, RHOD, RHOU, RHOV, EWWIND, EWPERT       &  !VAST  6
         , NSWIND, NSPERT, EOF, NR1, DENSTOT, HRHO, HPRES, DSUNLAT, DSUNLON    &  !VAST  7
         , DSUNLS, DRADAU, DOWLT, CORLIM, IUPDATE, PERTSTEP, INPUTFL, IUSTDOUT &  !VAST  8
         , ALS, SZANG, OWLT, SUNLAT, SUNLON, MARSAU, TLOCAL)                      !VAST  9
      USE vast_kind_param,ONLY: DOUBLE                                            !VAST 10
      INTEGER, INTENT(IN) :: ISETUP                                               !VAST 11
      INTEGER, INTENT(IN) :: JMONTE                                               !VAST 12
      INTEGER, INTENT(INOUT) :: NMONTE                                            !VAST 13
      INTEGER, INTENT(IN) :: ISTEP                                                !VAST 14
      INTEGER, INTENT(INOUT) :: MAXNUM                                            !VAST 15
      REAL(DOUBLE), INTENT(IN) :: ZHGT                                            !VAST 16
      REAL(DOUBLE), INTENT(IN) :: ZLAT                                            !VAST 17
      REAL(DOUBLE), INTENT(IN) :: ZLON                                            !VAST 18
      REAL(DOUBLE), INTENT(IN) :: ZSEC                                            !VAST 19
      REAL(DOUBLE), INTENT(IN) :: PZHGT                                           !VAST 20
      REAL(DOUBLE), INTENT(IN) :: PZLAT                                           !VAST 21
      REAL(DOUBLE), INTENT(IN) :: PZLON                                           !VAST 22
      REAL(DOUBLE), INTENT(IN) :: PZSEC                                           !VAST 23
      REAL(DOUBLE) , INTENT(OUT) :: DAY0                                          !VAST 24
      REAL(DOUBLE) , INTENT(OUT) :: TEMP                                          !VAST 25
      REAL(DOUBLE) , INTENT(OUT) :: PRES                                          !VAST 26
      REAL(DOUBLE) , INTENT(OUT) :: DENSLO                                        !VAST 27
      REAL(DOUBLE) , INTENT(OUT) :: DENS                                          !VAST 28
      REAL(DOUBLE) , INTENT(OUT) :: DENSHI                                        !VAST 29
      REAL(DOUBLE) , INTENT(OUT) :: DENSP                                         !VAST 30
      REAL(DOUBLE) , INTENT(OUT) :: RHOD                                          !VAST 31
      REAL(DOUBLE) , INTENT(OUT) :: RHOU                                          !VAST 32
      REAL(DOUBLE) , INTENT(OUT) :: RHOV                                          !VAST 33
      REAL(DOUBLE) , INTENT(OUT) :: EWWIND                                        !VAST 34
      REAL(DOUBLE) , INTENT(OUT) :: EWPERT                                        !VAST 35
      REAL(DOUBLE) , INTENT(OUT) :: NSWIND                                        !VAST 36
      REAL(DOUBLE) , INTENT(OUT) :: NSPERT                                        !VAST 37
      INTEGER, INTENT(INOUT) :: EOF                                               !VAST 38
      INTEGER, INTENT(INOUT) :: NR1                                               !VAST 39
      REAL(DOUBLE), INTENT(OUT) :: DENSTOT                                        !VAST 40
      REAL(DOUBLE) , INTENT(OUT) :: HRHO                                          !VAST 41
      REAL(DOUBLE) , INTENT(OUT) :: HPRES                                         !VAST 42
      REAL(DOUBLE) , INTENT(OUT) :: DSUNLAT                                       !VAST 43
      REAL(DOUBLE) , INTENT(OUT) :: DSUNLON                                       !VAST 44
      REAL(DOUBLE) , INTENT(OUT) :: DSUNLS                                        !VAST 45
      REAL(DOUBLE) , INTENT(OUT) :: DRADAU                                        !VAST 46
      REAL(DOUBLE) , INTENT(OUT) :: DOWLT                                         !VAST 47
      REAL(DOUBLE) , INTENT(OUT) :: CORLIM                                        !VAST 48
      INTEGER, INTENT(INOUT) :: IUPDATE                                           !VAST 49
      REAL(DOUBLE), INTENT(OUT) :: PERTSTEP                                       !VAST 50
      CHARACTER (LEN = 99), INTENT(IN) :: INPUTFL                                 !VAST 51
      INTEGER, INTENT(IN) :: IUSTDOUT                                             !VAST 52
      REAL(DOUBLE) , INTENT(OUT) :: ALS                                           !VAST 53
      REAL(DOUBLE) , INTENT(OUT) :: SZANG                                         !VAST 54
      REAL(DOUBLE) , INTENT(OUT) :: OWLT                                          !VAST 55
      REAL(DOUBLE) , INTENT(OUT) :: SUNLAT                                        !VAST 56
      REAL(DOUBLE) , INTENT(OUT) :: SUNLON                                        !VAST 57
      REAL(DOUBLE) , INTENT(OUT) :: MARSAU                                        !VAST 58
      REAL(DOUBLE) , INTENT(OUT) :: TLOCAL                                        !VAST 59
!VAST.../DATACOM_M10/ LOGSCALE(IN), IUP(INOUT)                                    !VAST 60
!VAST...Calls: SETUP_M10, RANDINIT_M10, DATASTEP_M10                              !VAST 61
      END SUBROUTINE                                                              !VAST 62
      END INTERFACE                                                               !VAST 63
      END MODULE                                                                  !VAST 64
!==========================================================================       !VAST 65
      MODULE trajest_M10_I                                                        !VAST 66
      INTERFACE                                                                   !VAST 67
!                                                                                 !VAST 68
      SUBROUTINE trajest_M10 (T, Z, XLAT, XLON, PT, PZ, PLAT, PLON, DENS1   &     !VAST 69
         , SCLHGT1, VZ, AZ, VLAT, VLON, DT, N)                                    !VAST 70
      USE vast_kind_param,ONLY: DOUBLE                                            !VAST 71
      REAL(DOUBLE), INTENT(INOUT) :: T                                            !VAST 72
      REAL(DOUBLE), INTENT(INOUT) :: Z                                            !VAST 73
      REAL(DOUBLE), INTENT(INOUT) :: XLAT                                         !VAST 74
      REAL(DOUBLE), INTENT(INOUT) :: XLON                                         !VAST 75
      REAL(DOUBLE), INTENT(OUT) :: PT                                             !VAST 76
      REAL(DOUBLE), INTENT(OUT) :: PZ                                             !VAST 77
      REAL(DOUBLE), INTENT(OUT) :: PLAT                                           !VAST 78
      REAL(DOUBLE), INTENT(OUT) :: PLON                                           !VAST 79
      REAL(DOUBLE), INTENT(IN) :: DENS1                                           !VAST 80
      REAL(DOUBLE), INTENT(IN) :: SCLHGT1                                         !VAST 81
      REAL(DOUBLE), INTENT(IN) :: VZ                                              !VAST 82
      REAL(DOUBLE), INTENT(IN) :: AZ                                              !VAST 83
      REAL(DOUBLE), INTENT(IN) :: VLAT                                            !VAST 84
      REAL(DOUBLE), INTENT(IN) :: VLON                                            !VAST 85
      REAL(DOUBLE), INTENT(IN) :: DT                                              !VAST 86
      INTEGER, INTENT(IN) :: N                                                    !VAST 87
      END SUBROUTINE                                                              !VAST 88
      END INTERFACE                                                               !VAST 89
      END MODULE                                                                  !VAST 90
!==========================================================================       !VAST 91
      MODULE initcode_M10_I                                                       !VAST 92
      INTERFACE                                                                   !VAST 93
!                                                                                 !VAST 94
      SUBROUTINE initcode_M10 (ZSEC, ZHGT, ZLAT, ZLON, DT, VZ, AZ, VLAT, VLON)    !VAST 95
      USE vast_kind_param,ONLY: DOUBLE                                            !VAST 96
      real(DOUBLE), INTENT(OUT) :: ZSEC                                           !VAST 97
      real(DOUBLE), INTENT(OUT) :: ZHGT                                           !VAST 98
      real(DOUBLE), INTENT(OUT) :: ZLAT                                           !VAST 99
      real(DOUBLE), INTENT(OUT) :: ZLON                                           !VAST100
      real(DOUBLE), INTENT(OUT) :: DT                                             !VAST101
      real(DOUBLE), INTENT(OUT) :: VZ                                             !VAST102
      real(DOUBLE), INTENT(OUT) :: AZ                                             !VAST103
      real(DOUBLE), INTENT(OUT) :: VLAT                                           !VAST104
      real(DOUBLE), INTENT(OUT) :: VLON                                           !VAST105
      END SUBROUTINE                                                              !VAST106
      END INTERFACE                                                               !VAST107
      END MODULE                                                                  !VAST108
!==========================================================================       !VAST109
      PROGRAM dumytraj_M10                                                        !DUMT  1
!-----------------------------------------------                                  !DUMT  2
!   M o d u l e s                                                                 !DUMT  3
!-----------------------------------------------                                  !DUMT  4
      USE vast_kind_param, ONLY:  DOUBLE                                          !DUMT  5
!...  Mars-GRAM 2010 Dummy Trajectory Program (Version 1.0) Nov, 2010             !DUMT  6
!                                                                                 !DUMT  7
!     A program to illustrate how Mars-GRAM subroutines can be incorp-            !DUMT  8
!     orated into a (double precision) trajectory-calculation code.               !DUMT  9
!                                                                                 !DUMT 10
!     SUBSTITUTE YOUR OWN TRAJECORY CODE FOR THIS MAIN DRIVER AND FOR             !DUMT 10a
!     SUBROUTINES trajest_M10 AND initcode_M10                                    !DUMT 11
!                                                                                 !DUMT 12
!     RETAIN THE SUBROUTINE Marstraj_M10.                                         !DUMT 13
!                                                                                 !DUMT 14
!     All interface between the main driver trajectory program and                !DUMT 15
!     Mars-GRAM is via the three calls to Marstraj_M10 (One with isetup           !DUMT 16
!     = 1, the others with isetup = 0).  Input to Mars-GRAM routine is            !DUMT 17
!     via the NAMELIST FORMAT file INPUT, called in the setup mode.  A            !DUMT 18
!     number of Monte-Carlo trajectories (NMONTE) can be calculated               !DUMT 19
!     during one program run. Each trajectory can have up to a maximum            !DUMT 20
!     of MAXNUM points, or exit from a given trajectory (and on to the            !DUMT 21
!     next Monte-Carlo case) can be controlled by the parameter EOF.              !DUMT 22
!                                                                                 !DUMT 23
!     To suppress output of the LIST and other files make sure to use             !DUMT 24
!     iup = 0 on the INPUT file (or permanently change it to 0 in the             !DUMT 25
!     Setup_M10 subroutine, near line 153).  A significant amount of              !DUMT 26
!     CPU run time is required to format the ASCII output for the LIST            !DUMT 27
!     and other files.                                                            !DUMT 28
!                                                                                 !DUMT 29
!.....................................................................            !DUMT 30
!                                                                                 !DUMT 31
!...Switches:                                                                     !DUMT 33
!-----------------------------------------------                                  !DUMT 34
!   I n t e r f a c e   B l o c k s                                               !DUMT 35
!-----------------------------------------------                                  !DUMT 36
      USE marstraj_M10_I                                                          !DUMT 37
      USE initcode_M10_I                                                          !DUMT 38
      USE trajest_M10_I                                                           !DUMT 39
      IMPLICIT NONE                                                               !DUMT 40
!-----------------------------------------------                                  !DUMT 41
!   L o c a l   V a r i a b l e s                                                 !DUMT 42
!-----------------------------------------------                                  !DUMT 43
      INTEGER :: EOF, IUSTDIN, IUSTDOUT, IUPDATE, NMONTE, MAXNUM, NR1, JMONTE, &  !DUMT 44
         NRX, ISTEP                                                               !DUMT 45
      REAL(DOUBLE) :: MARSAU, NSWIND, NSPERT, DSUNLAT, DSUNLON, DSUNLS, DRADAU &  !DUMT 46
         , DOWLT, ZHGT, ZLAT, ZLON, ZSEC, PZHGT, PZLAT, PZLON, PZSEC, DAY0,    &  !DUMT 47
         TEMP, PRES, DENSLO, DENS, DENSHI, DENSP, RHOD, RHOU, RHOV, EWWIND,    &  !DUMT 48
         EWPERT, DENS1, DSCLHGT1, PSCLHGT1, CORLIM, PERTSTEP, ALS, SZANG, OWLT &  !DUMT 49
         , SUNLAT, SUNLON, TLOCAL, DT, VZ, AZ, VLAT, VLON, DENS2, DSCLHGT2,    &  !DUMT 50
         PSCLHGT2                                                                 !DUMT 51
      CHARACTER(LEN=99) :: INPUTFL                                                !DUMT 52
!-----------------------------------------------                                  !DUMT 53
!...  Establish unit numbers for standard (screen) input and output               !DUMT 54
      IUSTDIN = 5                                                                 !DUMT 55
      IUSTDOUT = 6                                                                !DUMT 56
!...  Establish the name of the NAMELIST format input file                        !DUMT 57
      WRITE (IUSTDOUT, *) ' Enter file name for NAMELIST input'                   !DUMT 58
      READ (IUSTDIN, 10) INPUTFL                                                  !DUMT 59
   10 FORMAT(A)                                                                   !DUMT 60
!                                                                                 !DUMT 61
!     Set double precision ephemeris values to 0 if using built-in                !DUMT 62
!     ephemeris subroutine, otherwise compute double precision values             !DUMT 63
      DSUNLAT = 0.0D0                                                             !DUMT 64
      DSUNLON = 0.0D0                                                             !DUMT 65
      DSUNLS = 0.0D0                                                              !DUMT 66
      DRADAU = 0.0D0                                                              !DUMT 67
      DOWLT = 0.0D0                                                               !DUMT 68
!                                                                                 !DUMT 69
!     Set up the Mars-GRAM routine (ISETUP = 1)                                   !DUMT 70
      IUPDATE = 0                                                                 !DUMT 71
      CALL MARSTRAJ_M10 (1, 0, NMONTE, 0, MAXNUM, ZHGT, ZLAT, ZLON, ZSEC, PZHGT & !DUMT 72
         , PZLAT, PZLON, PZSEC, DAY0, TEMP, PRES, DENSLO, DENS, DENSHI, DENSP,  & !DUMT 73
         RHOD, RHOU, RHOV, EWWIND, EWPERT, NSWIND, NSPERT, EOF, NR1, DENS1,     & !DUMT 74
         DSCLHGT1, PSCLHGT1, DSUNLAT, DSUNLON, DSUNLS, DRADAU, DOWLT, CORLIM,   & !DUMT 75
         IUPDATE, PERTSTEP, INPUTFL, IUSTDOUT, ALS, SZANG, OWLT, SUNLAT, SUNLON & !DUMT 76
         , MARSAU, TLOCAL)                                                        !DUMT 77
!                                                                                 !DUMT 78
!...  Step through Monte Carlo runs                                               !DUMT 79
      L200: DO JMONTE = 1, NMONTE                                                 !DUMT 80
!       Initialize trajectory position                                            !DUMT 81
         CALL INITCODE_M10 (ZSEC, ZHGT, ZLAT, ZLON, DT, VZ, AZ, VLAT, VLON)       !DUMT 82
         NRX = NR1                                                                !DUMT 83
!                                                                                 !DUMT 84
!.....................................................................            !DUMT 85
!       If the user wants to have one or more profiles where random               !DUMT 86
!       perturbations are the same, then insert required logic here.              !DUMT 87
!                                                                                 !DUMT 88
!       Examples:                                                                 !DUMT 89
!.....................................................................            !DUMT 90
!                                                                                 !DUMT 91
!       To do a sequence of pairs of profiles, with the second member             !DUMT 92
!       of each pair having the same perturbations as the first of                !DUMT 93
!       the pair, use -                                                           !DUMT 94
!                                                                                 !DUMT 95
!       If (Mod(jmonte,2) == 0)NRx = NR1 - 11                                     !DUMT 96
!                                                                                 !DUMT 97
!       To do a sequence of profiles with all members of the sequence             !DUMT 98
!       having the same perturbations as the first profile, use -                 !DUMT 99
!                                                                                 !DUMT100
!       If (jmonte  > 1)NRx = NR1 - 11                                            !DUMT101
!                                                                                 !DUMT102
!       The algorithm used here ( NRx = NR1 - 11 ) must be the                    !DUMT103
!       reverse of the algorithm used in subroutine Randinit_M10 to               !DUMT104
!       increase NR1 between successive profiles (at line RNDI  8).               !DUMT105
!                                                                                 !DUMT106
!.....................................................................            !DUMT107
!                                                                                 !DUMT108
!       Set double precision ephemeris values to 0 if using built-in              !DUMT109
!       ephemeris subroutine, otherwise compute double precision values           !DUMT110
         DSUNLAT = 0.0D0                                                          !DUMT111
         DSUNLON = 0.0D0                                                          !DUMT112
         DSUNLS = 0.0D0                                                           !DUMT113
         DRADAU = 0.0D0                                                           !DUMT114
!       Initialize the Mars-GRAM variables (istep = 0) and compute                !DUMT115
!       density at beginning of first trajectory step                             !DUMT116
         CALL MARSTRAJ_M10 (0, JMONTE, NMONTE, 0, MAXNUM, ZHGT, ZLAT, ZLON,   &   !DUMT117
            ZSEC, ZHGT, ZLAT, ZLON, ZSEC, DAY0, TEMP, PRES, DENSLO, DENS,     &   !DUMT118
            DENSHI, DENSP, RHOD, RHOU, RHOV, EWWIND, EWPERT, NSWIND, NSPERT,  &   !DUMT119
            EOF, NRX, DENS1, DSCLHGT1, PSCLHGT1, DSUNLAT, DSUNLON, DSUNLS,    &   !DUMT120
            DRADAU, DOWLT, CORLIM, IUPDATE, PERTSTEP, INPUTFL, IUSTDOUT, ALS, &   !DUMT121
            SZANG, OWLT, SUNLAT, SUNLON, MARSAU, TLOCAL)                          !DUMT122
         NR1 = NRX                                                                !DUMT123
!       DENS1 is total density at beginning of 1st trajectory step                !DUMT124
         DO ISTEP = 1, MAXNUM                                                     !DUMT125
!       Estimate next trajectory position (based on 1st predictor                 !DUMT126
!       calculation and total density at beginning of trajectory step)            !DUMT127
!       and store previous position for which Mars-GRAM was called.               !DUMT128
            CALL TRAJEST_M10 (ZSEC, ZHGT, ZLAT, ZLON, PZSEC, PZHGT, PZLAT,    &   !DUMT129
               PZLON, DENS1, DSCLHGT1, VZ, AZ, VLAT, VLON, DT, ISTEP - 1)         !DUMT130
!       *** NOTE - If height zhgt is above reference ellipsoid,                   !DUMT131
!       then MOLAhgts should be set to 0 in the NAMELIST input file               !DUMT132
!       (read in by the Setup_M10 subroutine) ***                                 !DUMT133
!       *** NOTE - If computed longitude zlon is positive East, then              !DUMT134
!       LonEW should be set to 1 in the NAMELIST input file (read in by           !DUMT135
!       the Setup_M10 subroutine) ***                                             !DUMT136
!                                                                                 !DUMT137
!       If using high precision ephemeris routine re-evalute double               !DUMT138
!       precision parameters here, analogous to using the marsephm_M10            !DUMT139
!       subroutine                                                                !DUMT140
!                                                                                 !DUMT141
!       Evaluate density at estimated next trajectory position                    !DUMT142
            CALL MARSTRAJ_M10 (0, 0, NMONTE, ISTEP, MAXNUM, ZHGT, ZLAT, ZLON,   & !DUMT143
               ZSEC, PZHGT, PZLAT, PZLON, PZSEC, DAY0, TEMP, PRES, DENSLO, DENS & !DUMT144
               , DENSHI, DENSP, RHOD, RHOU, RHOV, EWWIND, EWPERT, NSWIND,       & !DUMT145
               NSPERT, EOF, NR1, DENS2, DSCLHGT2, PSCLHGT2, DSUNLAT, DSUNLON,   & !DUMT146
               DSUNLS, DRADAU, DOWLT, CORLIM, IUPDATE, PERTSTEP, INPUTFL,       & !DUMT147
               IUSTDOUT, ALS, SZANG, OWLT, SUNLAT, SUNLON, MARSAU, TLOCAL)        !DUMT148
!       DENS2 is total density at estimated next trajectory position              !DUMT149
!                                                                                 !DUMT150
!       If the user desires to revise the trajectory position estimate            !DUMT151
!       (zhgt, zlat, zlon), this can be done here (e.g. via a corrector           !DUMT152
!       step in a predictor-corrector solution and/or via a scheme                !DUMT153
!       that uses variation of total density along the trajectory step,           !DUMT154
!       i.e. between DENS1 at the beginning of the step and DENS2 at              !DUMT155
!       the end of the step).  Interpolation between DENS1 and DENS2              !DUMT156
!       can be done in an appropriate manner as selected by the user              !DUMT157
!       (e.g. linear interpolation may be valid for small trajectory              !DUMT158
!       time steps). No further calls to Marstraj_M10 should be done              !DUMT159
!       during this predictor-corrector process of updating zhgt, zlat,           !DUMT160
!       and zlon (as this will lead to erroneous results for the                  !DUMT161
!       density perturbation values).                                             !DUMT162
!                                                                                 !DUMT163
!       Store total density and scale height for use as value at                  !DUMT164
!       beginning of next trajectory step                                         !DUMT165
            DENS1 = DENS2                                                         !DUMT166
            DSCLHGT1 = DSCLHGT2                                                   !DUMT167
!                                                                                 !DUMT168
!       If the user wishes to call the atmosphere model without                   !DUMT169
!       updating the perturbations or total perturbation step size,               !DUMT170
!       this can be done here by using iupdate < 0                                !DUMT171
!                                                                                 !DUMT172
!     Example:                                                                    !DUMT173
!......................................................................           !DUMT174
!       iupdate = -1                                                              !DUMT175
!       Call Marstraj_M10(0,0,NMONTE,istep,MAXNUM,(zhgt+pzhgt)/2.0d0,          &  !DUMT176
!        (zlat+pzlat)/2.0d0,(zlon+pzlon)/2.0d0,(zsec+pzsec)/2.0d0,             &  !DUMT177
!        pzhgt,pzlat,pzlon,pzsec,DAY0,TEMP,PRES,                               &  !DUMT178
!        DENSLO,DENS,DENSHI,DENSP,RHOd,RHOu,RHOv,EWWIND,EWpert,NSWIND,         &  !DUMT179
!        NSpert,EOF,NR1,DENS2,DSCLHGT2,PSCLHGT2,dsunlat,dsunlon,dsunLs,        &  !DUMT180
!        dradau,dowlt,corlim,iupdate,pertstep,INPUTFL,iustdout,                &  !DUMT181
!        ALS,szang,owlt,sunlat,sunlon,MarsAU,TLOCAL)                              !DUMT182
!       iupdate = 0                                                               !DUMT183
!......................................................................           !DUMT184
!       End this trajectory calculation if parameter EOF is 1                     !DUMT185
            IF (EOF /= 1) CYCLE                                                   !DUMT186
            CYCLE  L200                                                           !DUMT187
         END DO                                                                   !DUMT188
      END DO L200                                                                 !DUMT189
      STOP                                                                        !DUMT190
      END PROGRAM dumytraj_M10                                                    !DUMT191
!                                                                                 !DUMT192
!---------------------------------------------------------------------            !TRAJ  1
      subroutine trajest_M10(t, z, xlat, xlon, pt, pz, plat, plon, dens1,      &  !TRAJ  2
         sclhgt1, vz, az, vlat, vlon, dt, n)                                      !TRAJ  3
!-----------------------------------------------                                  !TRAJ  4
!   M o d u l e s                                                                 !TRAJ  5
!-----------------------------------------------                                  !TRAJ  6
      USE vast_kind_param, ONLY:  double                                          !TRAJ  7
!                                                                                 !TRAJ  8
!...  Dummy trajectory model to estimate height (z), latitude (xlat),             !TRAJ  9
!     and longitude (xlon) at time (t), from previous time (pt),                  !TRAJ 10
!     previous height (phgt), previous latitude (plat), and previous              !TRAJ 11
!     longitude (plon).  This simple example estimates trajectory                 !TRAJ 12
!     position by assuming Newton's forward difference to compute                 !TRAJ 13
!     displacements from velocity components. In general it may also              !TRAJ 14
!     uses the (total) density and density scale height at the                    !TRAJ 15
!     beginning of the trajectory step (although density and scale                !TRAJ 16
!     height are not actually used in this dummy version)                         !TRAJ 17
!                                                                                 !TRAJ 18
!     SUBSTITUTE YOUR OWN (double precision) TRAJECTORY POSITION                  !TRAJ 19
!     ESTIMATING ROUTINE                                                          !TRAJ 20
!                                                                                 !TRAJ 21
!......................................................................           !TRAJ 22
!...Switches:                                                                     !TRAJ 24
      implicit none                                                               !TRAJ 25
!-----------------------------------------------                                  !TRAJ 26
!   D u m m y   A r g u m e n t s                                                 !TRAJ 27
!-----------------------------------------------                                  !TRAJ 28
      integer , intent(in) :: n                                                   !TRAJ 29
      real(double) , intent(inout) :: t                                           !TRAJ 30
      real(double) , intent(inout) :: z                                           !TRAJ 31
      real(double) , intent(inout) :: xlat                                        !TRAJ 32
      real(double) , intent(inout) :: xlon                                        !TRAJ 33
      real(double) , intent(out) :: pt                                            !TRAJ 34
      real(double) , intent(out) :: pz                                            !TRAJ 35
      real(double) , intent(out) :: plat                                          !TRAJ 36
      real(double) , intent(out) :: plon                                          !TRAJ 37
      real(double) , intent(in) :: dens1                                          !TRAJ 38
      real(double) , intent(in) :: sclhgt1                                        !TRAJ 39
      real(double) , intent(in) :: vz                                             !TRAJ 40
      real(double) , intent(in) :: az                                             !TRAJ 41
      real(double) , intent(in) :: vlat                                           !TRAJ 42
      real(double) , intent(in) :: vlon                                           !TRAJ 43
      real(double) , intent(in) :: dt                                             !TRAJ 44
!-----------------------------------------------                                  !TRAJ 45
!   L o c a l   V a r i a b l e s                                                 !TRAJ 46
!-----------------------------------------------                                  !TRAJ 47
      real(double) :: density, scalehgt                                           !TRAJ 48
                                                                                  !TRAJ 49
      save density, scalehgt                                                      !TRAJ 50
!-----------------------------------------------                                  !TRAJ 51
!...  Store density and scale height at beginning of trajectory step              !TRAJ 52
!     (these are not actually used in this example version)                       !TRAJ 53
      density = dens1                                                             !TRAJ 54
      scalehgt = sclhgt1                                                          !TRAJ 55
!...  Store current position and time as previous position and time               !TRAJ 56
      pt = t                                                                      !TRAJ 57
      pz = z                                                                      !TRAJ 58
      plat = xlat                                                                 !TRAJ 59
      plon = xlon                                                                 !TRAJ 60
!...  If desired, update time increment dt here (or use input value)              !TRAJ 61
!...  Evaluate new time t, using time increment, dt                               !TRAJ 62
      t = pt + dt                                                                 !TRAJ 63
!...  Use vz and az to compute height change by Newton's forward                  !TRAJ 64
!     difference                                                                  !TRAJ 65
      z = pz + vz*dt + az*(2.0D0*n + 1.0D0)*dt**2                                 !TRAJ 66
!...  Use vlat and vlon to compaute latitude-longitude change                     !TRAJ 67
!     (NOTE: There are no corrections here for going over the pole)               !TRAJ 68
      xlat = plat + vlat*dt                                                       !TRAJ 69
      xlon = plon + vlon*dt                                                       !TRAJ 70
!     *** NOTE - If height z is above reference ellipsoid,                        !TRAJ 71
!     then MOLAhgts should be set to 0 in the NAMELIST input file                 !TRAJ 72
!     (read in by the Setup_M10 subroutine) ***                                   !TRAJ 73
!     *** NOTE - If longitude xlon is positive East, then LonEW should            !TRAJ 74
!     be set to 1 in the NAMELIST input file (read in by the Setup_M10            !TRAJ 75
!     subroutine) ***                                                             !TRAJ 76
!...  Stop if dummy trajectory goes over the pole without corrections             !TRAJ 77
      if (dabs(xlat) > 90.0D0) stop &                                             !TRAJ 78
         ' Dummy trajectory crossed pole without corrections'                     !TRAJ 79
!...  Insure that longitude stays in 0-360 bounds                                 !TRAJ 80
      if (xlon < 0.0D0) xlon = xlon + 360.0D0                                     !TRAJ 81
      if (xlon >= 360.0D0) xlon = xlon - 360.0D0                                  !TRAJ 82
      return                                                                      !TRAJ 83
      end subroutine trajest_M10                                                  !TRAJ 84
                                                                                  !TRAJ 85
                                                                                  !TRAJ 86
!----------------------------------------------------------------------           !TINI  1
      subroutine initcode_M10(zsec, zhgt, zlat, zlon, dt, vz, az, vlat, vlon)     !TINI  2
!-----------------------------------------------                                  !TINI  3
!   M o d u l e s                                                                 !TINI  4
!-----------------------------------------------                                  !TINI  5
      USE vast_kind_param, ONLY:  double                                          !TINI  6
!...Switches:                                                                     !TINI  8
      implicit none                                                               !TINI  9
!-----------------------------------------------                                  !TINI 10
!   D u m m y   A r g u m e n t s                                                 !TINI 11
!-----------------------------------------------                                  !TINI 12
      real(double) , intent(out) :: zsec                                          !TINI 13
      real(double) , intent(out) :: zhgt                                          !TINI 14
      real(double) , intent(out) :: zlat                                          !TINI 15
      real(double) , intent(out) :: zlon                                          !TINI 16
      real(double) , intent(out) :: dt                                            !TINI 17
      real(double) , intent(out) :: vz                                            !TINI 18
      real(double) , intent(out) :: az                                            !TINI 19
      real(double) , intent(out) :: vlat                                          !TINI 20
      real(double) , intent(out) :: vlon                                          !TINI 21
!-----------------------------------------------                                  !TINI 22
!   L o c a l   V a r i a b l e s                                                 !TINI 23
!-----------------------------------------------                                  !TINI 24
      real(double) :: t1, t2, z1, z2                                              !TINI 25
                                                                                  !TINI 26
      save t1, t2, z1, z2                                                         !TINI 27
!-----------------------------------------------                                  !TINI 28
!...  Dummy trajectory initialization routine to set up starting values           !TINI 29
!     of position                                                                 !TINI 30
!                                                                                 !TINI 31
!     SUBSTITUTE YOUR OWN (double precision) TRAJECTORY POSITION                  !TINI 32
!     INITIALIZATION ROUTINE                                                      !TINI 33
!                                                                                 !TINI 34
!     *** NOTE - If height zhgt is above reference ellipsoid,                     !TINI 35
!     then MOLAhgts should be set to 0 in the NAMELIST input file                 !TINI 36
!     (read in by the Setup_M10 subroutine) ***                                   !TINI 37
!     *** NOTE - If longitude zlon is positive East, then LonEW should            !TINI 38
!     be set to 1 in the NAMELIST input file (read in by the Setup_M10            !TINI 39
!     subroutine) ***                                                             !TINI 40
!                                                                                 !TINI 41
!...  The dummy values below reproduce the vertical profile of the                !TINI 42
!     reference case (with LonEast = 0).  Other values of initial                 !TINI 43
!     position (zhgt, zlat, and zlon) and change in position (set                 !TINI 44
!     by vz, vlat, and vlon in the trajest_M10 subroutine), can be used           !TINI 45
!     to simulate various positions for comparison with output                    !TINI 46
!     from the stand-alone Mars-GRAM program.                                     !TINI 47
!     zsec = 0.0d0                                                                !TINI 48
!     zhgt = -5.0D0                                                               !TINI 49
!     zlat = 22.48D0                                                              !TINI 50
!     zlon = 47.97D0                                                              !TINI 51
!     dt = 500.0D0                                                                !TINI 52
!     vlat = 0.001D0                                                              !TINI 53
!     vlon = 0.001D0                                                              !TINI 54
!     vz = 0.01D0                                                                 !TINI 55
!     az = 0.0                                                                    !TINI 56
!                                                                                 !TINI 57
!...  Set initial position values                                                 !TINI 58
      zsec = 0.0D0                                                                !TINI 59
      zhgt = -5.0D0                                                               !TINI 60
      zlat = 22.48D0                                                              !TINI 61
      zlon = 47.97D0                                                              !TINI 62
!...  Set the time increment (dt, sec)                                            !TINI 63
      dt = 500.0D0                                                                !TINI 64
!...  Compute the northward velocity. Convert to a rate of change of              !TINI 65
!     latitude with time (vlat, deg./s)                                           !TINI 66
      vlat = 0.001D0                                                              !TINI 67
!...  Compute eastward velocity. Convert to rate of change of longitude           !TINI 68
!     with time (vlon, deg./s)                                                    !TINI 69
      vlon = 0.001D0                                                              !TINI 70
!...  Set vertical velocity (vz, km/s) and "acceleration" (az, km/s**2)           !TINI 71
      vz = 0.01D0                                                                 !TINI 72
      az = 0.0                                                                    !TINI 73
!...  Alternately, set heights z1 and z2 (km) at times t1 and t2 (sec),           !TINI 74
!     and use vz and az computed from these data. NOTE: Height z = zhgt           !TINI 75
!     at time t = 0 is set in initial position, above.                            !TINI 76
      t1 = 0.0D0                                                                  !TINI 77
      t2 = 0.0D0                                                                  !TINI 78
      z1 = 300.0D0                                                                !TINI 79
      z2 = 1000.0D0                                                               !TINI 80
      if (dabs(t1)>0.0D0 .and. dabs(t2)>0.0D0 .and. dabs(t1-t2)>0.0d0) then       !TINI 81
         vz = (z1 - zhgt)*t2/(t1*(t2 - t1)) - (z2 - zhgt)*t1/(t2*(t2 - t1))       !TINI 82
         az = (z2 - zhgt)/(t2*(t2 - t1)) - (z1 - zhgt)/(t1*(t2 - t1))             !TINI 83
      endif                                                                       !TINI 84
      return                                                                      !TINI 85
      end subroutine initcode_M10                                                 !TINI 86
!----------------------------------------------------------------------           !TINI 87
