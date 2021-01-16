      MODULE marstraj_M10_I                                                       !VAST  1
      INTERFACE                                                                   !VAST  2
!...                                                                              !VAST  3
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
!======================================================================           !VAST 65
      MODULE trajest_M10_I                                                        !VAST 66
      INTERFACE                                                                   !VAST 67
!...                                                                              !VAST 68
      SUBROUTINE trajest_M10 (T, Z, XLAT, XLON, PT, PZ, PLAT, PLON, DENS1     &   !VAST 69
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
!=======================================================================          !VAST 91
      MODULE initcode_M10_I                                                       !VAST 92
      INTERFACE                                                                   !VAST 93
!...                                                                              !VAST 94
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
!======================================================================           !VAST109
      PROGRAM multtraj_M10                                                        !MULT  1
!-----------------------------------------------                                  !MULT  2
!   M o d u l e s                                                                 !MULT  3
!-----------------------------------------------                                  !MULT  4
      USE vast_kind_param, ONLY:  DOUBLE                                          !MULT  5
!...  Mars-GRAM 2010 Multi-Trajectory Program (Version 1.0) Nov, 2010             !MULT  6
!                                                                                 !MULT  7
!     A program to illustrate how Mars-GRAM subroutines can be incorp-            !MULT  8
!     orated into a (double precision) trajectory-calculation code.               !MULT  9
!                                                                                 !MULT 10
!     SUBSTITUTE YOUR OWN TRAJECORY CODE FOR THIS MAIN DRIVER AND FOR             !MULT 11
!     SUBROUTINES trajest_M10 AND initcode_M10                                    !MULT 12
!                                                                                 !MULT 13
!     RETAIN THE SUBROUTINE Marstraj_M10.                                         !MULT 14
!                                                                                 !MULT 15
!     All interface between the main driver trajectory program and                !MULT 16
!     Mars-GRAM is via the three calls to Marstraj_M10 (One with isetup           !MULT 17
!     = 1, the others with isetup = 0).  Input to Mars-GRAM routine is            !MULT 18
!     via the NAMELIST FORMAT file INPUT, called in the setup mode.  A            !MULT 19
!     number of Monte-Carlo trajectories (NMONTE) can be calculated               !MULT 20
!     during one program run. Each trajectory can have up to a maximum            !MULT 21
!     of MAXNUM points, or exit from a given trajectory (and on to the            !MULT 22
!     next Monte-Carlo case) can be controlled by the parameter EOF.              !MULT 23
!                                                                                 !MULT 24
!     To suppress output of the LIST and other files make sure to use             !MULT 25
!     iup = 0 on the INPUT file (or permanently change it to 0 in the             !MULT 26
!     Setup_M10 subroutine, near line 153).  A significant amount of              !MULT 27
!     CPU run time is required to format the ASCII output for the LIST            !MULT 28
!     and other files.                                                            !MULT 29
!                                                                                 !MULT 30
!.....................................................................            !MULT 31
!                                                                                 !MULT 32
!...                                                                              !MULT 33
!...Switches:                                                                     !MULT 34
!-----------------------------------------------                                  !MULT 35
!   I n t e r f a c e   B l o c k s                                               !MULT 36
!-----------------------------------------------                                  !MULT 37
      USE marstraj_M10_I                                                          !MULT 38
      USE initcode_M10_I                                                          !MULT 39
      USE random_M10_I                                                            !MULT 40
      USE ppnd_M10_I                                                              !MULT 41
      USE trajest_M10_I                                                           !MULT 42
      IMPLICIT NONE                                                               !MULT 43
!-----------------------------------------------                                  !MULT 44
!   L o c a l   V a r i a b l e s                                                 !MULT 45
!-----------------------------------------------                                  !MULT 46
      INTEGER :: EOF                                                              !MULT 47
      INTEGER , DIMENSION(6) :: NR1, IUPDATE                                      !MULT 48
      INTEGER :: NMULTI, IUSTDIN, IUSTDOUT, NMONTE, MAXNUM, IPR, JMONTE, NRX, L & !MULT 49
         , ISTEP                                                                  !MULT 50
      REAL(DOUBLE), DIMENSION(6) :: ZHGT, ZLAT, ZLON, ZSEC, PZHGT, PZLAT, PZLON & !MULT 51
         , PZSEC, CORLIM, PERTSTEP, TEMP, PRES, DENSLO, DENS, DENSHI, DENSP,    & !MULT 52
         RHOD, RHOU, RHOV, EWWIND, EWPERT, NSWIND, NSPERT, DENS1, DSCLHGT1,     & !MULT 53
         PSCLHGT1, DSCLHGT2, PSCLHGT2, DENS2                                      !MULT 54
      REAL(DOUBLE) :: MARSAU, DSUNLAT, DSUNLON, DSUNLS, DRADAU, DOWLT, DAY0,    & !MULT 55
         ALS, SZANG, OWLT, SUNLAT, SUNLON, TLOCAL, DT, VZ, AZ, VLAT, VLON, SIGD & !MULT 56
         , SIGU                                                                   !MULT 57
      CHARACTER(LEN=99) :: INPUTFL                                                !MULT 58
!-----------------------------------------------                                  !MULT 59
!...  Set nmulti = 1 to 6 to do multiple trajectories at once                     !MULT 60
      NMULTI = 1                                                                  !MULT 61
!...  Establish unit numbers for standard (screen) input and output               !MULT 62
      IUSTDIN = 5                                                                 !MULT 63
      IUSTDOUT = 6                                                                !MULT 64
!...  Establish the name of the NAMELIST format input file                        !MULT 65
      WRITE (IUSTDOUT, *) ' Enter file name for NAMELIST input'                   !MULT 66
      READ (IUSTDIN, 10) INPUTFL                                                  !MULT 67
   10 FORMAT(A)                                                                   !MULT 68
!                                                                                 !MULT 69
!     Set double precision ephemeris values to 0 if using built-in                !MULT 70
!     ephemeris subroutine, otherwise compute double precision values             !MULT 71
      DSUNLAT = 0.0D0                                                             !MULT 72
      DSUNLON = 0.0D0                                                             !MULT 73
      DSUNLS = 0.0D0                                                              !MULT 74
      DRADAU = 0.0D0                                                              !MULT 75
      DOWLT = 0.0D0                                                               !MULT 76
!                                                                                 !MULT 77
!     Set up the Mars-GRAM routine (ISETUP = 1)                                   !MULT 78
      IUPDATE(1) = 0                                                              !MULT 79
      CALL MARSTRAJ_M10 (1, 0, NMONTE, 0, MAXNUM, ZHGT(1), ZLAT(1), ZLON(1),    & !MULT 80
         ZSEC(1), PZHGT(1), PZLAT(1), PZLON(1), PZSEC(1), DAY0, TEMP(1), PRES(1 & !MULT 81
         ), DENSLO(1), DENS(1), DENSHI(1), DENSP(1), RHOD(1), RHOU(1), RHOV(1)  & !MULT 82
         , EWWIND(1), EWPERT(1), NSWIND(1), NSPERT(1), EOF, NR1(1), DENS1(1),   & !MULT 83
         DSCLHGT1(1), PSCLHGT1(1), DSUNLAT, DSUNLON, DSUNLS, DRADAU, DOWLT,     & !MULT 84
         CORLIM(1), IUPDATE(1), PERTSTEP(1), INPUTFL, IUSTDOUT, ALS, SZANG,     & !MULT 85
         OWLT, SUNLAT, SUNLON, MARSAU, TLOCAL)                                    !MULT 86
      IF (NMULTI > 1) THEN                                                        !MULT 87
         DO IPR = 2, NMULTI                                                       !MULT 88
            ZSEC(IPR) = ZSEC(IPR-1)                                               !MULT 89
            ZHGT(IPR) = ZHGT(IPR-1)                                               !MULT 90
            ZLAT(IPR) = ZLAT(IPR-1)                                               !MULT 91
            ZLON(IPR) = ZLON(IPR-1)                                               !MULT 92
            PZSEC(IPR) = PZSEC(IPR-1)                                             !MULT 93
            PZHGT(IPR) = PZHGT(IPR-1)                                             !MULT 94
            PZLAT(IPR) = PZLAT(IPR-1)                                             !MULT 95
            PZLON(IPR) = PZLON(IPR-1)                                             !MULT 96
            TEMP(IPR) = TEMP(IPR-1)                                               !MULT 97
            PRES(IPR) = PRES(IPR-1)                                               !MULT 98
            DENSLO(IPR) = DENSLO(IPR-1)                                           !MULT 99
            DENS(IPR) = DENS(IPR-1)                                               !MULT100
            DENSHI(IPR) = DENSHI(IPR-1)                                           !MULT101
            DENSP(IPR) = DENSP(IPR-1)                                             !MULT102
            RHOD(IPR) = RHOD(IPR-1)                                               !MULT103
            RHOU(IPR) = RHOU(IPR-1)                                               !MULT104
            RHOV(IPR) = RHOV(IPR-1)                                               !MULT105
            EWWIND(IPR) = EWWIND(IPR-1)                                           !MULT106
            EWPERT(IPR) = EWPERT(IPR-1)                                           !MULT107
            NSWIND(IPR) = NSWIND(IPR-1)                                           !MULT108
            NSPERT(IPR) = NSPERT(IPR-1)                                           !MULT109
            DENS1(IPR) = DENS1(IPR-1)                                             !MULT110
            DSCLHGT1(IPR) = DSCLHGT1(IPR-1)                                       !MULT111
            PSCLHGT1(IPR) = PSCLHGT1(IPR-1)                                       !MULT112
            PERTSTEP(IPR) = PERTSTEP(IPR-1)                                       !MULT113
            IUPDATE(IPR) = IUPDATE(IPR-1)                                         !MULT114
!...     To generate different perturbations adjust NR1(ipr) for ipr>1            !MULT115
            NR1(IPR) = NR1(IPR-1) + 7*IPR                                         !MULT116
         END DO                                                                   !MULT117
      ENDIF                                                                       !MULT118
!                                                                                 !MULT119
!                                                                                 !MULT120
!...  Step through Monte Carlo runs                                               !MULT121
      L200: DO JMONTE = 1, NMONTE                                                 !MULT122
!       Initialize trajectory position                                            !MULT123
         CALL INITCODE_M10 (ZSEC(1), ZHGT(1), ZLAT(1), ZLON(1), DT, VZ, AZ, &     !MULT124
            VLAT, VLON)                                                           !MULT125
!       Set double precision ephemeris values to 0 if using built-in              !MULT126
!       ephemeris subroutine, otherwise compute double precision values           !MULT127
         DSUNLAT = 0.0D0                                                          !MULT128
         DSUNLON = 0.0D0                                                          !MULT129
         DSUNLS = 0.0D0                                                           !MULT130
         DRADAU = 0.0D0                                                           !MULT131
!       Initialize the Mars-GRAM variables (istep = 0) and compute                !MULT132
!       density at beginning of first trajectory step                             !MULT133
         NRX = NR1(1)                                                             !MULT134
!                                                                                 !MULT135
!.....................................................................            !MULT136
!       If the user wants to have one or more profiles where random               !MULT137
!       perturbations are the same, then insert required logic here.              !MULT138
!                                                                                 !MULT139
!       Examples:                                                                 !MULT140
!.....................................................................            !MULT141
!                                                                                 !MULT142
!       To do a sequence of pairs of profiles, with the second member             !MULT143
!       of each pair having the same perturbations as the first of                !MULT144
!       the pair, use -                                                           !MULT145
!                                                                                 !MULT146
!       If (Mod(jmonte,2) == 0)NRx = NR1(1) - 11                                  !MULT147
!                                                                                 !MULT148
!       To do a sequence of profiles with all members of the sequence             !MULT149
!       having the same perturbations as the first profile, use -                 !MULT150
!                                                                                 !MULT151
         IF (JMONTE > 1) NRX = NR1(1) - 11                                        !MULT152
!                                                                                 !MULT153
!       The algorithm used here ( NRx = NR1 - 11 ) must be the                    !MULT154
!       reverse of the algorithm used in subroutine Randinit_M10 to               !MULT155
!       increase NR1 between successive profiles (at line RNDI  8).               !MULT156
!                                                                                 !MULT157
!.....................................................................            !MULT158
!                                                                                 !MULT159
         CALL MARSTRAJ_M10 (0, JMONTE, NMONTE, 0, MAXNUM, ZHGT(1), ZLAT(1),     & !MULT160
            ZLON(1), ZSEC(1), ZHGT(1), ZLAT(1), ZLON(1), ZSEC(1), DAY0, TEMP(1) & !MULT161
            , PRES(1), DENSLO(1), DENS(1), DENSHI(1), DENSP(1), RHOD(1), RHOU(1 & !MULT162
            ), RHOV(1), EWWIND(1), EWPERT(1), NSWIND(1), NSPERT(1), EOF, NR1(1) & !MULT163
            , DENS1(1), DSCLHGT1(1), PSCLHGT1(1), DSUNLAT, DSUNLON, DSUNLS,     & !MULT164
            DRADAU, DOWLT, CORLIM(1), IUPDATE(1), PERTSTEP(1), INPUTFL,         & !MULT165
            IUSTDOUT, ALS, SZANG, OWLT, SUNLAT, SUNLON, MARSAU, TLOCAL)           !MULT166
         NR1(1) = NRX                                                             !MULT167
         IF (NMULTI > 1) THEN                                                     !MULT168
            SIGD = 100.0D0*(DENSHI(1)-DENS(1))/DENS(1)                            !MULT169
            SIGU = SIGD                                                           !MULT170
            DO IPR = 2, NMULTI                                                    !MULT171
               ZSEC(IPR) = ZSEC(IPR-1)                                            !MULT172
               ZHGT(IPR) = ZHGT(IPR-1)                                            !MULT173
               ZLAT(IPR) = ZLAT(IPR-1)                                            !MULT174
               ZLON(IPR) = ZLON(IPR-1)                                            !MULT175
               PZSEC(IPR) = PZSEC(IPR-1)                                          !MULT176
               PZHGT(IPR) = PZHGT(IPR-1)                                          !MULT177
               PZLAT(IPR) = PZLAT(IPR-1)                                          !MULT178
               PZLON(IPR) = PZLON(IPR-1)                                          !MULT179
               TEMP(IPR) = TEMP(IPR-1)                                            !MULT180
               PRES(IPR) = PRES(IPR-1)                                            !MULT181
               DENSLO(IPR) = DENSLO(IPR-1)                                        !MULT182
               DENS(IPR) = DENS(IPR-1)                                            !MULT183
               DENSHI(IPR) = DENSHI(IPR-1)                                        !MULT184
               RHOD(IPR) = RANDOM_M10(L)                                          !MULT185
               RHOD(IPR) = PPND_M10(RHOD(IPR),L)                                  !MULT186
               DENSP(IPR) = RHOD(IPR)*SIGD                                        !MULT187
               RHOU(IPR) = RANDOM_M10(L)                                          !MULT188
               RHOU(IPR) = PPND_M10(RHOU(IPR),L)                                  !MULT189
               EWPERT(IPR) = RHOU(IPR)*SIGU                                       !MULT190
               RHOV(IPR) = RANDOM_M10(L)                                          !MULT191
               RHOV(IPR) = PPND_M10(RHOV(IPR),L)                                  !MULT192
               NSPERT(IPR) = RHOV(IPR)*SIGU                                       !MULT193
               EWWIND(IPR) = EWWIND(IPR-1)                                        !MULT194
               NSWIND(IPR) = NSWIND(IPR-1)                                        !MULT195
               DENS1(IPR) = DENS1(IPR-1)                                          !MULT196
               DSCLHGT1(IPR) = DSCLHGT1(IPR-1)                                    !MULT197
               PERTSTEP(IPR) = PERTSTEP(IPR-1)                                    !MULT198
               IUPDATE(IPR) = IUPDATE(IPR-1)                                      !MULT199
!...       To generate different perturbations adjust NR1(ipr), ipr>1             !MULT200
               NR1(IPR) = NR1(IPR-1) + 7*IPR                                      !MULT201
            END DO                                                                !MULT202
         ENDIF                                                                    !MULT203
!       TEMP, PRES, DENS are mean temperature, pressure, density at               !MULT204
!       beginning of 1st trajectory step; DENS1 is total (mean plus               !MULT205
!       perturbed) density at beginning of 1st trajectory step                    !MULT206
         DO ISTEP = 1, MAXNUM                                                     !MULT207
!       Estimate next trajectory position (based on 1st predictor                 !MULT208
!       calculation and total density at beginning of trajectory step)            !MULT209
!       and store previous position for which Mars-GRAM was called.               !MULT210
!                                                                                 !MULT211
!       If using externally-calculated, high-precision ephemeris                  !MULT212
!       routine, recalculate values here                                          !MULT213
!                                                                                 !MULT214
            DO IPR = 1, NMULTI                                                    !MULT215
               CALL TRAJEST_M10 (ZSEC(IPR), ZHGT(IPR), ZLAT(IPR), ZLON(IPR),  &   !MULT216
                  PZSEC(IPR), PZHGT(IPR), PZLAT(IPR), PZLON(IPR), DENS1(IPR), &   !MULT217
                  DSCLHGT1(IPR), VZ, AZ, VLAT, VLON, DT, ISTEP - 1)               !MULT218
!       *** NOTE - If height zhgt is above reference ellipsoid,                   !MULT219
!       then MOLAhgts should be set to 0 in the NAMELIST input file               !MULT220
!       (read in by the Setup_M10 subroutine) ***                                 !MULT221
!       *** NOTE - If computed longitude zlon is positive East, then              !MULT222
!       LonEW should be set to 1 in the NAMELIST input file (read in by           !MULT223
!       the Setup_M10 subroutine) ***                                             !MULT224
!                                                                                 !MULT225
!       If using high precision ephemeris routine re-evalute double               !MULT226
!       precision parameters here, analogous to using the marsephm_M10            !MULT227
!       subroutine                                                                !MULT228
!                                                                                 !MULT229
!       Evaluate density at estimated next trajectory position                    !MULT230
               CALL MARSTRAJ_M10 (0, 0, NMONTE, ISTEP, MAXNUM, ZHGT(IPR), ZLAT( & !MULT231
                  IPR), ZLON(IPR), ZSEC(IPR), PZHGT(IPR), PZLAT(IPR), PZLON(IPR & !MULT232
                  ), PZSEC(IPR), DAY0, TEMP(IPR), PRES(IPR), DENSLO(IPR), DENS( & !MULT233
                  IPR), DENSHI(IPR), DENSP(IPR), RHOD(IPR), RHOU(IPR), RHOV(IPR & !MULT234
                  ), EWWIND(IPR), EWPERT(IPR), NSWIND(IPR), NSPERT(IPR), EOF,   & !MULT235
                  NR1(IPR), DENS2(IPR), DSCLHGT2(IPR), PSCLHGT2(IPR), DSUNLAT,  & !MULT236
                  DSUNLON, DSUNLS, DRADAU, DOWLT, CORLIM(IPR), IUPDATE(IPR),    & !MULT237
                  PERTSTEP(IPR), INPUTFL, IUSTDOUT, ALS, SZANG, OWLT, SUNLAT,   & !MULT238
                  SUNLON, MARSAU, TLOCAL)                                         !MULT239
!                                                                                 !MULT240
!       TEMP, PRES, DENS are mean temperature, pressure, density at               !MULT241
!       estimated next trajectory position; DENS2 is total (mean plus             !MULT242
!       perturbed) density at estimated next trajectory position                  !MULT243
!                                                                                 !MULT244
!       If the user desires to revise the trajectory position estimate            !MULT245
!       (zhgt, zlat, zlon), this can be done here (e.g. via a corrector           !MULT246
!       step in a predictor-corrector solution and/or via a scheme                !MULT247
!       that uses variation of total density along the trajectory step,           !MULT248
!       i.e. between DENS1 at the beginning of the step and DENS2 at              !MULT249
!       the end of the step).  Interpolation between DENS1 and DENS2              !MULT250
!       can be done in an appropriate manner as selected by the user              !MULT251
!       (e.g. linear interpolation may be valid for small trajectory              !MULT252
!       time steps). No further calls to Marstraj_M10 (except as noted            !MULT253
!       below, using iupdate < 0) should be done during this predictor-           !MULT254
!       corrector process of updating zhgt, zlat, and zlon (as this               !MULT255
!       will lead to erroneous results for density perturbation values)           !MULT256
!                                                                                 !MULT257
!       Store total density and scale heights for use as value at                 !MULT258
!       beginning of next trajectory step                                         !MULT259
               DENS1(IPR) = DENS2(IPR)                                            !MULT260
               DSCLHGT1(IPR) = DSCLHGT2(IPR)                                      !MULT261
               PSCLHGT1(IPR) = PSCLHGT2(IPR)                                      !MULT262
!                                                                                 !MULT263
!       If the user wishes to call the atmosphere model without                   !MULT264
!       updating the perturbations or total perturbation step size,               !MULT265
!       this can be done here by using iupdate < 0                                !MULT266
!                                                                                 !MULT267
!     Example:                                                                    !MULT268
!......................................................................           !MULT269
!       iupdate = -1                                                              !MULT270
!       Call Marstraj_M10(0,0,NMONTE,istep,MAXNUM,(zhgt(ipr)+                &    !MULT271
!        pzhgt(ipr))/2.0d0,(zlat(ipr)+pzlat(ipr))/2.0d0,(zlon(ipr)+          &    !MULT272
!        pzlon(ipr))/2.0d0,(zsec(ipr)+pzsec(ipr))/2.0d0,pzhgt(ipr),          &    !MULT273
!        pzlat(ipr),pzlon(ipr),pzsec(ipr),DAY0,TEMP(ipr),PRES(ipr),          &    !MULT274
!        DENSLO(ipr),DENS(ipr),DENSHI(ipr),DENSP(ipr),RHOd(ipr),             &    !MULT275
!        RHOu(ipr),RHOv(ipr),EWWIND(ipr),EWpert(ipr),NSWIND(ipr),            &    !MULT276
!        NSpert(ipr),EOF,NR1(ipr),DENS1(ipr),DSCLHGT1(ipr),                  &    !MULT277
!        PSCLHGT1(ipr),dsunlat,dsunlon,dsunLs,dradau,dowlt,                  &    !MULT278
!        corlim(ipr),iupdate(ipr),pertstep(ipr),INPUTFL,iustdout,            &    !MULT279
!        ALS,szang,owlt,sunlat,sunlon,MarsAU,TLOCAL)                              !MULT280
!       iupdate = 0                                                               !MULT281
!......................................................................           !MULT282
            END DO                                                                !MULT283
!       End this trajectory calculation if parameter EOF is 1                     !MULT284
            IF (EOF /= 1) CYCLE                                                   !MULT285
            CYCLE  L200                                                           !MULT286
         END DO                                                                   !MULT287
      END DO L200                                                                 !MULT288
      STOP                                                                        !MULT289
      END PROGRAM multtraj_M10                                                    !MULT290
!                                                                                 !MULT291
!---------------------------------------------------------------------            !TRAJ  1
      subroutine trajest_M10(t, z, xlat, xlon, pt, pz, plat, plon, dens1,   &     !TRAJ  2
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
!...                                                                              !TRAJ 23
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
      if (dabs(xlat) > 90.0D0) stop ' Crossed pole without corrections'           !TRAJ 78
!...  Insure that longitude stays in 0-360 bounds                                 !TRAJ 80
      if (xlon < 0.0D0) xlon = xlon + 360.0D0                                     !TRAJ 81
      if (xlon >= 360.0D0) xlon = xlon - 360.0D0                                  !TRAJ 82
      return                                                                      !TRAJ 83
      end subroutine trajest_M10                                                  !TRAJ 84
!                                                                                 !TRAJ 86
!----------------------------------------------------------------------           !TINI  1
      subroutine initcode_M10(zsec, zhgt, zlat, zlon, dt, vz, az, vlat, vlon)     !TINI  2
!-----------------------------------------------                                  !TINI  3
!   M o d u l e s                                                                 !TINI  4
!-----------------------------------------------                                  !TINI  5
      USE vast_kind_param, ONLY:  double                                          !TINI  6
!...                                                                              !TINI  7
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
