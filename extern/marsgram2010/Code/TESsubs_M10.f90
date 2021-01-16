      subroutine rdtessrf_M10(gcmdir, version)                                    !RDTS  1
!-----------------------------------------------                                  !RDTS  2
!   M o d u l e s                                                                 !RDTS  3
!-----------------------------------------------                                  !RDTS  4
      USE vast_kind_param, ONLY:  double                                          !RDTS  5
      USE mgcmparm_M10_C                                                          !RDTS  6
      USE surftes_M10_C                                                           !RDTS  7
!---    Reads NASA Ames Mars General Circulation Model (MGCM) surface             !RDTS  8
!       data (in binary format) for TES Mapping Years, and loads into             !RDTS  9
!       data arrays for common surfTES                                            !RDTS 10
!       GCMDIR is directory name where MGCM data resides                          !RDTS 11
!...                                                                              !RDTS 12
!...Switches:                                                                     !RDTS 13
      implicit none                                                               !RDTS 14
!-----------------------------------------------                                  !RDTS 15
!   D u m m y   A r g u m e n t s                                                 !RDTS 16
!-----------------------------------------------                                  !RDTS 17
      character(len=99) , intent(in) :: gcmdir                                    !RDTS 18
      character , intent(in) :: version                                           !RDTS 19
!-----------------------------------------------                                  !RDTS 20
!   L o c a l   P a r a m e t e r s                                               !RDTS 21
!-----------------------------------------------                                  !RDTS 22
      character(len=11), parameter :: sysform = 'unformatted'                     !RDTS 23
!-----------------------------------------------                                  !RDTS 24
!   L o c a l   V a r i a b l e s                                                 !RDTS 25
!-----------------------------------------------                                  !RDTS 26
      integer :: lastls, ilatstep, lendir, m, lsea, ls, lat, i, k, lon, ils, &    !RDTS 27
         jlon                                                                     !RDTS 28
      real(double) :: xlat, ylat                                                  !RDTS 29
!-----------------------------------------------                                  !RDTS 30
!---    Set parameter for form= in binary file open statement                     !RDTS 31
!                                                                                 !RDTS 32
!---    Initialize last Ls value processed to 0                                   !RDTS 33
      lastls = 0                                                                  !RDTS 34
!---    Set ilatstep = latitude step size x 10                                    !RDTS 35
      ilatstep = 1800/(ntlat - 1)                                                 !RDTS 36
!---    Compute string length for directory name                                  !RDTS 37
      lendir = Len_Trim(gcmdir)                                                   !RDTS 38
      if (lendir<1 .or. lendir>99) lendir = 99                                    !RDTS 39
!---    Step through all dust optical depths                                      !RDTS 40
      do m = 1, ntesy                                                             !RDTS 41
!---      Open surface data files for surface level                               !RDTS 42
         open(33, file=gcmdir(1:lendir)//'sfc00'//tesyr(m)//version//'.bin', &    !RDTS 43
            form=sysform, status='old', position='asis')                          !RDTS 44
!---      Open surface data files for 5 meter level above surface                 !RDTS 45
         open(34, file=gcmdir(1:lendir)//'sfc05'//tesyr(m)//version//'.bin', &    !RDTS 46
            form=sysform, status='old', position='asis')                          !RDTS 47
!---      Open surface data files for 30 meter level above surface                !RDTS 48
         open(35, file=gcmdir(1:lendir)//'sfc30'//tesyr(m)//version//'.bin', &    !RDTS 49
            form=sysform, status='old', position='asis')                          !RDTS 50
!---      Step through all Ls values                                              !RDTS 51
         do lsea = 30, 360, 30                                                    !RDTS 52
            ls = lsea/30                                                          !RDTS 53
!---      Step through all latitudes                                              !RDTS 54
            do lat = -900, 900, ilatstep                                          !RDTS 55
               xlat = lat/10.0D0                                                  !RDTS 56
               i = 1 + (lat + 900)/ilatstep                                       !RDTS 57
!---      Step through all boundary layer levels                                  !RDTS 58
               do k = 1, ntbl                                                     !RDTS 59
!---      Step through all longitudes                                             !RDTS 60
                  if (k == 1) then                                                !RDTS 61
                     do lon = ntlon, 1, -1                                        !RDTS 62
!---        Read (binary) tide coefficients for temperature and wind              !RDTS 63
!           components at all boundary layer levels                               !RDTS 64
                        read (32 + k, end=99) ils, ylat, jlon, ttsa0(k,i,lon,ls & !RDTS 65
                           ,m), ttsa1(k,i,lon,ls,m), ttsp1(k,i,lon,ls,m), ttsa2 & !RDTS 66
                           (k,i,lon,ls,m), ttsp2(k,i,lon,ls,m)                    !RDTS 67
!---         Assume surface wind = 0 (no slip condition)                          !RDTS 68
                        tusa0(k,i,lon,ls,m) = 0.0D0                               !RDTS 69
                        tusa1(k,i,lon,ls,m) = 0.0D0                               !RDTS 70
                        tusp1(k,i,lon,ls,m) = 0.0D0                               !RDTS 71
                        tusa2(k,i,lon,ls,m) = 0.0D0                               !RDTS 72
                        tusp2(k,i,lon,ls,m) = 0.0D0                               !RDTS 73
                        tvsa0(k,i,lon,ls,m) = 0.0D0                               !RDTS 74
                        tvsa1(k,i,lon,ls,m) = 0.0D0                               !RDTS 75
                        tvsp1(k,i,lon,ls,m) = 0.0D0                               !RDTS 76
                        tvsa2(k,i,lon,ls,m) = 0.0D0                               !RDTS 77
                        tvsp2(k,i,lon,ls,m) = 0.0D0                               !RDTS 78
                        if (ils /= lsea) stop ' Bad surface Ls'                   !RDTS 79
!---        Reset value of last Ls processed                                      !RDTS 80
                        lastls = ils                                              !RDTS 81
                        if (dabs(ylat-xlat)>0.0d0) stop ' Bad surface Latitude'   !RDTS 82
                        if (jlon == 9*lon) cycle                                  !RDTS 83
                        stop ' Bad surface Longitude'                             !RDTS 84
                     end do                                                       !RDTS 85
                  else                                                            !RDTS 86
                     do lon = ntlon, 1, -1                                        !RDTS 87
!---         Assume surface wind = 0 (no slip condition)                          !RDTS 88
                        read (32 + k, end=99) ils, ylat, jlon, ttsa0(k,i,lon,ls & !RDTS 89
                           ,m), ttsa1(k,i,lon,ls,m), ttsp1(k,i,lon,ls,m), ttsa2 & !RDTS 90
                           (k,i,lon,ls,m), ttsp2(k,i,lon,ls,m), tusa0(k,i,lon,  & !RDTS 91
                           ls,m), tusa1(k,i,lon,ls,m), tusp1(k,i,lon,ls,m),     & !RDTS 92
                           tusa2(k,i,lon,ls,m), tusp2(k,i,lon,ls,m), tvsa0(k,i, & !RDTS 93
                           lon,ls,m), tvsa1(k,i,lon,ls,m), tvsp1(k,i,lon,ls,m)  & !RDTS 94
                           , tvsa2(k,i,lon,ls,m), tvsp2(k,i,lon,ls,m)             !RDTS 95
                        if (ils /= lsea) stop ' Bad surface Ls'                   !RDTS 96
!---        Reset value of last Ls processed                                      !RDTS 97
                        lastls = ils                                              !RDTS 98
                        if (dabs(ylat-xlat)>0.0d0) stop ' Bad surface Latitude'   !RDTS 99
                        if (jlon == 9*lon) cycle                                  !RDTS100
                        stop ' Bad surface Longitude'                             !RDTS101
                     end do                                                       !RDTS102
                  endif                                                           !RDTS103
               end do                                                             !RDTS104
            end do                                                                !RDTS105
         end do                                                                   !RDTS106
!---      Set all values at Ls=0 to values at Ls=360                              !RDTS107
         ttsa0(:ntbl,:ntlat,1:ntlon,0,m) = ttsa0(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS108
         ttsa1(:ntbl,:ntlat,1:ntlon,0,m) = ttsa1(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS109
         ttsp1(:ntbl,:ntlat,1:ntlon,0,m) = ttsp1(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS110
         ttsa2(:ntbl,:ntlat,1:ntlon,0,m) = ttsa2(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS111
         ttsp2(:ntbl,:ntlat,1:ntlon,0,m) = ttsp2(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS112
         tusa0(:ntbl,:ntlat,1:ntlon,0,m) = tusa0(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS113
         tusa1(:ntbl,:ntlat,1:ntlon,0,m) = tusa1(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS114
         tusp1(:ntbl,:ntlat,1:ntlon,0,m) = tusp1(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS115
         tusa2(:ntbl,:ntlat,1:ntlon,0,m) = tusa2(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS116
         tusp2(:ntbl,:ntlat,1:ntlon,0,m) = tusp2(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS117
         tvsa0(:ntbl,:ntlat,1:ntlon,0,m) = tvsa0(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS118
         tvsa1(:ntbl,:ntlat,1:ntlon,0,m) = tvsa1(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS119
         tvsp1(:ntbl,:ntlat,1:ntlon,0,m) = tvsp1(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS120
         tvsa2(:ntbl,:ntlat,1:ntlon,0,m) = tvsa2(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS121
         tvsp2(:ntbl,:ntlat,1:ntlon,0,m) = tvsp2(:ntbl,:ntlat,1:ntlon,12,m)       !RDTS122
!           Set all values at Lon=0 to values at Lon=360                          !RDTS123
         ttsa0(:ntbl,:ntlat,0,:12,m) = ttsa0(:ntbl,:ntlat,ntlon,:12,m)            !RDTS124
         ttsa1(:ntbl,:ntlat,0,:12,m) = ttsa1(:ntbl,:ntlat,ntlon,:12,m)            !RDTS125
         ttsp1(:ntbl,:ntlat,0,:12,m) = ttsp1(:ntbl,:ntlat,ntlon,:12,m)            !RDTS126
         ttsa2(:ntbl,:ntlat,0,:12,m) = ttsa2(:ntbl,:ntlat,ntlon,:12,m)            !RDTS127
         ttsp2(:ntbl,:ntlat,0,:12,m) = ttsp2(:ntbl,:ntlat,ntlon,:12,m)            !RDTS128
         tusa0(:ntbl,:ntlat,0,:12,m) = tusa0(:ntbl,:ntlat,ntlon,:12,m)            !RDTS129
         tusa1(:ntbl,:ntlat,0,:12,m) = tusa1(:ntbl,:ntlat,ntlon,:12,m)            !RDTS130
         tusp1(:ntbl,:ntlat,0,:12,m) = tusp1(:ntbl,:ntlat,ntlon,:12,m)            !RDTS131
         tusa2(:ntbl,:ntlat,0,:12,m) = tusa2(:ntbl,:ntlat,ntlon,:12,m)            !RDTS132
         tusp2(:ntbl,:ntlat,0,:12,m) = tusp2(:ntbl,:ntlat,ntlon,:12,m)            !RDTS133
         tvsa0(:ntbl,:ntlat,0,:12,m) = tvsa0(:ntbl,:ntlat,ntlon,:12,m)            !RDTS134
         tvsa1(:ntbl,:ntlat,0,:12,m) = tvsa1(:ntbl,:ntlat,ntlon,:12,m)            !RDTS135
         tvsp1(:ntbl,:ntlat,0,:12,m) = tvsp1(:ntbl,:ntlat,ntlon,:12,m)            !RDTS136
         tvsa2(:ntbl,:ntlat,0,:12,m) = tvsa2(:ntbl,:ntlat,ntlon,:12,m)            !RDTS137
         tvsp2(:ntbl,:ntlat,0,:12,m) = tvsp2(:ntbl,:ntlat,ntlon,:12,m)            !RDTS138
!---      Close input file units                                                  !RDTS139
         close(33)                                                                !RDTS140
         close(34)                                                                !RDTS141
         close(35)                                                                !RDTS142
         cycle                                                                    !RDTS143
!---      Terminate if not all Ls values processed                                !RDTS144
   99    continue                                                                 !RDTS145
         if (lastls == 360) cycle                                                 !RDTS146
         stop ' Incomplete TES surface GCM data'                                  !RDTS147
      end do                                                                      !RDTS148
      return                                                                      !RDTS149
      end subroutine rdtessrf_M10                                                 !RDTS150
!                                                                                 !RDTS151
!------------------------------------------------------------------------------   !RDTM  1
      subroutine rdtesmgcm_M10(gcmdir, version)                                   !RDTM  2
!-----------------------------------------------                                  !RDTM  3
!   M o d u l e s                                                                 !RDTM  4
!-----------------------------------------------                                  !RDTM  5
      USE vast_kind_param, ONLY:  double                                          !RDTM  6
      USE mgcmtes_M10_C                                                           !RDTM  7
      USE mgcmparm_M10_C                                                          !RDTM  8
      USE parameters_M10_C                                                        !RDTM  9
!-----------------------------------------------                                  !RDTM 10
!---    Reads NASA Ames Mars General Circulation Model (MGCM) -5 to 80            !RDTM 11
!       km data (in binary format) for TES years 1& 2 and loads into              !RDTM 12
!       data arrays for common MGCMTES                                            !RDTM 13
!       GCMDIR is directory name where MGCM data resides                          !RDTM 14
!...                                                                              !RDTM 15
!...Switches:                                                                     !RDTM 16
      implicit none                                                               !RDTM 17
!-----------------------------------------------                                  !RDTM 18
!   D u m m y   A r g u m e n t s                                                 !RDTM 19
!-----------------------------------------------                                  !RDTM 20
      character(len=99) , intent(in) :: gcmdir                                    !RDTM 21
      character , intent(in) :: version                                           !RDTM 22
!-----------------------------------------------                                  !RDTM 23
!   L o c a l   P a r a m e t e r s                                               !RDTM 24
!-----------------------------------------------                                  !RDTM 25
      character(len=11), parameter :: sysform = 'unformatted'                     !RDTM 26
!-----------------------------------------------                                  !RDTM 27
!   L o c a l   V a r i a b l e s                                                 !RDTM 28
!-----------------------------------------------                                  !RDTM 29
      integer :: lastls, ilatstep, lendir, m, lsea, ls, lat, i, k, ils, ihgt,  &  !RDTM 30
         khgt                                                                     !RDTM 31
      real(double) :: xlat, ylat                                                  !RDTM 32
!-----------------------------------------------                                  !RDTM 33
!---    Set parameter for form= in binary file open statement                     !RDTM 34
!                                                                                 !RDTM 35
!---    Initialize last Ls value processed to 0                                   !RDTM 36
      lastls = 0                                                                  !RDTM 37
!---    Set ilatstep = latitude step size x 10                                    !RDTM 38
      ilatstep = 1800/(nlat - 1)                                                  !RDTM 39
!---    Compute string length for directory name                                  !RDTM 40
      lendir = Len_Trim(gcmdir)                                                   !RDTM 41
      if (lendir<1 .or. lendir>99) lendir = 99                                    !RDTM 42
!---    Step through all dust optical depths                                      !RDTM 43
      do m = 1, ntesy                                                             !RDTM 44
!---      Open MGCM input files for temperature, pressure, and density            !RDTM 45
         open(32, file=gcmdir(1:lendir)//'tpdlo'//tesyr(m)//version//'.bin',  &   !RDTM 46
            form=sysform, status='old', position='asis')                          !RDTM 47
!---      Open MGCM input files for wind components                               !RDTM 48
         open(33, file=gcmdir(1:lendir)//'uvlo'//tesyr(m)//version//'.bin',   &   !RDTM 49
            form=sysform, status='old', position='asis')                          !RDTM 50
!---      Step through all Ls values                                              !RDTM 51
         do lsea = 30, 360, 30                                                    !RDTM 52
            ls = lsea/30                                                          !RDTM 53
!---      Step through all latitude grid points                                   !RDTM 54
            do lat = -900, 900, ilatstep                                          !RDTM 55
               xlat = lat/10.0D0                                                  !RDTM 56
               i = 1 + (lat + 900)/ilatstep                                       !RDTM 57
!---      Step through all height levels                                          !RDTM 58
               do k = nthgt, 1, -1                                                !RDTM 59
!---        Read (binary) tide coefficients for temperature, pressure,            !RDTM 60
!           and density                                                           !RDTM 61
                  read (32, end=99) ils, ihgt, ylat, ttza0(k,i,ls,m), ttza1(k,i & !RDTM 62
                     ,ls,m), ttzp1(k,i,ls,m), ttza2(k,i,ls,m), ttzp2(k,i,ls,m)  & !RDTM 63
                     , tdza0(k,i,ls,m), tdza1(k,i,ls,m), tdzp1(k,i,ls,m), tdza2 & !RDTM 64
                     (k,i,ls,m), tdzp2(k,i,ls,m), tpza0(k,i,ls,m)                 !RDTM 65
                  if (ils /= lsea) stop ' Bad tpd Ls'                             !RDTM 66
                  khgt = 5*(k - 14)                                               !RDTM 67
                  if (k < 16) khgt = k - 6                                        !RDTM 68
                  if (ihgt /= khgt) stop ' Bad tpd Height'                        !RDTM 69
                  if (dabs(ylat-xlat)>0.0d0) stop ' Bad tpd Latitude'             !RDTM 70
!---        Read (binary) tide coefficients for wind components                   !RDTM 71
                  read (33, end=99) ils, ihgt, ylat, tuza0(k,i,ls,m), tuza1(k,i & !RDTM 72
                     ,ls,m), tuzp1(k,i,ls,m), tuza2(k,i,ls,m), tuzp2(k,i,ls,m)  & !RDTM 73
                     , tvza0(k,i,ls,m), tvza1(k,i,ls,m), tvzp1(k,i,ls,m), tvza2 & !RDTM 74
                     (k,i,ls,m), tvzp2(k,i,ls,m)                                  !RDTM 75
                  if (ils /= lsea) stop ' Bad uv Ls'                              !RDTM 76
!---        Reset value of last Ls processed                                      !RDTM 77
                  lastls = ils                                                    !RDTM 78
                  if (ihgt /= khgt) stop ' Bad uv Height'                         !RDTM 79
                  if (dabs(ylat-xlat)<=0.0d0) cycle                               !RDTM 80
                  stop ' Bad uv Latitude'                                         !RDTM 81
               end do                                                             !RDTM 82
            end do                                                                !RDTM 83
         end do                                                                   !RDTM 84
!---      Set data for Ls = 0 to data for Ls = 360                                !RDTM 85
         ttza0(:nthgt,:nlat,0,m) = ttza0(:nthgt,:nlat,12,m)                       !RDTM 86
         ttza1(:nthgt,:nlat,0,m) = ttza1(:nthgt,:nlat,12,m)                       !RDTM 87
         ttzp1(:nthgt,:nlat,0,m) = ttzp1(:nthgt,:nlat,12,m)                       !RDTM 88
         ttza2(:nthgt,:nlat,0,m) = ttza2(:nthgt,:nlat,12,m)                       !RDTM 89
         ttzp2(:nthgt,:nlat,0,m) = ttzp2(:nthgt,:nlat,12,m)                       !RDTM 90
         tdza0(:nthgt,:nlat,0,m) = tdza0(:nthgt,:nlat,12,m)                       !RDTM 91
         tdza1(:nthgt,:nlat,0,m) = tdza1(:nthgt,:nlat,12,m)                       !RDTM 92
         tdzp1(:nthgt,:nlat,0,m) = tdzp1(:nthgt,:nlat,12,m)                       !RDTM 93
         tdza2(:nthgt,:nlat,0,m) = tdza2(:nthgt,:nlat,12,m)                       !RDTM 94
         tdzp2(:nthgt,:nlat,0,m) = tdzp2(:nthgt,:nlat,12,m)                       !RDTM 95
         tpza0(:nthgt,:nlat,0,m) = tpza0(:nthgt,:nlat,12,m)                       !RDTM 96
         tuza0(:nthgt,:nlat,0,m) = tuza0(:nthgt,:nlat,12,m)                       !RDTM 97
         tuza1(:nthgt,:nlat,0,m) = tuza1(:nthgt,:nlat,12,m)                       !RDTM 98
         tuzp1(:nthgt,:nlat,0,m) = tuzp1(:nthgt,:nlat,12,m)                       !RDTM 99
         tuza2(:nthgt,:nlat,0,m) = tuza2(:nthgt,:nlat,12,m)                       !RDTM100
         tuzp2(:nthgt,:nlat,0,m) = tuzp2(:nthgt,:nlat,12,m)                       !RDTM101
         tvza0(:nthgt,:nlat,0,m) = tvza0(:nthgt,:nlat,12,m)                       !RDTM102
         tvza1(:nthgt,:nlat,0,m) = tvza1(:nthgt,:nlat,12,m)                       !RDTM103
         tvzp1(:nthgt,:nlat,0,m) = tvzp1(:nthgt,:nlat,12,m)                       !RDTM104
         tvza2(:nthgt,:nlat,0,m) = tvza2(:nthgt,:nlat,12,m)                       !RDTM105
         tvzp2(:nthgt,:nlat,0,m) = tvzp2(:nthgt,:nlat,12,m)                       !RDTM106
!---    Close input files to re-use same unit number for next dust                !RDTM107
!       value                                                                     !RDTM108
         close(32)                                                                !RDTM109
         close(33)                                                                !RDTM110
         cycle                                                                    !RDTM111
!---    Terminate if not all Ls values have been processed                        !RDTM112
   99    continue                                                                 !RDTM113
         if (lastls == 360) cycle                                                 !RDTM114
         stop ' Incomplete -5 to 80 km MGCM data'                                 !RDTM115
      end do                                                                      !RDTM116
      return                                                                      !RDTM117
      end subroutine rdtesmgcm_M10                                                !RDTM118
!                                                                                 !RDTM119
!------------------------------------------------------------------------------   !RDTT  1
      subroutine rdtestgcm_M10(gcmdir, version)                                   !RDTT  2
!-----------------------------------------------                                  !RDTT  3
!   M o d u l e s                                                                 !RDTT  4
!-----------------------------------------------                                  !RDTT  5
      USE vast_kind_param, ONLY:  double                                          !RDTT  6
      USE tgcmtes_M10_C                                                           !RDTT  7
      USE mgcmparm_M10_C                                                          !RDTT  8
      USE parameters_M10_C                                                        !RDTT  9
!-----------------------------------------------                                  !RDTT 10
!---    Reads University of Michigan Mars Thermospheric General Circu-            !RDTT 11
!       lation Model (MTGCM) data (in binary format) for TES years 1&2            !RDTT 12
!       and loads into dataarrays for common TGCMTES                              !RDTT 13
!       GCMDIR is directory name where MTGCM data resides                         !RDTT 14
!...                                                                              !RDTT 15
!...Switches:                                                                     !RDTT 16
      implicit none                                                               !RDTT 17
!-----------------------------------------------                                  !RDTT 18
!   D u m m y   A r g u m e n t s                                                 !RDTT 19
!-----------------------------------------------                                  !RDTT 20
      character(len=99) , intent(in) :: gcmdir                                    !RDTT 21
      character , intent(in) :: version                                           !RDTT 22
!-----------------------------------------------                                  !RDTT 23
!   L o c a l   P a r a m e t e r s                                               !RDTT 24
!-----------------------------------------------                                  !RDTT 25
      character(len=11), parameter :: sysform = 'unformatted'                     !RDTT 26
!-----------------------------------------------                                  !RDTT 27
!   L o c a l   V a r i a b l e s                                                 !RDTT 28
!-----------------------------------------------                                  !RDTT 29
      integer :: lastls, ilatstep, ilat1, ilat2, lendir, n, m, lsea, ls, lat, i & !RDTT 30
         , iyr, ils, nhgttop, k, ihgt                                             !RDTT 31
      real(double) :: xlat, ylat                                                  !RDTT 32
!-----------------------------------------------                                  !RDTT 33
!---    Set parameter for form= in binary file open statement                     !RDTT 34
!                                                                                 !RDTT 35
!---    Initialize last Ls value processed to 0                                   !RDTT 36
      lastls = 0                                                                  !RDTT 37
!---    Set ilatstep = latitude step size x 10                                    !RDTT 38
      ilatstep = 1800/nlatt                                                       !RDTT 39
!---    Set initial and final latitudes (x 10) for stepping                       !RDTT 40
      ilat1 = (-900) + ilatstep/2                                                 !RDTT 41
      ilat2 = 900 - ilatstep/2                                                    !RDTT 42
!---    Compute string length for directory name                                  !RDTT 43
      lendir = Len_Trim(gcmdir)                                                   !RDTT 44
      if (lendir<1 .or. lendir>99) lendir = 99                                    !RDTT 45
!---    Step through all solar activity levels                                    !RDTT 46
      do n = 1, ntf10                                                             !RDTT 47
         open(34, file=gcmdir(1:lendir)//'zfTES'//soltes(n)//version//'.txt',   & !RDTT 48
            status='old', position='asis')                                        !RDTT 49
!---    Step through all dust optical depths                                      !RDTT 50
         do m = 1, ntesy                                                          !RDTT 51
!---      Open MTGCM data files for temperature, pressure, and density            !RDTT 52
            open(32, file=gcmdir(1:lendir)//'tpd'//soltes(n)//tesyr(m)//version & !RDTT 53
               //'.bin', form=sysform, status='old', position='asis')             !RDTT 54
!---      Open MTGCM data files for wind components                               !RDTT 55
            open(33, file=gcmdir(1:lendir)//'uv'//soltes(n)//tesyr(m)//version  & !RDTT 56
               //'.bin', form=sysform, status='old', position='asis')             !RDTT 57
!---      Step through all Ls values                                              !RDTT 58
            do lsea = 30, 360, 30                                                 !RDTT 59
               ls = lsea/30                                                       !RDTT 60
!---      Step through all latitudes                                              !RDTT 61
               do lat = ilat1, ilat2, ilatstep                                    !RDTT 62
                  xlat = lat/10.0D0                                               !RDTT 63
                  i = 1 + (lat - ilat1)/ilatstep                                  !RDTT 64
!---        Read tide coefficient data for ZF=height of 1.26 nbar level           !RDTT 65
                  read (34, *, end=99) iyr, ils, ylat, tzfa0(i,ls,m,n), tzfa1(i & !RDTT 66
                     ,ls,m,n), tzfp1(i,ls,m,n), tzfa2(i,ls,m,n), tzfp2(i,ls,m,n & !RDTT 67
                     ), nhgttop                                                   !RDTT 68
                  if (nhgttop /= 240) then                                        !RDTT 69
                     write (*, *) 'iyr,ils,ylat,nhgttop=', iyr, ils, ylat,      & !RDTT 70
                        nhgttop                                                   !RDTT 71
                     stop ' Bad nhgttop value'                                    !RDTT 72
                  endif                                                           !RDTT 73
                  if (iyr /= m) stop ' Bad ZF year value'                         !RDTT 74
                  if (ils /= lsea) stop ' Bad ZF Ls'                              !RDTT 75
                  if (dabs(ylat-xlat) > 0.0d0) stop ' Bad ZF Latitude'            !RDTT 76
                  nhgttop = (nhgttop - 75)/5                                      !RDTT 77
                  iztop(ls,m,n) = nhgttop                                         !RDTT 78
!---      Step through all heights                                                !RDTT 79
                  do k = 1, nhgttop                                               !RDTT 80
!---        Read (binary) tide coefficients for temperature, pressure,            !RDTT 81
!           and density                                                           !RDTT 82
                     read (32, end=99) ils, ihgt, ylat, ttta0(k,i,ls,m,n),      & !RDTT 83
                        ttta1(k,i,ls,m,n), tttp1(k,i,ls,m,n), ttta2(k,i,ls,m,n) & !RDTT 84
                        , tttp2(k,i,ls,m,n), tpta0(k,i,ls,m,n), tpta1(k,i,ls,m, & !RDTT 85
                        n), tptp1(k,i,ls,m,n), tpta2(k,i,ls,m,n), tptp2(k,i,ls, & !RDTT 86
                        m,n), tdta0(k,i,ls,m,n), tdta1(k,i,ls,m,n), tdtp1(k,i,  & !RDTT 87
                        ls,m,n), tdta2(k,i,ls,m,n), tdtp2(k,i,ls,m,n)             !RDTT 88
                     if (ils /= lsea) stop ' Bad tpd Ls'                          !RDTT 89
                     if (ihgt /= 80 + 5*(k - 1)) stop ' Bad tpd Height'           !RDTT 90
                     if (dabs(ylat-xlat) > 0.0d0) stop ' Bad tpd Latitude'        !RDTT 91
!---        Read (binary) tide coefficients for wind components                   !RDTT 92
                     read (33, end=99) ils, ihgt, ylat, tuta0(k,i,ls,m,n),      & !RDTT 93
                        tuta1(k,i,ls,m,n), tutp1(k,i,ls,m,n), tuta2(k,i,ls,m,n) & !RDTT 94
                        , tutp2(k,i,ls,m,n), tvta0(k,i,ls,m,n), tvta1(k,i,ls,m, & !RDTT 95
                        n), tvtp1(k,i,ls,m,n), tvta2(k,i,ls,m,n), tvtp2(k,i,ls, & !RDTT 96
                        m,n)                                                      !RDTT 97
                     if (ils /= lsea) stop ' Bad uv Ls'                           !RDTT 98
!---        Reset last Ls value processed                                         !RDTT 99
                     lastls = ils                                                 !RDTT100
                     if (ihgt /= 80 + 5*(k - 1)) stop ' Bad uv Height'            !RDTT101
                     if (dabs(ylat-xlat)<=0.0d0) cycle                            !RDTT102
                     stop ' Bad uv Latitude'                                      !RDTT103
                  end do                                                          !RDTT104
               end do                                                             !RDTT105
            end do                                                                !RDTT106
!---      Set all values at Ls=0 to values at Ls=360                              !RDTT107
            iztop(0,m,n) = iztop(12,m,n)                                          !RDTT108
            tzfa0(:nlatt,0,m,n) = tzfa0(:nlatt,12,m,n)                            !RDTT109
            tzfa1(:nlatt,0,m,n) = tzfa1(:nlatt,12,m,n)                            !RDTT110
            tzfp1(:nlatt,0,m,n) = tzfp1(:nlatt,12,m,n)                            !RDTT111
            tzfa2(:nlatt,0,m,n) = tzfa2(:nlatt,12,m,n)                            !RDTT112
            tzfp2(:nlatt,0,m,n) = tzfp2(:nlatt,12,m,n)                            !RDTT113
            ttta0(:iztop(12,m,n),:nlatt,0,m,n) = ttta0(:iztop(12,m,n),:nlatt,12 & !RDTT114
               ,m,n)                                                              !RDTT115
            ttta1(:iztop(12,m,n),:nlatt,0,m,n) = ttta1(:iztop(12,m,n),:nlatt,12 & !RDTT116
               ,m,n)                                                              !RDTT117
            tttp1(:iztop(12,m,n),:nlatt,0,m,n) = tttp1(:iztop(12,m,n),:nlatt,12 & !RDTT118
               ,m,n)                                                              !RDTT119
            ttta2(:iztop(12,m,n),:nlatt,0,m,n) = ttta2(:iztop(12,m,n),:nlatt,12 & !RDTT120
               ,m,n)                                                              !RDTT121
            tttp2(:iztop(12,m,n),:nlatt,0,m,n) = tttp2(:iztop(12,m,n),:nlatt,12 & !RDTT122
               ,m,n)                                                              !RDTT123
            tpta0(:iztop(12,m,n),:nlatt,0,m,n) = tpta0(:iztop(12,m,n),:nlatt,12 & !RDTT124
               ,m,n)                                                              !RDTT125
            tpta1(:iztop(12,m,n),:nlatt,0,m,n) = tpta1(:iztop(12,m,n),:nlatt,12 & !RDTT126
               ,m,n)                                                              !RDTT127
            tptp1(:iztop(12,m,n),:nlatt,0,m,n) = tptp1(:iztop(12,m,n),:nlatt,12 & !RDTT128
               ,m,n)                                                              !RDTT129
            tpta2(:iztop(12,m,n),:nlatt,0,m,n) = tpta2(:iztop(12,m,n),:nlatt,12 & !RDTT130
               ,m,n)                                                              !RDTT131
            tptp2(:iztop(12,m,n),:nlatt,0,m,n) = tptp2(:iztop(12,m,n),:nlatt,12 & !RDTT132
               ,m,n)                                                              !RDTT133
            tdta0(:iztop(12,m,n),:nlatt,0,m,n) = tdta0(:iztop(12,m,n),:nlatt,12 & !RDTT134
               ,m,n)                                                              !RDTT135
            tdta1(:iztop(12,m,n),:nlatt,0,m,n) = tdta1(:iztop(12,m,n),:nlatt,12 & !RDTT136
               ,m,n)                                                              !RDTT137
            tdtp1(:iztop(12,m,n),:nlatt,0,m,n) = tdtp1(:iztop(12,m,n),:nlatt,12 & !RDTT138
               ,m,n)                                                              !RDTT139
            tdta2(:iztop(12,m,n),:nlatt,0,m,n) = tdta2(:iztop(12,m,n),:nlatt,12 & !RDTT140
               ,m,n)                                                              !RDTT141
            tdtp2(:iztop(12,m,n),:nlatt,0,m,n) = tdtp2(:iztop(12,m,n),:nlatt,12 & !RDTT142
               ,m,n)                                                              !RDTT143
            tuta0(:iztop(12,m,n),:nlatt,0,m,n) = tuta0(:iztop(12,m,n),:nlatt,12 & !RDTT144
               ,m,n)                                                              !RDTT145
            tuta1(:iztop(12,m,n),:nlatt,0,m,n) = tuta1(:iztop(12,m,n),:nlatt,12 & !RDTT146
               ,m,n)                                                              !RDTT147
            tutp1(:iztop(12,m,n),:nlatt,0,m,n) = tutp1(:iztop(12,m,n),:nlatt,12 & !RDTT148
               ,m,n)                                                              !RDTT149
            tuta2(:iztop(12,m,n),:nlatt,0,m,n) = tuta2(:iztop(12,m,n),:nlatt,12 & !RDTT150
               ,m,n)                                                              !RDTT151
            tutp2(:iztop(12,m,n),:nlatt,0,m,n) = tutp2(:iztop(12,m,n),:nlatt,12 & !RDTT152
               ,m,n)                                                              !RDTT153
            tvta0(:iztop(12,m,n),:nlatt,0,m,n) = tvta0(:iztop(12,m,n),:nlatt,12 & !RDTT154
               ,m,n)                                                              !RDTT155
            tvta1(:iztop(12,m,n),:nlatt,0,m,n) = tvta1(:iztop(12,m,n),:nlatt,12 & !RDTT156
               ,m,n)                                                              !RDTT157
            tvtp1(:iztop(12,m,n),:nlatt,0,m,n) = tvtp1(:iztop(12,m,n),:nlatt,12 & !RDTT158
               ,m,n)                                                              !RDTT159
            tvta2(:iztop(12,m,n),:nlatt,0,m,n) = tvta2(:iztop(12,m,n),:nlatt,12 & !RDTT160
               ,m,n)                                                              !RDTT161
            tvtp2(:iztop(12,m,n),:nlatt,0,m,n) = tvtp2(:iztop(12,m,n),:nlatt,12 & !RDTT162
               ,m,n)                                                              !RDTT163
!---    Close input file unit numbers                                             !RDTT164
            close(32)                                                             !RDTT165
            close(33)                                                             !RDTT166
            cycle                                                                 !RDTT167
!---    Terminate if not all Ls values processed                                  !RDTT168
   99       continue                                                              !RDTT169
            if (lastls == 360) cycle                                              !RDTT170
            stop ' Incomplete 80-240  km MTGCM data'                              !RDTT171
         end do                                                                   !RDTT172
         close(34)                                                                !RDTT173
      end do                                                                      !RDTT174
      return                                                                      !RDTT175
      end subroutine rdtestgcm_M10                                                !RDTT176
!                                                                                 !RDTT177
!-------------------------------------------------------------------------------  !TGTP  1
      subroutine tesgterp_M10(khgt, time, tmgcm, pmgcm, dmgcm, umgcm, vmgcm,    & !TGTP  2
         tempday, presday, densday, uwndday, vwndday, tempmax, tempmin, densmax & !TGTP  3
         , densmin, mtesy, idaydata)                                              !TGTP  4
!-----------------------------------------------                                  !TGTP  5
!   M o d u l e s                                                                 !TGTP  6
!-----------------------------------------------                                  !TGTP  7
      USE vast_kind_param, ONLY:  double                                          !TGTP  8
      USE testerp_M10_C                                                           !TGTP  9
      USE mgcmtes_M10_C                                                           !TGTP 10
      USE parameters_M10_C                                                        !TGTP 11
!-----------------------------------------------                                  !TGTP 12
!---    Interpolates Ames Mars General Circulation Model (MGCM) data              !TGTP 13
!       to a given latitude, time of year (Ls), for a given TES year,             !TGTP 14
!       height index (khgt) and time of day (time).                               !TGTP 15
!       Some input data is provided by the Common "Interp".                       !TGTP 16
!---    Set parameter values for number of heights, latitudes                     !TGTP 17
!...                                                                              !TGTP 18
!...Switches:                                                                     !TGTP 19
!-----------------------------------------------                                  !TGTP 20
!   I n t e r f a c e   B l o c k s                                               !TGTP 21
!-----------------------------------------------                                  !TGTP 22
      use tidex_M10_I                                                             !TGTP 23
      use tidey_M10_I                                                             !TGTP 24
      use twod_M10_I                                                              !TGTP 25
      implicit none                                                               !TGTP 26
!-----------------------------------------------                                  !TGTP 27
!   D u m m y   A r g u m e n t s                                                 !TGTP 28
!-----------------------------------------------                                  !TGTP 29
      integer , intent(in) :: khgt                                                !TGTP 30
      integer , intent(in) :: mtesy                                               !TGTP 31
      integer , intent(in) :: idaydata                                            !TGTP 32
      real(double) , intent(in)  :: time                                          !TGTP 33
      real(double) , intent(out)  :: tmgcm                                        !TGTP 34
      real(double) , intent(out) :: pmgcm                                         !TGTP 35
      real(double) , intent(out)  :: dmgcm                                        !TGTP 36
      real(double) , intent(out)  :: umgcm                                        !TGTP 37
      real(double) , intent(out)  :: vmgcm                                        !TGTP 38
      real(double) , intent(out)  :: tempday                                      !TGTP 39
      real(double) , intent(out) :: presday                                       !TGTP 40
      real(double) , intent(out)  :: densday                                      !TGTP 41
      real(double) , intent(out)  :: uwndday                                      !TGTP 42
      real(double) , intent(out)  :: vwndday                                      !TGTP 43
      real(double) , intent(out)  :: tempmax                                      !TGTP 44
      real(double) , intent(out)  :: tempmin                                      !TGTP 45
      real(double) , intent(out)  :: densmax                                      !TGTP 46
      real(double) , intent(out) :: densmin                                       !TGTP 47
!-----------------------------------------------                                  !TGTP 48
!   L o c a l   V a r i a b l e s                                                 !TGTP 49
!-----------------------------------------------                                  !TGTP 50
      integer :: i, l, itime                                                      !TGTP 51
      real(double), dimension(2,2) :: tm, dm, um, vm, r0, tday, dday, uday,     & !TGTP 52
         vday, tmax, tmin, dmax, dmin                                             !TGTP 53
      real(double) :: polefac, upolefac, t0, a1t, p1t, a2t, p2t, d0, a1d, p1d,  & !TGTP 54
         a2d, p2d, p0, xtime, ttime, dtime, u0, a1, p1, a2, p2, v0, rmgcm         !TGTP 55
!                                                                                 !TGTP 56
!-----------------------------------------------                                  !TGTP 57
!---    MGCM -5 to 80 km data arrays for interpolation                            !TGTP 58
!---    Establish MGCM values at corners of a 2-dimensional cube in               !TGTP 59
!       latitude-Ls space, at the given height index (khgt), and                  !TGTP 60
!       time of day (time)                                                        !TGTP 61
      do i = 1, 2                                                                 !TGTP 62
         polefac = 1.0D0                                                          !TGTP 63
         upolefac = 1.0D0                                                         !TGTP 64
         if (ilat == 1) then                                                      !TGTP 65
            polefac = i - 1.0D0                                                   !TGTP 66
         else if (ilat == ntlat - 1) then                                         !TGTP 67
            polefac = 2.0D0 - i                                                   !TGTP 68
         endif                                                                    !TGTP 69
         if (ilatw == 2) then                                                     !TGTP 70
            if (i == 1) upolefac = wpolefac                                       !TGTP 71
         else if (ilatw == ntlat - 1) then                                        !TGTP 72
            if (i == 2) upolefac = wpolefac                                       !TGTP 73
         endif                                                                    !TGTP 74
         do l = 1, 2                                                              !TGTP 75
!---      Daily mean temperature                                                  !TGTP 76
            t0 = ttza0(khgt,ilat+i-1,ls+l-1,mtesy)                                !TGTP 77
            tday(i,l) = t0                                                        !TGTP 78
!---      Temperature tide amplitudes and phases                                  !TGTP 79
            a1t = ttza1(khgt,ilat+i-1,ls+l-1,mtesy)*polefac                       !TGTP 80
            p1t = ttzp1(khgt,ilat+i-1,ls+l-1,mtesy)                               !TGTP 81
            a2t = ttza2(khgt,ilat+i-1,ls+l-1,mtesy)*polefac                       !TGTP 82
            p2t = ttzp2(khgt,ilat+i-1,ls+l-1,mtesy)                               !TGTP 83
!---      Temperature at corners of 2-D cube                                      !TGTP 84
            tm(i,l) = tidex_M10(t0,a1t,p1t,a2t,p2t,time)                          !TGTP 85
!---      Daily mean density                                                      !TGTP 86
            d0 = tdza0(khgt,ilat+i-1,ls+l-1,mtesy)                                !TGTP 87
            dday(i,l) = d0                                                        !TGTP 88
!---      Density tide amplitudes and phases                                      !TGTP 89
            a1d = tdza1(khgt,ilat+i-1,ls+l-1,mtesy)*polefac                       !TGTP 90
            p1d = tdzp1(khgt,ilat+i-1,ls+l-1,mtesy)                               !TGTP 91
            a2d = tdza2(khgt,ilat+i-1,ls+l-1,mtesy)*polefac                       !TGTP 92
            p2d = tdzp2(khgt,ilat+i-1,ls+l-1,mtesy)                               !TGTP 93
!---      Density at corners of 2-D cube                                          !TGTP 94
            dm(i,l) = tidey_M10(d0,a1d,p1d,a2d,p2d,time)                          !TGTP 95
!---      Daily average pressure P0                                               !TGTP 96
            p0 = tpza0(khgt,ilat+i-1,ls+l-1,mtesy)                                !TGTP 97
!---      Gas constant from pressure, density and temperature                     !TGTP 98
            r0(i,l) = 190.0D0                                                     !TGTP 99
            if (dabs(t0)>0.0D0 .and. dabs(d0)>0.0D0) r0(i,l) = p0/(t0*d0)         !TGTP100
!---      Max and Min temperature and density at corners of 2-D cube              !TGTP101
            tmax(i,l) = -9999.0D0                                                 !TGTP102
            tmin(i,l) = 9999.0D0                                                  !TGTP103
            dmax(i,l) = -9999.0D0                                                 !TGTP104
            dmin(i,l) = 9999.0D0                                                  !TGTP105
            if (idaydata > 0) then                                                !TGTP106
               do itime = 0, 23                                                   !TGTP107
                  xtime = float(itime)                                            !TGTP108
                  ttime = tidex_M10(t0,a1t,p1t,a2t,p2t,xtime)                     !TGTP109
                  dtime = tidey_M10(d0,a1d,p1d,a2d,p2d,xtime)                     !TGTP110
!---              ptime = dtime*r0(i,l)*ttime                                     !TGTP111
                  tmax(i,l) = dmax1(ttime,tmax(i,l))                              !TGTP112
                  tmin(i,l) = min(ttime,tmin(i,l))                                !TGTP113
                  dmax(i,l) = dmax1(dtime,dmax(i,l))                              !TGTP114
                  dmin(i,l) = min(dtime,dmin(i,l))                                !TGTP115
               end do                                                             !TGTP116
            endif                                                                 !TGTP117
!---      Daily mean EW wind                                                      !TGTP118
            u0 = tuza0(khgt,ilat+i-1,ls+l-1,mtesy)                                !TGTP119
            uday(i,l) = u0                                                        !TGTP120
!---      EW wind tide amplitudes and phases                                      !TGTP121
            a1 = tuza1(khgt,ilat+i-1,ls+l-1,mtesy)*upolefac                       !TGTP122
            p1 = tuzp1(khgt,ilat+i-1,ls+l-1,mtesy)                                !TGTP123
            a2 = tuza2(khgt,ilat+i-1,ls+l-1,mtesy)*upolefac                       !TGTP124
            p2 = tuzp2(khgt,ilat+i-1,ls+l-1,mtesy)                                !TGTP125
!---      EW wind at corners of 2-D cube                                          !TGTP126
            um(i,l) = tidex_M10(u0,a1,p1,a2,p2,time)                              !TGTP127
!---      Daily mean NS wind                                                      !TGTP128
            v0 = tvza0(khgt,ilatw+i-1,ls+l-1,mtesy)                               !TGTP129
            vday(i,l) = v0                                                        !TGTP130
!---      NS wind tide amplitudes and phases                                      !TGTP131
            a1 = tvza1(khgt,ilatw+i-1,ls+l-1,mtesy)*upolefac                      !TGTP132
            p1 = tvzp1(khgt,ilatw+i-1,ls+l-1,mtesy)                               !TGTP133
            a2 = tvza2(khgt,ilatw+i-1,ls+l-1,mtesy)*upolefac                      !TGTP134
            p2 = tvzp2(khgt,ilatw+i-1,ls+l-1,mtesy)                               !TGTP135
!---      NS wind at corners of 2-D cube                                          !TGTP136
            vm(i,l) = tidex_M10(v0,a1,p1,a2,p2,time)                              !TGTP137
         end do                                                                   !TGTP138
      end do                                                                      !TGTP139
!---    Use 2-D interpolation to get temperature, pressure, gas                   !TGTP140
!       constant, EW wind, and NS wind at given latitude, and Ls                  !TGTP141
      call twod_M10 (dlat, dls, tm, tmgcm)                                        !TGTP142
      call twod_M10 (dlat, dls, tday, tempday)                                    !TGTP143
      call twod_M10 (dlat, dls, tmax, tempmax)                                    !TGTP144
      call twod_M10 (dlat, dls, tmin, tempmin)                                    !TGTP145
      call twod_M10 (dlat, dls, dmax, densmax)                                    !TGTP146
      call twod_M10 (dlat, dls, dmin, densmin)                                    !TGTP147
      call twod_M10 (dlat, dls, dm, dmgcm)                                        !TGTP148
      call twod_M10 (dlat, dls, dday, densday)                                    !TGTP149
      call twod_M10 (dlat, dls, r0, rmgcm)                                        !TGTP150
      call twod_M10 (dlat, dls, um, umgcm)                                        !TGTP151
      call twod_M10 (dlat, dls, uday, uwndday)                                    !TGTP152
      call twod_M10 (dlatw, dls, vm, vmgcm)                                       !TGTP153
      call twod_M10 (dlatw, dls, vday, vwndday)                                   !TGTP154
!---    Compute pressure from temperature, density, and gas constant              !TGTP155
      pmgcm = dmgcm*rmgcm*tmgcm                                                   !TGTP156
      presday = densday*rmgcm*tempday                                             !TGTP157
      return                                                                      !TGTP158
      end subroutine tesgterp_M10                                                 !TGTP159
!                                                                                 !TGTP160
!-------------------------------------------------------------------------------  !TSTP  1
      subroutine tessrftrp_M10(khgt, time, tmgcm, pmgcm, dmgcm, umgcm, vmgcm,   & !TSTP  2
         hpres, hdens, ctopohgt, tempday, presday, densday, uwndday, vwndday,   & !TSTP  3
         hpres0, tempmax, tempmin, densmax, densmin, tat5m, mtesy, idaydata)      !TSTP  4
!-----------------------------------------------                                  !TSTP  5
!   M o d u l e s                                                                 !TSTP  6
!-----------------------------------------------                                  !TSTP  7
      USE vast_kind_param, ONLY:  double                                          !TSTP  8
      USE surftes_M10_C                                                           !TSTP  9
      USE mgcmtes_M10_C                                                           !TSTP 10
      USE testerp_M10_C                                                           !TSTP 11
      USE mgcmparm_M10_C                                                          !TSTP 12
      USE parameters_M10_C                                                        !TSTP 13
!-----------------------------------------------                                  !TSTP 14
!---    Interpolates Ames Mars General Circulation Model (MGCM) surface           !TSTP 15
!       data to a given latitude, longitude, time of year (Ls), for a             !TSTP 16
!       given TES year (mtesy), height index (khgt) and time of day               !TSTP 17
!       (time).  Some input data is provided by the Common "Interp".              !TSTP 18
!---    Set parameter values for number of heights, boundary layer                !TSTP 19
!       levels, latitudes, longitudes                                             !TSTP 20
!...                                                                              !TSTP 21
!...Switches:                                                                     !TSTP 22
!-----------------------------------------------                                  !TSTP 23
!   I n t e r f a c e   B l o c k s                                               !TSTP 24
!-----------------------------------------------                                  !TSTP 25
      use tidex_M10_I                                                             !TSTP 26
      use threed_M10_I                                                            !TSTP 27
      use tidey_M10_I                                                             !TSTP 28
      use twod_M10_I                                                              !TSTP 29
      use zlogr_M10_I                                                             !TSTP 30
      implicit none                                                               !TSTP 31
!-----------------------------------------------                                  !TSTP 32
!   D u m m y   A r g u m e n t s                                                 !TSTP 33
!-----------------------------------------------                                  !TSTP 34
      integer , intent(in) :: khgt                                                !TSTP 35
      integer , intent(in) :: mtesy                                               !TSTP 36
      integer , intent(in) :: idaydata                                            !TSTP 37
      real(double) , intent(in)  :: time                                          !TSTP 38
      real(double) , intent(out)  :: tmgcm                                        !TSTP 39
      real(double) , intent(out) :: pmgcm                                         !TSTP 40
      real(double) , intent(out) :: dmgcm                                         !TSTP 41
      real(double) , intent(out)  :: umgcm                                        !TSTP 42
      real(double) , intent(out)  :: vmgcm                                        !TSTP 43
      real(double) , intent(out) :: hpres                                         !TSTP 44
      real(double) , intent(out) :: hdens                                         !TSTP 45
      real(double) , intent(in) :: ctopohgt                                       !TSTP 46
      real(double) , intent(out)  :: tempday                                      !TSTP 47
      real(double) , intent(out) :: presday                                       !TSTP 48
      real(double) , intent(out) :: densday                                       !TSTP 49
      real(double) , intent(out)  :: uwndday                                      !TSTP 50
      real(double) , intent(out)  :: vwndday                                      !TSTP 51
      real(double) , intent(out) :: hpres0                                        !TSTP 52
      real(double) , intent(out)  :: tempmax                                      !TSTP 53
      real(double) , intent(out)  :: tempmin                                      !TSTP 54
      real(double) , intent(out) :: densmax                                       !TSTP 55
      real(double) , intent(out) :: densmin                                       !TSTP 56
      real(double) , intent(inout) :: tat5m                                       !TSTP 57
!-----------------------------------------------                                  !TSTP 58
!   L o c a l   V a r i a b l e s                                                 !TSTP 59
!-----------------------------------------------                                  !TSTP 60
      integer :: i, j, l, itime, k1h                                              !TSTP 61
      real(double), dimension(2,2,2) :: tm, um, vm, ts0                           !TSTP 62
      real(double), dimension(2,2) :: tz1, dz1, rz0, pzh, pzh1, tz0, dz0, dzh, &  !TSTP 63
         dzh1                                                                     !TSTP 64
      real(double), dimension(2,2,2) :: tday, uday, vday, tmax, tmin              !TSTP 65
      real(double), dimension(2,2) :: dmax, dmin, t1max, t1min, pz0               !TSTP 66
      real(double) :: polefac, upolefac, t0, a1t, p1t, a2t, p2t, xtime, ttime, &  !TSTP 67
         u0, a1, p1, a2, p2, v0, tszero, d0, dtime, t1time, p0, pzk1h, pzk1h1, &  !TSTP 68
         dz1h, dzk1h, dzk1h1, hdens0, tzero, dzero, pzero, tbar0, rzero, t1st, &  !TSTP 69
         d1st, d1max, d1min, tmax1, tmin1, p1st, tbar, height, z1st, hdmin,    &  !TSTP 70
         hdmax                                                                    !TSTP 71
!-----------------------------------------------                                  !TSTP 72
!---    MGCM surface data arrays                                                  !TSTP 73
!---    MGCM -5 to 80 km data arrays                                              !TSTP 74
!---    Establish MGCM surface values at corners of a 3-dimensional               !TSTP 75
!       cube in latitude-longitude-Ls space, at a given height                    !TSTP 76
!       index (khgt) and time of day (time)                                       !TSTP 77
      do i = 1, 2                                                                 !TSTP 78
         polefac = 1.0D0                                                          !TSTP 79
         upolefac = 1.0D0                                                         !TSTP 80
         if (ilat == 1) then                                                      !TSTP 81
            polefac = i - 1.0D0                                                   !TSTP 82
         else if (ilat == ntlat - 1) then                                         !TSTP 83
            polefac = 2.0D0 - i                                                   !TSTP 84
         endif                                                                    !TSTP 85
         if (ilatw == 2) then                                                     !TSTP 86
            if (i == 1) upolefac = wpolefac                                       !TSTP 87
         else if (ilatw == ntlat - 1) then                                        !TSTP 88
            if (i == 2) upolefac = wpolefac                                       !TSTP 89
         endif                                                                    !TSTP 90
         do j = 1, 2                                                              !TSTP 91
            do l = 1, 2                                                           !TSTP 92
!---      Daily mean temperature at level khgt                                    !TSTP 93
               t0 = ttsa0(khgt,ilat+i-1,jlon+j-1,ls+l-1,mtesy)                    !TSTP 94
               tday(i,j,l) = t0                                                   !TSTP 95
!---      Temperature tide amplitudes and phases                                  !TSTP 96
               a1t = ttsa1(khgt,ilat+i-1,jlon+j-1,ls+l-1,mtesy)*polefac           !TSTP 97
               p1t = ttsp1(khgt,ilat+i-1,jlon+j-1,ls+l-1,mtesy)                   !TSTP 98
               a2t = ttsa2(khgt,ilat+i-1,jlon+j-1,ls+l-1,mtesy)*polefac           !TSTP 99
               p2t = ttsp2(khgt,ilat+i-1,jlon+j-1,ls+l-1,mtesy)                   !TSTP100
!---      Temperature at corners of 3-D cube                                      !TSTP101
               tm(i,j,l) = tidex_M10(t0,a1t,p1t,a2t,p2t,time)                     !TSTP102
!---      Daily mean temperature at surface                                       !TSTP103
               ts0(i,j,l) = ttsa0(1,ilat+i-1,jlon+j-1,ls+l-1,mtesy)               !TSTP104
!---      Max and Min temperatures at corners of 3-D cube                         !TSTP105
               tmax(i,j,l) = -9999.0D0                                            !TSTP106
               tmin(i,j,l) = 9999.0D0                                             !TSTP107
               if (idaydata > 0) then                                             !TSTP108
                  do itime = 0, 23                                                !TSTP109
                     xtime = float(itime)                                         !TSTP110
                     ttime = tidex_M10(t0,a1t,p1t,a2t,p2t,xtime)                  !TSTP111
                     tmax(i,j,l) = dmax1(ttime,tmax(i,j,l))                       !TSTP112
                     tmin(i,j,l) = min(ttime,tmin(i,j,l))                         !TSTP113
                  end do                                                          !TSTP114
               endif                                                              !TSTP115
!---      Daily mean EW wind at level khgt                                        !TSTP116
               u0 = tusa0(khgt,ilat+i-1,jlonw+j-1,ls+l-1,mtesy)                   !TSTP117
               uday(i,j,l) = u0                                                   !TSTP118
!---      EW wind tide coefficient amplitudes and phases                          !TSTP119
               a1 = tusa1(khgt,ilat+i-1,jlonw+j-1,ls+l-1,mtesy)*upolefac          !TSTP120
               p1 = tusp1(khgt,ilat+i-1,jlonw+j-1,ls+l-1,mtesy)                   !TSTP121
               a2 = tusa2(khgt,ilat+i-1,jlonw+j-1,ls+l-1,mtesy)*upolefac          !TSTP122
               p2 = tusp2(khgt,ilat+i-1,jlonw+j-1,ls+l-1,mtesy)                   !TSTP123
!---      EW wind at corners of 3-D cube                                          !TSTP124
               um(i,j,l) = tidex_M10(u0,a1,p1,a2,p2,time)                         !TSTP125
!---      Daily mean NS wind at level khgt                                        !TSTP126
               v0 = tvsa0(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mtesy)                   !TSTP127
               vday(i,j,l) = v0                                                   !TSTP128
!---      NS wind coefficient amplitudes and phases                               !TSTP129
               a1 = tvsa1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mtesy)*upolefac          !TSTP130
               p1 = tvsp1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mtesy)                   !TSTP131
               a2 = tvsa2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mtesy)*upolefac          !TSTP132
               p2 = tvsp2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mtesy)                   !TSTP133
!---      NS wind at corners of 3-D cube                                          !TSTP134
               vm(i,j,l) = tidex_M10(v0,a1,p1,a2,p2,time)                         !TSTP135
            end do                                                                !TSTP136
         end do                                                                   !TSTP137
      end do                                                                      !TSTP138
!---    Use 3-D interpolation to get temperature, EW wind, NS wind,               !TSTP139
!       and daily mean surface temperature at given latitude,                     !TSTP140
!       longitude, Ls                                                             !TSTP141
      call threed_M10 (dlat, dlon, dls, tm, tmgcm, 0)                             !TSTP142
      call threed_M10 (dlat, dlonw, dls, um, umgcm, 0)                            !TSTP143
      call threed_M10 (dlatw, dlon, dls, vm, vmgcm, 0)                            !TSTP144
      call threed_M10 (dlat, dlon, dls, ts0, tszero, 0)                           !TSTP145
      call threed_M10 (dlat, dlon, dls, tday, tempday, 0)                         !TSTP146
      call threed_M10 (dlat, dlon, dls, tmax, tempmax, 0)                         !TSTP147
      call threed_M10 (dlat, dlon, dls, tmin, tempmin, 0)                         !TSTP148
      call threed_M10 (dlat, dlonw, dls, uday, uwndday, 0)                        !TSTP149
      call threed_M10 (dlatw, dlon, dls, vday, vwndday, 0)                        !TSTP150
!---    k1h = height index just below k1st                                        !TSTP151
      k1h = k1st - 1                                                              !TSTP152
      k1h = max0(1,k1h)                                                           !TSTP153
!---    Establish MGCM values at height levels k1h, k1st and corners of           !TSTP154
!       a 2-dimensional cube in latitude-Ls, at given time of day                 !TSTP155
!       (time)                                                                    !TSTP156
      do i = 1, 2                                                                 !TSTP157
         polefac = 1.0D0                                                          !TSTP158
         if (ilat == 1) then                                                      !TSTP159
            polefac = i - 1.0D0                                                   !TSTP160
         else if (ilat == ntlat - 1) then                                         !TSTP161
            polefac = 2.0D0 - i                                                   !TSTP162
         endif                                                                    !TSTP163
         do l = 1, 2                                                              !TSTP164
!---      Daily average pressure and density at level k1h                         !TSTP165
            pzh(i,l) = tpza0(k1h,ilat+i-1,ls+l-1,mtesy)                           !TSTP166
            dzh(i,l) = tdza0(k1h,ilat+i-1,ls+l-1,mtesy)                           !TSTP167
            pzh1(i,l) = tpza0(k1h+1,ilat+i-1,ls+l-1,mtesy)                        !TSTP168
            dzh1(i,l) = tdza0(k1h+1,ilat+i-1,ls+l-1,mtesy)                        !TSTP169
!---      Density tide coefficient amplitudes and phases                          !TSTP170
            d0 = tdza0(k1st,ilat+i-1,ls+l-1,mtesy)                                !TSTP171
            a1 = tdza1(k1st,ilat+i-1,ls+l-1,mtesy)*polefac                        !TSTP172
            p1 = tdzp1(k1st,ilat+i-1,ls+l-1,mtesy)                                !TSTP173
            a2 = tdza2(k1st,ilat+i-1,ls+l-1,mtesy)*polefac                        !TSTP174
            p2 = tdzp2(k1st,ilat+i-1,ls+l-1,mtesy)                                !TSTP175
!---      Density values at corners of 2-D cube                                   !TSTP176
            dz1(i,l) = tidey_M10(d0,a1,p1,a2,p2,time)                             !TSTP177
!---      Daily average density at level k1st                                     !TSTP178
            dz0(i,l) = d0                                                         !TSTP179
!---      Level k1st density at corners of 2-D cube                               !TSTP180
            dmax(i,l) = -9999.0D0                                                 !TSTP181
            dmin(i,l) = 9999.0D0                                                  !TSTP182
            if (idaydata > 0) then                                                !TSTP183
               do itime = 0, 23                                                   !TSTP184
                  xtime = float(itime)                                            !TSTP185
                  dtime = tidey_M10(d0,a1,p1,a2,p2,xtime)                         !TSTP186
                  dmax(i,l) = dmax1(dtime,dmax(i,l))                              !TSTP187
                  dmin(i,l) = min(dtime,dmin(i,l))                                !TSTP188
               end do                                                             !TSTP189
            endif                                                                 !TSTP190
!---      Temperature tide coefficient amplitudes and phases                      !TSTP191
            t0 = ttza0(k1st,ilat+i-1,ls+l-1,mtesy)                                !TSTP192
            a1 = ttza1(k1st,ilat+i-1,ls+l-1,mtesy)*polefac                        !TSTP193
            p1 = ttzp1(k1st,ilat+i-1,ls+l-1,mtesy)                                !TSTP194
            a2 = ttza2(k1st,ilat+i-1,ls+l-1,mtesy)*polefac                        !TSTP195
            p2 = ttzp2(k1st,ilat+i-1,ls+l-1,mtesy)                                !TSTP196
!---      temperature values at corners of 2-D cube                               !TSTP197
            tz1(i,l) = tidex_M10(t0,a1,p1,a2,p2,time)                             !TSTP198
!---      Level k1st Temperature at corners of 2-D cube                           !TSTP199
            t1max(i,l) = -9999.0D0                                                !TSTP200
            t1min(i,l) = 9999.0D0                                                 !TSTP201
            if (idaydata > 0) then                                                !TSTP202
               do itime = 0, 23                                                   !TSTP203
                  xtime = float(itime)                                            !TSTP204
                  t1time = tidex_M10(t0,a1,p1,a2,p2,xtime)                        !TSTP205
                  t1max(i,l) = dmax1(t1time,t1max(i,l))                           !TSTP206
                  t1min(i,l) = min(t1time,t1min(i,l))                             !TSTP207
               end do                                                             !TSTP208
            endif                                                                 !TSTP209
!---      Daily average temperature at level k1st                                 !TSTP210
            tz0(i,l) = t0                                                         !TSTP211
!---      Daily average pressure at level k1st                                    !TSTP212
            p0 = tpza0(k1st,ilat+i-1,ls+l-1,mtesy)                                !TSTP213
            pz0(i,l) = p0                                                         !TSTP214
!---      Gas constant from pressure, density, and temperature                    !TSTP215
            rz0(i,l) = 190.0D0                                                    !TSTP216
            if (dabs(t0)<=0.0D0 .or. dabs(d0)<=0.0D0) cycle                       !TSTP217
            rz0(i,l) = p0/(t0*d0)                                                 !TSTP218
         end do                                                                   !TSTP219
      end do                                                                      !TSTP220
!---    Do 2-D interpolation on pressure                                          !TSTP221
      call twod_M10 (dlat, dls, pzh, pzk1h)                                       !TSTP222
      call twod_M10 (dlat, dls, pzh1, pzk1h1)                                     !TSTP223
!---    Daily average pressure scale height                                       !TSTP224
      dz1h = 1.0D0                                                                !TSTP225
      if (k1st >= 16) dz1h = 5.0D0                                                !TSTP226
      hpres0 = dz1h/zlogr_M10(pzk1h,pzk1h1,'TSTP-01')                             !TSTP227
!---    Do 2-D interpolation on density                                           !TSTP228
      call twod_M10 (dlat, dls, dzh, dzk1h)                                       !TSTP229
      call twod_M10 (dlat, dls, dzh1, dzk1h1)                                     !TSTP230
!---    Daily average density scale height                                        !TSTP231
      hdens0 = dz1h/zlogr_M10(dzk1h,dzk1h1,'TSTP-02')                             !TSTP232
!---    Do 2-D interpolation on daily mean temperature                            !TSTP233
      call twod_M10 (dlat, dls, tz0, tzero)                                       !TSTP234
      call twod_M10 (dlat, dls, dz0, dzero)                                       !TSTP235
      call twod_M10 (dlat, dls, pz0, pzero)                                       !TSTP236
!---    Daily average layer mean temperature                                      !TSTP237
      tbar0 = (tzero + tszero)/2.0D0                                              !TSTP238
!---    Do 2-D interpolation on gas constant                                      !TSTP239
      call twod_M10 (dlat, dls, rz0, rzero)                                       !TSTP240
!---    Do 2-D interpolation on temperature and density                           !TSTP241
      call twod_M10 (dlat, dls, tz1, t1st)                                        !TSTP242
      call twod_M10 (dlat, dls, dz1, d1st)                                        !TSTP243
!---    Do 2-D interpolation on max,min pressure at level k1st                    !TSTP244
      call twod_M10 (dlat, dls, dmax, d1max)                                      !TSTP245
      call twod_M10 (dlat, dls, dmin, d1min)                                      !TSTP246
!---    Do 2-D interpolation on max,min temperature at level k1st                 !TSTP247
      call twod_M10 (dlat, dls, t1max, tmax1)                                     !TSTP248
      call twod_M10 (dlat, dls, t1min, tmin1)                                     !TSTP249
!---    Pressure from gas law                                                     !TSTP250
      p1st = d1st*rzero*t1st                                                      !TSTP251
!---    Layer mean temperature at current time                                    !TSTP252
      if (khgt==2 .and. tat5m<=0.0D0) tat5m = tmgcm                               !TSTP253
      tbar = (t1st + tat5m)/2.0D0                                                 !TSTP254
!---    Pressure scale height and density scale height at current time            !TSTP255
      hpres = hpres0*tbar/tbar0                                                   !TSTP256
      hdens = hdens0*tbar/tbar0                                                   !TSTP257
!---    Adjust pressure to height level, using pressure scale height              !TSTP258
      height = ctopohgt + dzbl(khgt)                                              !TSTP259
      z1st = (-6.0D0) + k1st                                                      !TSTP260
      if (k1st >= 16) z1st = 10.0D0 + 5.0D0*(k1st - 16.0D0)                       !TSTP261
      pmgcm = p1st*dexp((z1st - height)/hpres)                                    !TSTP262
      presday = pzero*dexp((z1st - height)/hpres0)                                !TSTP263
!---    Compute density from gas law, using pressure and temperature              !TSTP264
      dmgcm = pmgcm/(rzero*tmgcm)                                                 !TSTP265
      densday = presday/(rzero*tempday)                                           !TSTP266
!---    Daily maximum and minimum density                                         !TSTP267
      densmin = 9999.0D0                                                          !TSTP268
      densmax = -9999.0D0                                                         !TSTP269
      if (idaydata > 0) then                                                      !TSTP270
         hdmin = hdens0*0.5D0*(tmax1 + tempmax)/tbar0                             !TSTP271
         hdmax = hdens0*0.5D0*(tmax1 + tempmin)/tbar0                             !TSTP272
         densmax = d1max*dexp((z1st - height)/hdmax)                              !TSTP273
         densmin = d1min*dexp((z1st - height)/hdmin)                              !TSTP274
      endif                                                                       !TSTP275
      return                                                                      !TSTP276
      end subroutine tessrftrp_M10                                                !TSTP277
!                                                                                 !TSTP278
!-------------------------------------------------------------------------------  !TTTP  1
      subroutine testterp_M10(khgtt, time, ttgcm, ptgcm, dtgcm, utgcm, vtgcm,   & !TTTP  2
         zf, tempday, presday, densday, uwndday, vwndday, tempmax, tempmin,     & !TTTP  3
         densmax, densmin, mtesy, idaydata)                                       !TTTP  4
!-----------------------------------------------                                  !TTTP  5
!   M o d u l e s                                                                 !TTTP  6
!-----------------------------------------------                                  !TTTP  7
      USE vast_kind_param, ONLY:  double                                          !TTTP  8
      USE tgcmtes_M10_C                                                           !TTTP  9
      USE testerp_M10_C                                                           !TTTP 10
      USE parameters_M10_C                                                        !TTTP 11
!-----------------------------------------------                                  !TTTP 12
!---    Interpolates University of Michigan Mars Thermospheric General            !TTTP 13
!       Circulation Model (MTGCM) data to a given latitude, time of               !TTTP 14
!       year (Ls), for a given TES year, height index (khgtt) and                 !TTTP 15
!       time of day (time).                                                       !TTTP 16
!       Some input data is provided by the Common "Interp".                       !TTTP 17
!---    Set parameter values for number of heights (nthgtt), number               !TTTP 18
!       of latitudes (nlatt), and number of dust optical depth values             !TTTP 19
!...                                                                              !TTTP 20
!...Switches:                                                                     !TTTP 21
!-----------------------------------------------                                  !TTTP 22
!   I n t e r f a c e   B l o c k s                                               !TTTP 23
!-----------------------------------------------                                  !TTTP 24
      use tidex_M10_I                                                             !TTTP 25
      use ttidey_M10_I                                                            !TTTP 26
      use threed_M10_I                                                            !TTTP 27
      implicit none                                                               !TTTP 28
!-----------------------------------------------                                  !TTTP 29
!   D u m m y   A r g u m e n t s                                                 !TTTP 30
!-----------------------------------------------                                  !TTTP 31
      integer , intent(in) :: khgtt                                               !TTTP 32
      integer , intent(in) :: mtesy                                               !TTTP 33
      integer , intent(in) :: idaydata                                            !TTTP 34
      real(double) , intent(in)  :: time                                          !TTTP 35
      real(double) , intent(out) :: ttgcm                                         !TTTP 36
      real(double) , intent(out) :: ptgcm                                         !TTTP 37
      real(double) , intent(out) :: dtgcm                                         !TTTP 38
      real(double) , intent(out) :: utgcm                                         !TTTP 39
      real(double) , intent(out) :: vtgcm                                         !TTTP 40
      real(double) , intent(out) :: zf                                            !TTTP 41
      real(double) , intent(out) :: tempday                                       !TTTP 42
      real(double) , intent(out) :: presday                                       !TTTP 43
      real(double) , intent(out) :: densday                                       !TTTP 44
      real(double) , intent(out) :: uwndday                                       !TTTP 45
      real(double) , intent(out) :: vwndday                                       !TTTP 46
      real(double) , intent(out) :: tempmax                                       !TTTP 47
      real(double) , intent(out) :: tempmin                                       !TTTP 48
      real(double) , intent(out) :: densmax                                       !TTTP 49
      real(double) , intent(out) :: densmin                                       !TTTP 50
!-----------------------------------------------                                  !TTTP 51
!   L o c a l   V a r i a b l e s                                                 !TTTP 52
!-----------------------------------------------                                  !TTTP 53
      integer :: i, l, n, itime                                                   !TTTP 54
      real(double), dimension(2,2,2) :: tt, pt, ut, vt, r0, dt, zt, tday, pday  & !TTTP 55
         , uday, vday, tmax, tmin, dmax, dmin                                     !TTTP 56
      real(double) :: polefac, t0, a1, p1, a2, p2, xtime, ttime, p0, a1p, p1p,  & !TTTP 57
         a2p, p2p, d0, dtime, u0, v0, z0, rtgcm                                   !TTTP 58
!-----------------------------------------------                                  !TTTP 59
!---    MTGCM 80 to 240  km data arrays for interpolation                         !TTTP 60
!---    Establish MTGCM values at corners of a 3-dimensional cube in              !TTTP 61
!       latitude-Ls-dust-F107 space, at the given height index (khgtt),           !TTTP 62
!       and time of day (time)                                                    !TTTP 63
      do i = 1, 2                                                                 !TTTP 64
         polefac = 1.0D0                                                          !TTTP 65
         if (ilatt == 1) then                                                     !TTTP 66
            if (i == 1) polefac = tpolefac                                        !TTTP 67
         else if (ilatt == nlatt - 1) then                                        !TTTP 68
            if (i == 2) polefac = tpolefac                                        !TTTP 69
         endif                                                                    !TTTP 70
         do l = 1, 2                                                              !TTTP 71
            do n = 1, 2                                                           !TTTP 72
!---      Daily mean temperature                                                  !TTTP 73
               t0 = ttta0(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP 74
               tday(i,l,n) = t0                                                   !TTTP 75
!---      Temperature tide amplitudes and phases                                  !TTTP 76
               a1 = ttta1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP 77
               p1 = tttp1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP 78
               a2 = ttta2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP 79
               p2 = tttp2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP 80
!---      Temperature at corners of 3-D cube                                      !TTTP 81
               tt(i,l,n) = tidex_M10(t0,a1,p1,a2,p2,time)                         !TTTP 82
!---      Max and Min temperatures at corners of 3-D cube                         !TTTP 83
               tmax(i,l,n) = -9999.0D0                                            !TTTP 84
               tmin(i,l,n) = 9999.0D0                                             !TTTP 85
               if (idaydata > 0) then                                             !TTTP 86
                  do itime = 0, 23                                                !TTTP 87
                     xtime = float(itime)                                         !TTTP 88
                     ttime = tidex_M10(t0,a1,p1,a2,p2,xtime)                      !TTTP 89
                     tmax(i,l,n) = dmax1(ttime,tmax(i,l,n))                       !TTTP 90
                     tmin(i,l,n) = min(ttime,tmin(i,l,n))                         !TTTP 91
                  end do                                                          !TTTP 92
               endif                                                              !TTTP 93
!---      Daily mean pressure                                                     !TTTP 94
               p0 = tpta0(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP 95
               pday(i,l,n) = p0                                                   !TTTP 96
!---      Pressure tide amplitudes and phases                                     !TTTP 97
               a1p = tpta1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac         !TTTP 98
               p1p = tptp1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                 !TTTP 99
               a2p = tpta2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac         !TTTP100
               p2p = tptp2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                 !TTTP101
!---      Pressure at corners of 3-D cube                                         !TTTP102
               pt(i,l,n) = ttidey_M10(p0,a1p,p1p,a2p,p2p,time)                    !TTTP103
!---      Daily mean density                                                      !TTTP104
               d0 = tdta0(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP105
!---      Density tide amplitudes and phases                                      !TTTP106
               a1 = tdta1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP107
               p1 = tdtp1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP108
               a2 = tdta2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP109
               p2 = tdtp2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP110
!---      Density at corners of 3-D cube                                          !TTTP111
               dt(i,l,n) = ttidey_M10(d0,a1,p1,a2,p2,time)                        !TTTP112
!---      Max and Min densities at corners of 3-D cube                            !TTTP113
               dmax(i,l,n) = -9999.0D0                                            !TTTP114
               dmin(i,l,n) = 9999.0D0                                             !TTTP115
               if (idaydata > 0) then                                             !TTTP116
                  do itime = 0, 23                                                !TTTP117
                     xtime = float(itime)                                         !TTTP118
                     dtime = ttidey_M10(d0,a1,p1,a2,p2,xtime)                     !TTTP119
                     dmax(i,l,n) = dmax1(dtime,dmax(i,l,n))                       !TTTP120
                     dmin(i,l,n) = min(dtime,dmin(i,l,n))                         !TTTP121
                  end do                                                          !TTTP122
               endif                                                              !TTTP123
!---      Gas constant from pressure, density, and temperature                    !TTTP124
               r0(i,l,n) = 190.0D0                                                !TTTP125
               if (dabs(t0)>0.0D0 .and. dabs(d0)>0.0D0) r0(i,l,n) = p0/(t0*d0)    !TTTP126
!---      Daily mean EW wind                                                      !TTTP127
               u0 = tuta0(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP128
               uday(i,l,n) = u0                                                   !TTTP129
!---      EW wind tide amplitudes and phases                                      !TTTP130
               a1 = tuta1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP131
               p1 = tutp1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP132
               a2 = tuta2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP133
               p2 = tutp2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP134
!---      EW wind at corners of 3-D cube                                          !TTTP135
               ut(i,l,n) = tidex_M10(u0,a1,p1,a2,p2,time)                         !TTTP136
!---      Daily mean NS wind                                                      !TTTP137
               v0 = tvta0(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP138
               vday(i,l,n) = v0                                                   !TTTP139
!---      NS wind tide amplitudes and phases                                      !TTTP140
               a1 = tvta1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP141
               p1 = tvtp1(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP142
               a2 = tvta2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac          !TTTP143
               p2 = tvtp2(khgtt,ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                  !TTTP144
!---      NS wind at corners of 3-D cube                                          !TTTP145
               vt(i,l,n) = tidex_M10(v0,a1,p1,a2,p2,time)                         !TTTP146
!---      Tide amplitudes and phases for ZF=height of 1.26 nbar level             !TTTP147
               z0 = tzfa0(ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                        !TTTP148
               a1 = tzfa1(ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac                !TTTP149
               p1 = tzfp1(ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                        !TTTP150
               a2 = tzfa2(ilatt+i-1,ls+l-1,mtesy,mf10+n-1)*polefac                !TTTP151
               p2 = tzfp2(ilatt+i-1,ls+l-1,mtesy,mf10+n-1)                        !TTTP152
!---      ZF values at corners of 3-D cube                                        !TTTP153
               zt(i,l,n) = tidex_M10(z0,a1,p1,a2,p2,time)                         !TTTP154
            end do                                                                !TTTP155
         end do                                                                   !TTTP156
      end do                                                                      !TTTP157
!---    Use 3-D interpolation to get temperature, pressure, density,              !TTTP158
!       gas constant, EW wind, NS wind, and ZF height at given lati-              !TTTP159
!       tude, Ls, dust optical depth, and solar activity                          !TTTP160
      call threed_M10 (dlatt, dls, df10, tt, ttgcm, 0)                            !TTTP161
      call threed_M10 (dlatt, dls, df10, tday, tempday, 0)                        !TTTP162
      call threed_M10 (dlatt, dls, df10, tmax, tempmax, 0)                        !TTTP163
      call threed_M10 (dlatt, dls, df10, tmin, tempmin, 0)                        !TTTP164
      if (idaydata == 1) then                                                     !TTTP165
         call threed_M10 (dlatt, dls, df10, dmax, densmax, 1)                     !TTTP166
      else                                                                        !TTTP167
         call threed_M10 (dlatt, dls, df10, dmax, densmax, 0)                     !TTTP168
      endif                                                                       !TTTP169
      call threed_M10 (dlatt, dls, df10, dmin, densmin, 1)                        !TTTP170
      call threed_M10 (dlatt, dls, df10, pt, ptgcm, 1)                            !TTTP171
      call threed_M10 (dlatt, dls, df10, pday, presday, 1)                        !TTTP172
      call threed_M10 (dlatt, dls, df10, dt, dtgcm, 1)                            !TTTP173
      call threed_M10 (dlatt, dls, df10, r0, rtgcm, 0)                            !TTTP174
!---    Daily density from gas constant                                           !TTTP175
      densday = presday/(rtgcm*tempday)                                           !TTTP176
      call threed_M10 (dlatt, dls, df10, ut, utgcm, 0)                            !TTTP177
      call threed_M10 (dlatt, dls, df10, uday, uwndday, 0)                        !TTTP178
      call threed_M10 (dlatt, dls, df10, vt, vtgcm, 0)                            !TTTP179
      call threed_M10 (dlatt, dls, df10, vday, vwndday, 0)                        !TTTP180
      call threed_M10 (dlatt, dls, df10, zt, zf, 0)                               !TTTP181
      return                                                                      !TTTP182
      end subroutine testterp_M10                                                 !TTTP183
!                                                                                 !TTTP184
!-------------------------------------------------------------------------------  !TTDY  1
      real(kind(0.0d0)) function ttidey_M10 (a0, a1, phi1, a2, phi2, t)           !TTDY  2
!-----------------------------------------------                                  !TTDY  3
!   M o d u l e s                                                                 !TTDY  4
!-----------------------------------------------                                  !TTDY  5
      USE vast_kind_param, ONLY:  double                                          !TTDY  6
!...                                                                              !TTDY  7
!...Switches:                                                                     !TTDY  8
      implicit none                                                               !TTDY  9
!-----------------------------------------------                                  !TTDY 10
!   D u m m y   A r g u m e n t s                                                 !TTDY 11
!-----------------------------------------------                                  !TTDY 12
      real(double) , intent(in) :: a0                                             !TTDY 13
      real(double) , intent(in) :: a1                                             !TTDY 14
      real(double) , intent(in) :: phi1                                           !TTDY 15
      real(double) , intent(in) :: a2                                             !TTDY 16
      real(double) , intent(in) :: phi2                                           !TTDY 17
      real(double) , intent(in) :: t                                              !TTDY 18
!-----------------------------------------------                                  !TTDY 19
!   L o c a l   V a r i a b l e s                                                 !TTDY 20
!-----------------------------------------------                                  !TTDY 21
      real(double) :: pi                                                          !TTDY 22
!-----------------------------------------------                                  !TTDY 23
!---    Tide value at local solar time t, from mean value A0, amplitude           !TTDY 24
!       A1 and phase phi1 of 24-hour period component, and amplitude A2           !TTDY 25
!       and phase phi2 of 12-hour period component.  Amplitudes A1 and            !TTDY 26
!       A2 are in relative units (% of mean term A0).  Phases are in              !TTDY 27
!       hours of local solar time.                                                !TTDY 28
!---    This form, based on cosine variation of log of tide, allows               !TTDY 29
!       amplitudes to exceed 100% without tide going negative (as                 !TTDY 30
!       required for temperature, density, and pressure).  This form is           !TTDY 31
!       used for new, higher-altitude MTGCM data, where tidal ampitudes           !TTDY 32
!       are more likely to get large.                                             !TTDY 33
      pi = 4.0D0*datan(1.0D0)                                                     !TTDY 34
      ttidey_M10 = a0*(1.0D0 + 0.01D0*a1)**dcos(pi*(t - phi1)/12.0D0)*(1.0D0 +  & !TTDY 35
         0.01D0*a2)**dcos(pi*(t - phi2)/6.0D0)                                    !TTDY 36
      return                                                                      !TTDY 37
      end function ttidey_M10                                                     !TTDY 38
!                                                                                 !TTDY 39
!-------------------------------------------------------------------------------  !TESG  1
      subroutine tesgcm_M10(chgt, clat, clonw, als, time, ctemp, cpres, cdens,  & !TESG  2
         cuwin, cvwin, blwindew, blwindns, blwindvert, hpres, hdens, zf,        & !TESG  3
         pertfact, ctopohgt, hgtasfc, careoid, tempday, presday, densday,       & !TESG  4
         ewwnday, nswnday, bluday, blvday, tempmax, tempmin, densmax, densmin,  & !TESG  5
         tgrnd, calbedo, icepolar, tat5m, requa, rpole, mapyear, idaydata)        !TESG  6
!-----------------------------------------------                                  !TESG  7
!   M o d u l e s                                                                 !TESG  8
!-----------------------------------------------                                  !TESG  9
      USE vast_kind_param, ONLY:  double                                          !TESG 10
      USE tgcmoffset_M10_C                                                        !TESG 11
      USE therm_M10_C                                                             !TESG 12
      USE mgcmparm_M10_C                                                          !TESG 13
      USE testerp_M10_C                                                           !TESG 14
      USE parameters_M10_C                                                        !TESG 15
!-----------------------------------------------                                  !TESG 16
!---    Uses interpolation routines to evaluate:                                  !TESG 17
!                                                                                 !TESG 18
!       ctemp    = temperature (K) at current position                            !TESG 19
!       cpres    = pressure (N/m**2) at current position                          !TESG 20
!       cdens    = density (kg/m**3) at current position                          !TESG 21
!       cuwin    = eastward wind component (m/s) at current position              !TESG 22
!       cvwin    = northward wind component (m/s) at current position             !TESG 23
!       blwinew  = eastward b.l. slope wind (m/s)                                 !TESG 24
!       blwinns  = northward b.l. slope wind (m/s)                                !TESG 25
!       Hpres    = pressure scale height (km) at current position                 !TESG 26
!       Hdens    = density scale height (km) at current position                  !TESG 27
!       ZF       = height of 1.26 nbar level at current position                  !TESG 28
!       pertfact = perturbation factor from random perturbation model             !TESG 29
!       ctopohgt = topographic height (km) at current position                    !TESG 30
!       careoid  = local radius (km) of MOLA 1/2 degree areoid                    !TESG 31
!       TempDay  = Local daily average temperature (K)                            !TESG 32
!       PresDay  = Local daily average pressure (N/m**2)                          !TESG 33
!       DensDay  = Local daily average density (kg/m**3)                          !TESG 34
!       EWwnDay  = Local daily average Eastward wind (m/s)                        !TESG 35
!       NSwnDay  = Local daily average Northward wind (m/s)                       !TESG 36
!       Tempmax  = Local daily maximum temperature (K)                            !TESG 37
!       Tempmin  = Local daily minimum temperature (K)                            !TESG 38
!       Densmax  = Local daily maximum density (kg/m**3)                          !TESG 39
!       Densmin  = Local daily minimum density (kg/m**3)                          !TESG 40
!       Tgrnd    = ground surface temperature (K)                                 !TESG 41
!       calbedo  = surface albedo                                                 !TESG 42
!       icepolar = polar ice indicator (0=no; 1=yes)                              !TESG 43
!                                                                                 !TESG 44
!       at the current height (chgt), latitude (clat), current (West)             !TESG 45
!       longitude (clonw), for time of year given by Ls=als, and time             !TESG 46
!       of day (time).  Interpolation is done using either boundary               !TESG 47
!       layer or -5 to 80 km data from Ames Mars General Circulation              !TESG 48
!       model (MGCM) or from 80 to 240 km data from the University of             !TESG 49
!       Michigan Mars Thermospheric General Circulation Model (MTGCM).            !TESG 50
!                                                                                 !TESG 51
!---    Set parameter values for number of MGCM heights (tnhgt), number           !TESG 52
!       of MTGCM heights (nthgtt), number of MGCM boundary layer levels           !TESG 53
!       (nbl), number of MGCM latitudes (nlat), number of MTGCM lati-             !TESG 54
!       tudes (nlatt), number of MGCM longitudes (nlon), and minimum              !TESG 55
!       perturbation magnitude at surface (pert0)                                 !TESG 56
!...                                                                              !TESG 57
!...Switches:                                                                     !TESG 58
!-----------------------------------------------                                  !TESG 59
!   I n t e r f a c e   B l o c k s                                               !TESG 60
!-----------------------------------------------                                  !TESG 61
      use ifloor_M10_I                                                            !TESG 62
      use zlogr_M10_I                                                             !TESG 63
      use rellips_M10_I                                                           !TESG 64
      use testterp_M10_I                                                          !TESG 65
      use tesgterp_M10_I                                                          !TESG 66
      use tessrftrp_M10_I                                                         !TESG 67
      use subltchk_M10_I                                                          !TESG 68
      use cp_M10_I                                                                !TESG 69
      use bltp_M10_I                                                              !TESG 70
      use slopewind_M10_I                                                         !TESG 71
      implicit none                                                               !TESG 72
!-----------------------------------------------                                  !TESG 73
!   D u m m y   A r g u m e n t s                                                 !TESG 74
!-----------------------------------------------                                  !TESG 75
      integer , intent(out) :: icepolar                                           !TESG 76
      integer , intent(in)  :: mapyear                                            !TESG 77
      integer , intent(in)  :: idaydata                                           !TESG 78
      real(double) , intent(inout)  :: chgt                                       !TESG 79
      real(double) , intent(in)  :: clat                                          !TESG 80
      real(double) , intent(in) :: clonw                                          !TESG 81
      real(double) , intent(in) :: als                                            !TESG 82
      real(double) , intent(in) :: time                                           !TESG 83
      real(double) , intent(out)  :: ctemp                                        !TESG 84
      real(double) , intent(out) :: cpres                                         !TESG 85
      real(double) , intent(out) :: cdens                                         !TESG 86
      real(double) , intent(out)  :: cuwin                                        !TESG 87
      real(double) , intent(out)  :: cvwin                                        !TESG 88
      real(double) , intent(out)  :: blwindew                                     !TESG 89
      real(double) , intent(out)  :: blwindns                                     !TESG 90
      real(double) , intent(out)  :: blwindvert                                   !TESG 91
      real(double) , intent(out)  :: hpres                                        !TESG 92
      real(double) , intent(out)  :: hdens                                        !TESG 93
      real(double) , intent(out)  :: zf                                           !TESG 94
      real(double) , intent(out) :: pertfact                                      !TESG 95
      real(double) , intent(inout) :: ctopohgt                                    !TESG 96
      real(double) , intent(in) :: hgtasfc                                        !TESG 97
      real(double) , intent(inout) :: careoid                                     !TESG 98
      real(double) , intent(out)  :: tempday                                      !TESG 99
      real(double) , intent(out)  :: presday                                      !TESG100
      real(double) , intent(out) :: densday                                       !TESG101
      real(double) , intent(out) :: ewwnday                                       !TESG102
      real(double) , intent(out) :: nswnday                                       !TESG103
      real(double) , intent(out) :: bluday                                        !TESG104
      real(double) , intent(out) :: blvday                                        !TESG105
      real(double) , intent(out)  :: tempmax                                      !TESG106
      real(double) , intent(out)  :: tempmin                                      !TESG107
      real(double) , intent(out) :: densmax                                       !TESG108
      real(double) , intent(out) :: densmin                                       !TESG109
      real(double) , intent(out) :: tgrnd                                         !TESG110
      real(double) , intent(inout) :: calbedo                                     !TESG111
      real(double) , intent(out)  :: tat5m                                        !TESG112
      real(double) , intent(in)  :: requa                                         !TESG113
      real(double) , intent(in)  :: rpole                                         !TESG114
!-----------------------------------------------                                  !TESG115
!   L o c a l   P a r a m e t e r s                                               !TESG116
!-----------------------------------------------                                  !TESG117
      real(double), parameter :: pert0 = 0.02D0                                   !TESG118
!-----------------------------------------------                                  !TESG120
!   L o c a l   V a r i a b l e s                                                 !TESG121
!-----------------------------------------------                                  !TESG122
      integer :: khgt, khgtt, lhgtt, jbl, itime                                   !TESG123
      real(double) ::  steplat, tsteplat, tlat1st, steplon, gz, oldrref ,       & !TESG124
         hgtk1, globoffst, curoffset, offset1, offset2, tmgcm1,                 & !TESG125
         pmgcm1, dmgcm1, umgcm1, vmgcm1, tday1, pday1, dday1, uday1, vday1,     & !TESG126
         tmax1, tmin1, dmaxa, dmin1, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2,    & !TESG127
         tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2, dmin2, hdensc  & !TESG128
         , hdensdayc, z1, z2, hpresday, zf80, z1ofs, ofsmgcm, ofsz1, tmgcmx,    & !TESG129
         pmgcmx, dmgcmx, umgcmx, vmgcmx, tdayx, pdayx, ddayx, udayx, vdayx,     & !TESG130
         tmaxx, tminx, dmaxx, dminx, hden12, ofsmult1, hpres1, hdens1, z2x,     & !TESG131
         dtdz, tbar, z2ofs, ofsmult2, ofsz2, rgas, rgasday, dhgt, r1, r2, rgas1 & !TESG132
         , rgas2, z0, tcheck, tsubl, uhgt, factor, z5, zeval, rref, topohgt,    & !TESG133
         albedo, cpoft, polefac, blu, blv, blw                                    !TESG134
!-----------------------------------------------                                  !TESG135
      pertfact = 0.0D0                                                            !TESG137
!---    Initialize ground surface temperature and polar ice indicator             !TESG138
      tgrnd = 999.9D0                                                             !TESG139
      icepolar = 99                                                               !TESG140
!---    Insure latitude, longitude, Ls, and time of day within proper             !TESG141
!       bounds                                                                    !TESG142
      if (dabs(clat) > 90.0D0) stop ' Latitude error in TESGCM_M10'               !TESG143
      if (dabs(clonw) > 360.0D0) stop ' Longitude error: TESGCM_M10'              !TESG144
      if (als<0.0D0 .or. als>360.0D0) stop ' Ls error: TESGCM_M10'                !TESG145
      if (time<0.0D0 .or. time>24.0D0) stop ' time error in TESGCM_M10'           !TESG146
!---    Latitude step size for MGCM and MTGCM data                                !TESG147
      steplat = 180.0D0/(nlat - 1.0D0)                                            !TESG148
      tsteplat = 180.0D0/nlatt                                                    !TESG149
!---    Most southerly MTGCM latitude                                             !TESG150
      tlat1st = (-90.0D0) + tsteplat/2.0D0                                        !TESG151
!---    Longitude step size for MGCM boundary layer data                          !TESG152
      steplon = 360.0D0/nlon                                                      !TESG153
!---    MGCM height index (khgt) for current height (chgt)                        !TESG154
      khgt = ifloor_M10(chgt + 6.0D0)                                             !TESG155
      if (khgt > 16) khgt = 16 + ifloor_M10((chgt - 10.0D0)/5.0D0)                !TESG156
!---    Insure khgt within proper limits                                          !TESG157
      khgt = max0(1,khgt)                                                         !TESG158
      khgt = min0(nthgt - 1,khgt)                                                 !TESG159
!---    MGCM latitude index (ilat) from current latitude (clat)                   !TESG160
      ilat = 1 + ifloor_M10((clat + 90.0D0)/steplat)                              !TESG161
      ilat = min0(nlat - 1,ilat)                                                  !TESG162
!---    MGCM wind latitude index (ilatw).  MGCM V winds are offset in             !TESG163
!       lat by 1/2 lat grid step.  No lat offset for U (Arakawa C-grid)           !TESG164
      ilatw = ifloor_M10((clat + 90.0D0 + 1.5D0*steplat)/steplat)                 !TESG165
!---    Insure ilatw within proper bounds                                         !TESG166
      ilatw = max0(2,ilatw)                                                       !TESG167
      ilatw = min0(nlat - 1,ilatw)                                                !TESG168
!---    MTGCM latitude index (ilatt) from current latitude (clat)                 !TESG169
      ilatt = 1 + ifloor_M10((clat - tlat1st)/tsteplat)                           !TESG170
!---    Insure ilatt within proper bounds                                         !TESG171
      ilatt = max0(1,ilatt)                                                       !TESG172
      ilatt = min0(nlatt - 1,ilatt)                                               !TESG173
!---    MGCM boundary layer longitude index (jlon)                                !TESG174
      jlon = ifloor_M10(clonw/steplon)                                            !TESG175
      jlon = min0(nlon - 1,jlon)                                                  !TESG176
!---    Lon offset for C-GRID (1/2 step Eastward for U component)                 !TESG177
      jlonw = ifloor_M10((clonw + steplon/2.0D0)/steplon)                         !TESG178
      if (jlonw > nlon - 1) jlonw = 0                                             !TESG179
!---    Time of year index (ls) from input Ls value (als)                         !TESG180
      ls = ifloor_M10(als/30.0D0)                                                 !TESG181
      ls = min0(11,ls)                                                            !TESG182
!---    Increment of MGCM latitude (dlat) from grid point                         !TESG183
      dlat = (clat - steplat*(ilat - 1.0D0) + 90.0D0)/steplat                     !TESG184
!---    Increment of MTGCM latitude (dlatt) from grid point                       !TESG185
      dlatt = (clat - tsteplat*(ilatt - 1.0D0) - tlat1st)/tsteplat                !TESG186
!---    Insure dlatt within proper bounds near poles                              !TESG187
      tpolefac = 1.0D0                                                            !TESG188
      if (ilatt == 1) then                                                        !TESG189
         tpolefac = 0.5D0                                                         !TESG190
         if (dlatt <= 0.0D0) then                                                 !TESG191
            dlatt = 0.0D0                                                         !TESG192
            tpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !TESG193
         endif                                                                    !TESG194
      else if (ilatt >= nlat - 1) then                                            !TESG195
         tpolefac = 0.5D0                                                         !TESG196
         if (dlatt >= 1.0D0) then                                                 !TESG197
            dlatt = 1.0D0                                                         !TESG198
            tpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !TESG199
         endif                                                                    !TESG200
      endif                                                                       !TESG201
!---    Increment of MGCM longitude (dlon) from grid point                        !TESG202
      dlon = (clonw - steplon*jlon)/steplon                                       !TESG203
      dlonw = (clonw - steplon*(jlonw - 0.5D0))/steplon                           !TESG204
      if (dlonw > 40.0D0) dlonw = dlonw - 40.0D0                                  !TESG205
!---    Increment of MGCM latitude from (offset) wind grid point                  !TESG206
      dlatw = (clat - steplat*(ilatw - 2.0D0) + 86.25D0)/steplat                  !TESG207
      wpolefac = 1.0D0                                                            !TESG208
      if (ilatw == 2) then                                                        !TESG209
         wpolefac = 0.75D0                                                        !TESG210
         if (dlatw <= 0.0D0) then                                                 !TESG211
            wpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !TESG212
            dlatw = 0.0D0                                                         !TESG213
         endif                                                                    !TESG214
      else if (ilatw >= nlat - 1) then                                            !TESG215
         wpolefac = 0.75D0                                                        !TESG216
         if (dlatw >= 1.0D0) then                                                 !TESG217
            wpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !TESG218
            dlatw = 1.0D0                                                         !TESG219
         endif                                                                    !TESG220
      endif                                                                       !TESG221
!---    Increment of solar activity (F10.7 at 1AU) for MTGCM data                 !TESG222
      mf10 = 1                                                                    !TESG223
      if (f107 > f10tes(2)) mf10 = 2                                              !TESG224
      df10 = zlogr_M10(f107,f10tes(mf10),'TESG-01')/zlogr_M10(f10tes(mf10+1),   & !TESG225
         f10tes(mf10),'TESG-02')                                                  !TESG226
!---    Get areoid radius and topographic height at current lat, lon              !TESG227
      call rellips_M10 (clat, clonw, careoid, chgt, gz, oldrref, ctopohgt,      & !TESG228
         calbedo, requa, rpole)                                                   !TESG229
!---    Use topographic height if input height is <= -8.7 km                      !TESG233
      if (chgt <= (-8.7D0)) chgt = ctopohgt + hgtasfc                             !TESG234
!---    Find height index (k1st) of first -5 to 80 km MGCM level above            !TESG235
!       surface topographic height                                                !TESG236
      k1st = ifloor_M10(7.0D0 + ctopohgt + 0.3D0)                                 !TESG237
      if (k1st >= 16) k1st = ifloor_M10(15.0D0 + (ctopohgt + 1.0D0)/5.0D0)        !TESG238
      k1st = max0(1,k1st)                                                         !TESG239
      hgtk1 = (-6.0D0) + k1st                                                     !TESG240
      if (k1st >= 16) hgtk1 = 10.0D0 + 5.0D0*(k1st - 16.0D0)                      !TESG241
!---    Find Ls increment (dls) from Ls "grid" on input data                      !TESG242
      dls = (als - 30.0D0*ls)/30.0D0                                              !TESG243
!---    Initialize ZF = height of 1.26 nbar level (output value if                !TESG244
!       current height < 80 km)                                                   !TESG245
      zf = 999.0D0                                                                !TESG246
!---    Assign MTGCM height offset from input zoffset or array offsets            !TESG247
      globoffst = 0.0D0                                                           !TESG248
      curoffset = 0.0D0                                                           !TESG249
      if (ibougher == 2) then                                                     !TESG250
         offset1 = toffsets(ls,mapyear)                                           !TESG251
         offset2 = toffsets(ls+1,mapyear)                                         !TESG252
         globoffst = offset1 + (offset2 - offset1)*dls                            !TESG253
      else                                                                        !TESG254
         call testterp_M10 (1, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1, zf & !TESG255
            , tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1,    & !TESG256
            mapyear, idaydata)                                                    !TESG257
         call testterp_M10 (2, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2, zf & !TESG258
            , tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2, dmin2,    & !TESG259
            mapyear, idaydata)                                                    !TESG260
         hdensc = 5.0D0/zlogr_M10(dmgcm1,dmgcm2,'TESG-03')                        !TESG261
         hdensdayc = 5.0D0/zlogr_M10(dday1,dday2,'TESG-04')                       !TESG262
         call tesgterp_M10 (nthgt, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2 & !TESG263
            , tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2, dmin2,    & !TESG264
            mapyear, idaydata)                                                    !TESG265
         if (ibougher==3 .or. ibougher<2) then                                    !TESG266
            curoffset = hdensdayc*zlogr_M10(dday2,dday1,'TESG-05')                !TESG267
         else                                                                     !TESG268
            curoffset = hdensc*zlogr_M10(dmgcm2,dmgcm1,'TESG-06')                 !TESG269
         endif                                                                    !TESG270
      endif                                                                       !TESG271
      select case (ibougher)                                                      !TESG272
      case (:0)                                                                   !TESG273
         hgtoffset = zoffset                                                      !TESG274
      case (1)                                                                    !TESG275
         hgtoffset = zoffset + 1.2D0*dsin(2.0D0*datan(1.0D0)*als/45.0D0)          !TESG276
      case (2)                                                                    !TESG277
         hgtoffset = globoffst                                                    !TESG278
      case default                                                                !TESG279
         hgtoffset = curoffset                                                    !TESG280
      end select                                                                  !TESG281
!---    MTGCM height index (khgtt) for current height                             !TESG282
      khgtt = ifloor_M10((chgt - hgtoffset - 75.0D0)/5.0D0)                       !TESG283
!---    Insure khgtt within proper limits                                         !TESG284
      khgtt = max0(1,khgtt)                                                       !TESG285
      lhgtt = 1                                                                   !TESG286
      if (khgtt==1 .and. hgtoffset<(-4.0D0)) then                                 !TESG287
         khgtt = 2                                                                !TESG288
         lhgtt = 2                                                                !TESG289
      endif                                                                       !TESG290
      khgtt = min0(nthgtt - 1,khgtt)                                              !TESG291
!---    Initialize MGCM height offset to zero                                     !TESG292
      ofszl = 0.0D0                                                               !TESG293
!---    Use MTGCM interpolation if height >= 80 km                                !TESG294
      if (chgt >= 80.0D0 + hgtoffset + 5.0D0*(lhgtt - 1.0D0)) then                !TESG295
!---      Get temperature, pressure, density, and wind components at              !TESG296
!         height indexes above and below current height                           !TESG297
         call testterp_M10 (khgtt, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1 & !TESG298
            , zf, tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1 & !TESG299
            , mapyear, idaydata)                                                  !TESG300
         call testterp_M10 (khgtt + 1, time, tmgcm2, pmgcm2, dmgcm2, umgcm2,    & !TESG301
            vmgcm2, zf, tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2  & !TESG302
            , dmin2, mapyear, idaydata)                                           !TESG303
!---      Height grid points above and below current height                       !TESG304
         z1 = 75.0D0 + 5.0D0*khgtt + hgtoffset                                    !TESG305
         z2 = 80.0D0 + 5.0D0*khgtt + hgtoffset                                    !TESG306
!---      Apply MTGCM height offset to ZF altitude                                !TESG307
         zf = zf + hgtoffset                                                      !TESG308
!---      Pressure and density scale heights                                      !TESG309
         hpres = (z2 - z1)/zlogr_M10(pmgcm1,pmgcm2,'TESG-07')                     !TESG310
         hpresday = (z2 - z1)/zlogr_M10(pday1,pday2,'TESG-08')                    !TESG311
         hdens = (z2 - z1)/zlogr_M10(dmgcm1,dmgcm2,'TESG-09')                     !TESG312
         ofszl = hgtoffset                                                        !TESG313
!---    Use MGCM interpolation at 75 km and MTGCM interpolation at 80             !TESG314
!       km if height between 75 and 80 km                                         !TESG315
      else if (chgt >= 75.0D0) then                                               !TESG316
!---      Get temperature, pressure, density, and wind components at              !TESG317
!         heights above and below current height                                  !TESG318
         call tesgterp_M10 (khgt, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1  & !TESG319
            , tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1,    & !TESG320
            mapyear, idaydata)                                                    !TESG321
         call testterp_M10 (lhgtt, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2 & !TESG322
            , zf80, tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2,     & !TESG323
            dmin2, mapyear, idaydata)                                             !TESG324
         z1 = 75.0D0                                                              !TESG325
         z2 = 80.0D0 + hgtoffset + 5.0D0*(lhgtt - 1.0D0)                          !TESG326
!---      Apply 'equivalent' multiplier for offset between 75 & 80 km             !TESG327
         if (ibougher <= 1) then                                                  !TESG328
            z1ofs = 60.0D0                                                        !TESG329
            ofsmgcm = hgtoffset - curoffset                                       !TESG330
            ofsz1 = ofsmgcm*(z1 - z1ofs)/(z2 - z1ofs)                             !TESG331
            call tesgterp_M10 (khgt + 1, time, tmgcmx, pmgcmx, dmgcmx, umgcmx,  & !TESG332
               vmgcmx, tdayx, pdayx, ddayx, udayx, vdayx, tmaxx, tminx, dmaxx,  & !TESG333
               dminx, mapyear, idaydata)                                          !TESG334
            hden12 = 5.0D0/dlog(dmgcm1/dmgcmx)                                    !TESG335
            ofsmult1 = dexp(ofsz1/hden12)                                         !TESG336
!---        Local MGCM height offset                                              !TESG337
            ofszl = ofsmgcm*(chgt - z1ofs)/(z2 - z1ofs)                           !TESG338
            pmgcm1 = pmgcm1*ofsmult1                                              !TESG339
            dmgcm1 = dmgcm1*ofsmult1                                              !TESG340
            pday1 = pday1*ofsmult1                                                !TESG341
            dday1 = dday1*ofsmult1                                                !TESG342
            dmaxa = dmaxa*ofsmult1                                                !TESG343
            dmin1 = dmin1*ofsmult1                                                !TESG344
         endif                                                                    !TESG345
!---      Pressure and density scale heights (km)                                 !TESG346
         hpres = (z2 - z1)/zlogr_M10(pmgcm1,pmgcm2,'TESG-10')                     !TESG347
         hpresday = (z2 - z1)/zlogr_M10(pday1,pday2,'TESG-11')                    !TESG348
         hdens = (z2 - z1)/zlogr_M10(dmgcm1,dmgcm2,'TESG-12')                     !TESG349
!---    Use TESsrftrp_M10 routine if height within boundary layer                 !TESG350
      else if (chgt <= ctopohgt + dzbl(nbl)) then                                 !TESG351
!---      Set index for surface layer data                                        !TESG352
         jbl = 1                                                                  !TESG353
         if (chgt >= ctopohgt + dzbl(2)) jbl = 2                                  !TESG354
!---      Get temperature, pressure, density, and wind components at              !TESG355
!         heights above and below current height                                  !TESG356
         call tessrftrp_M10 (jbl + 1, time, tmgcm2, pmgcm2, dmgcm2, umgcm2,     & !TESG357
            vmgcm2, hpres, hdens, ctopohgt, tday2, pday2, dday2, uday2, vday2,  & !TESG358
            presday, tmax2, tmin2, dmax2, dmin2, tat5m, mapyear, idaydata)        !TESG359
         call tessrftrp_M10 (jbl, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1  & !TESG360
            , hpres1, hdens1, ctopohgt, tday1, pday1, dday1, uday1, vday1,      & !TESG361
            hpresday, tmax1, tmin1, dmaxa, dmin1, tat5m, mapyear, idaydata)       !TESG362
!---      Heights at two boundary layer levels                                    !TESG363
         z1 = ctopohgt + dzbl(jbl)                                                !TESG364
         z2 = ctopohgt + dzbl(jbl+1)                                              !TESG365
!---      Get Temp at 1st height above BL, for density scale height               !TESG366
         call tesgterp_M10 (k1st, time, tmgcmx, pmgcmx, dmgcmx, umgcmx, vmgcmx  & !TESG367
            , tdayx, pdayx, ddayx, udayx, vdayx, tmaxx, tminx, dmaxx, dminx,    & !TESG368
            mapyear, idaydata)                                                    !TESG369
!---      Temperature gradient for density scale height calculation               !TESG370
         z2x = (-6.0D0) + k1st                                                    !TESG371
         if (k1st >= 16) z2x = 10.0D0 + 5.0D0*(k1st - 16.0D0)                     !TESG372
         dtdz = (tmgcmx - tmgcm1)/(z2x - z1)                                      !TESG373
         if (chgt <= ctopohgt) dtdz = 0.0D0                                       !TESG374
!---      Average layer temperature for density scale height                      !TESG375
         tbar = (tmgcm1 + tmgcm2)/2.0D0                                           !TESG376
!---      Density scale height from pressure scale height and                     !TESG377
!         temperature gradient                                                    !TESG378
         hdens = hpres/(1.0D0 + (hpres/tbar)*dtdz)                                !TESG379
!---      Perturbation factor = surface value                                     !TESG380
         pertfact = pert0                                                         !TESG381
!---    Use TESGterp_M10 routine if height above boundary layer levels            !TESG382
!        and height <= 75 km                                                      !TESG383
      else if (chgt >= hgtk1) then                                                !TESG384
!---      Get temperature, pressure, density, and wind components at              !TESG385
!         heights above and below current height                                  !TESG386
         call tesgterp_M10 (khgt, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1  & !TESG387
            , tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1,    & !TESG388
            mapyear, idaydata)                                                    !TESG389
         call tesgterp_M10 (khgt + 1, time, tmgcm2, pmgcm2, dmgcm2, umgcm2,     & !TESG390
            vmgcm2, tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2,     & !TESG391
            dmin2, mapyear, idaydata)                                             !TESG392
!---      Heights at grid points above and below current level                    !TESG393
         z1 = (-6.0D0) + khgt                                                     !TESG394
         z2 = (-5.0D0) + khgt                                                     !TESG395
         if (khgt >= 16) then                                                     !TESG396
            z1 = 10.0D0 + 5.0D0*(khgt - 16)                                       !TESG397
            z2 = 15.0D0 + 5.0D0*(khgt - 16)                                       !TESG398
         endif                                                                    !TESG399
!---      Apply 'equivalent' multiplier for offset below 75 km                    !TESG400
         if (ibougher <= 1) then                                                  !TESG401
            z1ofs = 60.0D0                                                        !TESG402
            z2ofs = 80.0D0 + hgtoffset + 5.0D0*(lhgtt - 1.0D0)                    !TESG403
            ofsmgcm = hgtoffset - curoffset                                       !TESG404
            if (z1 <= z1ofs) then                                                 !TESG405
               ofsmult1 = 1.0D0                                                   !TESG406
            else                                                                  !TESG407
               ofsz1 = ofsmgcm*(z1 - z1ofs)/(z2ofs - z1ofs)                       !TESG408
               hden12 = 5.0D0/dlog(dmgcm1/dmgcm2)                                 !TESG409
               ofsmult1 = dexp(ofsz1/hden12)                                      !TESG410
            endif                                                                 !TESG411
            if (z2 <= z1ofs) then                                                 !TESG412
               ofsmult2 = 1.0D0                                                   !TESG413
            else                                                                  !TESG414
               ofsz2 = ofsmgcm*(z2 - z1ofs)/(z2ofs - z1ofs)                       !TESG415
               hden12 = 5.0D0/dlog(dmgcm1/dmgcm2)                                 !TESG416
               ofsmult2 = dexp(ofsz2/hden12)                                      !TESG417
            endif                                                                 !TESG418
!---        Local MGCM height offset                                              !TESG419
            if (chgt > z1ofs) ofszl = ofsmgcm*(chgt - z1ofs)/(z2ofs - z1ofs)      !TESG420
            pmgcm1 = pmgcm1*ofsmult1                                              !TESG421
            dmgcm1 = dmgcm1*ofsmult1                                              !TESG422
            pmgcm2 = pmgcm2*ofsmult2                                              !TESG423
            dmgcm2 = dmgcm2*ofsmult2                                              !TESG424
            pday1 = pday1*ofsmult1                                                !TESG425
            dday1 = dday1*ofsmult1                                                !TESG426
            dmaxa = dmaxa*ofsmult1                                                !TESG427
            dmin1 = dmin1*ofsmult1                                                !TESG428
            pday2 = pday2*ofsmult2                                                !TESG429
            dday2 = dday2*ofsmult2                                                !TESG430
            dmax2 = dmax2*ofsmult2                                                !TESG431
            dmin2 = dmin2*ofsmult2                                                !TESG432
         endif                                                                    !TESG433
!---      Pressure and density scale heights (km)                                 !TESG434
         hpres = (z2 - z1)/zlogr_M10(pmgcm1,pmgcm2,'TESG-13')                     !TESG435
         hpresday = (z2 - z1)/zlogr_M10(pday1,pday2,'TESG-14')                    !TESG436
         hdens = (z2 - z1)/zlogr_M10(dmgcm1,dmgcm2,'TESG-15')                     !TESG437
!---    Use TESsrftrp_M10 at top of boundary layer and TESGterp_M10 at            !TESG438
!       1st level above boundary layer if height between boundary layer           !TESG439
!       and height index k1st                                                     !TESG440
      else                                                                        !TESG441
!---      Get temperature, pressure, density, and wind components at              !TESG442
!         heights above and below current height                                  !TESG443
         call tessrftrp_M10 (nbl, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1  & !TESG444
            , hpres, hdens, ctopohgt, tday1, pday1, dday1, uday1, vday1,        & !TESG445
            hpresday, tmax1, tmin1, dmaxa, dmin1, tat5m, mapyear, idaydata)       !TESG446
         call tesgterp_M10 (k1st, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2  & !TESG447
            , tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2, dmin2,    & !TESG448
            mapyear, idaydata)                                                    !TESG449
!---      Heights at grid points above and below current level                    !TESG450
         z1 = ctopohgt + dzbl(nbl)                                                !TESG451
         z2 = (-6.0D0) + k1st                                                     !TESG452
         if (k1st >= 16) z2 = 10.0D0 + 5.0D0*(k1st - 16.0D0)                      !TESG453
!---      Temperature gradient and mean temperature for density scale             !TESG454
!         height calculation                                                      !TESG455
         dtdz = (tmgcm2 - tmgcm1)/(z2 - z1)                                       !TESG456
         tbar = (tmgcm1 + tmgcm2)/2.0D0                                           !TESG457
!---      Density scale height from pressure scale height and                     !TESG458
!         temperature gradient                                                    !TESG459
         hdens = hpres/(1.0D0 + (hpres/tbar)*dtdz)                                !TESG460
      endif                                                                       !TESG461
!---    Get gas constant from pressure, density, and temperature                  !TESG462
      if (chgt <= ctopohgt) then                                                  !TESG463
         rgas = pmgcm1/(dmgcm1*tmgcm1)                                            !TESG464
         rgasday = pday1/(dday1*tday1)                                            !TESG465
         dhgt = (ctopohgt - z1)/(z2 - z1)                                         !TESG466
      else                                                                        !TESG467
         dhgt = (chgt - z1)/(z2 - z1)                                             !TESG468
         r1 = pmgcm1/(dmgcm1*tmgcm1)                                              !TESG469
         r2 = pmgcm2/(dmgcm2*tmgcm2)                                              !TESG470
         rgas1 = pday1/(dday1*tday1)                                              !TESG471
         rgas2 = pday2/(dday2*tday2)                                              !TESG472
         rgas = r1 + dhgt*(r2 - r1)                                               !TESG473
         rgasday = rgas1 + dhgt*(rgas2 - rgas1)                                   !TESG474
      endif                                                                       !TESG475
!---    Use logarithmic wind and temperature profiles (with surface               !TESG476
!       roughness z0) if height below lowest boundary layer level                 !TESG477
      if (chgt < ctopohgt + dzbl(2)) then                                         !TESG478
!---      Convert surface roughness to km                                         !TESG479
         z0 = zwsfc/1000.0D0                                                      !TESG480
!---      Save ground surface temperature for output                              !TESG481
         tgrnd = tmgcm1                                                           !TESG482
!---      Consistent with Ames MGCM, use z0 = 0.01 cm (1.0e-7 km) if              !TESG483
!         over ice (T <= CO2 sublimation temperature + 5K)                        !TESG484
         tcheck = tmgcm1                                                          !TESG485
         call subltchk_M10 (tcheck, pmgcm2, tsubl)                                !TESG486
!---      If surface temperature near sublimation point, set polar ice            !TESG487
!           indicator on (= 1) and re-set surface roughness                       !TESG488
         icepolar = 0                                                             !TESG489
         if (tmgcm1 <= tsubl + 5.0D0) then                                        !TESG490
            z0 = 1.0D-7                                                           !TESG491
            icepolar = 1                                                          !TESG492
         endif                                                                    !TESG493
         uhgt = chgt - ctopohgt                                                   !TESG494
         uhgt = dmax1(z0,uhgt)                                                    !TESG495
!---      Compute logarithmic boundary layer shape factor for surface             !TESG496
!         to lowest boundary layer level                                          !TESG497
         factor = zlogr_M10(uhgt,z0,'TESG-16')/zlogr_M10(dzbl(2),z0,'TESG-17')    !TESG498
!---      Apply factor for wind; assume no-slip condition at surface              !TESG499
         cuwin = umgcm2*factor                                                    !TESG500
         cvwin = vmgcm2*factor                                                    !TESG501
         ewwnday = uday2*factor                                                   !TESG502
         nswnday = vday2*factor                                                   !TESG503
!---      Set up parameters to evaluate temperature boundary layer                !TESG504
!         Convert heights to meters for input to bltp_M10 subroutine              !TESG505
         z5 = dzbl(2)*1000.0D0                                                    !TESG506
         zeval = uhgt*1000.0D0                                                    !TESG507
!---      Get value of local gravity                                              !TESG508
         call rellips_M10 (clat, clonw, rref, chgt, gz, oldrref, topohgt,       & !TESG509
            albedo, requa, rpole)                                                 !TESG510
!---      Use Ames MGCM boundary layer model for current temperature              !TESG511
!         Get specific heat at constant pressure                                  !TESG512
         cpoft = cp_M10(tmgcm2)                                                   !TESG513
         call bltp_M10 (gz, cpoft, tmgcm1, z5, tmgcm2, umgcm2, vmgcm2, zeval,   & !TESG514
            factor, ctemp)                                                        !TESG515
!---      Use Ames MGCM boundary layer model for daily avg temperature            !TESG516
         cpoft = cp_M10(tday2)                                                    !TESG517
         call bltp_M10 (gz, cpoft, tday1, z5, tday2, uday2, vday2, zeval,       & !TESG518
            factor, tempday)                                                      !TESG519
!---      Use Ames MGCM boundary layer model for daily max temperature            !TESG520
         cpoft = cp_M10(tmax2)                                                    !TESG521
         call bltp_M10 (gz, cpoft, tmax1, z5, tmax2, uday2, vday2, zeval,       & !TESG522
            factor, tempmax)                                                      !TESG523
!---      Use Ames MGCM boundary layer model for daily min temperature            !TESG524
         cpoft = cp_M10(tmin2)                                                    !TESG525
         call bltp_M10 (gz, cpoft, tmin1, z5, tmin2, uday2, vday2, zeval,       & !TESG526
            factor, tempmin)                                                      !TESG527
!---      Pressure at current position from pressure scale height                 !TESG528
         cpres = pmgcm2*dexp((z2 - chgt)/hpres)                                   !TESG529
         presday = pday2*dexp((z2 - chgt)/hpresday)                               !TESG530
!---      Density at current position from gas law                                !TESG531
         cdens = cpres/(rgas*ctemp)                                               !TESG532
         densday = presday/(rgasday*tempday)                                      !TESG533
         densmin = 9999.0D0                                                       !TESG534
         densmax = -9999.0D0                                                      !TESG535
         if (idaydata > 0) then                                                   !TESG536
!---      Daily maximum and minimum density                                       !TESG537
            densmin = densday*(dmin1/dday1 + factor*(dmin2/dday2 - dmin1/dday1) & !TESG538
               )                                                                  !TESG539
            densmax = densday*(dmaxa/dday1 + factor*(dmax2/dday2 - dmaxa/dday1) & !TESG540
               )                                                                  !TESG541
         endif                                                                    !TESG542
!---    Use linear height interpolation if above logarithmic                      !TESG543
!       surface layer                                                             !TESG544
      else                                                                        !TESG545
         dhgt = (chgt - z1)/(z2 - z1)                                             !TESG546
         cuwin = umgcm1 + dhgt*(umgcm2 - umgcm1)                                  !TESG547
         cvwin = vmgcm1 + dhgt*(vmgcm2 - vmgcm1)                                  !TESG548
         ewwnday = uday1 + dhgt*(uday2 - uday1)                                   !TESG549
         nswnday = vday1 + dhgt*(vday2 - vday1)                                   !TESG550
!---      Interpolate temperature to current height                               !TESG551
         ctemp = tmgcm1 + dhgt*(tmgcm2 - tmgcm1)                                  !TESG552
         tempday = tday1 + dhgt*(tday2 - tday1)                                   !TESG553
         tempmax = tmax1 + dhgt*(tmax2 - tmax1)                                   !TESG554
         tempmin = tmin1 + dhgt*(tmin2 - tmin1)                                   !TESG555
!---      Pressure at current position from pressure scale height                 !TESG556
         cpres = pmgcm2*dexp((z2 - chgt)/hpres)                                   !TESG557
         presday = pday2*dexp((z2 - chgt)/hpresday)                               !TESG558
!---      Density at current position from gas law                                !TESG559
         cdens = cpres/(rgas*ctemp)                                               !TESG560
         densday = presday/(rgasday*tempday)                                      !TESG561
         densmin = 9999.0D0                                                       !TESG562
         densmax = -9999.0D0                                                      !TESG563
         if (idaydata > 0) then                                                   !TESG564
!---      Daily maximum and minimum density                                       !TESG565
            densmin = densday*(dmin1/dday1 + dhgt*(dmin2/dday2 - dmin1/dday1))    !TESG566
            densmax = densday*(dmaxa/dday1 + dhgt*(dmax2/dday2 - dmaxa/dday1))    !TESG567
         endif                                                                    !TESG568
      endif                                                                       !TESG569
      if (chgt < ctopohgt + 0.5D0) then                                           !TESG570
         if (dabs(clat) >= 85.0D0) then                                           !TESG571
            polefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                         !TESG572
            cpres = polefac*cpres + (1.0D0 - polefac)*presday                     !TESG573
            cdens = polefac*cdens + (1.0D0 - polefac)*densday                     !TESG574
            densmin = 9999.0D0                                                    !TESG575
            densmax = -9999.0D0                                                   !TESG576
            if (idaydata > 0) then                                                !TESG577
               densmax = polefac*densmax + (1.0D0 - polefac)*densday              !TESG578
               densmin = polefac*densmin + (1.0D0 - polefac)*densday              !TESG579
            endif                                                                 !TESG580
         endif                                                                    !TESG581
      endif                                                                       !TESG582
!---    Set specific bogus values of pressure or density scale heights            !TESG583
!       are out of range                                                          !TESG584
      hpres = dmax1(-9.99D0,hpres)                                                !TESG585
      hpres = min(99.99D0,hpres)                                                  !TESG586
      hdens = dmax1(-9.99D0,hdens)                                                !TESG587
      hdens = min(99.99D0,hdens)                                                  !TESG588
!---    Compute perturbation factor, unless it has already been set               !TESG604
      if (pertfact < pert0) then                                                  !TESG605
!---      Perturbation factor from new, simplified model                          !TESG606
        If (chgt < 100.0d0)Then                                                   !TESG607
          pertfact = 0.30d0*dexp((chgt-100.0d0)/35.0d0)                           !TESG608
          If (pertfact < pert0)pertfact = pert0                                   !TESG609
        Else                                                                      !TESG610
          pertfact = 0.30d0 + 0.5d-2*(chgt-100.0d0)                               !TESG610a
          If (pertfact > 0.45d0)pertfact = 0.45d0                                 !TESG610b
        Endif                                                                     !TESG610c
      endif                                                                       !TESG611
!---    Get slope winds (0 below surface and > 4.5 km above surface)              !TESG612
      call slopewind_M10 (clat, clonw, chgt, time, cuwin, cvwin, blwindew,      & !TESG613
         blwindns, blwindvert)                                                    !TESG614
!---    Compute daily average slope winds                                         !TESG615
      bluday = 0.0D0                                                              !TESG616
      blvday = 0.0D0                                                              !TESG617
      do itime = 0, 22, 2                                                         !TESG618
         call slopewind_M10 (clat, clonw, chgt, dble(itime), cuwin, cvwin, blu  & !TESG619
            , blv, blw)                                                           !TESG620
         bluday = bluday + blu                                                    !TESG621
         blvday = blvday + blv                                                    !TESG622
      end do                                                                      !TESG623
      bluday = bluday/12.0D0                                                      !TESG624
      blvday = blvday/12.0D0                                                      !TESG625
      return                                                                      !TESG626
      end subroutine tesgcm_M10                                                   !TESG627
!                                                                                 !TESG628
!-------------------------------------------------------------------------------  !PTRP  1
      subroutine profterp_M10(chgt, clat, clon, tin, pin, din, uin, vin, ptemp  & !PTRP  2
         , ppres, pdens, puwin, pvwin, nprof, profnear, proffar, profwgt,       & !PTRP  3
         wavepert)                                                                !PTRP  4
!-----------------------------------------------                                  !PTRP  5
!   M o d u l e s                                                                 !PTRP  6
!-----------------------------------------------                                  !PTRP  7
      USE vast_kind_param, ONLY:  double                                          !PTRP  8
      USE pterp_M10_C                                                             !PTRP  9
!---    Interpolates profile data to current position (chgt,clat,clon)            !PTRP 10
!       and weights results (with factor profwgt) with input values               !PTRP 11
!       (tin,pin,din,uin,vin), yielding weighted average (ptemp,ppres,            !PTRP 12
!       pdens,puwin,pvwin).  Input profnear is lat-lon radius over                !PTRP 13
!       which profile is weighted with 1.0; proffar is lat-lon radius             !PTRP 14
!       beyond which profile is given zero weight.                                !PTRP 15
!...                                                                              !PTRP 16
!...Switches:                                                                     !PTRP 17
      implicit none                                                               !PTRP 18
!-----------------------------------------------                                  !PTRP 19
!   G l o b a l   P a r a m e t e r s                                             !PTRP 20
!-----------------------------------------------                                  !PTRP 21
!-----------------------------------------------                                  !PTRP 22
!   D u m m y   A r g u m e n t s                                                 !PTRP 23
!-----------------------------------------------                                  !PTRP 24
      integer , intent(inout) :: nprof                                            !PTRP 25
      real(double) , intent(in) :: chgt                                           !PTRP 26
      real(double) , intent(in) :: clat                                           !PTRP 27
      real(double) , intent(in) :: clon                                           !PTRP 28
      real(double) , intent(in) :: tin                                            !PTRP 29
      real(double) , intent(in) :: pin                                            !PTRP 30
      real(double) , intent(in) :: din                                            !PTRP 31
      real(double) , intent(in) :: uin                                            !PTRP 32
      real(double) , intent(in) :: vin                                            !PTRP 33
      real(double) , intent(inout) :: ptemp                                       !PTRP 34
      real(double) , intent(inout) :: ppres                                       !PTRP 35
      real(double) , intent(out) :: pdens                                         !PTRP 36
      real(double) , intent(out) :: puwin                                         !PTRP 37
      real(double) , intent(out) :: pvwin                                         !PTRP 38
      real(double) , intent(in) :: profnear                                       !PTRP 39
      real(double) , intent(in) :: proffar                                        !PTRP 40
      real(double) , intent(out) :: profwgt                                       !PTRP 41
      real(double) , intent(in) :: wavepert                                       !PTRP 42
!-----------------------------------------------                                  !PTRP 43
!   L o c a l   V a r i a b l e s                                                 !PTRP 44
!-----------------------------------------------                                  !PTRP 45
      integer , dimension(2) :: ia                                                !PTRP 46
      integer :: i1, i2, ni, i                                                    !PTRP 47
      real(double), dimension(2) :: adll                                          !PTRP 48
      real(double) :: pi2, dlat1, dlat2, dlon1, dlon2, radius1, radius2, factor & !PTRP 49
         , pilat, dplon, pilon, facthgt, factll, dlat, dlon, radius, tpdwgt,    & !PTRP 50
         uvwgt                                                                    !PTRP 51
!-----------------------------------------------                                  !PTRP 52
      data ia/ 2*0/                                                               !PTRP 53
      data adll/ 2*0.0D0/                                                         !PTRP 54
!---    Calculate pi/2                                                            !PTRP 55
      pi2 = 2.0D0*datan(1.0D0)                                                    !PTRP 56
      i1 = 0                                                                      !PTRP 57
      i2 = 0                                                                      !PTRP 58
      ni = 0                                                                      !PTRP 59
      profwgt = 0.0                                                               !PTRP 60
!---    Find nearest pair of points above and below current height                !PTRP 61
      do i = 1, nprof - 1                                                         !PTRP 62
         if ((chgt - phgt(i))*(chgt - phgt(i+1))>=0.0D0 .and.                   & !PTRP 63
            dabs(chgt-phgt(i)) > 0.0d0)cycle                                      !PTRP 64
         ni = ni + 1                                                              !PTRP 65
         if (ni > 2) stop ' Too many height pairs in profile'                     !PTRP 66
         ia(ni) = i                                                               !PTRP 67
         dlat1 = dabs(clat - plat(i))                                             !PTRP 68
         dlat2 = dabs(clat - plat(i+1))                                           !PTRP 69
         dlon1 = dabs(clon - plon(i))                                             !PTRP 70
         dlon2 = dabs(clon - plon(i+1))                                           !PTRP 71
!---        Adjust lon difference for wrap at lon 360                             !PTRP 72
         if (dlon1 > 180.0D0) dlon1 = 360.0D0 - dlon1                             !PTRP 73
         if (dlon2 > 180.0D0) dlon2 = 360.0D0 - dlon2                             !PTRP 74
!---        Lat-lon radius from positions of points i1 and i2                     !PTRP 75
         radius1 = dsqrt(dlat1**2 + dlon1**2)                                     !PTRP 76
         radius2 = dsqrt(dlat2**2 + dlon2**2)                                     !PTRP 77
         adll(ni) = (radius1 + radius2)/2.0D0                                     !PTRP 78
      end do                                                                      !PTRP 79
      if (ni > 0) then                                                            !PTRP 80
         i1 = ia(1)                                                               !PTRP 81
         if (ni==2 .and. adll(2)<adll(1)) i1 = ia(2)                              !PTRP 82
         i2 = i1 + 1                                                              !PTRP 83
      endif                                                                       !PTRP 84
      if (i1 == 0) then                                                           !PTRP 85
         pdens = 0.0D0                                                            !PTRP 86
         puwin = 0.0D0                                                            !PTRP 87
         pvwin = 0.0D0                                                            !PTRP 88
         go to 50                                                                 !PTRP 89
      endif                                                                       !PTRP 90
!---    Compute factor for linear height interpolation                            !PTRP 91
      factor = (chgt - phgt(i1))/(phgt(i2)-phgt(i1))                              !PTRP 92
!---    Linear height interpolation for lat,lon,temperature,winds                 !PTRP 93
      pilat = plat(i1) + factor*(plat(i2)-plat(i1))                               !PTRP 94
      dplon = plon(i2) - plon(i1)                                                 !PTRP 95
      if (dplon > 180.0D0) dplon = dplon - 360.0D0                                !PTRP 96
      if (dplon < (-180.0D0)) dplon = dplon + 360.0D0                             !PTRP 97
      pilon = plon(i1) + factor*dplon                                             !PTRP 98
      ptemp = ptmp(i1) + factor*(ptmp(i2)-ptmp(i1))                               !PTRP 99
      puwin = puwn(i1) + factor*(puwn(i2)-puwn(i1))                               !PTRP100
      pvwin = pvwn(i1) + factor*(pvwn(i2)-pvwn(i1))                               !PTRP101
!---    Power-law interpolation for density (unless profile density               !PTRP102
!       is zero, for which zero weight will be used)                              !PTRP103
      pdens = 0.0D0                                                               !PTRP104
      if (pden(i1) > 0.0D0) pdens = pden(i1)*(pden(i2)/pden(i1))**factor          !PTRP105
!---    Power-law interpolation for pressure (unless profile pressure             !PTRP106
!       is zero, for which zero weight will be used)                              !PTRP107
      ppres = 0.0D0                                                               !PTRP108
      if (pprs(i1) > 0.0D0) ppres = pprs(i1)*(pprs(i2)/pprs(i1))**factor          !PTRP109
!---    Initialize weighting factor components for height and lat-lon             !PTRP110
      facthgt = 1.0D0                                                             !PTRP111
      factll = 1.0D0                                                              !PTRP112
      if (i1 == 1) then                                                           !PTRP113
!---    Sine-squared variation of height weighting from 0 at 1st point            !PTRP114
!       to 1 at 2nd point                                                         !PTRP115
         facthgt = (chgt - phgt(1))/(phgt(2)-phgt(1))                             !PTRP116
         facthgt = dsin(pi2*facthgt)**2                                           !PTRP117
      else if (i2 == nprof) then                                                  !PTRP118
!---    Sine-squared variation of height weighting from 0 at next-to-             !PTRP119
!       last point to 1 at last point                                             !PTRP120
         facthgt = (chgt - phgt(nprof))/(phgt(nprof-1)-phgt(nprof))               !PTRP121
         facthgt = dsin(pi2*facthgt)**2                                           !PTRP122
      endif                                                                       !PTRP123
!---    Compute absolute lat-lon difference of current position from              !PTRP124
!       profile lat-lon                                                           !PTRP125
      dlat = dabs(clat - pilat)                                                   !PTRP126
      dlon = dabs(clon - pilon)                                                   !PTRP127
!---    Adjust lon difference for wrap at lon 360                                 !PTRP128
      if (dlon > 180.0D0) dlon = 360.0D0 - dlon                                   !PTRP129
!---    Lat-lon radius of current position from profile lat-lon                   !PTRP130
      radius = dsqrt(dlat**2 + dlon**2)                                           !PTRP131
!---    Use weight=0 if radius>proffar, weight=1 if radius<profnear,              !PTRP132
!       with sine-squared variation between proffar and profnear                  !PTRP133
      if (radius >= proffar) then                                                 !PTRP134
         factll = 0.0D0                                                           !PTRP135
      else if (radius <= profnear) then                                           !PTRP136
         factll = 1.0D0                                                           !PTRP137
      else                                                                        !PTRP138
         factll = (proffar - radius)/(proffar - profnear)                         !PTRP139
         factll = dsin(pi2*factll)**2                                             !PTRP140
      endif                                                                       !PTRP141
!---    Total weight = product of weights for lat-lon and height                  !PTRP142
      profwgt = factll*facthgt                                                    !PTRP143
   50 continue                                                                    !PTRP144
      tpdwgt = profwgt                                                            !PTRP145
      uvwgt = profwgt                                                             !PTRP146
!---    Set profile weight to zero for p,d, & t if profile values are 0           !PTRP147
      if (dabs(ptemp*ppres*pdens) <= 0.0D0) tpdwgt = 0.0D0                        !PTRP148
!---    Set profile weight to zero for u & v if profile values are 0              !PTRP149
      if (dabs(puwin) + dabs(pvwin) <= 0.0D0) uvwgt = 0.0D0                       !PTRP150
!---    Apply wave perturbation effect to profile values                          !PTRP151
      ppres = ppres*(1. + wavepert)                                               !PTRP152
      pdens = pdens*(1. + wavepert)                                               !PTRP153
!---    Apply weighted averaging of profile values with input values              !PTRP154
      ptemp = tpdwgt*ptemp + (1.0D0 - tpdwgt)*tin                                 !PTRP155
      ppres = tpdwgt*ppres + (1.0D0 - tpdwgt)*pin                                 !PTRP156
      pdens = tpdwgt*pdens + (1.0D0 - tpdwgt)*din                                 !PTRP157
      puwin = uvwgt*puwin + (1.0D0 - uvwgt)*uin                                   !PTRP158
      pvwin = uvwgt*pvwin + (1.0D0 - uvwgt)*vin                                   !PTRP159
      return                                                                      !PTRP160
      end subroutine profterp_M10                                                 !PTRP161
!                                                                                 !PTRP162
!------------------------------------------------------------------------------   !RDPF  1
      subroutine rdprof_M10(profile, nprof, loneast)                              !RDPF  2
!-----------------------------------------------                                  !RDPF  3
!   M o d u l e s                                                                 !RDPF  4
!-----------------------------------------------                                  !RDPF  5
      USE vast_kind_param, ONLY:  double                                          !RDPF  6
      USE pterp_M10_C                                                             !RDPF  7
      USE parameters_M10_C                                                        !RDPF  8
!-----------------------------------------------                                  !RDPF  9
!---    Reads alternate profile data file profile. Returns number of              !RDPF 10
!       lines of data (nprof).  Converts input longitudes from East to            !RDPF 11
!       West if LonEast = 1                                                       !RDPF 12
!...                                                                              !RDPF 13
!...Switches:                                                                     !RDPF 14
      implicit none                                                               !RDPF 15
!-----------------------------------------------                                  !RDPF 16
!   D u m m y   A r g u m e n t s                                                 !RDPF 17
!-----------------------------------------------                                  !RDPF 18
      integer , intent(inout) :: nprof                                            !RDPF 19
      integer , intent(in) :: loneast                                             !RDPF 20
      character(len=99) , intent(in) :: profile                                   !RDPF 21
!-----------------------------------------------                                  !RDPF 22
!   L o c a l   V a r i a b l e s                                                 !RDPF 23
!-----------------------------------------------                                  !RDPF 24
      integer :: lenprof, n                                                       !RDPF 25
      real(double) :: zhgt, xlat, xlon, t, p, d, u, v                             !RDPF 26
      character :: dummy                                                          !RDPF 27
!-----------------------------------------------                                  !RDPF 28
!---    Compute string length for profile file name                               !RDPF 29
      lenprof = Len_Trim(profile)                                                 !RDPF 30
      if (lenprof<1 .or. lenprof>99) lenprof = 99                                 !RDPF 31
!---    Open profile data file                                                    !RDPF 32
      open(33, file=profile(1:lenprof), status='old', position='asis')            !RDPF 33
!---    Read and ignore header line                                               !RDPF 34
      read (33, 5) dummy                                                          !RDPF 35
    5 format(a1)                                                                  !RDPF 36
      n = 0                                                                       !RDPF 37
!---    Start of loop to read profile data                                        !RDPF 38
   10 continue                                                                    !RDPF 39
      read (33, *, end=99) zhgt, xlat, xlon, t, p, d, u, v                        !RDPF 40
!---    Convert negative longitudes                                               !RDPF 41
      if (xlon < 0.0D0) xlon = xlon + 360.0D0                                     !RDPF 42
!---    Convert to West Longitude if LonEast = 1                                  !RDPF 43
      if (loneast == 1) xlon = 360.0D0 - xlon                                     !RDPF 44
!---    Count number of lines read                                                !RDPF 45
      n = n + 1                                                                   !RDPF 46
!---    Store profile data in arrays, for common pterp                            !RDPF 47
      phgt(n) = zhgt                                                              !RDPF 48
!---    Stop if two successive heights are the same                               !RDPF 49
      if (n>1 .and. dabs(phgt(n)-phgt(n-1)) <= 0.0) then                          !RDPF 50
         write (*, *) n, phgt(n)                                                  !RDPF 51
         stop ' Consecutive profile heights cannot be same'                       !RDPF 52
      endif                                                                       !RDPF 53
      plat(n) = xlat                                                              !RDPF 54
      plon(n) = xlon                                                              !RDPF 55
      ptmp(n) = t                                                                 !RDPF 56
      pprs(n) = p                                                                 !RDPF 57
      pden(n) = d                                                                 !RDPF 58
      puwn(n) = u                                                                 !RDPF 59
      pvwn(n) = v                                                                 !RDPF 60
      nprof = n                                                                   !RDPF 61
!---    Cycle back to read another line of profile data                           !RDPF 62
      go to 10                                                                    !RDPF 63
!---    Close profile input file when end-of-file encountered                     !RDPF 64
   99 continue                                                                    !RDPF 65
      close(33)                                                                   !RDPF 66
      return                                                                      !RDPF 67
      end subroutine rdprof_M10                                                   !RDPF 68
!----------------------------------------------------------------------           !RDPF 69
