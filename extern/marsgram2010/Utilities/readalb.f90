      module vast_kind_param                                                      !VAST  1
         integer, parameter :: byte_log = selected_int_kind(2)                    !VAST  2
         integer, parameter :: short_log = selected_int_kind(4)                   !VAST  3
         integer, parameter :: long_log = selected_int_kind(18)                   !VAST  4
         integer, parameter :: byte = selected_int_kind(2)                        !VAST  5
         integer, parameter :: short = selected_int_kind(4)                       !VAST  6
         integer, parameter :: long = selected_int_kind(18)                       !VAST  7
         integer, parameter :: double = selected_real_kind(14)                    !VAST  8
         integer, parameter :: extended = selected_real_kind(30)                  !VAST  9
         integer, parameter :: double_ext = selected_real_kind(50)                !VAST 10
         integer, parameter :: dble_complex = selected_real_kind(14)              !VAST 11
         integer, parameter :: ext_complex = selected_real_kind(30)               !VAST 12
      end module vast_kind_param                                                  !VAST 13
!=============================================================================    !VAST 14
      program readalb                                                             !RALB  1
!-----------------------------------------------                                  !RALB  2
!   M o d u l e s                                                                 !RALB  3
!-----------------------------------------------                                  !RALB  4
      USE vast_kind_param, ONLY:  double                                          !RALB  5
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:58:52  03/23/10        !RALB  6
!...Switches:                                                                     !RALB  7
      implicit none                                                               !RALB  8
!-----------------------------------------------                                  !RALB  9
!   L o c a l   P a r a m e t e r s                                               !RALB 10
!-----------------------------------------------                                  !RALB 11
      integer, parameter :: nlat = 181                                            !RALB 12
      integer, parameter :: nlon = 361                                            !RALB 13
!-----------------------------------------------                                  !RALB 14
!   L o c a l   V a r i a b l e s                                                 !RALB 15
!-----------------------------------------------                                  !RALB 16
      integer :: numread, jlon, ilat                                              !RALB 17
      real(double), dimension(0:nlat,0:nlon) :: albedo                            !RALB 18
      real(double) :: steplat, steplon, albnorth, albsouth, alat, alon, alb       !RALB 19
!-----------------------------------------------                                  !RALB 21
      open(21, file='albedo1.txt', status='old', position='asis')                 !RALB 22
      open(22, file='albedo1.bin', form='unformatted', position='asis')           !RALB 23
      steplat = 180.0D0/(nlat - 1.0D0)                                            !RALB 24
      steplon = 360.0D0/(nlon - 1.0D0)                                            !RALB 25
      albnorth = 0.0D0                                                            !RALB 26
      albsouth = 0.0D0                                                            !RALB 27
      numread = 0                                                                 !RALB 28
   50 continue                                                                    !RALB 29
      read (21, *, err=50, end=99) alat, alon, alb                                !RALB 30
      jlon = idint(1.0D0 + alon/steplon)                                          !RALB 31
      ilat = idint(1.0D0 + (90.0D0 + alat)/steplat)                               !RALB 32
      if (jlon<1 .or. jlon>nlon-1) stop ' Bad lon'                                !RALB 33
      if (ilat<1 .or. ilat>nlat-1) stop ' Bad lat'                                !RALB 34
      albedo(ilat,jlon) = alb                                                     !RALB 35
!...    Set albedo near 0 longitude                                               !RALB 36
      if (jlon == nlon - 1) albedo(ilat,nlon) = alb                               !RALB 37
      if (jlon == 1) albedo(ilat,0) = alb                                         !RALB 38
      numread = numread + 1                                                       !RALB 39
      go to 50                                                                    !RALB 40
   99 continue                                                                    !RALB 41
      albnorth = albnorth + sum(albedo(nlat-1,1:nlon-1)/(nlon-1.0D0))             !RALB 42
      albsouth = sum(albedo(1,1:nlon-1)/(nlon-1.0D0))                             !RALB 43
      write (*, 110) ' albedo S,N = ', albsouth, albnorth                         !RALB 44
  110 format(a14,2f9.3)                                                           !RALB 45
!...    Set polar (90N and 90 S) albedo values                                    !RALB 46
      albedo(0,:nlon) = albsouth                                                  !RALB 47
      albedo(nlat,:nlon) = albnorth                                               !RALB 48
      write (22) albedo                                                           !RALB 49
      write (*, *) numread, ' data points processed'                              !RALB 50
      stop                                                                        !RALB 51
      end program readalb                                                         !RALB 52
