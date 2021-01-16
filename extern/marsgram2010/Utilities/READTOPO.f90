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
!===========================================================================      !VAST 14
      program main                                                                !RTOP  1
!-----------------------------------------------                                  !RTOP  2
!   M o d u l e s                                                                 !RTOP  3
!-----------------------------------------------                                  !RTOP  4
      USE vast_kind_param, ONLY:  double                                          !RTOP  5
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:53:17  03/23/10        !RTOP  6
!...Switches:                                                                     !RTOP  7
      implicit none                                                               !RTOP  8
!-----------------------------------------------                                  !RTOP  9
!   L o c a l   P a r a m e t e r s                                               !RTOP 10
!-----------------------------------------------                                  !RTOP 11
      integer, parameter :: nlat = 361                                            !RTOP 12
      integer, parameter :: nlon = 721                                            !RTOP 13
!-----------------------------------------------                                  !RTOP 14
!   L o c a l   V a r i a b l e s                                                 !RTOP 15
!-----------------------------------------------                                  !RTOP 16
      integer :: numread, num, ilat, jlon                                         !RTOP 17
      real(double) :: prad, areoid, topo, areonorth, areosouth, alat, alon, &     !RTOP 18
         toponorth, toposouth, steplat, steplon                                   !RTOP 19
      real(double), dimension(0:nlat,0:nlon) :: areorad, topomola                 !RTOP 20
!-----------------------------------------------                                  !RTOP 21
!...    Parameter (nlat = 181)                                                    !RTOP 22
!...    Parameter (nlon = 361)                                                    !RTOP 23
      open(21, file='MOLATOPH.TXT', status='old', position='asis')                !RTOP 24
      open(22, file='molatoph.bin', form='unformatted', position='asis')          !RTOP 25
      steplat = 180.0D0/(nlat - 1.0D0)                                            !RTOP 26
      steplon = 360.0D0/(nlon - 1.0D0)                                            !RTOP 27
      areonorth = 0.0D0                                                           !RTOP 28
      areosouth = 0.0D0                                                           !RTOP 29
      toponorth = 0.0D0                                                           !RTOP 30
      toposouth = 0.0D0                                                           !RTOP 31
      numread = 0                                                                 !RTOP 32
   10 format(2f8.1,2f12.2,f10.2,i6)                                               !RTOP 33
   50 continue                                                                    !RTOP 34
      read (21, 10, err=50, end=99) alon, alat, prad, areoid, topo, num           !RTOP 35
!...    Convert to West longitude                                                 !RTOP 36
      alon = 360.0D0 - alon                                                       !RTOP 37
      jlon = idint(1.0D0 + alon/steplon)                                          !RTOP 38
      ilat = idint(1.0D0 + (90.0D0 + alat)/steplat)                               !RTOP 39
      if (jlon<1 .or. jlon>nlon-1) stop ' Bad lon'                                !RTOP 40
      if (ilat<1 .or. ilat>nlat-1) stop ' Bad lat'                                !RTOP 41
      areorad(ilat,jlon) = areoid/1.0D3                                           !RTOP 42
      topomola(ilat,jlon) = topo/1.0D3                                            !RTOP 43
!...    Set areoid and topo height values near 0 longitude                        !RTOP 44
      if (jlon == nlon - 1) then                                                  !RTOP 45
         areorad(ilat,0) = areoid/1.0D3                                           !RTOP 46
         topomola(ilat,0) = topo/1.0D3                                            !RTOP 47
      endif                                                                       !RTOP 48
      if (jlon == 1) then                                                         !RTOP 49
         areorad(ilat,nlon) = areoid/1.0D3                                        !RTOP 50
         topomola(ilat,nlon) = topo/1.0D3                                         !RTOP 51
      endif                                                                       !RTOP 52
      numread = numread + 1                                                       !RTOP 53
      go to 50                                                                    !RTOP 54
   99 continue                                                                    !RTOP 55
      areonorth = sum(areorad(nlat-1,1:nlon-1)/(nlon-1.0D0))                      !RTOP 56
      areosouth = sum(areorad(1,1:nlon-1)/(nlon-1.0D0))                           !RTOP 57
      toponorth = sum(topomola(nlat-1,1:nlon-1)/(nlon-1.0D0))                     !RTOP 58
      toposouth = sum(topomola(1,1:nlon-1)/(nlon-1.0D0))                          !RTOP 59
      write (*, 110) ' areoid, topoheight S =', areosouth, toposouth              !RTOP 60
      write (*, 110) ' areoid, topoheight N =', areonorth, toponorth              !RTOP 61
  110 format(a24,f13.5,f11.5)                                                     !RTOP 62
!...    Set polar areoid and topo height values                                   !RTOP 63
      areorad(0,:nlon) = areosouth                                                !RTOP 64
      topomola(0,:nlon) = toposouth                                               !RTOP 65
      areorad(nlat,:nlon) = areonorth                                             !RTOP 66
      topomola(nlat,:nlon) = toponorth                                            !RTOP 67
      write (22) areorad, topomola                                                !RTOP 68
      write (*, *) numread, ' data points processed'                              !RTOP 69
      stop                                                                        !RTOP 70
      end program main                                                            !RTOP 71
