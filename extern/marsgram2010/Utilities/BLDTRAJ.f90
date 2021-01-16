      program main                                                                !BLDT  1
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:01:59  03/26/10        !BLDT  2
!...Switches:                                                                     !BLDT  3
      implicit none                                                               !BLDT  4
!-----------------------------------------------                                  !BLDT  5
!   L o c a l   V a r i a b l e s                                                 !BLDT  6
!-----------------------------------------------                                  !BLDT  7
      integer :: numz, numlat, numlon, ntime, iz, ilat, ilon                      !BLDT  8
      real :: lat1, lat2, lon1, lon2, z1, z2, dz, dlat, dlon, dt, time, z,   &    !BLDT  9
         xlat, xlon                                                               !BLDT 10
      character :: filename*60                                                    !BLDT 11
!-----------------------------------------------                                  !BLDT 12
      write (*, *) ' Enter trajectory file name'                                  !BLDT 13
      read (*, 5) filename                                                        !BLDT 14
    5 format(a)                                                                   !BLDT 15
      open(21, file=filename, position='asis')                                    !BLDT 16
      write (*, *) ' Enter z1,z2,dz (Real km)'                                    !BLDT 17
      read (*, *) z1, z2, dz                                                      !BLDT 18
      if (dz == 0.0) then                                                         !BLDT 19
         z2 = z1                                                                  !BLDT 20
         dz = 1.                                                                  !BLDT 21
      endif                                                                       !BLDT 22
      dz = sign(dz,z2 - z1)                                                       !BLDT 23
      numz = int(1.5 + abs((z2 - z1)/dz))                                         !BLDT 24
      write (*, *) ' Enter lat1,lat2,dlat (Real deg.)'                            !BLDT 25
      read (*, *) lat1, lat2, dlat                                                !BLDT 26
      if (abs(lat1) > 90.) lat1 = sign(90.,lat1)                                  !BLDT 27
      if (abs(lat2) > 90.) lat2 = sign(90.,lat2)                                  !BLDT 28
      if (dlat == 0.0) then                                                       !BLDT 29
         lat2 = lat1                                                              !BLDT 30
         dlat = 1.                                                                !BLDT 31
      endif                                                                       !BLDT 32
      dlat = sign(dlat,lat2 - lat1)                                               !BLDT 33
      numlat = int(1.5 + abs((lat2 - lat1)/dlat))                                 !BLDT 34
      write (*, *) ' Enter lon1,lon2,dlon (Real deg.)'                            !BLDT 35
      read (*, *) lon1, lon2, dlon                                                !BLDT 36
      if (abs(lon1) > 360.) lon1 = sign(360.,lon1)                                !BLDT 37
      if (abs(lon2) > 360.) lon2 = sign(360.,lon2)                                !BLDT 38
      if (dlon == 0.0) then                                                       !BLDT 39
         lon2 = lon1                                                              !BLDT 40
         dlon = 1.                                                                !BLDT 41
      endif                                                                       !BLDT 42
      dlon = sign(dlon,lon2 - lon1)                                               !BLDT 43
      numlon = int(1.5 + abs((lon2 - lon1)/dlon))                                 !BLDT 44
      write (*, *) ' Enter time increment (Real sec)'                             !BLDT 45
      read (*, *) dt                                                              !BLDT 46
      time = 0.                                                                   !BLDT 47
      ntime = 0                                                                   !BLDT 48
      write (*, 9) numz, numlat, numlon, numz*numlat*numlon                       !BLDT 49
    9 format(' Number height, lat, lon, total =',3i5,i7)                          !BLDT 50
      do iz = 1, numz                                                             !BLDT 51
         z = z1 + (iz - 1)*dz                                                     !BLDT 52
         do ilat = 1, numlat                                                      !BLDT 53
            xlat = lat1 + (ilat - 1)*dlat                                         !BLDT 54
            do ilon = 1, numlon                                                   !BLDT 55
               xlon = lon1 + (ilon - 1)*dlon                                      !BLDT 56
               ntime = ntime + 1                                                  !BLDT 57
               time = ntime*dt                                                    !BLDT 58
               write (21, 20) time, z, xlat, xlon                                 !BLDT 59
            end do                                                                !BLDT 60
         end do                                                                   !BLDT 61
      end do                                                                      !BLDT 62
   20 format(4f10.2)                                                              !BLDT 63
      write (*, *) ' Data written to file ', filename                             !BLDT 64
      stop                                                                        !BLDT 65
      end program main                                                            !BLDT 66
