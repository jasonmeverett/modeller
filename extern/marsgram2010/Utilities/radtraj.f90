      program radtraj                                                             !RADT   1
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:22:09  03/26/10        !RADT   2
!...Switches:                                                                     !RADT   3
      implicit none                                                               !RADT   4
!-----------------------------------------------                                  !RADT   5
!   L o c a l   V a r i a b l e s                                                 !RADT   6
!-----------------------------------------------                                  !RADT   7
      integer :: numz, numlat, numlon, numtime, ntime, ilat, ilon, iz             !RADT   8
      real :: lat1, lat2, lon1, lon2, z1, z2, dz, dlat, dlon, dt, time, xlat, &   !RADT   9
         xlon, z                                                                  !RADT  10
      character :: filename*60                                                    !RADT  11
!-----------------------------------------------                                  !RADT  12
      write (*, *) ' Enter rad trajectory file name'                              !RADT  13
      read (*, 2) filename                                                        !RADT  14
    2 format(a)                                                                   !RADT  15
      open(21, file=filename, position='asis')                                    !RADT  16
      write (*, *) ' Enter z1 or -10 for starting height'                         !RADT  17
      read (*, *) z1                                                              !RADT  18
    5 continue                                                                    !RADT  19
      write (*, *) ' Enter z2,dz (Real km)'                                       !RADT  20
      read (*, *) z2, dz                                                          !RADT  21
      if (dz<=0.0 .or. z2<=z1) then                                               !RADT  22
         write (*, *) ' dz must be > 0'                                           !RADT  23
         go to 5                                                                  !RADT  24
      endif                                                                       !RADT  25
      numz = int(1.5 + abs((z2 - z1)/dz))                                         !RADT  26
      write (*, *) ' Enter lat1,lat2,dlat (Real deg.)'                            !RADT  27
      read (*, *) lat1, lat2, dlat                                                !RADT  28
      if (abs(lat1) > 90.) lat1 = sign(90.,lat1)                                  !RADT  29
      if (abs(lat2) > 90.) lat2 = sign(90.,lat2)                                  !RADT  30
      if (dlat == 0.0) then                                                       !RADT  31
         lat2 = lat1                                                              !RADT  32
         dlat = 1.                                                                !RADT  33
      endif                                                                       !RADT  34
      dlat = sign(dlat,lat2 - lat1)                                               !RADT  35
      numlat = int(1.5 + abs((lat2 - lat1)/dlat))                                 !RADT  36
      write (*, *) ' Enter lon1,lon2,dlon (Real deg.)'                            !RADT  37
      read (*, *) lon1, lon2, dlon                                                !RADT  38
      if (abs(lon1) > 360.) lon1 = sign(360.,lon1)                                !RADT  39
      if (abs(lon2) > 360.) lon2 = sign(360.,lon2)                                !RADT  40
      if (dlon == 0.0) then                                                       !RADT  41
         lon2 = lon1                                                              !RADT  42
         dlon = 1.                                                                !RADT  43
      endif                                                                       !RADT  44
      dlon = sign(dlon,lon2 - lon1)                                               !RADT  45
      numlon = int(1.5 + abs((lon2 - lon1)/dlon))                                 !RADT  46
      write (*, *) ' Enter time increment (Real sec)'                             !RADT  47
      read (*, *) dt                                                              !RADT  48
      numtime = 1                                                                 !RADT  49
      time = 0.                                                                   !RADT  50
      if (numlat==1 .and. numlon==1) then                                         !RADT  51
         write (*, *) ' Enter number of times'                                    !RADT  52
         read (*, *) numtime                                                      !RADT  53
         write (*, 8) numz, numtime, numz*numtime                                 !RADT  54
    8    format(' Number height, time, total =',2i5,i7)                           !RADT  55
      else                                                                        !RADT  56
         write (*, 9) numz, numlat, numlon, numz*numlat*numlon                    !RADT  57
    9    format(' Number height, lat, lon, total =',3i5,i7)                       !RADT  58
      endif                                                                       !RADT  59
      do ntime = 1, numtime                                                       !RADT  60
         do ilat = 1, numlat                                                      !RADT  61
            xlat = lat1 + (ilat - 1)*dlat                                         !RADT  62
            do ilon = 1, numlon                                                   !RADT  63
               xlon = lon1 + (ilon - 1)*dlon                                      !RADT  64
               do iz = 1, numz                                                    !RADT  65
                  z = z1 + (iz - 1)*dz                                            !RADT  66
                  write (21, 30) time, z, xlat, xlon                              !RADT  67
               end do                                                             !RADT  68
               time = ntime*dt                                                    !RADT  69
            end do                                                                !RADT  70
         end do                                                                   !RADT  71
      end do                                                                      !RADT  72
   30 format(f12.2,3f10.2)                                                        !RADT  73
      write (*, *) ' Data written to file ', filename                             !RADT  74
      stop                                                                        !RADT  75
      end program radtraj                                                         !RADT  76
