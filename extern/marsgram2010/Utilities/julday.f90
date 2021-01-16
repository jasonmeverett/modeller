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
      MODULE caltojul_I                                                           !VAST 15
      INTERFACE                                                                   !VAST 16
!...Generated by Pacific-Sierra Research 77to90  4.4G  11:22:09  03/26/10         !VAST 17
      SUBROUTINE caltojul (IY, IM, ID, IHOUR, IMIN, SEC, XJD)                     !VAST 18
      USE vast_kind_param,ONLY: DOUBLE                                            !VAST 19
      integer, INTENT(IN) :: IY                                                   !VAST 20
      integer, INTENT(IN) :: IM                                                   !VAST 21
      integer, INTENT(IN) :: ID                                                   !VAST 22
      integer, INTENT(IN) :: IHOUR                                                !VAST 23
      integer, INTENT(IN) :: IMIN                                                 !VAST 24
      real(DOUBLE), INTENT(IN) :: SEC                                             !VAST 25
      real(DOUBLE), INTENT(OUT) :: XJD                                            !VAST 26
      END SUBROUTINE                                                              !VAST 27
      END INTERFACE                                                               !VAST 28
      END MODULE                                                                  !VAST 29
!============================================================================     !VAST 30
      MODULE jultocal_I                                                           !VAST 31
      INTERFACE                                                                   !VAST 32
!...Generated by Pacific-Sierra Research 77to90  4.4G  11:22:09  03/26/10         !VAST 33
      SUBROUTINE jultocal (JD, YEAR, MONTH, DAY, DAYFRAC)                         !VAST 34
      USE vast_kind_param,ONLY: DOUBLE                                            !VAST 35
      REAL(DOUBLE), INTENT(IN) :: JD                                              !VAST 36
      INTEGER, INTENT(OUT) :: YEAR                                                !VAST 37
      INTEGER, INTENT(INOUT) :: MONTH                                             !VAST 38
      INTEGER, INTENT(OUT) :: DAY                                                 !VAST 39
      REAL(DOUBLE), INTENT(OUT) :: DAYFRAC                                        !VAST 40
      END SUBROUTINE                                                              !VAST 41
      END INTERFACE                                                               !VAST 42
      END MODULE                                                                  !VAST 43
                                                                                  !VAST 44
!============================================================================     !VAST 45
      program main                                                                !JDAY  1
!-----------------------------------------------                                  !JDAY  2
!   M o d u l e s                                                                 !JDAY  3
!-----------------------------------------------                                  !JDAY  4
      USE vast_kind_param, ONLY:  double                                          !JDAY  5
!...  Program to calculate Julian day from year, month, day input, or             !JDAY  6
!     calculate year, month, day, & day fraction from Julian day input            !JDAY  7
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:22:09  03/26/10        !JDAY  8
!...Switches:                                                                     !JDAY  9
!-----------------------------------------------                                  !JDAY 10
!   I n t e r f a c e   B l o c k s                                               !JDAY 11
!-----------------------------------------------                                  !JDAY 12
      use caltojul_I                                                              !JDAY 13
      use jultocal_I                                                              !JDAY 14
      implicit none                                                               !JDAY 15
!-----------------------------------------------                                  !JDAY 16
!   L o c a l   V a r i a b l e s                                                 !JDAY 17
!-----------------------------------------------                                  !JDAY 18
      integer :: iyr, month, iday, ihr, min, ioption                              !JDAY 19
      real(double) :: day, sec, dayfrac                                           !JDAY 20
!-----------------------------------------------                                  !JDAY 21
      write (*, *) ' Enter 1 for Calendar date to Julian day, 2 for ',     &      !JDAY 22
         'Julian day to Calendar date'                                            !JDAY 23
      read (*, *) ioption                                                         !JDAY 24
      if (ioption <= 1) then                                                      !JDAY 25
   10    continue                                                                 !JDAY 26
         write (*, *) ' Enter year (4-digit), month, day'                         !JDAY 27
         read (*, *) iyr, month, iday                                             !JDAY 28
         if (iyr <= 0) go to 99                                                   !JDAY 29
         write (*, *) ' Enter hours, minutes, seconds'                            !JDAY 30
         read (*, *) ihr, min, sec                                                !JDAY 31
         call caltojul (iyr, month, iday, ihr, min, sec, day)                     !JDAY 32
         write (*, 20) day                                                        !JDAY 33
   20    format(' Julian day =',f14.5)                                            !JDAY 34
         go to 10                                                                 !JDAY 35
      endif                                                                       !JDAY 36
   30 continue                                                                    !JDAY 37
      write (*, *) ' Enter Julian day (xxxxxxx.xxxxx)'                            !JDAY 38
      read (*, *) day                                                             !JDAY 39
      if (day <= 0.0) go to 99                                                    !JDAY 40
      call jultocal (day, iyr, month, iday, dayfrac)                              !JDAY 41
      write (*, 40) iyr, month, iday, dayfrac                                     !JDAY 42
   40 format(' Year/Month/Day = ',i4,2i3,' DayFraction = ',f9.5)                  !JDAY 43
      go to 30                                                                    !JDAY 44
   99 continue                                                                    !JDAY 45
      stop                                                                        !JDAY 46
      end program main                                                            !JDAY 47
!                                                                                 !JDAY 48
!----------------------------------------------------------------------           !CTOJ  1
      SUBROUTINE CALTOJUL(IY, IM, ID, IHOUR, IMIN, SEC, XJD)                      !CTOJ  2
!-----------------------------------------------                                  !CTOJ  3
!   M o d u l e s                                                                 !CTOJ  4
!-----------------------------------------------                                  !CTOJ  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !CTOJ  6
!                                                                                 !CTOJ  7
!     Compute Julian day (Real*8) by method of Meeus, Astronomical                !CTOJ  8
!       Algorithms, 2nd Edition, 1998, page 61. Inputs are year iY,               !CTOJ  9
!       month iM, day of month iD, and time of day in hours, minutes,             !CTOJ 10
!       and seconds (all integer except seconds).  Output is Real*8               !CTOJ 11
!       Julian day, xJD.                                                          !CTOJ 12
!                                                                                 !CTOJ 13
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:22:09  03/26/10        !CTOJ 14
!...Switches:                                                                     !CTOJ 15
      IMPLICIT NONE                                                               !CTOJ 16
!-----------------------------------------------                                  !CTOJ 17
!   D u m m y   A r g u m e n t s                                                 !CTOJ 18
!-----------------------------------------------                                  !CTOJ 19
      INTEGER , INTENT(IN) :: IY                                                  !CTOJ 20
      INTEGER , INTENT(IN) :: IM                                                  !CTOJ 21
      INTEGER , INTENT(IN) :: ID                                                  !CTOJ 22
      INTEGER , INTENT(IN) :: IHOUR                                               !CTOJ 23
      INTEGER , INTENT(IN) :: IMIN                                                !CTOJ 24
      REAL(DOUBLE) , INTENT(IN) :: SEC                                            !CTOJ 25
      REAL(DOUBLE) , INTENT(OUT) :: XJD                                           !CTOJ 26
!-----------------------------------------------                                  !CTOJ 27
!   L o c a l   V a r i a b l e s                                                 !CTOJ 28
!-----------------------------------------------                                  !CTOJ 29
      INTEGER :: Y, M, A, B                                                       !CTOJ 30
      REAL(DOUBLE) :: D                                                           !CTOJ 31
!-----------------------------------------------                                  !CTOJ 32
      Y = IY                                                                      !CTOJ 33
      M = IM                                                                      !CTOJ 34
!...  Consider Jan or Feb as if months 13 and 14 of previous year                 !CTOJ 35
      IF (IM <= 2) THEN                                                           !CTOJ 36
         Y = IY - 1                                                               !CTOJ 37
         M = IM + 12                                                              !CTOJ 38
      ENDIF                                                                       !CTOJ 39
!...  Compute day of month plus fractional part                                   !CTOJ 40
      D = ID + IHOUR/2.4D1 + IMIN/1.440D3 + SEC/8.64D4                            !CTOJ 41
      A = IDINT(Y/100.0D0)                                                        !CTOJ 42
      B = 2 - A + IDINT(A/4.0D0)                                                  !CTOJ 43
!...  Compute Julian day with fractional part                                     !CTOJ 44
      XJD = IDINT(365.25D0*(Y + 4716)) + IDINT(30.6001D0*(M + 1)) + D + B - &     !CTOJ 45
         1524.5D0                                                                 !CTOJ 46
      RETURN                                                                      !CTOJ 47
      END SUBROUTINE CALTOJUL                                                     !CTOJ 48
!                                                                                 !CTOJ 49
!----------------------------------------------------------------------           !JCAL  1
      SUBROUTINE JULTOCAL(JD, YEAR, MONTH, DAY, DAYFRAC)                          !JCAL  2
!-----------------------------------------------                                  !JCAL  3
!   M o d u l e s                                                                 !JCAL  4
!-----------------------------------------------                                  !JCAL  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !JCAL  6
!...    Compute year, month, day of month, and fraction of day                    !JCAL  7
!       from Julian day JD.  Algorithm from Meeus, Astronomical                   !JCAL  8
!       Algorithms, 2nd edition, 1998, page 63                                    !JCAL  9
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:22:09  03/26/10        !JCAL 10
!...Switches:                                                                     !JCAL 11
      IMPLICIT NONE                                                               !JCAL 12
!-----------------------------------------------                                  !JCAL 13
!   D u m m y   A r g u m e n t s                                                 !JCAL 14
!-----------------------------------------------                                  !JCAL 15
      INTEGER , INTENT(OUT) :: YEAR                                               !JCAL 16
      INTEGER , INTENT(INOUT) :: MONTH                                            !JCAL 17
      INTEGER , INTENT(OUT) :: DAY                                                !JCAL 18
      REAL(DOUBLE) , INTENT(IN) :: JD                                             !JCAL 19
      REAL(DOUBLE) , INTENT(OUT) :: DAYFRAC                                       !JCAL 20
!-----------------------------------------------                                  !JCAL 21
!   L o c a l   V a r i a b l e s                                                 !JCAL 22
!-----------------------------------------------                                  !JCAL 23
      INTEGER :: Z, A, ALPHA, B, C, D, E                                          !JCAL 24
      REAL(DOUBLE) :: F, JDP                                                      !JCAL 25
!-----------------------------------------------                                  !JCAL 26
!...    Add 0.5 to Julian day                                                     !JCAL 27
      JDP = JD + 0.5D0                                                            !JCAL 28
!...    Get integer part and fractional part of JD+0.5                            !JCAL 29
      Z = INT(JDP)                                                                !JCAL 30
      F = JDP - Z                                                                 !JCAL 31
!...    Compute parameter A                                                       !JCAL 32
      IF (Z < 2299161) THEN                                                       !JCAL 33
         A = Z                                                                    !JCAL 34
      ELSE                                                                        !JCAL 35
         ALPHA = IDINT((Z - 1867216.25D0)/36524.25D0)                             !JCAL 36
         A = Z + 1 + ALPHA - IDINT(ALPHA/4.0D0)                                   !JCAL 37
      ENDIF                                                                       !JCAL 38
!...    Compute parameters B, C, D, and E                                         !JCAL 39
      B = A + 1524                                                                !JCAL 40
      C = IDINT((B - 122.1D0)/365.25D0)                                           !JCAL 41
      D = IDINT(365.25D0*C)                                                       !JCAL 42
      E = IDINT((B - D)/30.6001D0)                                                !JCAL 43
!...    Get integer day of month and fractional day from parameters               !JCAL 44
!         B, D, E, and F                                                          !JCAL 45
      DAYFRAC = B - D - IDINT(30.6001D0*E) + F                                    !JCAL 46
      DAY = IDINT(DAYFRAC)                                                        !JCAL 47
      DAYFRAC = DAYFRAC - DAY                                                     !JCAL 48
!...    Get month from parameter E                                                !JCAL 49
      IF (E < 2) THEN                                                             !JCAL 50
         STOP ' Bad month parameter E: too small'                                 !JCAL 51
      ELSE IF (E < 14) THEN                                                       !JCAL 52
         MONTH = E - 1                                                            !JCAL 53
      ELSE IF (E < 16) THEN                                                       !JCAL 54
         MONTH = E - 13                                                           !JCAL 55
      ELSE                                                                        !JCAL 56
         STOP ' Bad month parameter E: too large'                                 !JCAL 57
      ENDIF                                                                       !JCAL 58
!...    Get year from parameter C                                                 !JCAL 59
      IF (MONTH > 2) THEN                                                         !JCAL 60
         YEAR = C - 4716                                                          !JCAL 61
      ELSE                                                                        !JCAL 62
         YEAR = C - 4715                                                          !JCAL 63
      ENDIF                                                                       !JCAL 64
      RETURN                                                                      !JCAL 65
      END SUBROUTINE JULTOCAL                                                     !JCAL 66
!----------------------------------------------------------------------           !JCAL 67
