      module vast_kind_param                                        
         integer, parameter :: byte_log = selected_int_kind(2)      
         integer, parameter :: short_log = selected_int_kind(4)     
         integer, parameter :: long_log = selected_int_kind(18)     
         integer, parameter :: byte = selected_int_kind(2)          
         integer, parameter :: short = selected_int_kind(4)         
         integer, parameter :: long = selected_int_kind(18)         
         integer, parameter :: double = selected_real_kind(14)      
         integer, parameter :: extended = selected_real_kind(30)    
         integer, parameter :: double_ext = selected_real_kind(50)  
         integer, parameter :: dble_complex = selected_real_kind(14)
         integer, parameter :: ext_complex = selected_real_kind(30) 
      end module vast_kind_param                                    
!======================================================================
      module parameters_M10_C
      integer, parameter :: nhgt = 17 
      integer, parameter :: nhgtt = 19 
      integer, parameter :: nthgt = 30 
      integer, parameter :: nthgtt = 33 
      integer, parameter :: nlat = 25 
      integer, parameter :: nlatt = 36 
      integer, parameter :: ntlat = 25 
      integer, parameter :: nteslat = 25 
      integer, parameter :: nmtlat = 361 
      integer, parameter :: nalblat = 181 
      integer, parameter :: nlon = 40 
      integer, parameter :: ntlon = 40 
      integer, parameter :: nteslon = 40 
      integer, parameter :: nmtlon = 721 
      integer, parameter :: nalblon = 361 
      integer, parameter :: nbl = 3 
      integer, parameter :: ntbl = 3 
      integer, parameter :: nf10 = 2 
      integer, parameter :: ntf10 = 3 
      integer, parameter :: ntesy = 2 
      integer, parameter :: ntesls = 72 
      integer, parameter :: ndust = 3 
      integer, parameter :: npmax = 100000 
      integer, parameter :: nmapyear = 2 
      end module parameters_M10_C
!======================================================================
      module cosparnh_M10_C 
      USE vast_kind_param, ONLY:  double 
      real(double), dimension(164) :: zc, tc, pc, dc 
      end module cosparnh_M10_C 
!======================================================================
      module datacom_M10_C 
      USE vast_kind_param, ONLY:  double 
      integer :: mapyear, idaydata, npos, nvarx, nvary, logscale, iu0, iup,    &
         ipclat, molahgts 
      real(double) :: dtr, day, dustlat, dustlon, radmax, rref, als0, alsdur,  &
         intens, deltatex, rpscale, dusttau, dustmin, dustmax, dustod, dustnu, &
         dustdiam, dustdens, rwscale, wlscale, requa, rpole, wmscale, blwinfac 
      end module datacom_M10_C 
!======================================================================
      MODULE filename_M10_C 
      USE vast_kind_param, ONLY:  DOUBLE 
      CHARACTER(LEN=99) :: LSTFL, OUTFL 
      END MODULE filename_M10_C 
!======================================================================
      module interp_M10_C 
      USE vast_kind_param, ONLY:  double 
      integer :: ilat, jlon, ls, mdust, k1st, ilatw, ilatt, mf10 
      real(double) :: dlat, dlon, dls, ddust, dlatw, dlatt, df10, wpolefac,     &
         tpolefac 
      end module interp_M10_C 
!======================================================================
      module mgcmdata_M10_C 
      USE vast_kind_param, ONLY:  double
      USE parameters_M10_C
      real(double), dimension(nhgt,nlat,0:12,ndust) :: tza0, tza1, tzp1, tza2,  &
         tzp2, pza0, pza1, pzp1, pza2, pzp2, dza0, uza0, uza1, uzp1, uza2, uzp2 &
         , vza0, vza1, vzp1, vza2, vzp2 
      end module mgcmdata_M10_C 
!======================================================================
      module mgcmparm_M10_C 
      USE vast_kind_param, ONLY:  double
      USE parameters_M10_C
      real(double), dimension(ndust) :: dust 
      real(double), dimension(ntbl) :: dzbl 
      real(double), dimension(nf10) :: f10val 
      real(double), dimension(ntf10) :: f10tes 
      real(double) :: zwsfc 
      character(len=2), dimension(ndust) :: dustc 
      character(len=2), dimension(nf10) :: solact 
      character(len=2), dimension(ntesy) :: tesyr 
      character(len=2), dimension(ntf10) :: soltes 
      end module mgcmparm_M10_C 
!======================================================================
      module mgcmtes_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      real(double), dimension(nthgt,nlat,0:12,ntesy) :: ttza0, ttza1, ttzp1,   &
         ttza2, ttzp2, tdza0, tdza1, tdzp1, tdza2, tdzp2, tpza0, tuza0, tuza1, &
         tuzp1, tuza2, tuzp2, tvza0, tvza1, tvzp1, tvza2, tvzp2 
      end module mgcmtes_M10_C 
!======================================================================
      module pterp_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      real(double), dimension(npmax) :: phgt, plat, plon, ptmp, pprs, pden,    &
         puwn, pvwn 
      end module pterp_M10_C 
!======================================================================
      MODULE randcom_M10_C 
      USE vast_kind_param, ONLY:  DOUBLE 
      INTEGER :: IX, IY, IZ 
      END MODULE randcom_M10_C 
!======================================================================
      module surfdata_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      real(double), dimension(nbl,nlat,0:nlon,0:12,ndust) :: tsa0, tsa1, tsp1, &
        tsa2, tsp2, usa0, usa1, usp1, usa2, usp2, vsa0, vsa1, vsp1, vsa2, vsp2 
      end module surfdata_M10_C 
!======================================================================
      module surftes_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      real(double), dimension(ntbl,ntlat,0:ntlon,0:12,ntesy) :: ttsa0, ttsa1,  &
         ttsp1, ttsa2, ttsp2, tusa0, tusa1, tusp1, tusa2, tusp2, tvsa0, tvsa1, &
         tvsp1, tvsa2, tvsp2 
      end module surftes_M10_C 
!======================================================================
      module terhgt_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      real(double), dimension(0:nmtlat,0:nmtlon) :: areorad, topomola 
      real(double), dimension(0:nalblat,0:nalblon) :: albedo 
      end module terhgt_M10_C 
!======================================================================
      module tesdust_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      real(double), dimension(nmapyear,ntesls,nteslat,0:nteslon) :: testau 
      end module tesdust_M10_C 
!======================================================================
      module testerp_M10_C 
      USE vast_kind_param, ONLY:  double 
      integer :: ilat, jlon, ls, k1st, ilatw, jlonw, ilatt, mf10 
      real(double) :: dlat, dlon, dls, dlatw, dlonw, dlatt, df10, wpolefac,    &
         tpolefac 
      end module testerp_M10_C 
!======================================================================
      module tgcmdata_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      real(double), dimension(nhgtt,nlatt,0:12,ndust,nf10) :: tta0, tta1, ttp1 &
         , tta2, ttp2, pta0, pta1, ptp1, pta2, ptp2, dta0, dta1, dtp1, dta2,   &
         dtp2, uta0, uta1, utp1, uta2, utp2, vta0, vta1, vtp1, vta2, vtp2 
      real(double), dimension(nlatt,0:12,ndust,nf10) :: zfa0, zfa1, zfp1, zfa2 &
         , zfp2 
      end module tgcmdata_M10_C 
!======================================================================
      module tgcmoffset_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      integer :: ibougher 
      real(double), dimension(0:12,ndust) :: offsets 
      real(double), dimension(0:12,ntesy) :: toffsets 
      real(double) :: zoffset, hgtoffset, ofszl 
      end module tgcmoffset_M10_C 
!======================================================================
      module tgcmtes_M10_C 
      USE vast_kind_param, ONLY:  double 
      USE parameters_M10_C
      integer, dimension(0:12,ntesy,ntf10) :: iztop 
      real(double), dimension(nthgtt,nlatt,0:12,ntesy,ntf10) :: ttta0, ttta1,  &
         tttp1, ttta2, tttp2, tpta0, tpta1, tptp1, tpta2, tptp2, tdta0, tdta1, &
         tdtp1, tdta2, tdtp2, tuta0, tuta1, tutp1, tuta2, tutp2, tvta0, tvta1, &
         tvtp1, tvta2, tvtp2 
      real(double), dimension(nlatt,0:12,ntesy,ntf10) :: tzfa0, tzfa1, tzfp1,  &
         tzfa2, tzfp2 
      end module tgcmtes_M10_C 
!======================================================================
      module therm_M10_C 
      USE vast_kind_param, ONLY:  double 
      real(double), dimension(0:8) :: fmol 
      real(double) :: f107, stdl 
      end module therm_M10_C 
!======================================================================
      module wavecoef_M10_C 
      USE vast_kind_param, ONLY:  double 
      integer :: nwave, iuwave 
      real(double), dimension(100) :: wavetime 
      real(double), dimension(100,11) :: wavedata 
      real(double) :: wavea0, wavea1, wavephi1, wavea2, wavephi2, wavea3,    &
         wavephi3, wscale, phi1dot, phi2dot, phi3dot, wavedate 
      end module wavecoef_M10_C 
!======================================================================
