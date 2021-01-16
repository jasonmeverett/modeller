      MODULE atmos2_M10_I   
      INTERFACE
      SUBROUTINE atmos2_M10 (HGTIN, CLAT, CLON, MARSAU, SUNLAT, SUNLON, ALS, H  &
         , TEMP, DENST, UPFCTR, LWFCTR, PRES, THGT, CAREOID, ZF, IU0, DELTATEX  &
         , TEXOS, TBASE, HRHO, AMZ, DUSTTAU, DUSTMIN, DUSTMAX, DUSTOD, EWWIND   &
         , NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, HATZF, WAVEPERT, TEMPDAY     &
         , PRESDAY, DENSDAY, EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, HGTASFC, PATSURF &
         , TEMPMAX, TEMPMIN, DENSMAX, DENSMIN, TGRND, TALB, ICEPOLAR, GZ        &
         , OLDRREF, REQUA, RPOLE, MAPYEAR, PROFNEAR, PROFFAR, NPROF, PROFWGT    &
         , IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: HGTIN 
      REAL(DOUBLE), INTENT(IN) :: CLAT 
      REAL(DOUBLE), INTENT(IN) :: CLON 
      REAL(DOUBLE), INTENT(IN) :: MARSAU 
      REAL(DOUBLE), INTENT(IN) :: SUNLAT 
      REAL(DOUBLE), INTENT(IN) :: SUNLON 
      REAL(DOUBLE), INTENT(IN) :: ALS 
      REAL(DOUBLE), INTENT(INOUT) :: H 
      REAL(DOUBLE), INTENT(INOUT) :: TEMP 
      REAL(DOUBLE), INTENT(INOUT) :: DENST 
      REAL(DOUBLE), INTENT(OUT) :: UPFCTR 
      REAL(DOUBLE), INTENT(OUT) :: LWFCTR 
      REAL(DOUBLE), INTENT(INOUT) :: PRES 
      REAL(DOUBLE), INTENT(INOUT) :: THGT 
      REAL(DOUBLE), INTENT(INOUT) :: CAREOID 
      REAL(DOUBLE), INTENT(INOUT) :: ZF 
      INTEGER, INTENT(IN) :: IU0 
      REAL(DOUBLE), INTENT(IN) :: DELTATEX 
      REAL(DOUBLE), INTENT(OUT) :: TEXOS 
      REAL(DOUBLE), INTENT(OUT) :: TBASE 
      REAL(DOUBLE), INTENT(OUT) :: HRHO 
      REAL(DOUBLE), INTENT(OUT) :: AMZ 
      REAL(DOUBLE), INTENT(IN) :: DUSTTAU 
      REAL(DOUBLE), INTENT(IN) :: DUSTMIN 
      REAL(DOUBLE), INTENT(IN) :: DUSTMAX 
      REAL(DOUBLE), INTENT(INOUT) :: DUSTOD 
      REAL(DOUBLE), INTENT(OUT) :: EWWIND 
      REAL(DOUBLE), INTENT(OUT) :: NSWIND 
      REAL(DOUBLE), INTENT(OUT) :: BLWINDEW 
      REAL(DOUBLE), INTENT(OUT) :: BLWINDNS 
      REAL(DOUBLE), INTENT(OUT) :: BLWINDVERT 
      REAL(DOUBLE), INTENT(INOUT) :: HATZF 
      REAL(DOUBLE), INTENT(IN) :: WAVEPERT 
      REAL(DOUBLE), INTENT(OUT) :: TEMPDAY 
      REAL(DOUBLE), INTENT(INOUT) :: PRESDAY 
      REAL(DOUBLE), INTENT(INOUT) :: DENSDAY 
      REAL(DOUBLE), INTENT(OUT) :: EWWNDAY 
      REAL(DOUBLE), INTENT(OUT) :: NSWNDAY 
      REAL(DOUBLE), INTENT(OUT) :: BLUDAY 
      REAL(DOUBLE), INTENT(OUT) :: BLVDAY 
      REAL(DOUBLE), INTENT(IN) :: HGTASFC 
      REAL(DOUBLE), INTENT(OUT) :: PATSURF 
      REAL(DOUBLE), INTENT(OUT) :: TEMPMAX 
      REAL(DOUBLE), INTENT(OUT) :: TEMPMIN 
      REAL(DOUBLE), INTENT(INOUT) :: DENSMAX 
      REAL(DOUBLE), INTENT(INOUT) :: DENSMIN 
      REAL(DOUBLE), INTENT(OUT) :: TGRND 
      REAL(DOUBLE), INTENT(OUT) :: TALB 
      INTEGER, INTENT(INOUT) :: ICEPOLAR 
      REAL(DOUBLE), INTENT(INOUT) :: GZ 
      REAL(DOUBLE), INTENT(INOUT) :: OLDRREF 
      REAL(DOUBLE), INTENT(IN) :: REQUA 
      REAL(DOUBLE), INTENT(IN) :: RPOLE 
      INTEGER, INTENT(IN) :: MAPYEAR 
      REAL(DOUBLE), INTENT(IN) :: PROFNEAR 
      REAL(DOUBLE), INTENT(IN) :: PROFFAR 
      INTEGER, INTENT(INOUT) :: NPROF 
      REAL(DOUBLE), INTENT(INOUT) :: PROFWGT 
      INTEGER, INTENT(IN) :: IDAYDATA 
!VAST...Calls: TESOD_M10, DUSTVSLS_M10, DUSTFACT_M10, RELLIPS_M10
!VAST...Calls: MARSGCM_M10, TESGCM_M10, STEWART2_M10, CP_M10
!VAST...Calls: PROFTERP_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!=================================================================================
      MODULE bltp_M10_I   
      INTERFACE
      SUBROUTINE bltp_M10 (GZ, CP, TG, Z5, T5, U5, V5, ZEVAL, FACTOR, TEMPZ) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: GZ 
      REAL(DOUBLE), INTENT(IN) :: CP 
      REAL(DOUBLE), INTENT(IN) :: TG 
      REAL(DOUBLE), INTENT(IN) :: Z5 
      REAL(DOUBLE), INTENT(IN) :: T5 
      REAL(DOUBLE), INTENT(IN) :: U5 
      REAL(DOUBLE), INTENT(IN) :: V5 
      REAL(DOUBLE), INTENT(IN) :: ZEVAL 
      REAL(DOUBLE), INTENT(IN) :: FACTOR 
      REAL(DOUBLE), INTENT(OUT) :: TEMPZ 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================= 
      MODULE caltojul_M10_I   
      INTERFACE
      SUBROUTINE caltojul_M10 (IY, IM, ID, IHOUR, IMIN, SEC, XJD) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: IY 
      integer, INTENT(IN) :: IM 
      integer, INTENT(IN) :: ID 
      integer, INTENT(IN) :: IHOUR 
      integer, INTENT(IN) :: IMIN 
      real(DOUBLE), INTENT(IN) :: SEC 
      real(DOUBLE), INTENT(OUT) :: XJD 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE chkdt_M10_I   
      INTERFACE
      SUBROUTINE chkdt_M10 (MYEAR, MONTH, IDAY, IHOUR, MINUTES, SEC, ERR) 
      USE vast_kind_param,ONLY: DOUBLE 
      INTEGER, INTENT(INOUT) :: MYEAR 
      INTEGER, INTENT(IN) :: MONTH 
      INTEGER, INTENT(IN) :: IDAY 
      INTEGER, INTENT(IN) :: IHOUR 
      INTEGER, INTENT(IN) :: MINUTES 
      REAL(DOUBLE), INTENT(IN) :: SEC 
      INTEGER, INTENT(OUT) :: ERR 
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE cospar_M10_I   
      INTERFACE
      SUBROUTINE cospar_M10 (Z, T, P, RHO) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: Z 
      REAL(DOUBLE), INTENT(OUT) :: T 
      REAL(DOUBLE), INTENT(OUT) :: P 
      REAL(DOUBLE), INTENT(OUT) :: RHO 
!VAST.../COSPARNH_M10/ ZC(IN), TC(IN), PC(IN), DC(IN)
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE cp_M10_I   
      INTERFACE
      REAL(KIND(0.0D0)) FUNCTION cp_M10 (T) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: T 
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE datastep_M10_I
      INTERFACE
      SUBROUTINE datastep_M10(I,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,     &
     & RHOv,RHOw,EOF,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,     &
     & DENS,DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,VWpert,Hrho,       &
     & HSCALE,dsunlat,dsunlon,dsunLs,dradau,dowlt,LonEW,corlim,DENSTOT, &
     & numwave,hgtasfc,IERT,IUTC,pertstep,corlmin,iupdate,ALS,szang,    &
     & owlt,sunlat,sunlon,MarsAU,TLOCAL,profnear,proffar,nprof)
      USE vast_kind_param,ONLY: DOUBLE 
      INTEGER, INTENT(IN) :: I,LonEW,IERT,IUTC
      INTEGER, INTENT(INOUT) :: iupdate,numwave,nprof
      INTEGER, INTENT(INOUT) :: EOF
      Real(Double), INTENT(IN) :: DAY0,dsunlat,dsunlon,dsunLs,dradau,dowlt,corlmin
      Real(Double), INTENT(OUT) :: TEMP,PRES,DENSLO,DENS,DENSHI,DENSP,EWWIND
      Real(Double), INTENT(OUT) :: EWpert,VWpert,Hrho,HSCALE,corlim,DENSTOT
      Real(Double), INTENT(OUT) :: hgtasfc,pertstep,ALS,szang,owlt
      Real(Double), INTENT(OUT) :: sunlat,sunlon,NSWIND,NSpert,MARSAU,TLOCAL
      Real(Double), INTENT(OUT) :: profnear,proffar
      Real(Double), INTENT(INOUT) :: CHGT,CLAT,CLON,CSEC,DELHGT,DELLAT,DELLON
      Real(Double), INTENT(INOUT) :: DELTIME,RHOd,RHOu,RHOv,RHOw
      END SUBROUTINE
      END INTERFACE
      END MODULE
!================================================================================ 
      MODULE dustfact_M10_I   
      INTERFACE
      SUBROUTINE dustfact_M10 (CLAT, CLON, ALS, DUSTM, STORMDZ) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: CLAT 
      real(DOUBLE), INTENT(IN) :: CLON 
      real(DOUBLE), INTENT(IN) :: ALS 
      real(DOUBLE), INTENT(OUT) :: DUSTM 
      real(DOUBLE), INTENT(OUT) :: STORMDZ 
!VAST.../DATACOM_M10/ DTR(IN), DUSTLAT(IN), DUSTLON(IN), RADMAX(IN)
!VAST.../DATACOM_M10/ RREF(IN), ALS0(IN), ALSDUR(IN), INTENS(IN)
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE dustvsls_M10_I   
      INTERFACE
      REAL(KIND(0.0D0)) FUNCTION dustvsls_M10 (ALS, DUSTMIN, DUSTMAX) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: ALS 
      real(DOUBLE), INTENT(IN) :: DUSTMIN 
      real(DOUBLE), INTENT(IN) :: DUSTMAX 
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE escalc_M10_I   
      INTERFACE
      SUBROUTINE escalc_M10 (STDL, SIGMA, ES) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: STDL 
      real(DOUBLE), INTENT(IN) :: SIGMA 
      real(DOUBLE), DIMENSION(0:11), INTENT(OUT) :: ES 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE fourd_M10_I   
      INTERFACE
      SUBROUTINE fourd_M10 (DX, DY, DZ, DQ, ARRAY, VALUE, LINT) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: DX 
      REAL(DOUBLE), INTENT(IN) :: DY 
      REAL(DOUBLE), INTENT(IN) :: DZ 
      REAL(DOUBLE), INTENT(IN) :: DQ 
      REAL(DOUBLE), DIMENSION(2,2,2,2), INTENT(IN) :: ARRAY 
      REAL(DOUBLE), INTENT(OUT) :: VALUE 
      INTEGER, INTENT(IN) :: LINT 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE geocenttogeodet_M10_I   
      INTERFACE
      SUBROUTINE geocenttogeodet_M10 (R, ZIN, FI, H, A, B) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: R 
      REAL(DOUBLE), INTENT(IN) :: ZIN 
      REAL(DOUBLE), INTENT(OUT) :: FI 
      REAL(DOUBLE), INTENT(OUT) :: H 
      REAL(DOUBLE), INTENT(IN) :: A 
      REAL(DOUBLE), INTENT(IN) :: B 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE geodettogeocent_M10_I   
      INTERFACE
      SUBROUTINE geodettogeocent_M10 (FIDET, H, FICENT, RTOT, XY, Z, A, B) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: FIDET 
      real(DOUBLE), INTENT(IN) :: H 
      real(DOUBLE), INTENT(OUT) :: FICENT 
      real(DOUBLE), INTENT(OUT) :: RTOT 
      real(DOUBLE), INTENT(OUT) :: XY 
      real(DOUBLE), INTENT(OUT) :: Z 
      real(DOUBLE), INTENT(IN) :: A 
      real(DOUBLE), INTENT(IN) :: B 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE ifloor_M10_I   
      INTERFACE
      INTEGER FUNCTION ifloor_M10 (X) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: X 
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE marsephm_M10_I   
      INTERFACE
      SUBROUTINE marsephm_M10 (XDAY, SUNLAT, SUNLON, SUNLSUBS, RADIUS, OWLT    &
         , EOT) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: XDAY 
      real(DOUBLE), INTENT(OUT) :: SUNLAT 
      real(DOUBLE), INTENT(OUT) :: SUNLON 
      real(DOUBLE), INTENT(OUT) :: SUNLSUBS 
      real(DOUBLE), INTENT(OUT) :: RADIUS 
      real(DOUBLE), INTENT(OUT) :: OWLT 
      real(DOUBLE), INTENT(OUT) :: EOT 
!VAST...Calls: PERTURB_M10, RESCALE_M10, SHIFTDIF_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE marsgcm_M10_I   
      INTERFACE
      SUBROUTINE marsgcm_M10 (CHGT, CLAT, CLONW, ALS, DUSTTAU, TIME, CTEMP      &
         , CPRES, CDENS, CUWIN, CVWIN, BLWINDEW, BLWINDNS, BLWINDVERT, HPRES    &
         , HDENS, ZF, PERTFACT, CTOPOHGT, HGTASFC, CAREOID, TEMPDAY, PRESDAY    &
         , DENSDAY, EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, TEMPMAX, TEMPMIN, DENSMAX &
         , DENSMIN, TGRND, CALBEDO, ICEPOLAR, TAT5M, DUSTOFFSET, REQUA, RPOLE   &
         , IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(INOUT) :: CHGT 
      real(DOUBLE), INTENT(IN) :: CLAT 
      real(DOUBLE), INTENT(IN) :: CLONW 
      real(DOUBLE), INTENT(IN) :: ALS 
      real(DOUBLE), INTENT(IN) :: DUSTTAU 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(OUT) :: CTEMP 
      real(DOUBLE), INTENT(OUT) :: CPRES 
      real(DOUBLE), INTENT(OUT) :: CDENS 
      real(DOUBLE), INTENT(OUT) :: CUWIN 
      real(DOUBLE), INTENT(OUT) :: CVWIN 
      real(DOUBLE), INTENT(OUT) :: BLWINDEW 
      real(DOUBLE), INTENT(OUT) :: BLWINDNS 
      real(DOUBLE), INTENT(OUT) :: BLWINDVERT 
      real(DOUBLE), INTENT(OUT) :: HPRES 
      real(DOUBLE), INTENT(OUT) :: HDENS 
      real(DOUBLE), INTENT(OUT) :: ZF 
      real(DOUBLE), INTENT(OUT) :: PERTFACT 
      real(DOUBLE), INTENT(INOUT) :: CTOPOHGT 
      real(DOUBLE), INTENT(IN) :: HGTASFC 
      real(DOUBLE), INTENT(INOUT) :: CAREOID 
      real(DOUBLE), INTENT(OUT) :: TEMPDAY 
      real(DOUBLE), INTENT(OUT) :: PRESDAY 
      real(DOUBLE), INTENT(OUT) :: DENSDAY 
      real(DOUBLE), INTENT(OUT) :: EWWNDAY 
      real(DOUBLE), INTENT(OUT) :: NSWNDAY 
      real(DOUBLE), INTENT(OUT) :: BLUDAY 
      real(DOUBLE), INTENT(OUT) :: BLVDAY 
      real(DOUBLE), INTENT(OUT) :: TEMPMAX 
      real(DOUBLE), INTENT(OUT) :: TEMPMIN 
      real(DOUBLE), INTENT(OUT) :: DENSMAX 
      real(DOUBLE), INTENT(OUT) :: DENSMIN 
      real(DOUBLE), INTENT(OUT) :: TGRND 
      real(DOUBLE), INTENT(INOUT) :: CALBEDO 
      integer, INTENT(OUT) :: ICEPOLAR 
      real(DOUBLE), INTENT(OUT) :: TAT5M 
      real(DOUBLE), INTENT(IN) :: DUSTOFFSET 
      real(DOUBLE), INTENT(IN) :: REQUA 
      real(DOUBLE), INTENT(IN) :: RPOLE 
      integer, INTENT(IN) :: IDAYDATA 
!VAST.../MGCMPARM_M10/ DUST(INOUT), DZBL(INOUT), ZWSFC(IN)
!VAST.../MGCMPARM_M10/ F10VAL(INOUT)
!VAST.../INTERP_M10/ DLAT(OUT), DLON(OUT), DLS(OUT), DDUST(OUT)
!VAST.../INTERP_M10/ DLATW(OUT), DLATT(OUT), DF10(OUT), WPOLEFAC(OUT)
!VAST.../INTERP_M10/ TPOLEFAC(OUT), ILAT(OUT), JLON(OUT)
!VAST.../INTERP_M10/ LS(OUT), MDUST(OUT), K1ST(OUT), ILATW(OUT)
!VAST.../INTERP_M10/ ILATT(OUT), MF10(OUT)
!VAST.../TGCMOFFSET_M10/ OFFSETS(IN), ZOFFSET(IN), HGTOFFSET(OUT)
!VAST.../TGCMOFFSET_M10/ OFSZL(OUT), IBOUGHER(IN)
!VAST.../THERM_M10/ F107(INOUT)
!VAST...Calls: IFLOOR_M10, ZLOGR_M10, RELLIPS_M10, TWOD_M10
!VAST...Calls: TGCMTERP_M10, MGCMTERP_M10, SURFTERP_M10, SUBLTCHK_M10
!VAST...Calls: CP_M10, BLTP_M10, SLOPEWIND_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE mgcmterp_M10_I   
      INTERFACE
      SUBROUTINE mgcmterp_M10 (KHGT, TIME, TMGCM, PMGCM, DMGCM, UMGCM, VMGCM &
         , TEMPDAY, PRESDAY, DENSDAY, UWNDDAY, VWNDDAY, TEMPMAX, TEMPMIN     &
         , DENSMAX, DENSMIN, IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: KHGT 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(OUT) :: TMGCM 
      real(DOUBLE), INTENT(OUT) :: PMGCM 
      real(DOUBLE), INTENT(OUT) :: DMGCM 
      real(DOUBLE), INTENT(OUT) :: UMGCM 
      real(DOUBLE), INTENT(OUT) :: VMGCM 
      real(DOUBLE), INTENT(OUT) :: TEMPDAY 
      real(DOUBLE), INTENT(OUT) :: PRESDAY 
      real(DOUBLE), INTENT(OUT) :: DENSDAY 
      real(DOUBLE), INTENT(OUT) :: UWNDDAY 
      real(DOUBLE), INTENT(OUT) :: VWNDDAY 
      real(DOUBLE), INTENT(OUT) :: TEMPMAX 
      real(DOUBLE), INTENT(OUT) :: TEMPMIN 
      real(DOUBLE), INTENT(OUT) :: DENSMAX 
      real(DOUBLE), INTENT(OUT) :: DENSMIN 
      integer, INTENT(IN) :: IDAYDATA 
!VAST.../MGCMDATA_M10/ TZA0(IN), TZA1(IN), TZP1(IN), TZA2(IN)
!VAST.../MGCMDATA_M10/ TZP2(IN), PZA0(IN), PZA1(IN), PZP1(IN)
!VAST.../MGCMDATA_M10/ PZA2(IN), PZP2(IN), DZA0(IN), UZA0(IN)
!VAST.../MGCMDATA_M10/ UZA1(IN), UZP1(IN), UZA2(IN), UZP2(IN)
!VAST.../MGCMDATA_M10/ VZA0(IN), VZA1(IN), VZP1(IN), VZA2(IN)
!VAST.../MGCMDATA_M10/ VZP2(IN)
!VAST.../INTERP_M10/ DLAT(INOUT), DLS(INOUT), DDUST(INOUT)
!VAST.../INTERP_M10/ DLATW(INOUT), WPOLEFAC(IN), ILAT(IN)
!VAST.../INTERP_M10/ LS(IN), MDUST(IN), ILATW(IN)
!VAST...Calls: TIDEX_M10, TIDEY_M10, THREED_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE perturb_M10_I   
      INTERFACE
      SUBROUTINE perturb_M10 (DT, PBS) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: DT 
      real(DOUBLE), INTENT(OUT) :: PBS 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE ppnd_M10_I   
      INTERFACE
      REAL(KIND(0.0D0)) FUNCTION ppnd_M10 (P, IFAULT) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: P 
      integer, INTENT(OUT) :: IFAULT 
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE profterp_M10_I   
      INTERFACE
      SUBROUTINE profterp_M10 (CHGT, CLAT, CLON, TIN, PIN, DIN, UIN, VIN, PTEMP &
         , PPRES, PDENS, PUWIN, PVWIN, NPROF, PROFNEAR, PROFFAR, PROFWGT        &
         , WAVEPERT) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: CHGT 
      real(DOUBLE), INTENT(IN) :: CLAT 
      real(DOUBLE), INTENT(IN) :: CLON 
      real(DOUBLE), INTENT(IN) :: TIN 
      real(DOUBLE), INTENT(IN) :: PIN 
      real(DOUBLE), INTENT(IN) :: DIN 
      real(DOUBLE), INTENT(IN) :: UIN 
      real(DOUBLE), INTENT(IN) :: VIN 
      real(DOUBLE), INTENT(INOUT) :: PTEMP 
      real(DOUBLE), INTENT(INOUT) :: PPRES 
      real(DOUBLE), INTENT(OUT) :: PDENS 
      real(DOUBLE), INTENT(OUT) :: PUWIN 
      real(DOUBLE), INTENT(OUT) :: PVWIN 
      integer, INTENT(INOUT) :: NPROF 
      real(DOUBLE), INTENT(IN) :: PROFNEAR 
      real(DOUBLE), INTENT(IN) :: PROFFAR 
      real(DOUBLE), INTENT(OUT) :: PROFWGT 
      real(DOUBLE), INTENT(IN) :: WAVEPERT 
!VAST.../PTERP_M10/ PHGT(IN), PLAT(IN), PLON(IN), PTMP(IN)
!VAST.../PTERP_M10/ PPRS(IN), PDEN(IN), PUWN(IN), PVWN(IN)
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE prseas_M10_I   
      INTERFACE
      SUBROUTINE prseas_M10 (LSUN, LAT, PR) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: LSUN 
      REAL(DOUBLE), INTENT(IN) :: LAT 
      REAL(DOUBLE), INTENT(OUT) :: PR 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE qrhtp_M10_I   
      INTERFACE
      REAL(KIND(0.0D0)) FUNCTION qrhtp_M10 (RH, T, P) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: RH 
      real(DOUBLE), INTENT(IN) :: T 
      real(DOUBLE), INTENT(IN) :: P 
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE randinit_M10_I   
      INTERFACE
      SUBROUTINE randinit_M10 (J, NR1, RHOD, RHOU, RHOV, RHOW, IUP, IUSTDOUT) 
      USE vast_kind_param,ONLY: DOUBLE 
      INTEGER, INTENT(IN) :: J 
      INTEGER, INTENT(INOUT) :: NR1 
      REAL(DOUBLE), INTENT(OUT) :: RHOD 
      REAL(DOUBLE), INTENT(OUT) :: RHOU 
      REAL(DOUBLE), INTENT(OUT) :: RHOV 
      REAL(DOUBLE), INTENT(OUT) :: RHOW 
      INTEGER, INTENT(IN) :: IUP 
      INTEGER, INTENT(IN) :: IUSTDOUT 
!VAST.../RANDCOM_M10/ IX(OUT), IY(OUT), IZ(OUT)
!VAST...Calls: RANDOM_M10, PPND_M10
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================================================ 
      MODULE random_M10_I   
      INTERFACE
      REAL(KIND(0.0D0)) FUNCTION random_M10 (L) 
      integer, INTENT(OUT) :: L 
!VAST.../RANDCOM_M10/ IX(INOUT), IY(INOUT), IZ(INOUT)
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE rdprof_M10_I   
      INTERFACE
      SUBROUTINE rdprof_M10 (PROFILE, NPROF, LONEAST) 
      character (LEN = 99), INTENT(IN) :: PROFILE 
      integer, INTENT(INOUT) :: NPROF 
      integer, INTENT(IN) :: LONEAST 
!VAST.../PTERP_M10/ PHGT(INOUT), PLAT(OUT), PLON(OUT), PTMP(OUT)
!VAST.../PTERP_M10/ PPRS(OUT), PDEN(OUT), PUWN(OUT), PVWN(OUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE rdtesmgcm_M10_I   
      INTERFACE
      SUBROUTINE rdtesmgcm_M10 (GCMDIR, VERSION) 
      character (LEN = 99), INTENT(IN) :: GCMDIR 
      character (LEN = 1), INTENT(IN) :: VERSION 
!VAST.../MGCMPARM_M10/ TESYR(IN)
!VAST.../MGCMTES_M10/ TTZA0(INOUT), TTZA1(INOUT), TTZP1(INOUT)
!VAST.../MGCMTES_M10/ TTZA2(INOUT), TTZP2(INOUT), TDZA0(INOUT)
!VAST.../MGCMTES_M10/ TDZA1(INOUT), TDZP1(INOUT), TDZA2(INOUT)
!VAST.../MGCMTES_M10/ TDZP2(INOUT), TPZA0(INOUT), TUZA0(INOUT)
!VAST.../MGCMTES_M10/ TUZA1(INOUT), TUZP1(INOUT), TUZA2(INOUT)
!VAST.../MGCMTES_M10/ TUZP2(INOUT), TVZA0(INOUT), TVZA1(INOUT)
!VAST.../MGCMTES_M10/ TVZP1(INOUT), TVZA2(INOUT), TVZP2(INOUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE rdtessrf_M10_I   
      INTERFACE
      SUBROUTINE rdtessrf_M10 (GCMDIR, VERSION) 
      CHARACTER (LEN = 99), INTENT(IN) :: GCMDIR 
      CHARACTER (LEN = 1), INTENT(IN) :: VERSION 
!VAST.../MGCMPARM_M10/ TESYR(IN)
!VAST.../SURFTES_M10/ TTSA0(INOUT), TTSA1(INOUT), TTSP1(INOUT)
!VAST.../SURFTES_M10/ TTSA2(INOUT), TTSP2(INOUT), TUSA0(INOUT)
!VAST.../SURFTES_M10/ TUSA1(INOUT), TUSP1(INOUT), TUSA2(INOUT)
!VAST.../SURFTES_M10/ TUSP2(INOUT), TVSA0(INOUT), TVSA1(INOUT)
!VAST.../SURFTES_M10/ TVSP1(INOUT), TVSA2(INOUT), TVSP2(INOUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE rdtestgcm_M10_I   
      INTERFACE
      SUBROUTINE rdtestgcm_M10 (GCMDIR, VERSION) 
      character (LEN = 99), INTENT(IN) :: GCMDIR 
      character (LEN = 1), INTENT(IN) :: VERSION 
!VAST.../MGCMPARM_M10/ TESYR(IN), SOLTES(IN)
!VAST.../TGCMTES_M10/ TTTA0(INOUT), TTTA1(INOUT), TTTP1(INOUT)
!VAST.../TGCMTES_M10/ TTTA2(INOUT), TTTP2(INOUT), TPTA0(INOUT)
!VAST.../TGCMTES_M10/ TPTA1(INOUT), TPTP1(INOUT), TPTA2(INOUT)
!VAST.../TGCMTES_M10/ TPTP2(INOUT), TDTA0(INOUT), TDTA1(INOUT)
!VAST.../TGCMTES_M10/ TDTP1(INOUT), TDTA2(INOUT), TDTP2(INOUT)
!VAST.../TGCMTES_M10/ TUTA0(INOUT), TUTA1(INOUT), TUTP1(INOUT)
!VAST.../TGCMTES_M10/ TUTA2(INOUT), TUTP2(INOUT), TVTA0(INOUT)
!VAST.../TGCMTES_M10/ TVTA1(INOUT), TVTP1(INOUT), TVTA2(INOUT)
!VAST.../TGCMTES_M10/ TVTP2(INOUT), TZFA0(INOUT), TZFA1(INOUT)
!VAST.../TGCMTES_M10/ TZFP1(INOUT), TZFA2(INOUT), TZFP2(INOUT)
!VAST.../TGCMTES_M10/ IZTOP(INOUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE readmgcm_M10_I   
      INTERFACE
      SUBROUTINE readmgcm_M10 (GCMDIR, VERSION) 
      CHARACTER (LEN = 99), INTENT(IN) :: GCMDIR 
      CHARACTER (LEN = 1), INTENT(IN) :: VERSION 
!VAST.../MGCMPARM_M10/ DUSTC(IN)
!VAST.../MGCMDATA_M10/ TZA0(INOUT), TZA1(INOUT), TZP1(INOUT)
!VAST.../MGCMDATA_M10/ TZA2(INOUT), TZP2(INOUT), PZA0(INOUT)
!VAST.../MGCMDATA_M10/ PZA1(INOUT), PZP1(INOUT), PZA2(INOUT)
!VAST.../MGCMDATA_M10/ PZP2(INOUT), DZA0(INOUT), UZA0(INOUT)
!VAST.../MGCMDATA_M10/ UZA1(INOUT), UZP1(INOUT), UZA2(INOUT)
!VAST.../MGCMDATA_M10/ UZP2(INOUT), VZA0(INOUT), VZA1(INOUT)
!VAST.../MGCMDATA_M10/ VZP1(INOUT), VZA2(INOUT), VZP2(INOUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE readsurf_M10_I   
      INTERFACE
      SUBROUTINE readsurf_M10 (GCMDIR, VERSION) 
      CHARACTER (LEN = 99), INTENT(IN) :: GCMDIR 
      CHARACTER (LEN = 1), INTENT(IN) :: VERSION 
!VAST.../MGCMPARM_M10/ DUSTC(IN)
!VAST.../SURFDATA_M10/ TSA0(INOUT), TSA1(INOUT), TSP1(INOUT)
!VAST.../SURFDATA_M10/ TSA2(INOUT), TSP2(INOUT), USA0(INOUT)
!VAST.../SURFDATA_M10/ USA1(INOUT), USP1(INOUT), USA2(INOUT)
!VAST.../SURFDATA_M10/ USP2(INOUT), VSA0(INOUT), VSA1(INOUT)
!VAST.../SURFDATA_M10/ VSP1(INOUT), VSA2(INOUT), VSP2(INOUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE readtes_M10_I   
      INTERFACE
      SUBROUTINE readtes_M10 (DATADIR, VERSION) 
      character (LEN = 99), INTENT(IN) :: DATADIR 
      character (LEN = 1), INTENT(IN) :: VERSION 
!VAST.../TESDUST_M10/ TESTAU(INOUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE readtgcm_M10_I   
      INTERFACE
      SUBROUTINE readtgcm_M10 (GCMDIR, VERSION) 
      character (LEN = 99), INTENT(IN) :: GCMDIR 
      character (LEN = 1), INTENT(IN) :: VERSION 
!VAST.../MGCMPARM_M10/ DUST(IN), DUSTC(IN), SOLACT(IN)
!VAST.../TGCMDATA_M10/ TTA0(INOUT), TTA1(INOUT), TTP1(INOUT)
!VAST.../TGCMDATA_M10/ TTA2(INOUT), TTP2(INOUT), PTA0(INOUT)
!VAST.../TGCMDATA_M10/ PTA1(INOUT), PTP1(INOUT), PTA2(INOUT)
!VAST.../TGCMDATA_M10/ PTP2(INOUT), DTA0(INOUT), DTA1(INOUT)
!VAST.../TGCMDATA_M10/ DTP1(INOUT), DTA2(INOUT), DTP2(INOUT)
!VAST.../TGCMDATA_M10/ UTA0(INOUT), UTA1(INOUT), UTP1(INOUT)
!VAST.../TGCMDATA_M10/ UTA2(INOUT), UTP2(INOUT), VTA0(INOUT)
!VAST.../TGCMDATA_M10/ VTA1(INOUT), VTP1(INOUT), VTA2(INOUT)
!VAST.../TGCMDATA_M10/ VTP2(INOUT), ZFA0(INOUT), ZFA1(INOUT)
!VAST.../TGCMDATA_M10/ ZFP1(INOUT), ZFA2(INOUT), ZFP2(INOUT)
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE rellips_M10_I   
      INTERFACE
      SUBROUTINE rellips_M10 (LAT, LONW, RREF, Z, GZ, OLDRREF, CTOPOHGT  &
         , CALBEDO, REQUA, RPOLE) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: LAT 
      real(DOUBLE), INTENT(IN) :: LONW 
      real(DOUBLE), INTENT(INOUT) :: RREF 
      real(DOUBLE), INTENT(IN) :: Z 
      real(DOUBLE), INTENT(OUT) :: GZ 
      real(DOUBLE), INTENT(OUT) :: OLDRREF 
      real(DOUBLE), INTENT(INOUT) :: CTOPOHGT 
      real(DOUBLE), INTENT(INOUT) :: CALBEDO 
      real(DOUBLE), INTENT(IN) :: REQUA 
      real(DOUBLE), INTENT(IN) :: RPOLE 
!VAST...Calls: TOPOAREO_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE rescale_M10_I   
      INTERFACE
      SUBROUTINE rescale_M10 (X) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(INOUT) :: X 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE setup_M10_I   
      INTERFACE
      SUBROUTINE setup_M10 (CHGT, CLAT, CLON, CSEC, DAY0, RHOD, RHOU, RHOV    &
         , RHOW, DHGT, DLAT, DLON, DTIME, MAXNUM, NRN1, NMCR1, DSUNLS, DRADAU &
         , DOWLT, LNEW, INPUTFL, IUSTDOUT, IULIST, HGTASFC, INERT, INUTC      &
         , STEPMIN, PROFNR, PROFFR, NPROF) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(OUT) :: CHGT 
      REAL(DOUBLE), INTENT(OUT) :: CLAT 
      REAL(DOUBLE), INTENT(OUT) :: CLON 
      REAL(DOUBLE), INTENT(OUT) :: CSEC 
      REAL(DOUBLE), INTENT(OUT) :: DAY0 
      REAL(DOUBLE), INTENT(OUT) :: RHOD 
      REAL(DOUBLE), INTENT(OUT) :: RHOU 
      REAL(DOUBLE), INTENT(OUT) :: RHOV 
      REAL(DOUBLE), INTENT(OUT) :: RHOW 
      REAL(DOUBLE), INTENT(OUT) :: DHGT 
      REAL(DOUBLE), INTENT(OUT) :: DLAT 
      REAL(DOUBLE), INTENT(OUT) :: DLON 
      REAL(DOUBLE), INTENT(OUT) :: DTIME 
      INTEGER, INTENT(INOUT) :: MAXNUM 
      INTEGER, INTENT(OUT) :: NRN1 
      INTEGER, INTENT(OUT) :: NMCR1 
      REAL(DOUBLE), INTENT(IN) :: DSUNLS 
      REAL(DOUBLE), INTENT(IN) :: DRADAU 
      REAL(DOUBLE), INTENT(IN) :: DOWLT 
      INTEGER, INTENT(OUT) :: LNEW 
      CHARACTER (LEN = 99), INTENT(IN) :: INPUTFL 
      INTEGER, INTENT(IN) :: IUSTDOUT 
      INTEGER, INTENT(OUT) :: IULIST 
      REAL(DOUBLE), INTENT(OUT) :: HGTASFC 
      INTEGER, INTENT(OUT) :: INERT 
      INTEGER, INTENT(OUT) :: INUTC 
      REAL(DOUBLE), INTENT(OUT) :: STEPMIN 
      REAL(DOUBLE), INTENT(OUT) :: PROFNR 
      REAL(DOUBLE), INTENT(OUT) :: PROFFR 
      INTEGER, INTENT(INOUT) :: NPROF 
!VAST.../RANDCOM_M10/ IX(OUT), IY(OUT), IZ(OUT)
!VAST.../TERHGT_M10/ AREORAD(INOUT), TOPOMOLA(INOUT), ALBEDO(INOUT)
!VAST.../TGCMOFFSET_M10/ OFFSETS(INOUT), TOFFSETS(INOUT)
!VAST.../TGCMOFFSET_M10/ ZOFFSET(OUT), IBOUGHER(OUT)
!VAST.../COSPARNH_M10/ ZC(INOUT), TC(INOUT), PC(INOUT), DC(INOUT)
!VAST.../THERM_M10/ F107(OUT), STDL(OUT)
!VAST.../DATACOM_M10/ DTR(OUT), DAY(OUT), DUSTLAT(OUT), DUSTLON(OUT)
!VAST.../DATACOM_M10/ RADMAX(OUT), ALS0(OUT), ALSDUR(OUT)
!VAST.../DATACOM_M10/ INTENS(OUT), DTEX(OUT), RPFACTOR(OUT)
!VAST.../DATACOM_M10/ DUSTTAU(OUT), DUSTMIN(OUT), DUSTMAX(OUT)
!VAST.../DATACOM_M10/ DUSTNU(OUT), DUSTDIAM(OUT), DUSTDENS(OUT)
!VAST.../DATACOM_M10/ RWSCALE(OUT), WLSCALE(OUT), REQUA(OUT)
!VAST.../DATACOM_M10/ RPOLE(OUT), WMSCALE(OUT), BLWINFAC(OUT)
!VAST.../DATACOM_M10/ MAPYEAR(OUT), IDAYDATA(OUT), NPOS(OUT)
!VAST.../DATACOM_M10/ NVARX(OUT), NVARY(OUT), LOGSCALE(OUT)
!VAST.../DATACOM_M10/ IU0(OUT), IUP(OUT), IPCLAT(OUT), MOLAHGTS(OUT)
!VAST.../MGCMPARM_M10/ DUST(OUT), DZBL(OUT), ZWSFC(OUT), F10VAL(OUT)
!VAST.../MGCMPARM_M10/ F10TES(OUT), DUSTC(OUT), SOLACT(OUT)
!VAST.../MGCMPARM_M10/ TESYR(OUT), SOLTES(OUT)
!VAST.../WAVECOEF_M10/ WAVEA0(OUT), WAVEA1(OUT), WAVEPHI1(OUT)
!VAST.../WAVECOEF_M10/ WAVEA2(OUT), WAVEPHI2(OUT), WAVEA3(OUT)
!VAST.../WAVECOEF_M10/ WAVEPHI3(OUT), WAVETIME(INOUT), WAVEDATA(INOUT)
!VAST.../WAVECOEF_M10/ WSCALE(OUT), PHI1DOT(OUT), PHI2DOT(OUT)
!VAST.../WAVECOEF_M10/ PHI3DOT(OUT), WAVEDATE(OUT), NWAVE(OUT)
!VAST.../WAVECOEF_M10/ IUWAVE(OUT)
!VAST.../FILENAME_M10/ LSTFL(OUT), OUTFL(OUT)
!VAST...Calls: RDPROF_M10, READTES_M10, READSURF_M10, READMGCM_M10
!VAST...Calls: READTGCM_M10, RDTESSRF_M10, RDTESMGCM_M10
!VAST...Calls: RDTESTGCM_M10, CHKDT_M10, CALTOJUL_M10, MARSEPHM_M10
!VAST...Calls: RANDOM_M10, PPND_M10
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE shiftdif_M10_I   
      INTERFACE
! 
      SUBROUTINE shiftdif_M10 (X) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(INOUT) :: X 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE slopewind_M10_I   
      INTERFACE
! 
      SUBROUTINE slopewind_M10 (XLAT, XLONW, HGTMOLA, TIME, UMEAN, VMEAN, UEW &
         , VNS, WVERT) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: XLAT 
      real(DOUBLE), INTENT(IN) :: XLONW 
      real(DOUBLE), INTENT(IN) :: HGTMOLA 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(IN) :: UMEAN 
      real(DOUBLE), INTENT(IN) :: VMEAN 
      real(DOUBLE), INTENT(OUT) :: UEW 
      real(DOUBLE), INTENT(OUT) :: VNS 
      real(DOUBLE), INTENT(OUT) :: WVERT 
!VAST...Calls: TOPOAREO_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE species_M10_I   
      INTERFACE
! 
      SUBROUTINE species_M10 (HGT, XLAT, ALS, ZBASE, AMZ, DENS, PRES, TEMP, IUP &
         , FMOL, FMASS, FMOLH2O, FMASSH2O) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: HGT 
      real(DOUBLE), INTENT(IN) :: XLAT 
      real(DOUBLE), INTENT(IN) :: ALS 
      real(DOUBLE), INTENT(IN) :: ZBASE 
      real(DOUBLE), INTENT(INOUT) :: AMZ 
      real(DOUBLE), INTENT(IN) :: DENS 
      real(DOUBLE), INTENT(IN) :: PRES 
      real(DOUBLE), INTENT(IN) :: TEMP 
      integer, INTENT(IN) :: IUP 
      real(DOUBLE), DIMENSION(0:8), INTENT(OUT) :: FMOL 
      real(DOUBLE), DIMENSION(0:8), INTENT(OUT) :: FMASS 
      real(DOUBLE), INTENT(OUT) :: FMOLH2O 
      real(DOUBLE), INTENT(OUT) :: FMASSH2O 
!VAST...Calls: PRSEAS_M10, QRHTP_M10
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE stewart2_M10_I   
      INTERFACE
! 
      SUBROUTINE stewart2_M10 (RAUI, LAT, LON, LST, TOTALPRZ, TZ, TOTALMDZ    &
         , CHGT, RSTAR, H, MOLWTG, SIGMA, IU0, SUNLAT, DELTATEX, TINF, TF, ZF &
         , HRHO, REQUA, RPOLE, TGRAD) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: RAUI 
      real(DOUBLE), INTENT(IN) :: LAT 
      real(DOUBLE), INTENT(IN) :: LON 
      real(DOUBLE), INTENT(IN) :: LST 
      real(DOUBLE), INTENT(INOUT) :: TOTALPRZ 
      real(DOUBLE), INTENT(INOUT) :: TZ 
      real(DOUBLE), INTENT(INOUT) :: TOTALMDZ 
      real(DOUBLE), INTENT(IN) :: CHGT 
      real(DOUBLE), INTENT(IN) :: RSTAR 
      real(DOUBLE), INTENT(OUT) :: H 
      real(DOUBLE), INTENT(OUT) :: MOLWTG 
      real(DOUBLE), INTENT(IN) :: SIGMA 
      integer, INTENT(IN) :: IU0 
      real(DOUBLE), INTENT(IN) :: SUNLAT 
      real(DOUBLE), INTENT(IN) :: DELTATEX 
      real(DOUBLE), INTENT(OUT) :: TINF 
      real(DOUBLE), INTENT(IN) :: TF 
      real(DOUBLE), INTENT(IN) :: ZF 
      real(DOUBLE), INTENT(OUT) :: HRHO 
      real(DOUBLE), INTENT(IN) :: REQUA 
      real(DOUBLE), INTENT(IN) :: RPOLE 
      real(DOUBLE), INTENT(OUT) :: TGRAD 
!VAST.../THERM_M10/ F107(IN), STDL(INOUT), FMOL(OUT)
!VAST...Calls: ESCALC_M10, RELLIPS_M10, THERMPAR_M10, THERMOS_M10
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE subltchk_M10_I   
      INTERFACE
! 
      SUBROUTINE subltchk_M10 (TEMP, PRES, TSUBL) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(INOUT) :: TEMP 
      real(DOUBLE), INTENT(IN) :: PRES 
      real(DOUBLE), INTENT(OUT) :: TSUBL 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE surfterp_M10_I   
      INTERFACE
! 
      SUBROUTINE surfterp_M10 (KHGT, TIME, TMGCM, PMGCM, DMGCM, UMGCM, VMGCM   &
         , HPRES, HDENS, CTOPOHGT, TEMPDAY, PRESDAY, DENSDAY, UWNDDAY, VWNDDAY &
         , HPRES0, TEMPMAX, TEMPMIN, DENSMAX, DENSMIN, TAT5M, IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: KHGT 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(OUT) :: TMGCM 
      real(DOUBLE), INTENT(OUT) :: PMGCM 
      real(DOUBLE), INTENT(OUT) :: DMGCM 
      real(DOUBLE), INTENT(OUT) :: UMGCM 
      real(DOUBLE), INTENT(OUT) :: VMGCM 
      real(DOUBLE), INTENT(OUT) :: HPRES 
      real(DOUBLE), INTENT(OUT) :: HDENS 
      real(DOUBLE), INTENT(IN) :: CTOPOHGT 
      real(DOUBLE), INTENT(OUT) :: TEMPDAY 
      real(DOUBLE), INTENT(OUT) :: PRESDAY 
      real(DOUBLE), INTENT(OUT) :: DENSDAY 
      real(DOUBLE), INTENT(OUT) :: UWNDDAY 
      real(DOUBLE), INTENT(OUT) :: VWNDDAY 
      real(DOUBLE), INTENT(OUT) :: HPRES0 
      real(DOUBLE), INTENT(OUT) :: TEMPMAX 
      real(DOUBLE), INTENT(OUT) :: TEMPMIN 
      real(DOUBLE), INTENT(OUT) :: DENSMAX 
      real(DOUBLE), INTENT(OUT) :: DENSMIN 
      real(DOUBLE), INTENT(INOUT) :: TAT5M 
      integer, INTENT(IN) :: IDAYDATA 
!VAST.../SURFDATA_M10/ TSA0(IN), TSA1(IN), TSP1(IN), TSA2(IN)
!VAST.../SURFDATA_M10/ TSP2(IN), USA0(IN), USA1(IN), USP1(IN)
!VAST.../SURFDATA_M10/ USA2(IN), USP2(IN), VSA0(IN), VSA1(IN)
!VAST.../SURFDATA_M10/ VSP1(IN), VSA2(IN), VSP2(IN)
!VAST.../MGCMDATA_M10/ TZA0(IN), TZA1(IN), TZP1(IN), TZA2(IN)
!VAST.../MGCMDATA_M10/ TZP2(IN), PZA0(IN), PZA1(IN), PZP1(IN)
!VAST.../MGCMDATA_M10/ PZA2(IN), PZP2(IN), DZA0(IN)
!VAST.../INTERP_M10/ DLAT(INOUT), DLON(INOUT), DLS(INOUT)
!VAST.../INTERP_M10/ DDUST(INOUT), DLATW(INOUT), WPOLEFAC(IN)
!VAST.../INTERP_M10/ ILAT(IN), JLON(IN), LS(IN), MDUST(IN)
!VAST.../INTERP_M10/ K1ST(IN), ILATW(IN)
!VAST.../MGCMPARM_M10/ DZBL(IN)
!VAST...Calls: TIDEX_M10, FOURD_M10, TIDEY_M10, THREED_M10
!VAST...Calls: ZLOGR_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE tesgcm_M10_I   
      INTERFACE
!
      SUBROUTINE tesgcm_M10 (CHGT, CLAT, CLONW, ALS, TIME, CTEMP, CPRES, CDENS  &
         , CUWIN, CVWIN, BLWINDEW, BLWINDNS, BLWINDVERT, HPRES, HDENS, ZF       &
         , PERTFACT, CTOPOHGT, HGTASFC, CAREOID, TEMPDAY, PRESDAY, DENSDAY      &
         , EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, TEMPMAX, TEMPMIN, DENSMAX, DENSMIN &
         , TGRND, CALBEDO, ICEPOLAR, TAT5M, REQUA, RPOLE, MAPYEAR, IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(INOUT) :: CHGT 
      real(DOUBLE), INTENT(IN) :: CLAT 
      real(DOUBLE), INTENT(IN) :: CLONW 
      real(DOUBLE), INTENT(IN) :: ALS 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(OUT) :: CTEMP 
      real(DOUBLE), INTENT(OUT) :: CPRES 
      real(DOUBLE), INTENT(OUT) :: CDENS 
      real(DOUBLE), INTENT(OUT) :: CUWIN 
      real(DOUBLE), INTENT(OUT) :: CVWIN 
      real(DOUBLE), INTENT(OUT) :: BLWINDEW 
      real(DOUBLE), INTENT(OUT) :: BLWINDNS 
      real(DOUBLE), INTENT(OUT) :: BLWINDVERT 
      real(DOUBLE), INTENT(OUT) :: HPRES 
      real(DOUBLE), INTENT(OUT) :: HDENS 
      real(DOUBLE), INTENT(OUT) :: ZF 
      real(DOUBLE), INTENT(OUT) :: PERTFACT 
      real(DOUBLE), INTENT(INOUT) :: CTOPOHGT 
      real(DOUBLE), INTENT(IN) :: HGTASFC 
      real(DOUBLE), INTENT(INOUT) :: CAREOID 
      real(DOUBLE), INTENT(OUT) :: TEMPDAY 
      real(DOUBLE), INTENT(OUT) :: PRESDAY 
      real(DOUBLE), INTENT(OUT) :: DENSDAY 
      real(DOUBLE), INTENT(OUT) :: EWWNDAY 
      real(DOUBLE), INTENT(OUT) :: NSWNDAY 
      real(DOUBLE), INTENT(OUT) :: BLUDAY 
      real(DOUBLE), INTENT(OUT) :: BLVDAY 
      real(DOUBLE), INTENT(OUT) :: TEMPMAX 
      real(DOUBLE), INTENT(OUT) :: TEMPMIN 
      real(DOUBLE), INTENT(OUT) :: DENSMAX 
      real(DOUBLE), INTENT(OUT) :: DENSMIN 
      real(DOUBLE), INTENT(OUT) :: TGRND 
      real(DOUBLE), INTENT(INOUT) :: CALBEDO 
      integer, INTENT(OUT) :: ICEPOLAR 
      real(DOUBLE), INTENT(OUT) :: TAT5M 
      real(DOUBLE), INTENT(IN) :: REQUA 
      real(DOUBLE), INTENT(IN) :: RPOLE 
      integer, INTENT(IN) :: MAPYEAR 
      integer, INTENT(IN) :: IDAYDATA 
!VAST.../MGCMPARM_M10/ DZBL(INOUT), ZWSFC(IN), F10TES(INOUT)
!VAST.../TESTERP_M10/ DLAT(OUT), DLON(OUT), DLS(OUT), DLATW(OUT)
!VAST.../TESTERP_M10/ DLONW(OUT), DLATT(OUT), DF10(OUT), WPOLEFAC(OUT)
!VAST.../TESTERP_M10/ TPOLEFAC(OUT), ILAT(OUT), JLON(OUT)
!VAST.../TESTERP_M10/ LS(OUT), K1ST(OUT), ILATW(OUT), JLONW(OUT)
!VAST.../TESTERP_M10/ ILATT(OUT), MF10(OUT)
!VAST.../TGCMOFFSET_M10/ TOFFSETS(IN), ZOFFSET(IN), HGTOFFSET(OUT)
!VAST.../TGCMOFFSET_M10/ OFSZL(OUT), IBOUGHER(IN)
!VAST.../THERM_M10/ F107(INOUT)
!VAST...Calls: IFLOOR_M10, ZLOGR_M10, RELLIPS_M10, TESTTERP_M10
!VAST...Calls: TESGTERP_M10, TESSRFTRP_M10, SUBLTCHK_M10
!VAST...Calls: CP_M10, BLTP_M10, SLOPEWIND_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE tesgterp_M10_I   
      INTERFACE
      SUBROUTINE tesgterp_M10 (KHGT, TIME, TMGCM, PMGCM, DMGCM, UMGCM, VMGCM &
         , TEMPDAY, PRESDAY, DENSDAY, UWNDDAY, VWNDDAY, TEMPMAX, TEMPMIN     &
         , DENSMAX, DENSMIN, MTESY, IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: KHGT 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(OUT) :: TMGCM 
      real(DOUBLE), INTENT(OUT) :: PMGCM 
      real(DOUBLE), INTENT(OUT) :: DMGCM 
      real(DOUBLE), INTENT(OUT) :: UMGCM 
      real(DOUBLE), INTENT(OUT) :: VMGCM 
      real(DOUBLE), INTENT(OUT) :: TEMPDAY 
      real(DOUBLE), INTENT(OUT) :: PRESDAY 
      real(DOUBLE), INTENT(OUT) :: DENSDAY 
      real(DOUBLE), INTENT(OUT) :: UWNDDAY 
      real(DOUBLE), INTENT(OUT) :: VWNDDAY 
      real(DOUBLE), INTENT(OUT) :: TEMPMAX 
      real(DOUBLE), INTENT(OUT) :: TEMPMIN 
      real(DOUBLE), INTENT(OUT) :: DENSMAX 
      real(DOUBLE), INTENT(OUT) :: DENSMIN 
      integer, INTENT(IN) :: MTESY 
      integer, INTENT(IN) :: IDAYDATA 
!VAST.../MGCMTES_M10/ TTZA0(IN), TTZA1(IN), TTZP1(IN), TTZA2(IN)
!VAST.../MGCMTES_M10/ TTZP2(IN), TDZA0(IN), TDZA1(IN), TDZP1(IN)
!VAST.../MGCMTES_M10/ TDZA2(IN), TDZP2(IN), TPZA0(IN), TUZA0(IN)
!VAST.../MGCMTES_M10/ TUZA1(IN), TUZP1(IN), TUZA2(IN), TUZP2(IN)
!VAST.../MGCMTES_M10/ TVZA0(IN), TVZA1(IN), TVZP1(IN), TVZA2(IN)
!VAST.../MGCMTES_M10/ TVZP2(IN)
!VAST.../TESTERP_M10/ DLAT(INOUT), DLS(INOUT), DLATW(INOUT)
!VAST.../TESTERP_M10/ WPOLEFAC(IN), ILAT(IN), LS(IN), ILATW(IN)
!VAST...Calls: TIDEX_M10, TIDEY_M10, TWOD_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE tesod_M10_I   
      INTERFACE
! 
      SUBROUTINE tesod_M10 (MAPYEAR, XLS, YLAT, ZLON, DUSTOD) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: MAPYEAR 
      real(DOUBLE), INTENT(IN) :: XLS 
      real(DOUBLE), INTENT(IN) :: YLAT 
      real(DOUBLE), INTENT(IN) :: ZLON 
      real(DOUBLE), INTENT(OUT) :: DUSTOD 
!VAST.../TESDUST_M10/ TESTAU(IN)
!VAST...Calls: IFLOOR_M10, THREED_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE tessrftrp_M10_I   
      INTERFACE
      SUBROUTINE tessrftrp_M10 (KHGT, TIME, TMGCM, PMGCM, DMGCM, UMGCM, VMGCM  &
         , HPRES, HDENS, CTOPOHGT, TEMPDAY, PRESDAY, DENSDAY, UWNDDAY, VWNDDAY &
         , HPRES0, TEMPMAX, TEMPMIN, DENSMAX, DENSMIN, TAT5M, MTESY, IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: KHGT 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(OUT) :: TMGCM 
      real(DOUBLE), INTENT(OUT) :: PMGCM 
      real(DOUBLE), INTENT(OUT) :: DMGCM 
      real(DOUBLE), INTENT(OUT) :: UMGCM 
      real(DOUBLE), INTENT(OUT) :: VMGCM 
      real(DOUBLE), INTENT(OUT) :: HPRES 
      real(DOUBLE), INTENT(OUT) :: HDENS 
      real(DOUBLE), INTENT(IN) :: CTOPOHGT 
      real(DOUBLE), INTENT(OUT) :: TEMPDAY 
      real(DOUBLE), INTENT(OUT) :: PRESDAY 
      real(DOUBLE), INTENT(OUT) :: DENSDAY 
      real(DOUBLE), INTENT(OUT) :: UWNDDAY 
      real(DOUBLE), INTENT(OUT) :: VWNDDAY 
      real(DOUBLE), INTENT(OUT) :: HPRES0 
      real(DOUBLE), INTENT(OUT) :: TEMPMAX 
      real(DOUBLE), INTENT(OUT) :: TEMPMIN 
      real(DOUBLE), INTENT(OUT) :: DENSMAX 
      real(DOUBLE), INTENT(OUT) :: DENSMIN 
      real(DOUBLE), INTENT(INOUT) :: TAT5M 
      integer, INTENT(IN) :: MTESY 
      integer, INTENT(IN) :: IDAYDATA 
!VAST.../SURFTES_M10/ TTSA0(IN), TTSA1(IN), TTSP1(IN), TTSA2(IN)
!VAST.../SURFTES_M10/ TTSP2(IN), TUSA0(IN), TUSA1(IN), TUSP1(IN)
!VAST.../SURFTES_M10/ TUSA2(IN), TUSP2(IN), TVSA0(IN), TVSA1(IN)
!VAST.../SURFTES_M10/ TVSP1(IN), TVSA2(IN), TVSP2(IN)
!VAST.../MGCMTES_M10/ TTZA0(IN), TTZA1(IN), TTZP1(IN), TTZA2(IN)
!VAST.../MGCMTES_M10/ TTZP2(IN), TDZA0(IN), TDZA1(IN), TDZP1(IN)
!VAST.../MGCMTES_M10/ TDZA2(IN), TDZP2(IN), TPZA0(IN)
!VAST.../TESTERP_M10/ DLAT(INOUT), DLON(INOUT), DLS(INOUT)
!VAST.../TESTERP_M10/ DLATW(INOUT), DLONW(INOUT), WPOLEFAC(IN)
!VAST.../TESTERP_M10/ ILAT(IN), JLON(IN), LS(IN), K1ST(IN)
!VAST.../TESTERP_M10/ ILATW(IN), JLONW(IN)
!VAST.../MGCMPARM_M10/ DZBL(IN)
!VAST...Calls: TIDEX_M10, THREED_M10, TIDEY_M10, TWOD_M10
!VAST...Calls: ZLOGR_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE testterp_M10_I   
      INTERFACE
      SUBROUTINE testterp_M10 (KHGTT, TIME, TTGCM, PTGCM, DTGCM, UTGCM, VTGCM &
         , ZF, TEMPDAY, PRESDAY, DENSDAY, UWNDDAY, VWNDDAY, TEMPMAX, TEMPMIN  &
         , DENSMAX, DENSMIN, MTESY, IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: KHGTT 
      real(DOUBLE), INTENT(IN) :: TIME 
      real(DOUBLE), INTENT(OUT) :: TTGCM 
      real(DOUBLE), INTENT(OUT) :: PTGCM 
      real(DOUBLE), INTENT(OUT) :: DTGCM 
      real(DOUBLE), INTENT(OUT) :: UTGCM 
      real(DOUBLE), INTENT(OUT) :: VTGCM 
      real(DOUBLE), INTENT(OUT) :: ZF 
      real(DOUBLE), INTENT(OUT) :: TEMPDAY 
      real(DOUBLE), INTENT(OUT) :: PRESDAY 
      real(DOUBLE), INTENT(OUT) :: DENSDAY 
      real(DOUBLE), INTENT(OUT) :: UWNDDAY 
      real(DOUBLE), INTENT(OUT) :: VWNDDAY 
      real(DOUBLE), INTENT(OUT) :: TEMPMAX 
      real(DOUBLE), INTENT(OUT) :: TEMPMIN 
      real(DOUBLE), INTENT(OUT) :: DENSMAX 
      real(DOUBLE), INTENT(OUT) :: DENSMIN 
      integer, INTENT(IN) :: MTESY 
      integer, INTENT(IN) :: IDAYDATA 
!VAST.../TGCMTES_M10/ TTTA0(IN), TTTA1(IN), TTTP1(IN), TTTA2(IN)
!VAST.../TGCMTES_M10/ TTTP2(IN), TPTA0(IN), TPTA1(IN), TPTP1(IN)
!VAST.../TGCMTES_M10/ TPTA2(IN), TPTP2(IN), TDTA0(IN), TDTA1(IN)
!VAST.../TGCMTES_M10/ TDTP1(IN), TDTA2(IN), TDTP2(IN), TUTA0(IN)
!VAST.../TGCMTES_M10/ TUTA1(IN), TUTP1(IN), TUTA2(IN), TUTP2(IN)
!VAST.../TGCMTES_M10/ TVTA0(IN), TVTA1(IN), TVTP1(IN), TVTA2(IN)
!VAST.../TGCMTES_M10/ TVTP2(IN), TZFA0(IN), TZFA1(IN), TZFP1(IN)
!VAST.../TGCMTES_M10/ TZFA2(IN), TZFP2(IN)
!VAST.../TESTERP_M10/ DLS(INOUT), DLATT(INOUT), DF10(INOUT)
!VAST.../TESTERP_M10/ TPOLEFAC(IN), LS(IN), ILATT(IN), MF10(IN)
!VAST...Calls: TIDEX_M10, TTIDEY_M10, THREED_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE tgcmterp_M10_I   
      INTERFACE
! 
      SUBROUTINE tgcmterp_M10 (KHGTT, TIME, TTGCM, PTGCM, DTGCM, UTGCM, VTGCM &
         , ZF, TEMPDAY, PRESDAY, DENSDAY, UWNDDAY, VWNDDAY, TEMPMAX, TEMPMIN  &
         , DENSMAX, DENSMIN, IDAYDATA) 
      USE vast_kind_param,ONLY: DOUBLE 
      INTEGER, INTENT(IN) :: KHGTT 
      REAL(DOUBLE), INTENT(IN) :: TIME 
      REAL(DOUBLE), INTENT(OUT) :: TTGCM 
      REAL(DOUBLE), INTENT(OUT) :: PTGCM 
      REAL(DOUBLE), INTENT(OUT) :: DTGCM 
      REAL(DOUBLE), INTENT(OUT) :: UTGCM 
      REAL(DOUBLE), INTENT(OUT) :: VTGCM 
      REAL(DOUBLE), INTENT(OUT) :: ZF 
      REAL(DOUBLE), INTENT(OUT) :: TEMPDAY 
      REAL(DOUBLE), INTENT(OUT) :: PRESDAY 
      REAL(DOUBLE), INTENT(OUT) :: DENSDAY 
      REAL(DOUBLE), INTENT(OUT) :: UWNDDAY 
      REAL(DOUBLE), INTENT(OUT) :: VWNDDAY 
      REAL(DOUBLE), INTENT(OUT) :: TEMPMAX 
      REAL(DOUBLE), INTENT(OUT) :: TEMPMIN 
      REAL(DOUBLE), INTENT(OUT) :: DENSMAX 
      REAL(DOUBLE), INTENT(OUT) :: DENSMIN 
      INTEGER, INTENT(IN) :: IDAYDATA 
!VAST.../TGCMDATA_M10/ TTA0(IN), TTA1(IN), TTP1(IN), TTA2(IN)
!VAST.../TGCMDATA_M10/ TTP2(IN), PTA0(IN), PTA1(IN), PTP1(IN)
!VAST.../TGCMDATA_M10/ PTA2(IN), PTP2(IN), DTA0(IN), DTA1(IN)
!VAST.../TGCMDATA_M10/ DTP1(IN), DTA2(IN), DTP2(IN), UTA0(IN)
!VAST.../TGCMDATA_M10/ UTA1(IN), UTP1(IN), UTA2(IN), UTP2(IN)
!VAST.../TGCMDATA_M10/ VTA0(IN), VTA1(IN), VTP1(IN), VTA2(IN)
!VAST.../TGCMDATA_M10/ VTP2(IN), ZFA0(IN), ZFA1(IN), ZFP1(IN)
!VAST.../TGCMDATA_M10/ ZFA2(IN), ZFP2(IN)
!VAST.../INTERP_M10/ DLS(INOUT), DDUST(INOUT), DLATT(INOUT)
!VAST.../INTERP_M10/ DF10(INOUT), TPOLEFAC(IN), LS(IN), MDUST(IN)
!VAST.../INTERP_M10/ ILATT(IN), MF10(IN)
!VAST...Calls: TIDEX_M10, TIDEY_M10, FOURD_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE thermos_M10_I   
      INTERFACE
! 
      SUBROUTINE thermos_M10 (FLAG, ES, TINF, TF, LAT, LON, LST, ZF, RF, ZZF   &
         , TOTALPRZ, TOTALNDZ, TZ, MOLWTG, PRZ, NDZ, MDZ, TOTALMDZ, IU0, SCALE &
         , TGRAD, DMDZ, REQUA, RPOLE, FMOL) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: FLAG 
      real(DOUBLE), DIMENSION(0:11), INTENT(IN) :: ES 
      real(DOUBLE), INTENT(IN) :: TINF 
      real(DOUBLE), INTENT(IN) :: TF 
      real(DOUBLE), INTENT(IN) :: LAT 
      real(DOUBLE), INTENT(IN) :: LON 
      real(DOUBLE), INTENT(IN) :: LST 
      real(DOUBLE), INTENT(IN) :: ZF 
      real(DOUBLE), INTENT(IN) :: RF 
      real(DOUBLE), INTENT(IN) :: ZZF 
      real(DOUBLE), INTENT(OUT) :: TOTALPRZ 
      real(DOUBLE), INTENT(OUT) :: TOTALNDZ 
      real(DOUBLE), INTENT(OUT) :: TZ 
      real(DOUBLE), INTENT(OUT) :: MOLWTG 
      real(DOUBLE), DIMENSION(0:8), INTENT(INOUT) :: PRZ 
      real(DOUBLE), DIMENSION(0:8), INTENT(INOUT) :: NDZ 
      real(DOUBLE), DIMENSION(0:8), INTENT(INOUT) :: MDZ 
      real(DOUBLE), INTENT(OUT) :: TOTALMDZ 
      integer, INTENT(IN) :: IU0 
      real(DOUBLE), INTENT(IN) :: SCALE 
      real(DOUBLE), INTENT(OUT) :: TGRAD 
      real(DOUBLE), INTENT(OUT) :: DMDZ 
      real(DOUBLE), INTENT(IN) :: REQUA 
      real(DOUBLE), INTENT(IN) :: RPOLE 
      real(DOUBLE), DIMENSION(0:8), INTENT(OUT) :: FMOL 
!VAST...Calls: RELLIPS_M10, SUBLTCHK_M10
!...This routine performs I/O.
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE thermpar_M10_I   
      INTERFACE
! 
      SUBROUTINE thermpar_M10 (RAU, FBARR, LAT, LST, SUNLAT, TINF0, TF0, ZF0&
         , SCALE) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: RAU 
      real(DOUBLE), INTENT(IN) :: FBARR 
      real(DOUBLE), INTENT(IN) :: LAT 
      real(DOUBLE), INTENT(IN) :: LST 
      real(DOUBLE), INTENT(IN) :: SUNLAT 
      real(DOUBLE), INTENT(OUT) :: TINF0 
      real(DOUBLE), INTENT(OUT) :: TF0 
      real(DOUBLE), INTENT(OUT) :: ZF0 
      real(DOUBLE), INTENT(OUT) :: SCALE 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE threed_M10_I   
      INTERFACE
! 
      SUBROUTINE threed_M10 (DX, DY, DZ, ARRAY, VALUE, LINT) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: DX 
      REAL(DOUBLE), INTENT(IN) :: DY 
      REAL(DOUBLE), INTENT(IN) :: DZ 
      REAL(DOUBLE), DIMENSION(2,2,2), INTENT(IN) :: ARRAY 
      REAL(DOUBLE), INTENT(OUT) :: VALUE 
      INTEGER, INTENT(IN) :: LINT 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE tidex_M10_I   
      INTERFACE
! 
      REAL(KIND(0.0D0)) FUNCTION tidex_M10 (A0, A1, PHI1, A2, PHI2, T) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: A0 
      REAL(DOUBLE), INTENT(IN) :: A1 
      REAL(DOUBLE), INTENT(IN) :: PHI1 
      REAL(DOUBLE), INTENT(IN) :: A2 
      REAL(DOUBLE), INTENT(IN) :: PHI2 
      REAL(DOUBLE), INTENT(IN) :: T 
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE tidey_M10_I   
      INTERFACE
! 
      REAL(KIND(0.0D0)) FUNCTION tidey_M10 (A0, A1, PHI1, A2, PHI2, T) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: A0 
      real(DOUBLE), INTENT(IN) :: A1 
      real(DOUBLE), INTENT(IN) :: PHI1 
      real(DOUBLE), INTENT(IN) :: A2 
      real(DOUBLE), INTENT(IN) :: PHI2 
      real(DOUBLE), INTENT(IN) :: T 
      END FUNCTION  
      END INTERFACE 
      END MODULE
!================================================ 
      MODULE topoareo_M10_I   
      INTERFACE
! 
      SUBROUTINE topoareo_M10 (CLAT, CLONWIN, CAREOID, CTOPOHGT, CALBEDO, IAU) 
      USE vast_kind_param,ONLY: DOUBLE 
      REAL(DOUBLE), INTENT(IN) :: CLAT 
      REAL(DOUBLE), INTENT(IN) :: CLONWIN 
      REAL(DOUBLE), INTENT(INOUT) :: CAREOID 
      REAL(DOUBLE), INTENT(INOUT) :: CTOPOHGT 
      REAL(DOUBLE), INTENT(INOUT) :: CALBEDO 
      INTEGER, INTENT(IN) :: IAU 
!VAST.../TERHGT_M10/ AREORAD(IN), TOPOMOLA(IN), ALBEDO(IN)
!VAST...Calls: IFLOOR_M10, TWOD_M10
      END SUBROUTINE  
      END INTERFACE 
      END MODULE 
!================================================ 
      MODULE ttidey_M10_I   
      INTERFACE
      REAL(KIND(0.0D0)) FUNCTION ttidey_M10 (A0, A1, PHI1, A2, PHI2, T) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: A0 
      real(DOUBLE), INTENT(IN) :: A1 
      real(DOUBLE), INTENT(IN) :: PHI1 
      real(DOUBLE), INTENT(IN) :: A2 
      real(DOUBLE), INTENT(IN) :: PHI2 
      real(DOUBLE), INTENT(IN) :: T 
      END FUNCTION  
      END INTERFACE 
      END MODULE 
!================================================ 
      MODULE twod_M10_I   
      INTERFACE
! 
      SUBROUTINE twod_M10 (DX, DY, ARRAY, VALUE) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: DX 
      real(DOUBLE), INTENT(IN) :: DY 
      real(DOUBLE), DIMENSION(2,2), INTENT(IN) :: ARRAY 
      real(DOUBLE), INTENT(OUT) :: VALUE 
      END SUBROUTINE  
      END INTERFACE 
      END MODULE 
!================================================ 
      MODULE wavelon_M10_I   
      INTERFACE
! 
      SUBROUTINE wavelon_M10 (LONEW, WLON, CLAT, HEIGHT, DAY, WAVEPERT) 
      USE vast_kind_param,ONLY: DOUBLE 
      integer, INTENT(IN) :: LONEW 
      real(DOUBLE), INTENT(IN) :: WLON 
      real(DOUBLE), INTENT(IN) :: CLAT 
      real(DOUBLE), INTENT(IN) :: HEIGHT 
      real(DOUBLE), INTENT(IN) :: DAY 
      real(DOUBLE), INTENT(OUT) :: WAVEPERT 
!VAST.../WAVECOEF_M10/ WAVEA0(IN), WAVEA1(IN), WAVEPHI1(IN)
!VAST.../WAVECOEF_M10/ WAVEA2(IN), WAVEPHI2(IN), WAVEA3(IN)
!VAST.../WAVECOEF_M10/ WAVEPHI3(IN), WSCALE(IN), PHI1DOT(IN)
!VAST.../WAVECOEF_M10/ PHI2DOT(IN), PHI3DOT(IN), WAVEDATE(IN)
      END SUBROUTINE  
      END INTERFACE 
      END MODULE 
!================================================ 
      MODULE zlogr_M10_I   
      INTERFACE
! 
      REAL(KIND(0.0D0)) FUNCTION zlogr_M10 (ZNUMER, ZDENOM, LABEL) 
      USE vast_kind_param,ONLY: DOUBLE 
      real(DOUBLE), INTENT(IN) :: ZNUMER 
      real(DOUBLE), INTENT(IN) :: ZDENOM 
      character (LEN = 7), INTENT(IN) :: LABEL 
!...This routine performs I/O.
      END FUNCTION  
      END INTERFACE 
      END MODULE 
!================================================ 
