!     Subroutines and Functions for                                               !ATM2  1
!     Mars-GRAM 2010 - Version 1.0,  Nov 2010                                     !ATM2  2
!-------------------------------------------------------------------------------  !ATM2  3
!                                                                                 !ATM2  4
      SUBROUTINE ATMOS2_M10(HGTIN, CLAT, CLON, MARSAU, SUNLAT, SUNLON, ALS, H,  & !ATM2  5
         TEMP, DENST, UPFCTR, LWFCTR, PRES, THGT, CAREOID, ZF, IU0, DELTATEX,   & !ATM2  6
         TEXOS, TBASE, HRHO, AMZ, DUSTTAU, DUSTMIN, DUSTMAX, DUSTOD, EWWIND,    & !ATM2  7
         NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, HATZF, WAVEPERT, TEMPDAY,      & !ATM2  8
         PRESDAY, DENSDAY, EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, HGTASFC, PATSURF,  & !ATM2  9
         TEMPMAX, TEMPMIN, DENSMAX, DENSMIN, TGRND, TALB, ICEPOLAR, GZ, OLDRREF & !ATM2 10
         , REQUA, RPOLE, MAPYEAR, PROFNEAR, PROFFAR, NPROF, PROFWGT, IDAYDATA)    !ATM2 11
!-----------------------------------------------                                  !ATM2 12
!   M o d u l e s                                                                 !ATM2 13
!-----------------------------------------------                                  !ATM2 14
      USE vast_kind_param, ONLY:  DOUBLE                                          !ATM2 15
      USE THERM_M10_C                                                             !ATM2 16
!...                                                                              !ATM2 17
!...Switches:                                                                     !ATM2 18
!-----------------------------------------------                                  !ATM2 19
!   I n t e r f a c e   B l o c k s                                               !ATM2 20
!-----------------------------------------------                                  !ATM2 21
      USE tesod_M10_I                                                             !ATM2 22
      USE dustvsls_M10_I                                                          !ATM2 23
      USE dustfact_M10_I                                                          !ATM2 24
      USE rellips_M10_I                                                           !ATM2 25
      USE marsgcm_M10_I                                                           !ATM2 26
      USE tesgcm_M10_I                                                            !ATM2 27
      USE stewart2_M10_I                                                          !ATM2 28
      USE cp_M10_I                                                                !ATM2 29
      USE profterp_M10_I                                                          !ATM2 30
      IMPLICIT NONE                                                               !ATM2 31
!-----------------------------------------------                                  !ATM2 32
!   D u m m y   A r g u m e n t s                                                 !ATM2 33
!-----------------------------------------------                                  !ATM2 34
      REAL(DOUBLE), INTENT(IN) :: HGTIN                                           !ATM2 35
      REAL(DOUBLE), INTENT(IN) :: CLAT                                            !ATM2 36
      REAL(DOUBLE), INTENT(IN) :: CLON                                            !ATM2 37
      REAL(DOUBLE), INTENT(IN) :: MARSAU                                          !ATM2 38 
      REAL(DOUBLE), INTENT(IN) :: SUNLAT                                          !ATM2 39 
      REAL(DOUBLE), INTENT(IN) :: SUNLON                                          !ATM2 40 
      REAL(DOUBLE), INTENT(IN) :: ALS                                             !ATM2 41
      REAL(DOUBLE), INTENT(INOUT) :: H                                            !ATM2 42
      REAL(DOUBLE), INTENT(INOUT) :: TEMP                                         !ATM2 43 
      REAL(DOUBLE), INTENT(INOUT) :: DENST                                        !ATM2 44 
      REAL(DOUBLE), INTENT(OUT) :: UPFCTR                                         !ATM2 45
      REAL(DOUBLE), INTENT(OUT) :: LWFCTR                                         !ATM2 46
      REAL(DOUBLE), INTENT(INOUT) :: PRES                                         !ATM2 47
      REAL(DOUBLE), INTENT(INOUT) :: THGT                                         !ATM2 48
      REAL(DOUBLE), INTENT(INOUT) :: CAREOID                                      !ATM2 49
      REAL(DOUBLE), INTENT(INOUT) :: ZF                                           !ATM2 50
      INTEGER, INTENT(IN) :: IU0                                                  !ATM2 51
      REAL(DOUBLE), INTENT(IN) :: DELTATEX                                        !ATM2 52 
      REAL(DOUBLE), INTENT(OUT) :: TEXOS                                          !ATM2 53
      REAL(DOUBLE), INTENT(OUT) :: TBASE                                          !ATM2 54
      REAL(DOUBLE), INTENT(OUT) :: HRHO                                           !ATM2 55
      REAL(DOUBLE), INTENT(OUT) :: AMZ                                            !ATM2 56
      REAL(DOUBLE), INTENT(IN) :: DUSTTAU                                         !ATM2 57
      REAL(DOUBLE), INTENT(IN) :: DUSTMIN                                         !ATM2 58
      REAL(DOUBLE), INTENT(IN) :: DUSTMAX                                         !ATM2 59
      REAL(DOUBLE), INTENT(INOUT) :: DUSTOD                                       !ATM2 60 
      REAL(DOUBLE), INTENT(OUT) :: EWWIND                                         !ATM2 61
      REAL(DOUBLE), INTENT(OUT) :: NSWIND                                         !ATM2 62
      REAL(DOUBLE), INTENT(OUT) :: BLWINDEW                                       !ATM2 63 
      REAL(DOUBLE), INTENT(OUT) :: BLWINDNS                                       !ATM2 64 
      REAL(DOUBLE), INTENT(OUT) :: BLWINDVERT                                     !ATM2 65 
      REAL(DOUBLE), INTENT(INOUT) :: HATZF                                        !ATM2 66
      REAL(DOUBLE), INTENT(IN) :: WAVEPERT                                        !ATM2 67
      REAL(DOUBLE), INTENT(OUT) :: TEMPDAY                                        !ATM2 68
      REAL(DOUBLE), INTENT(INOUT) :: PRESDAY                                      !ATM2 69
      REAL(DOUBLE), INTENT(INOUT) :: DENSDAY                                      !ATM2 70
      REAL(DOUBLE), INTENT(OUT) :: EWWNDAY                                        !ATM2 71
      REAL(DOUBLE), INTENT(OUT) :: NSWNDAY                                        !ATM2 72
      REAL(DOUBLE), INTENT(OUT) :: BLUDAY                                         !ATM2 73
      REAL(DOUBLE), INTENT(OUT) :: BLVDAY                                         !ATM2 74
      REAL(DOUBLE), INTENT(IN) :: HGTASFC                                         !ATM2 75
      REAL(DOUBLE), INTENT(OUT) :: PATSURF                                        !ATM2 76
      REAL(DOUBLE), INTENT(OUT) :: TEMPMAX                                        !ATM2 77
      REAL(DOUBLE), INTENT(OUT) :: TEMPMIN                                        !ATM2 78
      REAL(DOUBLE), INTENT(INOUT) :: DENSMAX                                      !ATM2 79
      REAL(DOUBLE), INTENT(INOUT) :: DENSMIN                                      !ATM2 80
      REAL(DOUBLE), INTENT(OUT) :: TGRND                                          !ATM2 81
      REAL(DOUBLE), INTENT(OUT) :: TALB                                           !ATM2 82
      INTEGER, INTENT(INOUT) :: ICEPOLAR                                          !ATM2 83
      REAL(DOUBLE), INTENT(INOUT) :: GZ                                           !ATM2 84
      REAL(DOUBLE), INTENT(INOUT) :: OLDRREF                                      !ATM2 85
      REAL(DOUBLE), INTENT(IN) :: REQUA                                           !ATM2 86
      REAL(DOUBLE), INTENT(IN) :: RPOLE                                           !ATM2 87
      INTEGER, INTENT(IN) :: MAPYEAR                                              !ATM2 88
      REAL(DOUBLE), INTENT(IN) :: PROFNEAR                                        !ATM2 89
      REAL(DOUBLE), INTENT(IN) :: PROFFAR                                         !ATM2 90
      INTEGER, INTENT(INOUT) :: NPROF                                             !ATM2 91
      REAL(DOUBLE), INTENT(INOUT) :: PROFWGT                                      !ATM2 92
      INTEGER, INTENT(IN) :: IDAYDATA                                             !ATM2 93
!-----------------------------------------------                                  !ATM2 94
!   L o c a l   P a r a m e t e r s                                               !ATM2 95
!-----------------------------------------------                                  !ATM2 96
!-----------------------------------------------                                  !ATM2 99
!   L o c a l   V a r i a b l e s                                                 !ATM2100
!-----------------------------------------------                                  !ATM2101
      INTEGER :: ICEZ, ICEX, ICE1                                                 !ATM2102
      REAL(DOUBLE) :: NSWNDAY1, NSWND1, RSTAR, AMW,  TOPZ, TIME, DUSTM,         & !ATM2103
         STORMDZ, DUSTOFFSET, TAT5M, TEMPZ, PRESZ, DENSTZ, BLWUZ, BLWVZ, BLWWZ  & !ATM2104
         , HZ, HRHOZ, ZFZ, TFACTOR, TDAYZ, PDAYZ, DDAYZ, UDAYZ, VDAYZ, BLUDX,   & !ATM2105
         BLVDX, TMAXZ, TMINZ, DMAXZ, DMINZ, TGRNDZ, TALBZ, CHGT, TF, PX, DX, UX & !ATM2106
         , VX, UBLX, VBLX, WBLZ, HDX, ZFP, UPF, TOPHGTX, AREOIDX, TDZ, PDX, DDX & !ATM2107
         , UDX, VDX, TMAXX, TMINX, DMAXX, DMINX, TGRDNX, TALBX, TEMP1, PRES1,   & !ATM2108
         DENST1, EWWND1, H1, HRHO1, ZF1, TEMPDAY1, PRESDAY1, DENSDAY1, EWWNDAY1 & !ATM2109
         , BLUDAY1, BLVDAY1, TEMPMAX1, TEMPMIN1, DENSMAX1, DENSMIN1, TGRND1,    & !ATM2110
         SHGT, PRESTOP, TEMPTOP, DENSTOP, HTOP, AMTOP, TINF, HRHOTOP, TGRADTOP  & !ATM2111
         , DDTEX, AMZ1, TGRAD, WBLX, TDX, TGRNDX,                               & !ATM2113
         STEWFDENS, STEWFPRES, TOUT, POUT, DOUT,                                & !ATM2114
         UOUT, VOUT                                                               !ATM2115
!-----------------------------------------------                                  !ATM2116
!---  Rstar = Universal gas constant                                              !ATM2117
!---  AMW = average molecular weight up to turbopause                             !ATM2118
      DATA RSTAR, AMW/ 8.314472D3, 43.49D0/                                       !ATM2119
!---  Top height = 170 km for MapYear=0 otherwise Top height = 240 km             !ATM2122
      TOPZ = 170.0D0                                                              !ATM2123
      IF (MAPYEAR > 0) TOPZ = 240.0D0                                             !ATM2124
!---  Set slope winds to zero                                                     !ATM2125
      BLWINDEW = 0.0D0                                                            !ATM2126
      BLWINDNS = 0.0D0                                                            !ATM2127
!---  Local solar time (Mars hours = 1/24th Sol)                                  !ATM2128
      TIME = 12.0D0 + (SUNLON - CLON)/15.0D0                                      !ATM2129
      IF (TIME < 0.0D0) TIME = TIME + 24.0D0                                      !ATM2130
      IF (TIME > 24.0D0) TIME = TIME - 24.0D0                                     !ATM2131
      IF (MAPYEAR > 0) THEN                                                       !ATM2132
!---    Evaluate dust from Mapping Year 1 or 2 input file                         !ATM2133
         CALL TESOD_M10 (MAPYEAR, ALS, CLAT, CLON, DUSTOD)                        !ATM2134
      ELSE                                                                        !ATM2135
!---    Evaluate dust optical depth from NameList input parameters                !ATM2136
         IF (DUSTTAU > 0.0D0) THEN                                                !ATM2137
            DUSTOD = DUSTTAU                                                      !ATM2138
            DUSTOD = DMAX1(0.1D0,DUSTOD)                                          !ATM2139
         ELSE                                                                     !ATM2140
            DUSTOD = DUSTVSLS_M10(ALS,DUSTMIN,DUSTMAX)                            !ATM2141
         ENDIF                                                                    !ATM2142
      ENDIF                                                                       !ATM2143
!---  Evaluate factor for dust storm model                                        !ATM2144
      CALL DUSTFACT_M10 (CLAT, CLON, ALS, DUSTM, STORMDZ)                         !ATM2145
      DUSTOD = DUSTOD + DUSTM                                                     !ATM2146
!---  Evaluate MTGCM height offset due to dust storm                              !ATM2147
      DUSTOFFSET = STORMDZ                                                        !ATM2148
!---  Get areoid radius and topographic height at current lat, lon                !ATM2149
      CALL RELLIPS_M10 (CLAT, CLON, CAREOID, HGTIN, GZ, OLDRREF, THGT, TALB,    & !ATM2150
         REQUA, RPOLE)                                                            !ATM2151
!---  Evaluate pressure and temperature at surface, Patsurf and Tat5m             !ATM2152
      TAT5M = 0.0D0                                                               !ATM2153
      IF (MAPYEAR == 0) THEN                                                      !ATM2154
         CALL MARSGCM_M10 (THGT, CLAT, CLON, ALS, DUSTOD, TIME, TEMPZ, PRESZ,   & !ATM2155
            DENSTZ, EWWIND, NSWIND, BLWUZ, BLWVZ, BLWWZ, HZ, HRHOZ, ZFZ,        & !ATM2156
            TFACTOR, THGT, HGTASFC, CAREOID, TDAYZ, PDAYZ, DDAYZ, UDAYZ, VDAYZ  & !ATM2157
            , BLUDX, BLVDX, TMAXZ, TMINZ, DMAXZ, DMINZ, TGRNDZ, TALBZ, ICEZ,    & !ATM2158
            TAT5M, DUSTOFFSET, REQUA, RPOLE, IDAYDATA)                            !ATM2159
      ELSE                                                                        !ATM2160
         CALL TESGCM_M10 (THGT, CLAT, CLON, ALS, TIME, TEMPZ, PRESZ, DENSTZ,    & !ATM2161
            EWWIND, NSWIND, BLWUZ, BLWVZ, BLWWZ, HZ, HRHOZ, ZFZ, TFACTOR, THGT  & !ATM2162
            , HGTASFC, CAREOID, TDAYZ, PDAYZ, DDAYZ, UDAYZ, VDAYZ, BLUDX, BLVDX & !ATM2163
            , TMAXZ, TMINZ, DMAXZ, DMINZ, TGRNDZ, TALBZ, ICEZ, TAT5M, REQUA,    & !ATM2164
            RPOLE, MAPYEAR, IDAYDATA)                                             !ATM2165
      ENDIF                                                                       !ATM2166
      PATSURF = PRESZ                                                             !ATM2167
      TGRND = TGRNDZ                                                              !ATM2168
!---  Adjust Patsurf for wave perturbation                                        !ATM2169
      PATSURF = PATSURF*(1.0D0 + WAVEPERT)                                        !ATM2170
!---  Tat5m is air temperature at surface + 5 meters                              !ATM2171
!---  Set evaluation hgt (CHGT) to HGTIN or to thgt+hgtasfc if HGTIN <=           !ATM2172
!     -8.7 km                                                                     !ATM2173
      CHGT = HGTIN                                                                !ATM2174
      IF (HGTIN <= (-8.7D0)) CHGT = THGT + HGTASFC                                !ATM2175
!---  Compute atmosphere using only MGCM data if height below 80 km               !ATM2176
      IF (CHGT <= 80.0D0) THEN                                                    !ATM2177
         IF (MAPYEAR == 0) THEN                                                   !ATM2178
            CALL MARSGCM_M10 (CHGT, CLAT, CLON, ALS, DUSTOD, TIME, TEMP, PRES,  & !ATM2179
               DENST, EWWIND, NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, H, HRHO,  & !ATM2180
               ZF, TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY, PRESDAY, DENSDAY,  & !ATM2181
               EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, TEMPMAX, TEMPMIN, DENSMAX,     & !ATM2182
               DENSMIN, TGRNDZ, TALB, ICEPOLAR, TAT5M, DUSTOFFSET, REQUA, RPOLE & !ATM2183
               , IDAYDATA)                                                        !ATM2184
         ELSE                                                                     !ATM2185
            CALL TESGCM_M10 (CHGT, CLAT, CLON, ALS, TIME, TEMP, PRES, DENST,    & !ATM2186
               EWWIND, NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, H, HRHO, ZF,     & !ATM2187
               TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY, PRESDAY, DENSDAY,      & !ATM2188
               EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, TEMPMAX, TEMPMIN, DENSMAX,     & !ATM2189
               DENSMIN, TGRNDZ, TALB, ICEPOLAR, TAT5M, REQUA, RPOLE, MAPYEAR,   & !ATM2190
               IDAYDATA)                                                          !ATM2191
         ENDIF                                                                    !ATM2192
!---    Adjust PRES, and DENST for wave perturbation                              !ATM2193
         PRES = PRES*(1.0D0 + WAVEPERT)                                           !ATM2194
         DENST = DENST*(1.0D0 + WAVEPERT)                                         !ATM2195
         DENSDAY = DENSDAY*(1.0D0 + WAVEPERT)                                     !ATM2196
         PRESDAY = PRESDAY*(1.0D0 + WAVEPERT)                                     !ATM2197
         IF (IDAYDATA > 0) THEN                                                   !ATM2198
            DENSMAX = DENSMAX*(1.0D0 + WAVEPERT)                                  !ATM2199
            DENSMIN = DENSMIN*(1.0D0 + WAVEPERT)                                  !ATM2200
         ENDIF                                                                    !ATM2201
         HATZF = 0.0D0                                                            !ATM2202
         UPFCTR = 1.0D0 + TFACTOR                                                 !ATM2203
         LWFCTR = 1.0D0/TFACTOR                                                   !ATM2204
         AMZ = AMW                                                                !ATM2205
      ELSE IF (CHGT <= TOPZ) THEN                                                 !ATM2206
!---    This section uses MTGCM for density, pressure, temperature and            !ATM2207
!       Stewart model for mixing ratios                                           !ATM2208
!---    Evaluate MTGCM at current height                                          !ATM2209
         IF (MAPYEAR == 0) THEN                                                   !ATM2210
            CALL MARSGCM_M10 (CHGT, CLAT, CLON, ALS, DUSTOD, TIME, TEMP, PRES,  & !ATM2211
               DENST, EWWIND, NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, H, HRHO,  & !ATM2212
               ZF, TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY, PRESDAY, DENSDAY,  & !ATM2213
               EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, TEMPMAX, TEMPMIN, DENSMAX,     & !ATM2214
               DENSMIN, TGRNDZ, TALB, ICEPOLAR, TAT5M, DUSTOFFSET, REQUA, RPOLE & !ATM2215
               , IDAYDATA)                                                        !ATM2216
         ELSE                                                                     !ATM2217
            CALL TESGCM_M10 (CHGT, CLAT, CLON, ALS, TIME, TEMP, PRES, DENST,    & !ATM2218
               EWWIND, NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, H, HRHO, ZF,     & !ATM2219
               TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY, PRESDAY, DENSDAY,      & !ATM2220
               EWWNDAY, NSWNDAY, BLUDAY, BLVDAY, TEMPMAX, TEMPMIN, DENSMAX,     & !ATM2221
               DENSMIN, TGRNDZ, TALB, ICEPOLAR, TAT5M, REQUA, RPOLE, MAPYEAR,   & !ATM2222
               IDAYDATA)                                                          !ATM2223
         ENDIF                                                                    !ATM2224
!---    Adjust PRES, and DENST for wave perturbation                              !ATM2225
         PRES = PRES*(1.0D0 + WAVEPERT)                                           !ATM2226
         DENST = DENST*(1.0D0 + WAVEPERT)                                         !ATM2227
         DENSDAY = DENSDAY*(1.0D0 + WAVEPERT)                                     !ATM2228
         PRESDAY = PRESDAY*(1.0D0 + WAVEPERT)                                     !ATM2229
         IF (IDAYDATA > 0) THEN                                                   !ATM2230
            DENSMAX = DENSMAX*(1.0D0 + WAVEPERT)                                  !ATM2231
            DENSMIN = DENSMIN*(1.0D0 + WAVEPERT)                                  !ATM2232
         ENDIF                                                                    !ATM2233
!---    Compute molecular weight                                                  !ATM2234
         AMZ = DENST*RSTAR*TEMP/PRES                                              !ATM2235
!---    Find temperature TF at height ZF = altitude of 1.26 nbar level            !ATM2236
         IF (ZF > 900.0D0) THEN                                                   !ATM2237
            TF = 999.9D0                                                          !ATM2238
         ELSE                                                                     !ATM2239
            IF (MAPYEAR == 0) THEN                                                !ATM2240
               CALL MARSGCM_M10 (ZF, CLAT, CLON, ALS, DUSTOD, TIME, TF, PX, DX  & !ATM2241
                  , UX, VX, UBLX, VBLX, WBLZ, HATZF, HDX, ZFP, UPF, TOPHGTX,    & !ATM2242
                  HGTASFC, AREOIDX, TDZ, PDX, DDX, UDX, VDX, BLUDX, BLVDX,      & !ATM2243
                  TMAXX, TMINX, DMAXX, DMINX, TGRDNX, TALBX, ICEX, TAT5M,       & !ATM2244
                  DUSTOFFSET, REQUA, RPOLE, IDAYDATA)                             !ATM2245
            ELSE                                                                  !ATM2246
               CALL TESGCM_M10 (ZF, CLAT, CLON, ALS, TIME, TF, PX, DX, UX, VX,  & !ATM2247
                  UBLX, VBLX, WBLZ, HATZF, HDX, ZFP, UPF, TOPHGTX, HGTASFC,     & !ATM2248
                  AREOIDX, TDZ, PDX, DDX, UDX, VDX, BLUDX, BLVDX, TMAXX, TMINX  & !ATM2249
                  , DMAXX, DMINX, TGRDNX, TALBX, ICEX, TAT5M, REQUA, RPOLE,     & !ATM2250
                  MAPYEAR, IDAYDATA)                                              !ATM2251
            ENDIF                                                                 !ATM2252
!---      Adjust ZF for wave perturbation                                         !ATM2253
            ZF = ZF + HATZF*DLOG(1.0D0 + WAVEPERT)                                !ATM2254
         ENDIF                                                                    !ATM2255
!---    Save temperature at 1.26 nbar level                                       !ATM2256
         TBASE = TF                                                               !ATM2257
         UPFCTR = 1.0D0 + TFACTOR                                                 !ATM2258
         LWFCTR = 1.0D0/TFACTOR                                                   !ATM2259
!---    Use MTGCM data to get (unperturbed) values at TopZ km                     !ATM2260
         IF (MAPYEAR == 0) THEN                                                   !ATM2261
            CALL MARSGCM_M10 (TOPZ, CLAT, CLON, ALS, DUSTOD, TIME, TEMP1, PRES1 & !ATM2262
               , DENST1, EWWND1, NSWND1, BLWINDEW, BLWINDNS, BLWINDVERT, H1,    & !ATM2263
               HRHO1, ZF1, TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY1, PRESDAY1  & !ATM2264
               , DENSDAY1, EWWNDAY1, NSWNDAY1, BLUDAY1, BLVDAY1, TEMPMAX1,      & !ATM2265
               TEMPMIN1, DENSMAX1, DENSMIN1, TGRND1, TALB, ICE1, TAT5M,         & !ATM2266
               DUSTOFFSET, REQUA, RPOLE, IDAYDATA)                                !ATM2267
         ELSE                                                                     !ATM2268
            CALL TESGCM_M10 (TOPZ, CLAT, CLON, ALS, TIME, TEMP1, PRES1, DENST1  & !ATM2269
               , EWWND1, NSWND1, BLWINDEW, BLWINDNS, BLWINDVERT, H1, HRHO1, ZF1 & !ATM2270
               , TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY1, PRESDAY1, DENSDAY1  & !ATM2271
               , EWWNDAY1, NSWNDAY1, BLUDAY1, BLVDAY1, TEMPMAX1, TEMPMIN1,      & !ATM2272
               DENSMAX1, DENSMIN1, TGRND1, TALB, ICE1, TAT5M, REQUA, RPOLE,     & !ATM2273
               MAPYEAR, IDAYDATA)                                                 !ATM2274
         ENDIF                                                                    !ATM2275
!---    Evaluate Stewart thermosphere at TopZ for deltaTEX adjustment             !ATM2276
         SHGT = TOPZ                                                              !ATM2277
         CALL STEWART2_M10 (MARSAU, CLAT, CLON, TIME, PRESTOP, TEMPTOP, DENSTOP & !ATM2278
            , SHGT, RSTAR, HTOP, AMTOP, 0.0D0, IU0, SUNLAT, DELTATEX, TINF, TF  & !ATM2279
            , ZF1, HRHOTOP, REQUA, RPOLE, TGRADTOP)                               !ATM2280
!---    Adjust deltaTEX for temperature difference at TopZ                        !ATM2281
         DDTEX = DELTATEX + TEMP1 - TEMPTOP                                       !ATM2282
         SHGT = CHGT                                                              !ATM2283
         IF (CHGT < ZF) SHGT = ZF                                                 !ATM2284
!---    Evaluate thermospheric parameters at current height                       !ATM2285
         CALL STEWART2_M10 (MARSAU, CLAT, CLON, TIME, PRES1, TEMP1, DENST1,     & !ATM2286
            SHGT, RSTAR, H1, AMZ1, 0.0D0, IU0, SUNLAT, DDTEX, TINF, TF, ZF,     & !ATM2287
            HRHO1, REQUA, RPOLE, TGRAD)                                           !ATM2288
!---    Save exospheric temperature                                               !ATM2289
         TEXOS = TINF                                                             !ATM2290
      ELSE                                                                        !ATM2291
!---    For height above TopZ -  Use MTGCM data to get (unperturbed)              !ATM2292
!       values at TopZ                                                            !ATM2293
         IF (MAPYEAR == 0) THEN                                                   !ATM2294
            CALL MARSGCM_M10 (TOPZ, CLAT, CLON, ALS, DUSTOD, TIME, TEMP1, PRES1 & !ATM2295
               , DENST1, EWWIND, NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, H1,    & !ATM2296
               HRHO1, ZF, TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY1, PRESDAY1,  & !ATM2297
               DENSDAY1, EWWNDAY1, NSWNDAY1, BLUDAY1, BLVDAY1, TEMPMAX1,        & !ATM2298
               TEMPMIN1, DENSMAX1, DENSMIN1, TGRND1, TALB, ICE1, TAT5M,         & !ATM2299
               DUSTOFFSET, REQUA, RPOLE, IDAYDATA)                                !ATM2300
         ELSE                                                                     !ATM2301
            CALL TESGCM_M10 (TOPZ, CLAT, CLON, ALS, TIME, TEMP1, PRES1, DENST1  & !ATM2302
               , EWWIND, NSWIND, BLWINDEW, BLWINDNS, BLWINDVERT, H1, HRHO1, ZF  & !ATM2303
               , TFACTOR, THGT, HGTASFC, CAREOID, TEMPDAY1, PRESDAY1, DENSDAY1  & !ATM2304
               , EWWNDAY1, NSWNDAY1, BLUDAY1, BLVDAY1, TEMPMAX1, TEMPMIN1,      & !ATM2305
               DENSMAX1, DENSMIN1, TGRND1, TALB, ICE1, TAT5M, REQUA, RPOLE,     & !ATM2306
               MAPYEAR, IDAYDATA)                                                 !ATM2307
         ENDIF                                                                    !ATM2308
!---    Find temperature TF at height ZF  = height of 1.26 nbar level             !ATM2309
         IF (MAPYEAR == 0) THEN                                                   !ATM2310
            CALL MARSGCM_M10 (ZF, CLAT, CLON, ALS, DUSTOD, TIME, TF, PX, DX, UX & !ATM2311
               , VX, UBLX, VBLX, WBLX, HATZF, HDX, ZFP, UPF, TOPHGTX, HGTASFC,  & !ATM2312
               AREOIDX, TDX, PDX, DDX, UDX, VDX, BLUDX, BLVDX, TMAXX, TMINX,    & !ATM2313
               DMAXX, DMINX, TGRNDX, TALBX, ICEX, TAT5M, DUSTOFFSET, REQUA,     & !ATM2314
               RPOLE, IDAYDATA)                                                   !ATM2315
         ELSE                                                                     !ATM2316
            CALL TESGCM_M10 (ZF, CLAT, CLON, ALS, TIME, TF, PX, DX, UX, VX,     & !ATM2317
               UBLX, VBLX, WBLX, HATZF, HDX, ZFP, UPF, TOPHGTX, HGTASFC,        & !ATM2318
               AREOIDX, TDX, PDX, DDX, UDX, VDX, BLUDX, BLVDX, TMAXX, TMINX,    & !ATM2319
               DMAXX, DMINX, TGRNDX, TALBX, ICEX, TAT5M, REQUA, RPOLE, MAPYEAR  & !ATM2320
               , IDAYDATA)                                                        !ATM2321
         ENDIF                                                                    !ATM2322
!---    Evaluate Stewart thermosphere at TopZ for deltaTEX adjustment             !ATM2323
         SHGT = TOPZ                                                              !ATM2324
         CALL STEWART2_M10 (MARSAU, CLAT, CLON, TIME, PRESTOP, TEMPTOP, DENSTOP & !ATM2325
            , SHGT, RSTAR, HTOP, AMTOP, 0.0D0, IU0, SUNLAT, DELTATEX, TINF, TF  & !ATM2326
            , ZF, HRHOTOP, REQUA, RPOLE, TGRADTOP)                                !ATM2327
!---    Adjust deltaTEX for temperature difference at TopZ                        !ATM2328
         DDTEX = DELTATEX + TEMP1 - TEMPTOP                                       !ATM2329
!---    Adjust ZF for wave perturbation, using scale height at ZF                 !ATM2330
         ZF = ZF + HATZF*DLOG(1.0D0 + WAVEPERT)                                   !ATM2331
!---    Evaluate Stewart thermosphere at TopZ for adjustment factors              !ATM2332
         CALL STEWART2_M10 (MARSAU, CLAT, CLON, TIME, PRESTOP, TEMPTOP, DENSTOP & !ATM2333
            , SHGT, RSTAR, HTOP, AMTOP, 0.0D0, IU0, SUNLAT, DDTEX, TINF, TF, ZF & !ATM2334
            , HRHOTOP, REQUA, RPOLE, TGRADTOP)                                    !ATM2335
!---    Set daily average values to zero above TopZ                               !ATM2336
         TEMPDAY = 0.0D0                                                          !ATM2337
         PRESDAY = 0.0D0                                                          !ATM2338
         DENSDAY = 0.0D0                                                          !ATM2339
         EWWNDAY = 0.0D0                                                          !ATM2340
         NSWNDAY = 0.0D0                                                          !ATM2341
!---    Set daily max, min Temp and Density to zero above TopZ                    !ATM2342
         TEMPMAX = 0.0D0                                                          !ATM2343
         TEMPMIN = 0.0D0                                                          !ATM2344
         DENSMAX = 0.0D0                                                          !ATM2345
         DENSMIN = 0.0D0                                                          !ATM2346
!---    Save temperature at 1.26 nbar level                                       !ATM2347
         TBASE = TF                                                               !ATM2348
         SHGT = CHGT                                                              !ATM2349
!---    Evaluate thermospheric parameters at current height                       !ATM2350
         CALL STEWART2_M10 (MARSAU, CLAT, CLON, TIME, PRES, TEMP, DENST, SHGT,  & !ATM2351
            RSTAR, H, AMZ, 0.0D0, IU0, SUNLAT, DDTEX, TINF, TF, ZF, HRHO, REQUA & !ATM2352
            , RPOLE, TGRAD)                                                       !ATM2353
!---    Save exospheric temperature                                               !ATM2354
         TEXOS = TINF                                                             !ATM2355
!---        New thermospheric perturbation sigmas assumed to be 45%               !ATM2364            
            UPFCTR = 1.45d0                                                       !ATM2365 
            LWFCTR = 1.0d0/UPFCTR                                                 !ATM2366 
!---    Apply adjustment factors to density and pressure                          !ATM2379
         STEWFDENS = DENST1/DENSTOP                                               !ATM2380
         DENST = DENST*STEWFDENS*(1.0D0 + WAVEPERT)                               !ATM2381
         STEWFPRES = PRES1/PRESTOP                                                !ATM2382
         PRES = PRES*STEWFPRES*(1.0D0 + WAVEPERT)                                 !ATM2383
!---    Adjust molecular weight                                                   !ATM2384
         AMZ = RSTAR*DENST*TEMP/PRES                                              !ATM2385
      ENDIF                                                                       !ATM2386
!---  Use weighted average profile data if profnear > 0. Weight=1 if              !ATM2396
!     lat-lon radius < profnear. Weight=0 if lat-lon radius > proffar.            !ATM2397
      IF (PROFNEAR > 0.0D0) THEN                                                  !ATM2398
         CALL PROFTERP_M10 (CHGT, CLAT, CLON, TEMP, PRES, DENST, EWWIND, NSWIND & !ATM2399
            , TOUT, POUT, DOUT, UOUT, VOUT, NPROF, PROFNEAR, PROFFAR, PROFWGT,  & !ATM2400
            WAVEPERT)                                                             !ATM2401
         TEMP = TOUT                                                              !ATM2402
         PRES = POUT                                                              !ATM2403
         DENST = DOUT                                                             !ATM2404
         EWWIND = UOUT                                                            !ATM2405
         NSWIND = VOUT                                                            !ATM2406
      ENDIF                                                                       !ATM2407
      RETURN                                                                      !ATM2408
      END SUBROUTINE ATMOS2_M10                                                   !ATM2409
!                                                                                 !ATM2410
!------------------------------------------------------------------------------   !BLTP  1
      subroutine bltp_M10(gz, cp, tg, z5, t5, u5, v5, zeval, factor, tempz)       !BLTP  2
!-----------------------------------------------                                  !BLTP  3
!   M o d u l e s                                                                 !BLTP  4
!-----------------------------------------------                                  !BLTP  5
      USE vast_kind_param, ONLY:  double                                          !BLTP  6
!                                                                                 !BLTP  7
!---    Mars boundary layer temperature from methods used in the NASA             !BLTP  8
!       Ames Mars General Circulation Model (MGCM), as described by               !BLTP  9
!       Haberle et al., Jour. Geophys. Res. 104(E4), 8957-8974, 1999,             !BLTP 10
!       (referred to as H99 below).                                               !BLTP 11
!                                                                                 !BLTP 12
!---    Input parameters:                                                         !BLTP 13
!       gz = local acceleration of gravity (m/s**2)                               !BLTP 14
!       Cp = specific heat at constant pressure (joules kg**-1 K**-1)             !BLTP 15
!       Tg = temperature (K) at ground surface                                    !BLTP 16
!       z5 = first MGCM height level (nominally 5 m)                              !BLTP 17
!       T5 = temperature (K) at first MGCM level (nominally 5 m)                  !BLTP 18
!       U5 = zonal wind (m/s) at first MGCM level                                 !BLTP 19
!       V5 = meridional wind (m/s) at first MGCM level                            !BLTP 20
!       zeval = height (m) at which to evaluate temperature                       !BLTP 21
!       factor = factor for calculations = Log(zeval/z0)/Log(5./z0),              !BLTP 22
!                where z0 is surface roughness parameter (m)                      !BLTP 23
!---    Output parameter:                                                         !BLTP 24
!       Tempz = temperature (K) at height zeval                                   !BLTP 25
!---------------------------------------------------------------------            !BLTP 26
!                                                                                 !BLTP 27
!---    Set some initial values for iterative solution                            !BLTP 28
!...                                                                              !BLTP 29
!...Switches:                                                                     !BLTP 30
      implicit none                                                               !BLTP 31
!-----------------------------------------------                                  !BLTP 32
!   D u m m y   A r g u m e n t s                                                 !BLTP 33
!-----------------------------------------------                                  !BLTP 34
      real(double) , intent(in) :: gz                                             !BLTP 35
      real(double) , intent(in) :: cp                                             !BLTP 36
      real(double) , intent(in) :: tg                                             !BLTP 37
      real(double) , intent(in) :: z5                                             !BLTP 38
      real(double) , intent(in) :: t5                                             !BLTP 39
      real(double) , intent(in) :: u5                                             !BLTP 40
      real(double) , intent(in) :: v5                                             !BLTP 41
      real(double) , intent(in) :: zeval                                          !BLTP 42
      real(double) , intent(in) :: factor                                         !BLTP 43
      real(double) , intent(out) :: tempz                                         !BLTP 44
!-----------------------------------------------                                  !BLTP 45
!   L o c a l   V a r i a b l e s                                                 !BLTP 46
!-----------------------------------------------                                  !BLTP 47
      integer :: i                                                                !BLTP 48
      real(double) :: sqrtfh, dt, t0, thet5, udenom, ri                           !BLTP 49
!-----------------------------------------------                                  !BLTP 50
      sqrtfh = 1.0D0                                                              !BLTP 51
      dt = t5 - tg                                                                !BLTP 52
      t0 = t5                                                                     !BLTP 53
!---    Potential temperature at first MGCM level                                 !BLTP 54
      thet5 = t5 + gz*z5/cp                                                       !BLTP 55
!---    Iterative calculation of boundary layer parameters                        !BLTP 56
      do i = 1, 10                                                                !BLTP 57
!---      Richardson number from temperature and wind gradients                   !BLTP 58
         udenom = u5**2 + v5**2                                                   !BLTP 59
         udenom = dmax1(0.1D0,udenom)                                             !BLTP 60
         ri = (gz*sqrtfh/thet5)*((thet5 - tg)/(1.0D0 + sqrtfh))*z5/udenom         !BLTP 61
!---      Next iteration of temperature solution by method in Section             !BLTP 62
!         4 of H99 (convert from potential to regular temperature)                !BLTP 63
         tempz = tg + (thet5 - tg)*(1.0D0 + sqrtfh*factor)/(1.0D0 + sqrtfh) - &   !BLTP 64
            gz*zeval/cp                                                           !BLTP 65
!---      Change in temperature from previous iteration                           !BLTP 66
         dt = tempz - t0                                                          !BLTP 67
         t0 = tempz                                                               !BLTP 68
!---      End iteration if sufficient temperature precision achieved              !BLTP 69
         if (dabs(dt) < 0.01D0) return                                            !BLTP 70
!---      Next iteration of Sqrt(Fh), where Fh is stability function              !BLTP 71
!         from Section 4 of H99                                                   !BLTP 72
         if (ri < 0.0D0) then                                                     !BLTP 73
            sqrtfh = (1.0D0 - 16.0D0*ri)**0.25D0                                  !BLTP 74
         else                                                                     !BLTP 75
            sqrtfh = (1.0D0 + 15.0D0*ri/dsqrt(1.0D0 + 5.0D0*ri))**(-0.5D0)        !BLTP 76
         endif                                                                    !BLTP 77
      end do                                                                      !BLTP 78
      return                                                                      !BLTP 79
      end subroutine bltp_M10                                                     !BLTP 80
!                                                                                 !BLTP 81
!--------------------------------------------------------------------------       !CTOJ  1
      SUBROUTINE CALTOJUL_M10(IY, IM, ID, IHOUR, IMIN, SEC, XJD)                  !CTOJ  2
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
!---  Consider Jan or Feb as if months 13 and 14 of previous year                 !CTOJ 35
      IF (IM <= 2) THEN                                                           !CTOJ 36
         Y = IY - 1                                                               !CTOJ 37
         M = IM + 12                                                              !CTOJ 38
      ENDIF                                                                       !CTOJ 39
!---  Compute day of month plus fractional part                                   !CTOJ 40
      D = ID + IHOUR/2.4D1 + IMIN/1.440D3 + SEC/8.64D4                            !CTOJ 41
      A = IDINT(Y/100.0D0)                                                        !CTOJ 42
      B = 2 - A + IDINT(A/4.0D0)                                                  !CTOJ 43
!---  Compute Julian day with fractional part                                     !CTOJ 44
      XJD = IDINT(365.25D0*(Y + 4716)) + IDINT(30.6001D0*(M + 1)) + D + B -   &   !CTOJ 45
         1524.5D0                                                                 !CTOJ 46
      RETURN                                                                      !CTOJ 47
      END SUBROUTINE CALTOJUL_M10                                                 !CTOJ 48
!                                                                                 !CTOJ 49
!----------------------------------------------------------------------------     !COSP  1
      subroutine cospar_M10(z, t, p, rho)                                         !COSP  2
!-----------------------------------------------                                  !COSP  3
!   M o d u l e s                                                                 !COSP  4
!-----------------------------------------------                                  !COSP  5
      USE vast_kind_param, ONLY:  double                                          !COSP  6
      USE cosparnh_M10_C                                                          !COSP  7
!                                                                                 !COSP  8
!     COSPAR N.H. mean temperature (t, K), pressure (p, N/m**2) and               !COSP  9
!     density (rho, kg/m**3) versus height (z, km)                                !COSP 10
!     Note: input pressures (pc) are mb, densities (dc) are g/cm**3               !COSP 11
!                                                                                 !COSP 12
!     COSPAR values from Table XI, "The Mars Atmosphere: Observations             !COSP 13
!     and Model Profiles for Mars Missions", David E. Pitts et al.,               !COSP 14
!     eds., JSC-24455                                                             !COSP 15
!                                                                                 !COSP 16
!...Switches:                                                                     !COSP 18
      implicit none                                                               !COSP 19
!-----------------------------------------------                                  !COSP 20
!   D u m m y   A r g u m e n t s                                                 !COSP 21
!-----------------------------------------------                                  !COSP 22
      real(double) , intent(in) :: z                                              !COSP 23
      real(double) , intent(out) :: t                                             !COSP 24
      real(double) , intent(out) :: p                                             !COSP 25
      real(double) , intent(out) :: rho                                           !COSP 26
!-----------------------------------------------                                  !COSP 27
!   L o c a l   V a r i a b l e s                                                 !COSP 28
!-----------------------------------------------                                  !COSP 29
      integer :: iz                                                               !COSP 30
      real(double) :: dz, aexp, h, r1, r2, r                                      !COSP 31
!-----------------------------------------------                                  !COSP 32
!                                                                                 !COSP 33
!     1 km interval from -10 to 130 km, 10 km interval 130-360 km                 !COSP 34
      if (z < 130.0D0) then                                                       !COSP 35
         iz = idint(z + 11.0D0)                                                   !COSP 36
      else                                                                        !COSP 37
         iz = 141 + idint((z - 130.0D0)/10.0D0)                                   !COSP 38
      endif                                                                       !COSP 39
!     Set values to 0 if z out of range                                           !COSP 40
      if (iz<1 .or. iz>164) then                                                  !COSP 41
         t = 0.0D0                                                                !COSP 42
         p = 0.0D0                                                                !COSP 43
         rho = 0.0D0                                                              !COSP 44
         return                                                                   !COSP 45
      endif                                                                       !COSP 46
      iz = min0(163,iz)                                                           !COSP 47
!     Linear interpolation on temperature                                         !COSP 48
      dz = (z - zc(iz))/(zc(iz+1)-zc(iz))                                         !COSP 49
      t = tc(iz) + (tc(iz+1)-tc(iz))*dz                                           !COSP 50
!     Pressure from hydrostatic relation (with special isothermal case)           !COSP 51
      if (dabs(tc(iz+1)-tc(iz)) > 0.01D0) then                                    !COSP 52
         aexp = dlog(pc(iz+1)/pc(iz))/dlog(tc(iz+1)/tc(iz))                       !COSP 53
         p = 100.0D0*pc(iz)*(t/tc(iz))**aexp                                      !COSP 54
      else                                                                        !COSP 55
         h = (zc(iz+1)-zc(iz))/dlog(pc(iz)/pc(iz+1))                              !COSP 56
         p = 100.0D0*pc(iz)*dexp((-(z - zc(iz))/h))                               !COSP 57
      endif                                                                       !COSP 58
!     Linear interpolation on gas constant                                        !COSP 59
      r1 = pc(iz)/(dc(iz)*tc(iz))                                                 !COSP 60
      r2 = pc(iz+1)/(dc(iz+1)*tc(iz+1))                                           !COSP 61
      r = r1 + (r2 - r1)*dz                                                       !COSP 62
!     density from perfect gas law (and convert units to kg/m**3)                 !COSP 63
      rho = 10.0D0*p/(r*t)                                                        !COSP 64
      return                                                                      !COSP 65
      end subroutine cospar_M10                                                   !COSP 66
!                                                                                 !COSP 67
!-----------------------------------------------------------------------------    !CPOT  1
      REAL(KIND(0.0D0)) FUNCTION CP_M10 (T)                                       !CPOT  2
!-----------------------------------------------                                  !CPOT  3
!   M o d u l e s                                                                 !CPOT  4
!-----------------------------------------------                                  !CPOT  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !CPOT  6
!...Switches:                                                                     !CPOT  8
      IMPLICIT NONE                                                               !CPOT  9
!-----------------------------------------------                                  !CPOT 10
!   D u m m y   A r g u m e n t s                                                 !CPOT 11
!-----------------------------------------------                                  !CPOT 12
      REAL(DOUBLE) , INTENT(IN) :: T                                              !CPOT 13
!-----------------------------------------------                                  !CPOT 14
!   L o c a l   V a r i a b l e s                                                 !CPOT 15
!-----------------------------------------------                                  !CPOT 16
!-----------------------------------------------                                  !CPOT 17
!---  Specific heat at constant pressure, as function of temperature              !CPOT 18
!---  T in kelvins; Cp in joules kg**-1 K**-1                                     !CPOT 19
      CP_M10 = 639.5D0 + 0.123687D0*T + 0.00200225D0*T*T                          !CPOT 20
      RETURN                                                                      !CPOT 21
      END FUNCTION CP_M10                                                         !CPOT 22
!                                                                                 !CPOT 23
!-----------------------------------------------------------------------------    !CTOD  1
      subroutine geocenttogeodet_M10(r, zin, fi, h, a, b)                         !CTOD  2
!-----------------------------------------------                                  !CTOD  3
!   M o d u l e s                                                                 !CTOD  4
!-----------------------------------------------                                  !CTOD  5
      USE vast_kind_param, ONLY:  double                                          !CTOD  6
!---  Program to transform Cartesian to geodetic coordinates                      !CTOD  7
!     Code adapted from Polish version of K.M.Borkowski, Bull. Geod.              !CTOD  8
!     vol 63, pp.50-56 (1989).                                                    !CTOD  9
!---  Input:   r, z = equatorial and polar Cartesian components [km]              !CTOD 10
!              a, b = equatorial and polar planetary radii [km]                   !CTOD 11
!---  Output: fi, h = geodetic coord's (latitude [deg], height [km])              !CTOD 12
!     Special case for poles                                                      !CTOD 13
!...                                                                              !CTOD 14
!...Switches:                                                                     !CTOD 15
      implicit none                                                               !CTOD 16
!-----------------------------------------------                                  !CTOD 17
!   D u m m y   A r g u m e n t s                                                 !CTOD 18
!-----------------------------------------------                                  !CTOD 19
      real(double) , intent(in) :: r                                              !CTOD 20
      real(double) , intent(in) :: zin                                            !CTOD 21
      real(double) , intent(out) :: fi                                            !CTOD 22
      real(double) , intent(out) :: h                                             !CTOD 23
      real(double) , intent(in) :: a                                              !CTOD 24
      real(double) , intent(in) :: b                                              !CTOD 25
!-----------------------------------------------                                  !CTOD 26
!   L o c a l   V a r i a b l e s                                                 !CTOD 27
!-----------------------------------------------                                  !CTOD 28
      real(double) :: z, e, f, p, q, d, s, v, g, t                                !CTOD 29
!-----------------------------------------------                                  !CTOD 30
      if (dabs(r) < 0.001D0) then                                                 !CTOD 31
         fi = dsign(90.0D0,zin)                                                   !CTOD 32
         h = dabs(zin) - b                                                        !CTOD 33
      else                                                                        !CTOD 34
!---   Analytical solution for non-polar case                                     !CTOD 35
!      See also page K12 of Astronomical Almanac for iterative                    !CTOD 36
!      solution                                                                   !CTOD 37
         z = dabs(zin)                                                            !CTOD 38
         e = ((z + b)*b/a - a)/r                                                  !CTOD 39
         f = ((z - b)*b/a + a)/r                                                  !CTOD 40
         p = (e*f + 1.0D0)*4.0D0/3.0D0                                            !CTOD 41
         q = (e*e - f*f)*2.0D0                                                    !CTOD 42
         d = p*p*p + q*q                                                          !CTOD 43
         if (d >= 0.0D0) then                                                     !CTOD 44
            s = dsqrt(d) + q                                                      !CTOD 45
            s = dsign(dexp(dlog(dabs(s))/3.0D0),s)                                !CTOD 46
            v = p/s - s                                                           !CTOD 47
            v = -(q + q + v*v*v)/(3.0D0*p)                                        !CTOD 48
         else                                                                     !CTOD 49
            v = 2.0D0*dsqrt((-p))*dcos(dacos(q/p/dsqrt((-p)))/3.0D0)              !CTOD 50
         endif                                                                    !CTOD 51
         g = 0.5D0*(e + dsqrt(e*e + v))                                           !CTOD 52
         t = dsqrt(g*g + (f - v*g)/(g + g - e)) - g                               !CTOD 53
         fi = datan((1.0D0 - t*t)*a/(2.0D0*b*t))                                  !CTOD 54
         h = (r - a*t)*dcos(fi) + (z - b)*dsin(fi)                                !CTOD 55
!---     Convert to degrees                                                       !CTOD 56
         fi = fi*45.0D0/datan(1.0D0)                                              !CTOD 57
         if (zin < 0.0D0) fi = -fi                                                !CTOD 58
      endif                                                                       !CTOD 59
      return                                                                      !CTOD 60
      end subroutine geocenttogeodet_M10                                          !CTOD 61
!                                                                                 !CTOD 62
!--------------------------------------------------------------------------       !GTOC  1
      subroutine geodettogeocent_M10(fidet, h, ficent, rtot, xy, z, a, b)         !GTOC  2
!-----------------------------------------------                                  !GTOC  3
!   M o d u l e s                                                                 !GTOC  4
!-----------------------------------------------                                  !GTOC  5
      USE vast_kind_param, ONLY:  double                                          !GTOC  6
!---  Program to transform geodetic latitude, height to geocentric                !GTOC  7
!     latitude, radius                                                            !GTOC  8
!     Method from page K12 of Astronomical Almanac                                !GTOC  9
!---  Input:  fidet, h = geodetic lat (deg), height (km)                          !GTOC 10
!             a, b = equatorial and polar planetary radii (km)                    !GTOC 11
!---  Output: ficent = geocentric lat (deg)                                       !GTOC 12
!             rtot = geocentric total radius (km)                                 !GTOC 13
!             xy, z = equatorial, polar cartesian components (km)                 !GTOC 14
!...                                                                              !GTOC 15
!...Switches:                                                                     !GTOC 16
      implicit none                                                               !GTOC 17
!-----------------------------------------------                                  !GTOC 18
!   D u m m y   A r g u m e n t s                                                 !GTOC 19
!-----------------------------------------------                                  !GTOC 20
      real(double) , intent(in) :: fidet                                          !GTOC 21
      real(double) , intent(in) :: h                                              !GTOC 22
      real(double) , intent(out) :: ficent                                        !GTOC 23
      real(double) , intent(out) :: rtot                                          !GTOC 24
      real(double) , intent(out) :: xy                                            !GTOC 25
      real(double) , intent(out) :: z                                             !GTOC 26
      real(double) , intent(in) :: a                                              !GTOC 27
      real(double) , intent(in) :: b                                              !GTOC 28
!-----------------------------------------------                                  !GTOC 29
!   L o c a l   V a r i a b l e s                                                 !GTOC 30
!-----------------------------------------------                                  !GTOC 31
      real(double) :: pi180, omf, sphi, cphi, c                                   !GTOC 32
!-----------------------------------------------                                  !GTOC 33
      pi180 = datan(1.0D0)/45.0D0                                                 !GTOC 34
!---  Special case for poles                                                      !GTOC 35
      if (dabs(dabs(fidet)-90.0D0) <= 0.0) then                                   !GTOC 36
         ficent = dsign(90.0D0,fidet)                                             !GTOC 37
         rtot = b + h                                                             !GTOC 38
         xy = 0.0D0                                                               !GTOC 39
         z = dsign(rtot,fidet)                                                    !GTOC 40
      else                                                                        !GTOC 41
!---    1 - flattening                                                            !GTOC 42
         omf = b/a                                                                !GTOC 43
!---    Sin and Cos of geodetic lat                                               !GTOC 44
         sphi = dsin(pi180*fidet)                                                 !GTOC 45
         cphi = dcos(pi180*fidet)                                                 !GTOC 46
!---    Computational factor C for cartesian coordinates                          !GTOC 47
         c = 1.0D0/dsqrt(cphi**2 + (omf*sphi)**2)                                 !GTOC 48
!---    Polar and equatorial cartesian coordinates                                !GTOC 49
         z = (a*c*omf**2 + h)*sphi                                                !GTOC 50
         xy = (a*c + h)*cphi                                                      !GTOC 51
!---    Total geocentric radius                                                   !GTOC 52
         rtot = dsqrt(xy**2 + z**2)                                               !GTOC 53
!---    Geocentric latitude, deg                                                  !GTOC 54
         ficent = datan(z/xy)/pi180                                               !GTOC 55
      endif                                                                       !GTOC 56
      return                                                                      !GTOC 57
      end subroutine geodettogeocent_M10                                          !GTOC 58
!                                                                                 !GTOC 59
!-------------------------------------------------------------------------        !DSTF  1
      subroutine dustfact_M10(clat, clon, als, dustm, stormdz)                    !DSTF  2
!-----------------------------------------------                                  !DSTF  3
!   M o d u l e s                                                                 !DSTF  4
!-----------------------------------------------                                  !DSTF  5
      USE vast_kind_param, ONLY:  double                                          !DSTF  6
      USE datacom_M10_C                                                           !DSTF  7
!---  Computes dust storm intensity factor dustM                                  !DSTF  8
!---  as a function of the time since start of the storm,                         !DSTF  9
!---  (als - als0), measured in Ls angle (degrees), and as a                      !DSTF 10
!---  function of the storm intensity, intens.  dustM is for                      !DSTF 11
!---  magnitude of effect on dust optical depth (0.0 - 3.0).                      !DSTF 12
!...                                                                              !DSTF 13
!...Switches:                                                                     !DSTF 14
      implicit none                                                               !DSTF 15
!-----------------------------------------------                                  !DSTF 16
!   D u m m y   A r g u m e n t s                                                 !DSTF 17
!-----------------------------------------------                                  !DSTF 18
      real(double) , intent(in) :: clat                                           !DSTF 19
      real(double) , intent(in) :: clon                                           !DSTF 20
      real(double) , intent(in) :: als                                            !DSTF 21
      real(double) , intent(out) :: dustm                                         !DSTF 22
      real(double) , intent(out) :: stormdz                                       !DSTF 23
!-----------------------------------------------                                  !DSTF 24
!   L o c a l   V a r i a b l e s                                                 !DSTF 25
!-----------------------------------------------                                  !DSTF 26
      real(double) :: dls, dlsmax, sizefact, dns, dlon, dew, rad, raddust         !DSTF 27
!-----------------------------------------------                                  !DSTF 28
      dls = als - als0                                                            !DSTF 29
      dlsmax = alsdur                                                             !DSTF 30
      dlsmax = dmax1(12.0D0,dlsmax)                                               !DSTF 31
      dlsmax = min(48.0D0,dlsmax)                                                 !DSTF 32
!---  Return dust factor of 0 if Ls-Ls0 < 0 or > dlsmax degrees                   !DSTF 33
      if (dls<=0.0D0 .or. dls>dlsmax .or. intens<=0.0D0) then                     !DSTF 34
         dustm = 0.0D0                                                            !DSTF 35
         stormdz = 0.0D0                                                          !DSTF 36
         return                                                                   !DSTF 37
      endif                                                                       !DSTF 38
!---  Compute initial dustM factor (0-1) from time (Ls) profile                   !DSTF 39
      if (dls <= dlsmax/8.0D0) then                                               !DSTF 40
         dustm = 8.0D0*dls/dlsmax                                                 !DSTF 41
      else if (dls >= dlsmax/2.0D0) then                                          !DSTF 42
         dustm = 2.0D0*(1.0D0 - dls/dlsmax)                                       !DSTF 43
      else                                                                        !DSTF 44
         dustm = 1.0D0                                                            !DSTF 45
      endif                                                                       !DSTF 46
      sizefact = 1.0D0                                                            !DSTF 47
!---  Compute parameters of local storm if radmax is not 0                        !DSTF 48
      if (dabs(radmax) > 0.0D0) then                                              !DSTF 49
         sizefact = 0.0D0                                                         !DSTF 50
!---    dns,dew,rad = horizontal coordinates from center of dust storm            !DSTF 51
         dns = dtr*rref*(clat - dustlat)                                          !DSTF 52
         dlon = dabs(clon - dustlon)                                              !DSTF 53
         if (dlon > 180.0D0) dlon = 360.0D0 - dlon                                !DSTF 54
         dew = dtr*rref*dcos(dtr*clat)*dlon                                       !DSTF 55
         rad = dsqrt(dns**2 + dew**2)                                             !DSTF 56
!---    raddust = actual horizontal size of storm                                 !DSTF 57
         raddust = dustm*radmax                                                   !DSTF 58
!---    sizefact = position-dependent measure of relative storm effect            !DSTF 59
         if (rad < 2.0D0*raddust) sizefact = 0.5D0*(1.0D0 + dcos(90.0D0*dtr  &    !DSTF 60
            *rad/raddust))                                                        !DSTF 61
      endif                                                                       !DSTF 62
!---  Final factor dependent on position and on storm intensity                   !DSTF 63
      dustm = sizefact*dustm                                                      !DSTF 64
      stormdz = 5.0D0*dustm*dsqrt(intens)                                         !DSTF 65
      dustm = dustm*intens                                                        !DSTF 66
      return                                                                      !DSTF 67
      end subroutine dustfact_M10                                                 !DSTF 68
!                                                                                 !DSTF 69
!--------------------------------------------------------------------------       !DSTP  1
      Subroutine Datastep_M10(I,CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,        &      !DSTP  2
        RHOv,RHOw,EOF,DELHGT,DELLAT,DELLON,DELTIME,TEMP,PRES,DENSLO,       &      !DSTP  3
        DENS,DENSHI,DENSP,EWWIND,EWpert,NSWIND,NSpert,VWpert,Hrho,         &      !DSTP  4
        HSCALE,dsunlat,dsunlon,dsunLs,dradau,dowlt,LonEW,corlim,DENSTOT,   &      !DSTP  5
        numwave,hgtasfc,IERT,IUTC,pertstep,corlmin,iupdate,ALS,szang,      &      !DSTP  6
        owlt,sunlat,sunlon,MarsAU,TLOCAL,profnear,proffar,nprof)                  !DSTP  7
!-----------------------------------------------                                  !DSTP  8
!   M o d u l e s                                                                 !DSTP  9
!-----------------------------------------------                                  !DSTP 10
      USE vast_kind_param, ONLY:  double                                          !DSTP 11
      USE DATACOM_M10_C                                                           !DSTP 12
      USE cosparnh_M10_C                                                          !DSTP 13
      USE FILENAME_M10_C                                                          !DSTP 14
      USE Wavecoef_M10_C                                                          !DSTP 15
      USE THERM_M10_C                                                             !DSTP 16
      USE TGCMOffset_M10_C                                                        !DSTP 17
      USE parameters_M10_C                                                        !DSTP 18
!-----------------------------------------------                                  !DSTP 19
!---  If ipclat (in Common DATACOM) is 1, latitude CLAT and height CHGT           !DSTP 20
!     are planeto-centric, otherwise CLAT and CHGT are planeto-graphic,           !DSTP 21
!     with CHGT measured above reference ellipsoid.                               !DSTP 22
!     If input parameter MOLAhgts is 1, CHGT is height above the MOLA             !DSTP 23
!     areoid; otherwise CHGT is height above reference ellipsoid,                 !DSTP 24
!     in which case height is converted to HGTM, height above MOLA                !DSTP 25
!     areoid for call to subroutine ATMOS2_M10.                                   !DSTP 26
!---  If MOLAhgts = 1, then ipclat must also be 1                                 !DSTP 27
!                                                                                 !DSTP 28
      Implicit None                                                               !DSTP 29
!-----------------------------------------------                                  !DSTP 30
!   D u m m y   A r g u m e n t s                                                 !DSTP 31
!-----------------------------------------------                                  !DSTP 32
      INTEGER, INTENT(IN) :: I,LonEW,IERT,IUTC                                    !DSTP 33
      INTEGER, INTENT(INOUT) :: iupdate,numwave,nprof                             !DSTP 34
      INTEGER, INTENT(INOUT) :: EOF                                               !DSTP 35
      Real(Double), INTENT(IN) :: DAY0,dsunlat,dsunlon,dsunLs,dradau,dowlt        !DSTP 36
      Real(Double), INTENT(IN) :: corlmin                                         !DSTP 36a
      Real(Double), INTENT(OUT) :: TEMP,PRES,DENSLO,DENS,DENSHI,DENSP,EWWIND      !DSTP 37
      Real(Double), INTENT(OUT) :: EWpert,VWpert,Hrho,HSCALE,corlim,DENSTOT       !DSTP 38
      Real(Double), INTENT(OUT) :: hgtasfc,pertstep,ALS,szang,owlt                !DSTP 39
      Real(Double), INTENT(OUT) :: sunlat,sunlon,NSWIND,NSpert,MARSAU,TLOCAL      !DSTP 40
      Real(Double), INTENT(OUT) :: profnear,proffar                               !DSTP 41
      Real(Double), INTENT(INOUT) :: CHGT,CLAT,CLON,CSEC,DELHGT,DELLAT,DELLON     !DSTP 42
      Real(Double), INTENT(INOUT) :: DELTIME,RHOd,RHOu,RHOv,RHOw                  !DSTP 43
!-----------------------------------------------                                  !DSTP 44
!   L o c a l   V a r i a b l e s                                                 !DSTP 45
!-----------------------------------------------                                  !DSTP 46
      Integer :: ifault,L,icepolar                                                !DSTP 47
      Real(Double) :: onesol,airmass,albedo,Alogdens,AMz,bluday,blwindew,blwindns !DSTP 48
      Real(Double) :: blwindvert,careoid,CHGTc,CHGTg,CLATc,CLATg,corbeta,correl   !DSTP 49
      Real(Double) :: cszang,dareoid,dareaden,blvday,dcosp,DELEW,DELNS,DELZ,DENS0 !DSTP 50
      Real(Double) :: DensDay,DensMax,DensMin,DensRand,denswave,devav,devDay      !DSTP 51
      Real(Double) :: devhi,devlo,devmax,devmin,devtot,dmasden,DMINUS,dmixrat     !DSTP 52
      Real(Double) :: dnumden,DPLUS,dt,dtimecor,Dxy,Dz,Elon,HatZF,EOT,EWtot       !DSTP 53
      Real(Double) :: EWwnDay,ogz,OHGT,OHGTS,OLAT,OldHgt,Oldrref,OLON,PatSurf     !DSTP 54
      Real(Double) :: pcos,expfact,F1peak,FACTHI,FACTLO,fmassH2O,fmolH2O          !DSTP 55
      Real(Double) :: gz,HGTM,HGTMS,HLS,NStot,NSwnDay,offsetL,PresDay,preshgt     !DSTP 56
      Real(Double) :: PRESmb,profwgt,qsurf,radtotal,RSC,SIGD,sigmalevel,SIGU,SIGW !DSTP 57
      Real(Double) :: ssposr2,talb,Tbase,tcos,TempDay,TempMax,TempMin,Texos,thgt  !DSTP 58
      Real(Double) :: topohgt,topz,TrajHGT,TrajLAT,TrajLON,Tgrnd,TrajSEC,ttsec    !DSTP 59
      Real(Double) :: VAR,VARX,VARY,VLS,wavepert,Z1,Z2,Zbase,ZF,fmass(0:8)        !DSTP 60
      Real(Double) :: RANDOM_M10,PPND_M10,Cp_M10                                  !DSTP 61
!-----------------------------------------------                                  !DSTP 62
      Character(len=4) lonvar                                                     !DSTP 63
      Character(len=8) densunits                                                  !DSTP 64
      Save                                                                        !DSTP 65
      Parameter (onesol = 88775.245d0 )                                           !DSTP 66
      EOF = 0                                                                     !DSTP 67
!---  Top height = 170 for 2001 MGCM data = 240 km for TES yr1&2 data             !DSTP 68
      TopZ = 170.0d0                                                              !DSTP 69
      If (MapYear > 0)TopZ = 240.0d0                                              !DSTP 70
!---  Compute planeto-centric and planeto-graphic coordinates, as                 !DSTP 71
!      necessary: CLATc,CHGTc=planeto-centric; CLATg,CHGTg=planeto-               !DSTP 72
!      graphic (same as geodetic)                                                 !DSTP 73
      If (MOLAhgts == 1.or.ipclat == 1)Then                                       !DSTP 74
!---    Input CLAT is planeto-centric wrt MOLA or ellipsoid                       !DSTP 75
        CLATc = CLAT                                                              !DSTP 76
!---    Get MOLA radius Rref and ellipsoid radius Oldrref for current             !DSTP 77
!         position at height = 0                                                  !DSTP 78
        Call RELLIPS_M10(CLATc,CLON,Rref,0.0d0,ogz,Oldrref,topohgt,     &         !DSTP 79
          albedo,requa,rpole)                                                     !DSTP 80
!---    Interpret CHGT as planeto-centric radius if CHGT > 3000 km                !DSTP 81
        If (CHGT > 3.0d3)Then                                                     !DSTP 82
          RSC = CHGT                                                              !DSTP 83
          If (MOLAHgts == 1)Then                                                  !DSTP 84
            CHGTc = CHGT - Rref                                                   !DSTP 85
            HGTM = CHGTc                                                          !DSTP 86
          Else                                                                    !DSTP 87
            CHGTc = CHGT - Oldrref                                                !DSTP 88
            HGTM = CHGT - Rref                                                    !DSTP 89
          Endif                                                                   !DSTP 90
        Else                                                                      !DSTP 91
!---      Interpret CHGT as planeto-centric altitude above MOLA or                !DSTP 92
!         ellipsoid (depending on MOLAhgts value)                                 !DSTP 93
          CHGTc = CHGT                                                            !DSTP 94
!---      Total radius RSC                                                        !DSTP 95
          RSC = Rref + CHGTc                                                      !DSTP 96
          HGTM = CHGTc                                                            !DSTP 97
!---      Height wrt ellipsoid if MOLAhgts ne 1                                   !DSTP 98
          If (MOLAhgts /= 1)Then                                                  !DSTP 99
            RSC = Oldrref + CHGTc                                                 !DSTP100
            HGTM = RSC - Rref                                                     !DSTP101
          Endif                                                                   !DSTP102
        Endif                                                                     !DSTP103
!---    Get equatorial and polar cartesian components                             !DSTP104
        Dxy = RSC*dCos(DTR*CLATc)                                                 !DSTP105
        Dz = RSC*dSin(DTR*CLATc)                                                  !DSTP106
!---    Convert planeto-centric to planeto-graphic                                !DSTP107
        Call GeocenttoGeodet_M10(Dxy,Dz,CLATg,CHGTg,requa,rpole)                  !DSTP108
      Else                                                                        !DSTP109
!---    Error for radius input (height > 3000 km) if ipclat ne 1                  !DSTP110
        If (CHGT > 3.0d3)Stop ' Radius input requires ipclat=1'                   !DSTP111
!---    Input CLAT&CHGT are planeto-graphic (wrt to ellipsoid)                    !DSTP112
        CLATg = CLAT                                                              !DSTP113
        CHGTg = CHGT                                                              !DSTP114
!---    Convert planeto-graphic to planeto-centric                                !DSTP115
        Call GeodettoGeocent_M10(CLATg,CHGTg,CLATc,RSC,Dxy,Dz,requa,    &         !DSTP116
          rpole)                                                                  !DSTP117
!---    Get local radii for MOLA (Rref) and ellipsoid (Oldrref)                   !DSTP118
        Call RELLIPS_M10(CLATc,CLON,Rref,CHGTg,ogz,Oldrref,topohgt,     &         !DSTP119
          albedo,requa,rpole)                                                     !DSTP120
        CHGTc = RSC - Oldrref                                                     !DSTP121
        HGTM = RSC - RRef                                                         !DSTP122
      Endif                                                                       !DSTP123
!---  Set vertical and horizontal scale parameters                                !DSTP124
      VLS = 8.0d0*wlscale                                                         !DSTP125
      HLS = (30.0d0 + 0.01875d0*HGTM**2)                                          !DSTP126
      If (HLS > 600.0d0)HLS = 600.0d0                                             !DSTP127
      HLS = HLS*wlscale                                                           !DSTP128
!---  Relative displacements between previous and current position                !DSTP129
      DELNS = DTR*RSC*(DELLAT)/HLS                                                !DSTP130
      DELEW = -DTR*RSC*dCOS(DTR*CLATc)*DELLON/HLS                                 !DSTP131
      DELZ = DELHGT/VLS                                                           !DSTP132
      IF(NPOS <= 0)then                                                           !DSTP133
!---    Read new position if trajectory data file is being used                   !DSTP134
        READ (7,*,ERR=9998,END=999)TrajSEC,TrajHGT,TrajLAT,TrajLON                !DSTP135
!---    Convert negative longitudes                                               !DSTP136
        If (TrajLON < 0.0d0)TrajLON = TrajLON + 360.0d0                           !DSTP137
!---    Convert to West Longitude if LonEW = 1                                    !DSTP138
        If (LonEW == 1)TrajLON = 360.0d0 - TrajLON                                !DSTP139
        If (I > 0)Then                                                            !DSTP140
!---      Compute displacement magnitude of new from previous position            !DSTP141
          DELTIME = TrajSEC - CSEC                                                !DSTP142
          DELHGT = TrajHGT - CHGT                                                 !DSTP143
          DELLAT = dAbs(TrajLAT - CLAT)                                           !DSTP144
          DELLON = dAbs(TrajLON - CLON)                                           !DSTP145
        Endif                                                                     !DSTP146
!---    Correct DELLON for cases near 0/360 longitude discontinuity               !DSTP147
        If (DELLON > 180.0d0)DELLON = 360.0d0 - DELLON                            !DSTP148
        If (DELLON < 0.0d0)DELLON = DELLON + 360.0d0                              !DSTP149
!---    Correct DELLON and DELLAT near polar discontinuities                      !DSTP150
        If (DELLON > 90.0d0.and.(dAbs(TrajLAT) >= 70.0d0.or.           &          !DSTP151
          dAbs(CLAT) >= 70.0d0))Then                                              !DSTP152
          DELLON = dAbs(180.0d0 - DELLON)                                         !DSTP153
          DELLAT = 180.0d0 - dAbs(TrajLAT) - dAbs(CLAT)                           !DSTP154
        Endif                                                                     !DSTP155
!---    Relative displacements between previous and current position              !DSTP156
        DELNS = DTR*RSC*(DELLAT)/HLS                                              !DSTP157
        DELEW = -DTR*RSC*dCOS(DTR*CLATc)*DELLON/HLS                               !DSTP158
        DELZ = DELHGT/VLS                                                         !DSTP159
!---    Set current position to new position                                      !DSTP160
        CSEC = TrajSEC                                                            !DSTP161
        CHGT = TrajHGT                                                            !DSTP162
        CLAT = TrajLAT                                                            !DSTP163
        CLON = TrajLON                                                            !DSTP164
      Else If (I > 0) then                                                        !DSTP165
        CHGT = CHGT + DELHGT                                                      !DSTP166
        CLAT = CLAT + DELLAT                                                      !DSTP167
        CLON = CLON + DELLON                                                      !DSTP168
        CSEC = CSEC + DELTIME                                                     !DSTP169
      Endif                                                                       !DSTP170
!---  Correct latitude and longitude if position crosses either pole              !DSTP171
      IF(dABS(CLAT) > 90.0d0)then                                                 !DSTP172
        CLAT = dSIGN(180.0d0,CLAT) - CLAT                                         !DSTP173
        CLON = CLON + 180.0d0                                                     !DSTP174
        DELLAT = -DELLAT                                                          !DSTP175
      Endif                                                                       !DSTP176
      IF(CLON < 0.0d0)CLON = CLON + 360.0d0                                       !DSTP177
      IF(CLON >= 360.0d0) CLON = CLON - 360.0d0                                   !DSTP178
!---  Compute planeto-centric and planeto-grapic coordinates for new              !DSTP179
!     position, as necessary                                                      !DSTP180
      If (MOLAhgts == 1.or.ipclat == 1)Then                                       !DSTP181
!---    Input CLAT is planeto-centric wrt MOLA or ellipsoid                       !DSTP182
        CLATc = CLAT                                                              !DSTP183
!---    Get MOLA radius Rref and ellipsoid radius Oldrref for current             !DSTP184
!         position at height = 0                                                  !DSTP185
        Call RELLIPS_M10(CLATc,CLON,Rref,0.0d0,ogz,Oldrref,topohgt,     &         !DSTP186
          albedo,requa,rpole)                                                     !DSTP187
!---    Interpret CHGT as planeto-centric radius if CHGT > 3000 km                !DSTP188
        If (CHGT > 3.0d3)Then                                                     !DSTP189
          RSC = CHGT                                                              !DSTP190
          If (MOLAHgts == 1)Then                                                  !DSTP191
            CHGTc = CHGT - Rref                                                   !DSTP192
            HGTM = CHGTc                                                          !DSTP193
          Else                                                                    !DSTP194
            CHGTc = CHGT - Oldrref                                                !DSTP195
            HGTM = CHGT - Rref                                                    !DSTP196
          Endif                                                                   !DSTP197
        Else                                                                      !DSTP198
!---      Interpret CHGT as planeto-centric altitude above MOLA or                !DSTP199
!         ellipsoid (depending on MOLAhgts value)                                 !DSTP200
          CHGTc = CHGT                                                            !DSTP201
!---      Total radius RSC                                                        !DSTP202
          RSC = Rref + CHGTc                                                      !DSTP203
          HGTM = CHGTc                                                            !DSTP204
!---      Height wrt ellipsoid if MOLAhgts ne 1                                   !DSTP205
          If (MOLAhgts /= 1)Then                                                  !DSTP206
            RSC = Oldrref + CHGTc                                                 !DSTP207
            HGTM = RSC - Rref                                                     !DSTP208
          Endif                                                                   !DSTP209
        Endif                                                                     !DSTP210
!---    Get equatorial and polar cartesian components                             !DSTP211
        Dxy = RSC*dCos(DTR*CLATc)                                                 !DSTP212
        Dz = RSC*dSin(DTR*CLATc)                                                  !DSTP213
!---    Convert planeto-centric to planeto-graphic                                !DSTP214
        Call GeocenttoGeodet_M10(Dxy,Dz,CLATg,CHGTg,requa,rpole)                  !DSTP215
      Else                                                                        !DSTP216
!---    Error for radius input (height > 3000 km) if ipclat ne 1                  !DSTP217
        If (CHGT > 3.0d3)Stop ' Radius input requires ipclat=1'                   !DSTP218
!---    CLAT&CHGT are planeto-graphic (wrt to ellipsoid)                          !DSTP219
        CLATg = CLAT                                                              !DSTP220
        CHGTg = CHGT                                                              !DSTP221
!---    Convert planeto-graphic to planeto-centric                                !DSTP222
        Call GeodettoGeocent_M10(CLATg,CHGTg,CLATc,RSC,Dxy,Dz,requa,    &         !DSTP223
          rpole)                                                                  !DSTP224
!---    Get local radii for MOLA (Rref) and ellipsoid (Oldrref)                   !DSTP225
        Call RELLIPS_M10(CLATc,CLON,Rref,CHGTg,ogz,Oldrref,topohgt,     &         !DSTP226
          albedo,requa,rpole)                                                     !DSTP227
        CHGTc = RSC - Oldrref                                                     !DSTP228
        HGTM = RSC - Rref                                                         !DSTP229
      Endif                                                                       !DSTP230
      DAY = DAY0 + CSEC/8.6400d4                                                  !DSTP231
!---  Update wave coefficients if necessary                                       !DSTP232
      If (iuwave > 0.and.numwave < nwave)Then                                     !DSTP233
        If (CSEC >= wavetime(numwave+1))Then                                      !DSTP234
 100      numwave = numwave + 1                                                   !DSTP235
          WaveA0   = wavedata(numwave,1)                                          !DSTP236
          WaveDate = wavedata(numwave,2)                                          !DSTP237
          WaveA1   = wavedata(numwave,3)                                          !DSTP238
          Wavephi1 = wavedata(numwave,4)                                          !DSTP239
          phi1dot  = wavedata(numwave,5)                                          !DSTP240
          WaveA2   = wavedata(numwave,6)                                          !DSTP241
          Wavephi2 = wavedata(numwave,7)                                          !DSTP242
          phi2dot  = wavedata(numwave,8)                                          !DSTP243
          WaveA3   = wavedata(numwave,9)                                          !DSTP244
          Wavephi3 = wavedata(numwave,10)                                         !DSTP245
          phi3dot  = wavedata(numwave,11)                                         !DSTP246
!---      Check to see if more than one wave time exceeded                        !DSTP247
          If (numwave < nwave)Then                                                !DSTP248
            If (CSEC >= wavetime(numwave+1))Goto 100                              !DSTP249
          Endif                                                                   !DSTP250
!---      Write out updated wave coefficients                                     !DSTP251
          If (iup > 0)Then                                                        !DSTP252
            Write(iup,292)WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,       &         !DSTP253
              WaveA3,Wavephi3                                                     !DSTP254
  292       Format(' A0,A1,phi1,A2,phi2,A3,phi3=',F6.3,3(F6.3,F7.1))              !DSTP255
            If (WaveDate > 0.0d0)Write(iup,294)WaveDate,phi1dot,        &         !DSTP256
              phi2dot,phi3dot                                                     !DSTP257
  294       Format(' Traveling wave phases initialized at Julian DAY',  &         !DSTP258
              ' =',F13.3/' at phase rates (phi1dot,phi2dot,phi3dot,',   &         !DSTP259
              ' deg/day)=',3F8.3)                                                 !DSTP260
            lonvar = 'West'                                                       !DSTP261
            If (LonEW == 1)lonvar = 'East'                                        !DSTP262
            Write(iup,293)Wscale,lonvar                                           !DSTP263
  293       Format('   Wave Scale =',F8.1,' km.    Wave phases are in', &         !DSTP264
              ' degrees of ',A4,' Longitude')                                     !DSTP265
          Endif                                                                   !DSTP266
        Endif                                                                     !DSTP267
      Endif                                                                       !DSTP268
!---  Write wave coefficients if not read from file                               !DSTP269
      If (iuwave == 0.and.numwave == 0)Then                                       !DSTP270
          If (iup > 0)Then                                                        !DSTP271
            Write(iup,292)WaveA0,WaveA1,Wavephi1,WaveA2,Wavephi2,       &         !DSTP272
              WaveA3,Wavephi3                                                     !DSTP273
            If (WaveDate > 0.0d0)Write(iup,294)WaveDate,phi1dot,        &         !DSTP274
              phi2dot,phi3dot                                                     !DSTP275
            lonvar = 'West'                                                       !DSTP276
            If (LonEW == 1)lonvar = 'East'                                        !DSTP277
            Write(iup,293)Wscale,lonvar                                           !DSTP278
          Endif                                                                   !DSTP279
        numwave = 1                                                               !DSTP280
      Endif                                                                       !DSTP281
!---  Sun and Mars positions at new time                                          !DSTP282
!     Use high precision values if inputs > 0 or ephemeris subroutine             !DSTP283
!     otherwise                                                                   !DSTP284
      If (dradau > 0.0d0)Then                                                     !DSTP285
        SUNLON = dsunlon                                                          !DSTP286
        SUNLAT = dsunlat                                                          !DSTP287
        ALS = dsunLs                                                              !DSTP288
        MARSAU = dradau                                                           !DSTP289
        owlt = dowlt                                                              !DSTP290
      Else                                                                        !DSTP291
!---    Use built-in Mars ephemeris routine                                       !DSTP292
!---    Convert to Terrestrial (Dynamical) Time, if necessary                     !DSTP293
        ttsec = 0.0d0                                                             !DSTP294
        If (iutc == 1)Then                                                        !DSTP295
!---      Get terrestrial dynamical time offset (seconds)                         !DSTP296
          dt = (DAY - 2451545.0d0)/36525.0d0                                      !DSTP297
!---      Terrestrial time offset (in seconds) TT = UTC + ttsec                   !DSTP298
          ttsec = (64.184d0 + 95.0d0*dt + 35.0d0*dt**2)/86400.0d0                 !DSTP299
        Endif                                                                     !DSTP300
        Call marsephm_M10(DAY+ttsec,sunlat,sunlon,ALS,MARSAU,owlt,EOT)            !DSTP301
!---    Convert to Mars-Event Time, if necessary                                  !DSTP302
        If(IERT == 1)Call marsephm_M10(DAY+ttsec-owlt/1440.0d0,sunlat,   &        !DSTP303
          sunlon,ALS,MARSAU,owlt,EOT)                                             !DSTP304
!---    Convert planetographic sun latitude to planetocentric                     !DSTP305
        sunlat = dAtan(dTan(sunlat*DTR)/(3396.0d0/3378.32d0)**2)/ DTR             !DSTP306
      Endif                                                                       !DSTP307
!---  Evaluate longitude-dependent wave perturbation                              !DSTP308
      Call Wavelon_M10(LonEW,CLON,CLATc,HGTM,DAY,wavepert)                        !DSTP309
!---  Convert wave perturbation to % for output                                   !DSTP310
      denswave = 100.0d0*wavepert                                                 !DSTP311
!---  Evaluate atmospheric parameters                                             !DSTP312
      CALL ATMOS2_M10(HGTM,CLATc,CLON,MARSAU,SUNLAT,SUNLON,ALS,HSCALE,   &        !DSTP313
        TEMP,DENS,FACTHI,FACTLO,PRES,thgt,careoid,ZF,iu0,deltaTEX,Texos, &        !DSTP314
        Tbase,Hrho,AMz,Dusttau,Dustmin,Dustmax,DustOD,EWWIND,NSWIND,     &        !DSTP315
        blwindew,blwindns,blwindvert,HatZF,wavepert,TempDay,PresDay,     &        !DSTP316
        DensDay,EWwnDay,NSwnDay,bluday,blvday,hgtasfc,Patsurf,Tempmax,   &        !DSTP317
        Tempmin,Densmax,Densmin,Tgrnd,talb,icepolar,gz,Oldrref,          &        !DSTP318
        requa,rpole,MapYear,profnear,proffar,nprof,profwgt,idaydata)              !DSTP319
!---  Save value of height of 1.26 nbar level                                     !DSTP320
      Zbase = ZF                                                                  !DSTP321
!---  Compute exponential correlation across displacement from                    !DSTP322
!     previous position.                                                          !DSTP323
!---  Include effects of time displacement, using wind speed magnitude            !DSTP324
!     and horizontal perturbation scale size.                                     !DSTP325
      dtimecor = dSqrt(EWWIND**2 + NSWIND**2)*DELTIME/(1000.0d0*HLS)              !DSTP326
!---  Unperturbed mean density (approximate if HGTM > ZF)                         !DSTP327
      DENS0 = DENS/(1.0d0 + wavepert)                                             !DSTP328
!---  HGTM = height above MOLA areoid                                             !DSTP329
!---  HGTMS = height above local MOLA topographic surface                         !DSTP330
      HGTMS = HGTM - thgt                                                         !DSTP331
!---  Evaluate correlation and step size relative to accuracy limit               !DSTP332
      If (iupdate >= 0)pertstep = pertstep + dAbs(DELNS) + dAbs(DELEW)   &        !DSTP333
        + dAbs(DELZ) + dAbs(dtimecor)                                             !DSTP334
      corlim = -pertstep/DLog(0.995d0)                                            !DSTP335
      If (corlim <= corlmin.or.iupdate < 0)Then                                   !DSTP336
        CORREL = 1.0d0                                                            !DSTP337
        corbeta = 0.0d0                                                           !DSTP338
        If (iupdate < 0)Then                                                      !DSTP339
          iupdate = -1                                                            !DSTP340
        Else                                                                      !DSTP341
          iupdate = 0                                                             !DSTP342
        Endif                                                                     !DSTP343
      Else                                                                        !DSTP344
!---    Get uniform RANDOM number and Gaussian random number from PPND            !DSTP345
  480   Z2 = RANDOM_M10(L)                                                        !DSTP346
        IF (L  ==  1)GOTO 480                                                     !DSTP347
        Z1 = PPND_M10(Z2,IFAULT)                                                  !DSTP348
        IF (IFAULT  ==  1)STOP ' PPND ERROR'                                      !DSTP349
        CORREL = Dexp(-pertstep)                                                  !DSTP350
        corbeta = DSqrt(1.0d0 - CORREL**2)                                        !DSTP351
        pertstep = 0.0d0                                                          !DSTP352
        iupdate = 1                                                               !DSTP353
      Endif                                                                       !DSTP354
      DENSHI = DENS*FACTHI                                                        !DSTP355
      DPLUS = DENSHI - DENS                                                       !DSTP356
      FACTLO = 1.0d0/FACTHI                                                       !DSTP357
      DENSLO = DENS*FACTLO                                                        !DSTP358
      DMINUS = DENS - DENSLO                                                      !DSTP359
!---  Local time in "Martian hours" (1/24th Sols)                                 !DSTP360
      TLOCAL = 12.0d0 + (SUNLON - CLON)/15.0d0                                    !DSTP361
      IF (TLOCAL  <  0.0d0)TLOCAL = TLOCAL + 24.0                                 !DSTP362
      IF (TLOCAL  >  24.0d0)TLOCAL = TLOCAL - 24.0d0                              !DSTP363
!---  Output height above MOLA areoid (HGTM) or above local                       !DSTP364
!---  terrain (HGTMS)                                                             !DSTP365
      OHGT = HGTM                                                                 !DSTP366
      OHGTS = HGTMS                                                               !DSTP367
!---  Set output heights to terrain height if <= -8.7 km                          !DSTP368
      IF(OHGT  <=  -8.7d0)THEN                                                    !DSTP369
        OHGT = thgt + hgtasfc                                                     !DSTP370
        OHGTS = hgtasfc                                                           !DSTP371
      ENDIF                                                                       !DSTP372
!---  Current random density perturbation value, correlated with                  !DSTP373
!---  previous random density perturbation                                        !DSTP374
      RHOd = CORREL*RHOd + corbeta*Z1                                             !DSTP375
      IF(RHOd < 0.0d0)DensRand = RHOd*DMINUS*rpscale                              !DSTP376
      IF(RHOd >= 0.0d0)DensRand = RHOd*DPLUS*rpscale                              !DSTP377
!---  Add random density perturbation                                             !DSTP378
      DENSP = DENS + DensRand                                                     !DSTP379
!---  Check upper and lower bounds on density perturbations                       !DSTP380
      If (DENSP  <  0.1d0*DENS0)DENSP = 0.1d0*DENS0                               !DSTP381
      If (DENSP  >  10.0d0*DENS0)DENSP = 10.0d0*DENS0                             !DSTP382
!---  Save as total density, for output                                           !DSTP383
      DENSTOT = DENSP                                                             !DSTP384
!---  Standard deviation in random density perturbation (% of                     !DSTP385
!---  unperturbed mean) for output                                                !DSTP386
      SIGD = 100.0d0*dAbs(DENSHI-DENS0)/DENS0                                     !DSTP387
      SIGD = rpscale*SIGD                                                         !DSTP388
!---  Standard deviations for wind perturbations                                  !DSTP389
      SIGU = 2.0d0 + 0.1d0*CHGTc                                                  !DSTP390
!---  Added contribution to SIGU for near-surface heights                         !DSTP391
      If (OHGTS >= 0.0d0.and.OHGTS <= 4.5d0)SIGU = SIGU +               &         !DSTP392
        1.5d0*(1.0d0 - OHGTS/4.5d0)                                               !DSTP393
      If (SIGU > 25.0d0)SIGU = 25.0d0                                             !DSTP394
      SIGU = rwscale*SIGU                                                         !DSTP395
      If (SIGU > 50.0d0)SIGU = 50.0d0                                             !DSTP396
      SIGW = SIGU/5.0d0                                                           !DSTP397
!---  Added contribution to SIGW for near-surface heights                         !DSTP398
      If (OHGTS >= 0.0d0.and.OHGTS <= 4.5d0)SIGW = SIGW +               &         !DSTP399
        1.5d0*(1.0d0 - OHGTS/4.5d0)*rwscale                                       !DSTP400
!---  Adjust random DENSHI, DENSLO for rpscale                                    !DSTP401
      DENSHI = DENS + rpscale*(DENSHI - DENS)                                     !DSTP402
      DENSLO = DENS + rpscale*(DENSLO - DENS)                                     !DSTP403
      If (DENSLO  <  0.1d0*DENS0)DENSLO = 0.1d0*DENS0                             !DSTP404
!---  Convert random density perturbation to % of (unperturbed) mean              !DSTP405
      DensRand = 100.0d0*(DENSP - DENS)/DENS0                                     !DSTP406
!---  Compute total density perturbation as % of (unperturbed) mean               !DSTP407
      DENSP = DensRand + denswave                                                 !DSTP408
!---  Compute EW and NS wind perturbations and total wind                         !DSTP409
 586  If (dabs(corbeta) > 0.0d0)Then                                              !DSTP410
        Z2 = RANDOM_M10(L)                                                        !DSTP411
        If (L == 1)Goto 586                                                       !DSTP412
        Z1 = PPND_M10(Z2,ifault)                                                  !DSTP413
      Endif                                                                       !DSTP414
      RHOu = CORREL*RHOu + corbeta*Z1                                             !DSTP415
!---  Limit winds to sound speed/Sqrt(2)   (ssposr2)                              !DSTP416
!     Assume specific heat ratio = 4/3                                            !DSTP417
      ssposr2 = dSqrt(Cp_M10(TEMP)*TEMP/6.0d0)                                    !DSTP418
!---  Add slope winds, scale mean winds, and limit to 0.7*ssposr2                 !DSTP419
      EWWIND = wmscale*EWWIND  + blwinfac*blwindew                                !DSTP420
      NSWIND = wmscale*NSWIND  + blwinfac*blwindns                                !DSTP421
      If (dAbs(EWWIND) > 0.7d0*ssposr2)EWWIND=dSign(0.7d0*ssposr2,     &          !DSTP422
        EWWIND)                                                                   !DSTP423
      If (dAbs(NSWIND) > 0.7d0*ssposr2)NSWIND=dSign(0.7d0*ssposr2,     &          !DSTP424
        NSWIND)                                                                   !DSTP425
!---  Add slope winds, scale daily mean winds, and limit to 0.7*ssposr2           !DSTP426
      EWwnDay = wmscale*EWwnDay + blwinfac*bluday                                 !DSTP427
      NSwnDay = wmscale*NSwnDay + blwinfac*blvday                                 !DSTP428
      If (dAbs(EWwnDay) > 0.7d0*ssposr2)EWwnDay=dSign(0.7d0*ssposr2,   &          !DSTP429
        EWwnDay)                                                                  !DSTP430
      If (dAbs(NSwnDay) > 0.7d0*ssposr2)NSwnDay=dSign(0.7d0*ssposr2,   &          !DSTP431
        NSwnDay)                                                                  !DSTP432
!---  EW component of perturbation in wind and total wind                         !DSTP433
      EWpert = RHOu*SIGU                                                          !DSTP434
      EWtot = EWWIND + EWpert                                                     !DSTP435
      If (dAbs(EWtot) > ssposr2)Then                                              !DSTP436
        EWtot = dSign(ssposr2,EWtot)                                              !DSTP437
        EWpert = EWtot - EWWIND                                                   !DSTP438
      Endif                                                                       !DSTP439
 587  If(dabs(corbeta) > 0.0d0)Then                                               !DSTP440
        Z2 = RANDOM_M10(L)                                                        !DSTP441
        If (L == 1)Goto 587                                                       !DSTP442
        Z1 = PPND_M10(Z2,ifault)                                                  !DSTP443
      Endif                                                                       !DSTP444
      RHOv = CORREL*RHOv + corbeta*Z1                                             !DSTP445
!---  NS component of perturbation in wind and total wind                         !DSTP446
      NSpert = RHOv*SIGU                                                          !DSTP447
      NStot = NSWIND + NSpert                                                     !DSTP448
      If (dAbs(NStot) > ssposr2)Then                                              !DSTP449
        NStot = dSign(ssposr2,NStot)                                              !DSTP450
        NSpert = NStot - NSWIND                                                   !DSTP451
      Endif                                                                       !DSTP452
 588  If(dabs(corbeta) > 0.0d0)Then                                               !DSTP453
        Z2 = RANDOM_M10(L)                                                        !DSTP454
        If (L == 1)Goto 588                                                       !DSTP455
        Z1 = PPND_M10(Z2,ifault)                                                  !DSTP456
      Endif                                                                       !DSTP457
      RHOw = CORREL*RHOw + corbeta*Z1                                             !DSTP458
!---  Vertical component of perturbation in wind plus slope wind                  !DSTP459
      VWpert = RHOw*SIGW + blwinfac*blwindvert                                    !DSTP460
!---  Compute cosine of solar zenith angle                                        !DSTP461
      cszang = dSin(DTR*SUNLAT)*dSin(DTR*CLATc) + dCos(DTR*SUNLAT)*     &         !DSTP462
        dCos(DTR*CLATc)*dCos(DTR*(SUNLON-CLON))                                   !DSTP463
      szang = dAcos(cszang)/DTR                                                   !DSTP464
      F1peak = 999.9d0                                                            !DSTP465
!---  Compute height of F1 peak if solar zenith angle < 90 deg                    !DSTP466
      If (cszang > 0.0d0.and.Zbase < 900.0d0)Then                                 !DSTP467
!---    relative Air mass                                                         !DSTP468
        airmass = 1.0d0/(cszang + 0.15d0*(93.885d0-szang)**(-1.253d0))            !DSTP469
!---    F1 peak height (km)                                                       !DSTP470
        F1peak = Zbase + HatZF*dlog(airmass)                                      !DSTP471
      Endif                                                                       !DSTP472
!---  Write descriptively formatted data on LIST.txt file                         !DSTP473
      Elon = 360.0d0 - CLON                                                       !DSTP474
!---  Total radius (areoid + height)                                              !DSTP475
      radtotal = careoid + OHGT                                                   !DSTP476
!---  Compute height above reference ellipsoid                                    !DSTP477
      Oldhgt = radtotal - Oldrref                                                 !DSTP478
!---  Difference of MOLA areoid from ellipsoid radius                             !DSTP479
      dAreoid = careoid - Oldrref                                                 !DSTP480
!---  Set output value of local height offset for MGCM or MTGCM data              !DSTP481
      offsetL = hgtoffset                                                         !DSTP482
      If (OHGT <= 80.0d0)offsetL = ofszL                                          !DSTP483
      If(iup > 0)Then                                                             !DSTP484
!        Write(13,*)CSEC,CSEC/onesol,ALS,DustOD,OHGT,OHGTS,                 &     !DSTP485
!          owlt,thgt,radtotal,careoid,Oldhgt,HSCALE,Hrho,ibougher,offsetL,  &     !DSTP486
!          CLATc,CLON,Elon,CLATg,CHGTg,SUNLAT,MARSAU,SUNLON,TLOCAL                !DSTP487
        Write(iup,589)CSEC,CSEC/onesol,ALS,DustOD                                 !DSTP488
        Write(iup,590)OHGT,OHGTS,owlt                                             !DSTP489
        Write(iup,591)thgt,radtotal,careoid                                       !DSTP490
        Write(iup,592)Oldhgt,HSCALE,Hrho                                          !DSTP491
        Write(iup,593)ibougher,offsetL                                            !DSTP492
        Write(iup,594)CLATc,CLON,Elon                                             !DSTP493
        Write(iup,595)CLATg,CHGTg                                                 !DSTP494
        Write(iup,596)SUNLAT,MARSAU                                               !DSTP495
        Write(iup,597)SUNLON,TLOCAL                                               !DSTP496
      Endif                                                                       !DSTP497
  589 FORMAT(' Time (rel. to T0) =',F10.1,' sec. (',F8.3,' sols)','  Ls =',     & !DSTP498
        F6.1,'  Dust =',F5.2)                                                     !DSTP499
  590 FORMAT(' Height Above MOLA (or Surface)',' =',F8.3,' km (',F8.3,          & !DSTP500
        ' km)  OWLT =',F6.2,' Min')                                               !DSTP501
  591 FORMAT(' Topographic Height = ',F8.3,' km   Radius (Areoid) = ',F8.3,     & !DSTP502
        ' (',F8.3,') km')                                                         !DSTP503
  592 FORMAT(' Hgt Above Ellipsoid =',F8.3,' km   Scale Hgt H(p)=',F7.2,        & !DSTP504
        ' H(rho)=', F7.2,' km')                                                   !DSTP505
  593 FORMAT(' Height Offset Parameters:   ibougher =',I2,                      & !DSTP506
        '    Local Height Offset =',F7.3,' km')                                   !DSTP507
  594 FORMAT(' Planeto-Centric Lat = ',F7.2,' deg  Longitude = ',F7.2,' W (',   & !DSTP508
        F7.2,' E) deg.')                                                          !DSTP509
  595 FORMAT(' Planeto-Graphic Lat =',F8.2,                                     & !DSTP510
        ' deg  Planeto-Graphic Hgt (Ellps)=',F9.3,' km')                          !DSTP511
  596 FORMAT(' Planeto-Cent Sun Lat = ',F6.2,' deg  Mars Orbital Radius =',     & !DSTP512
        F6.3,' AU')                                                               !DSTP513
  597 FORMAT(' Sun Longitude =',F8.2,' deg.W      Local True Solar Time = ',    & !DSTP514
        F6.2,' Mars hrs')                                                         !DSTP515
      If (iup > 0.and.OHGT > 80.0d0)Then                                          !DSTP516
        Write(iup,598)Texos,Tbase,Zbase                                           !DSTP517
        Write(iup,599)szang,F1peak                                                !DSTP518
      Endif                                                                       !DSTP519
  598 Format(' Exospheric Temp. = ',F6.1,' K',8X,'Tbase = ',F6.1,        &        !DSTP520
        ' K',3X,' Zbase = ',F6.1,' km')                                           !DSTP521
  599 Format(' Solar Zenith Angle =',F6.1,                               &        !DSTP522
        ' deg     F1 peak =',F6.1,' km')                                          !DSTP523
!---  Compute percent deviations from COSPAR values                               !DSTP524
      Call cospar_M10(OHGT,tcos,pcos,dcosp)                                       !DSTP525
      If (dcosp <= 0.0d0)Then                                                     !DSTP526
        devlo = -99.9d0                                                           !DSTP527
        devav = -99.9d0                                                           !DSTP528
        devhi = -99.9d0                                                           !DSTP529
        devtot = -99.9d0                                                          !DSTP530
        devDay = -99.9d0                                                          !DSTP531
        devmax = -99.9d0                                                          !DSTP532
        devmin = -99.9d0                                                          !DSTP533
      Else                                                                        !DSTP534
        devlo = 100.0d0*(DENSLO-dcosp)/dcosp                                      !DSTP535
        devav = 100.0d0*(DENS-dcosp)/dcosp                                        !DSTP536
        devhi = 100.0d0*(DENSHI-dcosp)/dcosp                                      !DSTP537
        devtot = 100.0d0*(DENSTOT-dcosp)/dcosp                                    !DSTP538
        devDay = 100.0d0*(DensDay-dcosp)/dcosp                                    !DSTP539
        If (idaydata > 0)Then                                                     !DSTP540
        devmax = 100.0d0*(Densmax-dcosp)/dcosp                                    !DSTP541
        devmin = 100.0d0*(Densmin-dcosp)/dcosp                                    !DSTP542
        Endif                                                                     !DSTP543
      Endif                                                                       !DSTP544
      densunits = 'kg/m**3 '                                                      !DSTP545
!---  Convert density units to kg/km**3 if logscale = 3                           !DSTP546
      If (logscale  == 3)Then                                                     !DSTP547
        DENS = 1.0d9*DENS                                                         !DSTP548
        DENSLO = 1.0d9*DENSLO                                                     !DSTP549
        DENSHI = 1.0d9*DENSHI                                                     !DSTP550
        DENSTOT = 1.0d9*DENSTOT                                                   !DSTP551
        DensDay = 1.0d9*DensDay                                                   !DSTP552
        If (idaydata > 0)Then                                                     !DSTP553
        Densmax = 1.0d9*Densmax                                                   !DSTP554
        Densmin = 1.0d9*Densmin                                                   !DSTP555
        Endif                                                                     !DSTP556
        densunits = 'kg/km**3'                                                    !DSTP557
      Endif                                                                       !DSTP558
!---  Write formatted output to list file                                         !DSTP559
      If(iup > 0)Then                                                             !DSTP560
        Write(iup,600)TEMP,PRES,profwgt                                           !DSTP561
        Write(iup,601)DENSLO,DENS,DENSHI,densunits                                !DSTP562
        Write(iup,602)devlo,devav,devhi,iupdate                                   !DSTP563
        Write(iup,603)DENSTOT,densunits,DENSP,denswave                            !DSTP564
        Write(iup,604)EWWIND,EWpert,EWtot                                         !DSTP565
        Write(iup,605)NSWIND,NSpert,NStot,VWpert                                  !DSTP566
        Call species_M10(OHGT,CLATc,als,Zbase,AMz,DENS,PRES,TEMP,iup,    &        !DSTP567
          fmol,fmass,fmolH2O,fmassH2O)                                            !DSTP568
        If (I > 0.and.corlim < 1.0d0.and.dAbs(CLAT) < 89.99d0)           &        !DSTP569
          Write(iup,610)corlim                                                    !DSTP570
        Write(iup,650)                                                            !DSTP571
      Endif                                                                       !DSTP572
  600 FORMAT(' Temperature = ',F7.1,' K',7X,' Pressure =',1p,E10.3,0p,   &        !DSTP573
        ' N/m**2   profwgt =',F6.3)                                               !DSTP574
  601 FORMAT(' Density (Low, Avg., High) =',1p,3E12.3,0p,1X,A8)                   !DSTP575
  602 FORMAT(' Departure, COSPAR NH Mean =',F9.1,' %',2(F10.1,' %'),     &        !DSTP576
        '  iupdate =',I2)                                                         !DSTP577
  603 FORMAT(' Tot.Dens. =',1p,E10.3,0p,1X,A8,'  Dens.Pert. =',F7.2,     &        !DSTP578
        '% Wave =',F7.2,'% of mean')                                              !DSTP579
  604 FORMAT(' Eastward Wind (Mean,Perturbed,Total) =',3F7.1,' m/s   ',  &        !DSTP580
        ' VertWind')                                                              !DSTP581
  605 FORMAT(' Northward Wind(Mean,Perturbed,Total) =',3F7.1,' m/s',     &        !DSTP582
        F8.1,' m/s')                                                              !DSTP583
  610 Format(' Warning: Step size smaller than accuracy limit by a ',    &        !DSTP584
        'factor of',F6.3)                                                         !DSTP585
  650 FORMAT(' -----------------------------------------------------',   &        !DSTP586
        '------------------------')                                               !DSTP587
      PRESmb = PRES/100.                                                          !DSTP588
      sigmalevel = PRES/Patsurf                                                   !DSTP589
      preshgt = -HSCALE*dlog(sigmalevel)                                          !DSTP590
!---  Compute dust density variables by methods of Haberle et al.,                !DSTP591
!      Icarus, 50, 322 (1982) and Haberle et al., J. Geophys. Res.,               !DSTP592
!      104, 8957 (1999)                                                           !DSTP593
!---  Dust column areal density (kg/m**2)                                         !DSTP594
      dareaden = 5.0D-3*DustOD                                                    !DSTP595
!---  Dust mixing ratio (kg dust/kg air) at surface                               !DSTP596
      qsurf = dareaden*gz/(0.994d0*dExp(-Dustnu)*Patsurf)                         !DSTP597
!---  Dust mixing ratio at current position and pressure                          !DSTP598
      dmixrat = 0.0d0                                                             !DSTP599
      expfact = Dustnu*(1.0d0 - 1.0d0/sigmalevel)                                 !DSTP600
      If(expfact > -85.0d0)dmixrat = qsurf*dExp(expfact)                          !DSTP601
!---  Dust mass density (micrograms dust / m**3)                                  !DSTP602
      dmasden = 1.0d9*dmixrat*DENS                                                !DSTP603
      If (LOGSCALE == 3)dmasden = dmasden/1.0d9                                   !DSTP604
!---  Dust number density (number dust particles / m**3)                          !DSTP605
      dnumden = dmasden/(5.23599D-10*Dustdens*Dustdiam**3)                        !DSTP606
      If(NVARX == 9)VARX = PRESmb                                                 !DSTP607
      If(NVARY == 9)VARY = PRESmb                                                 !DSTP608
      If(NVARX == 10)VARX = preshgt                                               !DSTP609
      If(NVARY == 10)VARY = preshgt                                               !DSTP610
      If(NVARX == 11)VARX = sigmalevel                                            !DSTP611
      If(NVARY == 11)VARY = sigmalevel                                            !DSTP612
      If(NVARX == 12)VARX = Oldhgt                                                !DSTP613
      If(NVARY == 12)VARY = Oldhgt                                                !DSTP614
      If(NVARX == 13)VARX = CHGTg                                                 !DSTP615
      If(NVARY == 13)VARY = CHGTg                                                 !DSTP616
      If(NVARX == 14)VARX = CLATg                                                 !DSTP617
      If(NVARY == 14)VARY = CLATg                                                 !DSTP618
!---  Output deviations from COSPAR if logscale = 2                               !DSTP619
      If (logscale == 2)Then                                                      !DSTP620
        DENSLO = devlo                                                            !DSTP621
        DENS = devav                                                              !DSTP622
        DENSHI = devhi                                                            !DSTP623
        DENSTOT = devtot                                                          !DSTP624
        DensDay = devDay                                                          !DSTP625
        Densmax = devmax                                                          !DSTP626
        Densmin = devmin                                                          !DSTP627
        If (pcos <= 0.0d0)Then                                                    !DSTP628
          PRES = -99.9d0                                                          !DSTP629
          PresDay = -99.9d0                                                       !DSTP630
        Else                                                                      !DSTP631
          PRES = 100.0d0*(PRES-pcos)/pcos                                         !DSTP632
          PresDay = 100.0d0*(PresDay-pcos)/pcos                                   !DSTP633
        Endif                                                                     !DSTP634
      Endif                                                                       !DSTP635
!---  Write parameters on plot format files                                       !DSTP636
      IF(NVARX == 1)VARX = OHGT                                                   !DSTP637
      IF(NVARX == 2)VARX = OHGTS                                                  !DSTP638
      IF(NVARX == 3)VARX = CLATc                                                  !DSTP639
      IF(NVARX == 4)Then                                                          !DSTP640
        VARX = CLON                                                               !DSTP641
        If (LonEW == 1)VARX = 360.0d0 - CLON                                      !DSTP642
      Endif                                                                       !DSTP643
      If(NVARX == 15)Then                                                         !DSTP644
        VARX = CLON                                                               !DSTP645
        If (VARX > 180.0d0)VARX = VARX - 360.0d0                                  !DSTP646
        If (LonEW == 1)VARX = -VARX                                               !DSTP647
      Endif                                                                       !DSTP648
      IF(NVARX == 5)VARX = CSEC                                                   !DSTP649
      If(NVARX == 6)VARX = CSEC/onesol                                            !DSTP650
      If(NVARX == 7)VARX = ALS                                                    !DSTP651
      If(NVARX == 8)VARX = TLOCAL                                                 !DSTP652
      Alogdens = 0.0d0                                                            !DSTP653
      If (logscale  /=  2)Alogdens = dlog10(DENS)                                 !DSTP654
      If (logscale  ==  1)then                                                    !DSTP655
        DENS = Alogdens                                                           !DSTP656
        PRES = dlog10(PRES)                                                       !DSTP657
        DENSLO = dlog10(DENSLO)                                                   !DSTP658
        DENSHI = dLOG10(DENSHI)                                                   !DSTP659
        DENSTOT = dlog10(DENSTOT)                                                 !DSTP660
        If (OHGT <= TopZ) Then                                                    !DSTP661
          DensDay = dlog10(DensDay)                                               !DSTP662
          PresDay = dlog10(PresDay)                                               !DSTP663
          If (idaydata > 0)Then                                                   !DSTP664
          Densmax = dlog10(Densmax)                                               !DSTP665
          Densmin = dlog10(Densmin)                                               !DSTP666
          Endif                                                                   !DSTP667
        Endif                                                                     !DSTP668
      Endif                                                                       !DSTP669
      If (NVARY  ==  0.and.iup > 0)then                                           !DSTP670
        Write(21,796)VARX,DENSLO,DENS,DENSHI,DENSTOT,DustOD,radtotal,   &         !DSTP671
          gz,MARSAU,LOGSCALE,offsetL,ibougher,MapYear,profwgt                     !DSTP672
        Write(22,790)VARX,SIGD,DensRand,denswave,DENSP,corlim,SIGU,     &         !DSTP673
          SIGW,iupdate                                                            !DSTP674
        Write(23,781)VARX,EWWIND,EWpert,EWtot,NSWIND,NSpert,NStot,      &         !DSTP675
          VWpert,iupdate                                                          !DSTP676
        Write(24,798)VARX,TEMP,PRES,TEMP-273.15d0,PRESmb,Hrho,HSCALE,   &         !DSTP677
         AMz,thgt,Tgrnd,careoid,dAreoid,fmol,fmolH2O,LOGSCALE                     !DSTP678
        If(OHGT <= TopZ.and.profnear <= 0.0)Write(25,780)VARX,TempDay,  &         !DSTP679
          PresDay,DensDay,EWwnDay,NSwnDay,Tempmin,Tempmax,Densmin,      &         !DSTP680
          Densmax,LOGSCALE,DENS                                                   !DSTP681
        If(OHGT > 80.0d0)Write(26,791)VARX,Tbase,Zbase,F1peak,AMz,      &         !DSTP682
          Texos,hgtoffset,ibougher                                                !DSTP683
        Write(27,793)VARX,talb,cszang,dareaden,dmixrat,dmasden,dnumden, &         !DSTP684
          icepolar                                                                !DSTP685
      Else If (iup > 0) Then                                                      !DSTP686
        IF(NVARY == 1)VARY = OHGT                                                 !DSTP687
        IF(NVARY == 2)VARY = OHGTS                                                !DSTP688
        IF(NVARY == 3)VARY = CLATc                                                !DSTP689
        IF(NVARY == 4)Then                                                        !DSTP690
          VARY = CLON                                                             !DSTP691
          If (LonEW == 1)VARY = 360.0d0 - CLON                                    !DSTP692
        Endif                                                                     !DSTP693
        If(NVARY == 15)Then                                                       !DSTP694
          VARY = CLON                                                             !DSTP695
          If (VARY > 180.0d0)VARY = VARY - 360.0d0                                !DSTP696
          If (LonEW == 1)VARY = -VARY                                             !DSTP697
        Endif                                                                     !DSTP698
        If(NVARY == 5)VARY = CSEC                                                 !DSTP699
        If(NVARY == 6)VARY = CSEC/onesol                                          !DSTP700
        If(NVARY == 7)VARY = ALS                                                  !DSTP701
        If(NVARY == 8)VARY = TLOCAL                                               !DSTP702
        Write(21,797)VARX,VARY,DENSLO,DENS,DENSHI,DENSTOT,DustOD,       &         !DSTP703
          radtotal,gz,MARSAU,LOGSCALE,offsetL,ibougher,MapYear,profwgt            !DSTP704
        Write(22,795)VARX,VARY,SIGD,DensRand,denswave,DENSP,corlim,SIGU &         !DSTP705
          ,SIGW,iupdate                                                           !DSTP706
        Write(23,782)VARX,VARY,EWWIND,EWpert,EWtot,NSWIND,NSpert,NStot, &         !DSTP707
          VWpert,iupdate                                                          !DSTP708
        Write(24,799)VARX,VARY,TEMP,PRES,TEMP-273.15d0,PRESmb,Hrho,     &         !DSTP709
          HSCALE,AMz,thgt,Tgrnd,careoid,dAreoid,fmol,fmolH2O,LOGSCALE             !DSTP710
        If(OHGT <= TopZ.and.profnear <= 0.0)Write(25,785)VARX,VARY,     &         !DSTP711
          TempDay,PresDay,DensDay,EWwnDay,NSwnDay,Tempmin,Tempmax,      &         !DSTP712
          Densmin,Densmax,LOGSCALE,DENS                                           !DSTP713
        If(OHGT > 80.0d0)Write(26,792)VARX,VARY,Tbase,Zbase,F1peak,     &         !DSTP714
          AMz,Texos,hgtoffset,ibougher                                            !DSTP715
        Write(27,794)VARX,VARY,talb,cszang,dareaden,dmixrat,dmasden,    &         !DSTP716
          dnumden,icepolar                                                        !DSTP717
      Endif                                                                       !DSTP718
  780 Format(G13.5,F7.1,1p,2E11.3,0p,4F8.1,1p,2E11.3,I5,E14.3)                    !DSTP719
  781 FORMAT(G13.5,7F8.2,I3)                                                      !DSTP720
  782 Format(2G13.5,7F8.2,I3)                                                     !DSTP721
  785 Format(2G13.5,F7.1,1p,2E11.3,0p,4F8.1,1p,2E11.3,I5,E14.3)                   !DSTP722
  790 FORMAT(G13.5,F6.2,3F10.3,1p,E10.3,0p,2F7.2,I5)                              !DSTP723
  791 Format(G13.5,3F8.1,F8.2,F8.1,F10.3,I6)                                      !DSTP724
  792 Format(2G13.5,3F8.1,F8.2,F8.1,F10.3,I6)                                     !DSTP725
  793 Format(G13.5,F6.3,F9.5,1p,4E9.2,0p,I3)                                      !DSTP726
  794 Format(2G13.5,F6.3,F9.5,1p,4E9.2,0p,I3)                                     !DSTP727
  795 FORMAT(2G13.5,F6.2,3F10.3,1p,E10.3,0p,2F7.2,I5)                             !DSTP728
  796 FORMAT(G13.5,1p,4E11.3,0p,F7.4,F9.3,2F6.3,I5,F13.3,I6,I8,F9.3)              !DSTP729
  797 Format(2G13.5,1p,4E11.3,0p,F7.4,F9.3,2F6.3,I5,F13.3,I6,I8,F9.3)             !DSTP730
  798 FORMAT(G13.5,2(F7.1,1p,E11.3,0p),2F8.2,F6.2,F7.3,F6.1,F9.3,F8.3,  &         !DSTP731
        10F6.2,I5)                                                                !DSTP732
  799 Format(2G13.5,2(F7.1,1p,E11.3,0p),2F8.2,F6.2,F7.3,F6.1,F9.3,F8.3, &         !DSTP733
        10F6.2,I5)                                                                !DSTP734
!---  Write non-descriptively formatted data on OUTPUT file                       !DSTP735
      If (MOLAhgts == 1)Then                                                      !DSTP736
        VAR = OHGT                                                                !DSTP737
        IF(NVARX == 2.or.NVARY == 2)VAR = OHGTS                                   !DSTP738
        OLAT = CLATc                                                              !DSTP739
      Else                                                                        !DSTP740
        VAR = Oldhgt                                                              !DSTP741
        OLAT = CLATc                                                              !DSTP742
        If (ipclat /= 1)Then                                                      !DSTP743
          VAR = CHGTg                                                             !DSTP744
          OLAT = CLATg                                                            !DSTP745
        Endif                                                                     !DSTP746
      Endif                                                                       !DSTP747
      If(iup > 0)Then                                                             !DSTP748
        OLON = CLON                                                               !DSTP749
        If (LonEW == 1)OLON = 360.0d0 - CLON                                      !DSTP750
        If (NVARX == 15.or.NVARY == 15)Then                                       !DSTP751
          If(OLON > 180.0d0)OLON = OLON - 360.0d0                                 !DSTP752
        Endif                                                                     !DSTP753
        If(logscale == 0.or.logscale == 3)Then                                    !DSTP754
          WRITE(29,800)CSEC,VAR,OLAT,OLON,dens,TEMP,EWWIND,             &         !DSTP755
            NSWIND,SIGD,ALS,DustOD,TLOCAL,fmass,fmassH2O,DENSTOT/DENS             !DSTP756
        Else                                                                      !DSTP757
          WRITE(29,810)CSEC,VAR,OLAT,OLON,dens,TEMP,EWWIND,             &         !DSTP758
            NSWIND,SIGD,ALS,DustOD,TLOCAL,fmass,fmassH2O,DENSTOT/DENS             !DSTP759
        Endif                                                                     !DSTP760
      Endif                                                                       !DSTP761
  800 FORMAT(F10.0,2F7.2,F8.2,1P,E10.3,0P,3F7.1,2F6.1,F5.2,11F6.2,F7.3)           !DSTP762
  810 FORMAT(F10.0,2F7.2,F8.2,F10.3,3F7.1,2F6.1,F5.2,11F6.2,F7.3)                 !DSTP763
      Return                                                                      !DSTP764
  999 EOF = 1                                                                     !DSTP765
      If (NPOS <= 0)Rewind(7)                                                     !DSTP766
      Return                                                                      !DSTP767
 9998 Stop ' Error termination reading trajectory data file!'                     !DSTP768
      END                                                                         !DSTP769
!                                                                                 !DSTP770
!-----------------------------------------------------------------------------    !DVLS  1
      real(kind(0.0d0)) function dustvsls_M10 (als, dustmin, dustmax)             !DVLS  2
!-----------------------------------------------                                  !DVLS  3
!   M o d u l e s                                                                 !DVLS  4
!-----------------------------------------------                                  !DVLS  5
      USE vast_kind_param, ONLY:  double                                          !DVLS  6
!...                                                                              !DVLS  7
!...Switches:                                                                     !DVLS  8
      implicit none                                                               !DVLS  9
!-----------------------------------------------                                  !DVLS 10
!   D u m m y   A r g u m e n t s                                                 !DVLS 11
!-----------------------------------------------                                  !DVLS 12
      real(double) , intent(in) :: als                                            !DVLS 13
      real(double) , intent(in) :: dustmin                                        !DVLS 14
      real(double) , intent(in) :: dustmax                                        !DVLS 15
!-----------------------------------------------                                  !DVLS 16
!   L o c a l   V a r i a b l e s                                                 !DVLS 17
!-----------------------------------------------                                  !DVLS 18
      real(double) :: pi180                                                       !DVLS 19
!-----------------------------------------------                                  !DVLS 20
!---  Assumed seasonal variation (versus solar angle Ls) for non-                 !DVLS 21
!     dust-storm optical depth (Dustmin at Ls=90; Dustmax at Ls=270)              !DVLS 22
      pi180 = datan(1.0D0)/45.0D0                                                 !DVLS 23
      dustvsls_M10 = ((dustmax + dustmin) - (dustmax - dustmin)*dsin(pi180*als) & !DVLS 24
         )/2.0D0                                                                  !DVLS 25
      return                                                                      !DVLS 26
      end function dustvsls_M10                                                   !DVLS 27
!                                                                                 !DVLS 28
!-------------------------------------------------------------------------------- !ESCL  1
      SUBROUTINE ESCALC_M10(STDL, SIGMA, ES)                                      !ESCL  2
!-----------------------------------------------                                  !ESCL  3
!   M o d u l e s                                                                 !ESCL  4
!-----------------------------------------------                                  !ESCL  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !ESCL  6
!---  EPS ARE STD VARNS FROM NOMINAL VALUES                                       !ESCL  7
!---  0,2,....10  LONG - TERM                                                     !ESCL  8
!---  1,3.....,11  SHORT-TERM                                                     !ESCL  9
!...                                                                              !ESCL 10
!...Switches:                                                                     !ESCL 11
      IMPLICIT NONE                                                               !ESCL 12
!-----------------------------------------------                                  !ESCL 13
!   D u m m y   A r g u m e n t s                                                 !ESCL 14
!-----------------------------------------------                                  !ESCL 15
      REAL(DOUBLE) , INTENT(IN) :: STDL                                           !ESCL 16
      REAL(DOUBLE) , INTENT(IN) :: SIGMA                                          !ESCL 17
      REAL(DOUBLE) , INTENT(OUT) :: ES(0:11)                                      !ESCL 18
!-----------------------------------------------                                  !ESCL 19
!   L o c a l   V a r i a b l e s                                                 !ESCL 20
!-----------------------------------------------                                  !ESCL 21
      REAL(DOUBLE), DIMENSION(0:11) :: SIG, EPS                                   !ESCL 22
!-----------------------------------------------                                  !ESCL 23
      DATA EPS/ 12*0.0D0/                                                         !ESCL 24
      EPS(2:10:2) = STDL                                                          !ESCL 25
      EPS(3:9:2) = SIGMA                                                          !ESCL 26
      SIG(0) = 0.25D0                                                             !ESCL 27
!---  LONG TERM -FBAR                                                             !ESCL 28
!---  THESE ARE COEF. OF VARIATION, I.E. SIGMA/MU,    %/100.0                     !ESCL 29
      SIG(1) = 0.0D0                                                              !ESCL 30
!---  SHORT TERM-FBAR                                                             !ESCL 31
      SIG(2) = 0.16D0                                                             !ESCL 32
!---  LONG TERM-TINF                                                              !ESCL 33
      SIG(3) = 0.12D0                                                             !ESCL 34
!---  SHORT TERM-TINF                                                             !ESCL 35
      SIG(4) = 0.40D0                                                             !ESCL 36
!---  LONG TERM - FOXY                                                            !ESCL 37
      SIG(5) = 0.12D0                                                             !ESCL 38
!---  SHORT TERM -FOXY                                                            !ESCL 39
      SIG(6) = 0.0D0                                                              !ESCL 40
!---  LONG TERM -AOXY                                                             !ESCL 41
      SIG(7) = 0.21D0                                                             !ESCL 42
!---  SHORT TERM -AOXY                                                            !ESCL 43
      SIG(8) = 0.045D0                                                            !ESCL 44
!---  LONG TERM - ZF                                                              !ESCL 45
      SIG(9) = 0.0225D0                                                           !ESCL 46
!---  SHORT TERM - ZF                                                             !ESCL 47
      SIG(10) = 0.30D0                                                            !ESCL 48
!---  LONG TERM- DZDUST                                                           !ESCL 49
      SIG(11) = 0.0D0                                                             !ESCL 50
!---  SHORT TERM - DZDUST                                                         !ESCL 51
      ES(:11) = EPS(:11)*SIG(:11)                                                 !ESCL 52
      RETURN                                                                      !ESCL 53
      END SUBROUTINE ESCALC_M10                                                   !ESCL 54
!                                                                                 !ESCL 55
!------------------------------------------------------------------------------   !FORD  1
      subroutine fourd_M10(dx, dy, dz, dq, array, value, lint)                    !FORD  2
!-----------------------------------------------                                  !FORD  3
!   M o d u l e s                                                                 !FORD  4
!-----------------------------------------------                                  !FORD  5
      USE vast_kind_param, ONLY:  double                                          !FORD  6
!---    4-Dimensional linear interpolation within a 1x1x1x1 hypercube             !FORD  7
!       (x,y,z,q) of Array(2,2,2,2) to position dx,dy,dz,dq (all 0-1).            !FORD  8
!       Value is value of interpolated output.                                    !FORD  9
!...                                                                              !FORD 10
!...Switches:                                                                     !FORD 11
      implicit none                                                               !FORD 12
!-----------------------------------------------                                  !FORD 13
!   D u m m y   A r g u m e n t s                                                 !FORD 14
!-----------------------------------------------                                  !FORD 15
      integer , intent(in) :: lint                                                !FORD 16
      real(double) , intent(in) :: dx                                             !FORD 17
      real(double) , intent(in) :: dy                                             !FORD 18
      real(double) , intent(in) :: dz                                             !FORD 19
      real(double) , intent(in) :: dq                                             !FORD 20
      real(double) , intent(out) :: value                                         !FORD 21
      real(double) , intent(in) :: array(2,2,2,2)                                 !FORD 22
!-----------------------------------------------                                  !FORD 23
!   L o c a l   V a r i a b l e s                                                 !FORD 24
!-----------------------------------------------                                  !FORD 25
      real(double) :: dxp, dyp, dzp, dqp                                          !FORD 26
!-----------------------------------------------                                  !FORD 27
!---    Complementary displacements in x,y,z,q                                    !FORD 28
      dxp = 1.0D0 - dx                                                            !FORD 29
      dyp = 1.0D0 - dy                                                            !FORD 30
      dzp = 1.0D0 - dz                                                            !FORD 31
      dqp = 1.0D0 - dq                                                            !FORD 32
      if (lint /= 1) then                                                         !FORD 33
!---      Compute 4-dimensional linear interpolated value                         !FORD 34
         value = dxp*dyp*dzp*dqp*array(1,1,1,1) + dxp*dyp*dzp*dq*array(1,1,1,2) & !FORD 35
             + dxp*dyp*dz*dqp*array(1,1,2,1) + dxp*dy*dzp*dqp*array(1,2,1,1) +  & !FORD 36
            dx*dyp*dzp*dqp*array(2,1,1,1) + dxp*dyp*dz*dq*array(1,1,2,2) + dxp* & !FORD 37
            dy*dzp*dq*array(1,2,1,2) + dxp*dy*dz*dqp*array(1,2,2,1) + dx*dyp*   & !FORD 38
            dzp*dq*array(2,1,1,2) + dx*dyp*dz*dqp*array(2,1,2,1) + dx*dy*dzp*   & !FORD 39
            dqp*array(2,2,1,1) + dxp*dy*dz*dq*array(1,2,2,2) + dx*dyp*dz*dq*    & !FORD 40
            array(2,1,2,2) + dx*dy*dzp*dq*array(2,2,1,2) + dx*dy*dz*dqp*array(2 & !FORD 41
            ,2,2,1) + dx*dy*dz*dq*array(2,2,2,2)                                  !FORD 42
      else                                                                        !FORD 43
!---      Compute 4-dimensional linear interpolated value                         !FORD 44
         value = dexp(dxp*dyp*dzp*dqp*dlog(array(1,1,1,1))+dxp*dyp*dzp*dq*dlog( & !FORD 45
            array(1,1,1,2))+dxp*dyp*dz*dqp*dlog(array(1,1,2,1))+dxp*dy*dzp*dqp* & !FORD 46
            dlog(array(1,2,1,1))+dx*dyp*dzp*dqp*dlog(array(2,1,1,1))+dxp*dyp*dz & !FORD 47
            *dq*dlog(array(1,1,2,2))+dxp*dy*dzp*dq*dlog(array(1,2,1,2))+dxp*dy* & !FORD 48
            dz*dqp*dlog(array(1,2,2,1))+dx*dyp*dzp*dq*dlog(array(2,1,1,2))+dx*  & !FORD 49
            dyp*dz*dqp*dlog(array(2,1,2,1))+dx*dy*dzp*dqp*dlog(array(2,2,1,1))+ & !FORD 50
            dxp*dy*dz*dq*dlog(array(1,2,2,2))+dx*dyp*dz*dq*dlog(array(2,1,2,2)) & !FORD 51
            +dx*dy*dzp*dq*dlog(array(2,2,1,2))+dx*dy*dz*dqp*dlog(array(2,2,2,1) & !FORD 52
            )+dx*dy*dz*dq*dlog(array(2,2,2,2)))                                   !FORD 53
      endif                                                                       !FORD 54
      return                                                                      !FORD 55
      end subroutine fourd_M10                                                    !FORD 56
!                                                                                 !FORD 57
!-------------------------------------------------------------------------------  !GTRP  1
      subroutine mgcmterp_M10(khgt, time, tmgcm, pmgcm, dmgcm, umgcm, vmgcm,    & !GTRP  2
         tempday, presday, densday, uwndday, vwndday, tempmax, tempmin, densmax & !GTRP  3
         , densmin, idaydata)                                                     !GTRP  4
!-----------------------------------------------                                  !GTRP  5
!   M o d u l e s                                                                 !GTRP  6
!-----------------------------------------------                                  !GTRP  7
      USE vast_kind_param, ONLY:  double                                          !GTRP  8
      USE mgcmdata_M10_C                                                          !GTRP  9
      USE interp_M10_C                                                            !GTRP 10
!---    Interpolates Ames Mars General Circulation Model (MGCM) data              !GTRP 11
!       to a given latitude, time of year (Ls), and dust optical                  !GTRP 12
!       depth, for a given height index (khgt) and time of day (time).            !GTRP 13
!       Some input data is provided by the Common "Interp".                       !GTRP 14
!---    Set parameter values for number of heights, latitudes, and                !GTRP 15
!       number of dust optical depth values                                       !GTRP 16
!...                                                                              !GTRP 17
!...Switches:                                                                     !GTRP 18
!-----------------------------------------------                                  !GTRP 19
!   I n t e r f a c e   B l o c k s                                               !GTRP 20
!-----------------------------------------------                                  !GTRP 21
      use tidex_M10_I                                                             !GTRP 22
      use tidey_M10_I                                                             !GTRP 23
      use threed_M10_I                                                            !GTRP 24
      implicit none                                                               !GTRP 25
!-----------------------------------------------                                  !GTRP 26
!   G l o b a l   P a r a m e t e r s                                             !GTRP 27
!-----------------------------------------------                                  !GTRP 28
!-----------------------------------------------                                  !GTRP 29
!   D u m m y   A r g u m e n t s                                                 !GTRP 30
!-----------------------------------------------                                  !GTRP 31
      integer , intent(in) :: khgt                                                !GTRP 32
      integer , intent(in) :: idaydata                                            !GTRP 33
      real(double), intent(in)  :: time                                           !GTRP 34
      real(double), intent(out)  :: tmgcm                                         !GTRP 35
      real(double), intent(out)  :: pmgcm                                         !GTRP 36
      real(double), intent(out) :: dmgcm                                          !GTRP 37
      real(double), intent(out)  :: umgcm                                         !GTRP 38
      real(double), intent(out)  :: vmgcm                                         !GTRP 39
      real(double), intent(out)  :: tempday                                       !GTRP 40
      real(double), intent(out)  :: presday                                       !GTRP 41
      real(double), intent(out) :: densday                                        !GTRP 42
      real(double), intent(out)  :: uwndday                                       !GTRP 43
      real(double), intent(out)  :: vwndday                                       !GTRP 44
      real(double), intent(out)  :: tempmax                                       !GTRP 45
      real(double), intent(out)  :: tempmin                                       !GTRP 46
      real(double), intent(out)  :: densmax                                       !GTRP 47
      real(double), intent(out)  :: densmin                                       !GTRP 48
!-----------------------------------------------                                  !GTRP 49
!   L o c a l   V a r i a b l e s                                                 !GTRP 50
!-----------------------------------------------                                  !GTRP 51
      integer :: i, l, m, itime                                                   !GTRP 52
      real(double), dimension(2,2,2) :: tm, pm, um, vm, r0, tday, pday, uday,  &  !GTRP 53
         vday, tmax, tmin, dmax, dmin                                             !GTRP 54
      real(double) :: polefac, upolefac, t0, a1t, p1t, a2t, p2t, p0, a1p, p1p, &  !GTRP 55
         a2p, p2p, d0, xtime, ttime, ptime, dtime, u0, a1, p1, a2, p2, v0,     &  !GTRP 56
         rmgcm                                                                    !GTRP 57
!-----------------------------------------------                                  !GTRP 58
!---    MGCM 0-80 km data arrays for interpolation                                !GTRP 59
!---    Establish MGCM values at corners of a 3-dimensional cube in               !GTRP 60
!       latitude-Ls-dust space, at the given height index (khgt), and             !GTRP 61
!       time of day (time)                                                        !GTRP 62
      do i = 1, 2                                                                 !GTRP 63
         polefac = 1.0D0                                                          !GTRP 64
         upolefac = 1.0D0                                                         !GTRP 65
         if (ilat == 1) then                                                      !GTRP 66
            polefac = i - 1.0D0                                                   !GTRP 67
         else if (ilat == nlat - 1) then                                          !GTRP 68
            polefac = 2.0D0 - i                                                   !GTRP 69
         endif                                                                    !GTRP 70
         if (ilatw == 2) then                                                     !GTRP 71
            if (i == 1) upolefac = wpolefac                                       !GTRP 72
         else if (ilatw == nlat - 1) then                                         !GTRP 73
            if (i == 2) upolefac = wpolefac                                       !GTRP 74
         endif                                                                    !GTRP 75
         do l = 1, 2                                                              !GTRP 76
            do m = 1, 2                                                           !GTRP 77
!---      Daily mean temperature                                                  !GTRP 78
               t0 = tza0(khgt,ilat+i-1,ls+l-1,mdust+m-1)                          !GTRP 79
               tday(i,l,m) = t0                                                   !GTRP 80
!---      Temperature tide amplitudes and phases                                  !GTRP 81
               a1t = tza1(khgt,ilat+i-1,ls+l-1,mdust+m-1)*polefac                 !GTRP 82
               p1t = tzp1(khgt,ilat+i-1,ls+l-1,mdust+m-1)                         !GTRP 83
               a2t = tza2(khgt,ilat+i-1,ls+l-1,mdust+m-1)*polefac                 !GTRP 84
               p2t = tzp2(khgt,ilat+i-1,ls+l-1,mdust+m-1)                         !GTRP 85
!---      Temperature at corners of 3-D cube                                      !GTRP 86
               tm(i,l,m) = tidex_M10(t0,a1t,p1t,a2t,p2t,time)                     !GTRP 87
!---      Daily mean pressure                                                     !GTRP 88
               p0 = pza0(khgt,ilat+i-1,ls+l-1,mdust+m-1)                          !GTRP 89
               pday(i,l,m) = p0                                                   !GTRP 90
!---      Pressure tide amplitudes and phases                                     !GTRP 91
               a1p = pza1(khgt,ilat+i-1,ls+l-1,mdust+m-1)*polefac                 !GTRP 92
               p1p = pzp1(khgt,ilat+i-1,ls+l-1,mdust+m-1)                         !GTRP 93
               a2p = pza2(khgt,ilat+i-1,ls+l-1,mdust+m-1)*polefac                 !GTRP 94
               p2p = pzp2(khgt,ilat+i-1,ls+l-1,mdust+m-1)                         !GTRP 95
!---      Pressure at corners of 3-D cube                                         !GTRP 96
               pm(i,l,m) = tidey_M10(p0,a1p,p1p,a2p,p2p,time)                     !GTRP 97
!---      Daily average density D0                                                !GTRP 98
               d0 = dza0(khgt,ilat+i-1,ls+l-1,mdust+m-1)                          !GTRP 99
!---      Gas constant from pressure, density and temperature                     !GTRP100
               r0(i,l,m) = 190.0D0                                                !GTRP101
               if (dabs(t0)>0.0D0 .and. dabs(d0)>0.0D0) r0(i,l,m) = p0/(t0*d0)    !GTRP102
!---      Max and Min temperature and density at corners of 3-D cube              !GTRP103
               tmax(i,l,m) = -9999.0D0                                            !GTRP104
               tmin(i,l,m) = 9999.0D0                                             !GTRP105
               dmax(i,l,m) = -9999.0D0                                            !GTRP106
               dmin(i,l,m) = 9999.0D0                                             !GTRP107
               if (idaydata > 0) then                                             !GTRP108
                  do itime = 0, 23                                                !GTRP109
                     xtime = float(itime)                                         !GTRP110
                     ttime = tidex_M10(t0,a1t,p1t,a2t,p2t,xtime)                  !GTRP111
                     ptime = tidey_M10(p0,a1p,p1p,a2p,p2p,xtime)                  !GTRP112
                     dtime = ptime/(r0(i,l,m)*ttime)                              !GTRP113
                     tmax(i,l,m) = dmax1(ttime,tmax(i,l,m))                       !GTRP114
                     tmin(i,l,m) = min(ttime,tmin(i,l,m))                         !GTRP115
                     dmax(i,l,m) = dmax1(dtime,dmax(i,l,m))                       !GTRP116
                     dmin(i,l,m) = min(dtime,dmin(i,l,m))                         !GTRP117
                  end do                                                          !GTRP118
               endif                                                              !GTRP119
!---      Daily mean EW wind                                                      !GTRP120
               u0 = uza0(khgt,ilatw+i-1,ls+l-1,mdust+m-1)                         !GTRP121
               uday(i,l,m) = u0                                                   !GTRP122
!---      EW wind tide amplitudes and phases                                      !GTRP123
               a1 = uza1(khgt,ilatw+i-1,ls+l-1,mdust+m-1)*upolefac                !GTRP124
               p1 = uzp1(khgt,ilatw+i-1,ls+l-1,mdust+m-1)                         !GTRP125
               a2 = uza2(khgt,ilatw+i-1,ls+l-1,mdust+m-1)*upolefac                !GTRP126
               p2 = uzp2(khgt,ilatw+i-1,ls+l-1,mdust+m-1)                         !GTRP127
!---      EW wind at corners of 3-D cube                                          !GTRP128
               um(i,l,m) = tidex_M10(u0,a1,p1,a2,p2,time)                         !GTRP129
!---      Daily mean NS wind                                                      !GTRP130
               v0 = vza0(khgt,ilatw+i-1,ls+l-1,mdust+m-1)                         !GTRP131
               vday(i,l,m) = v0                                                   !GTRP132
!---      NS wind tide amplitudes and phases                                      !GTRP133
               a1 = vza1(khgt,ilatw+i-1,ls+l-1,mdust+m-1)*upolefac                !GTRP134
               p1 = vzp1(khgt,ilatw+i-1,ls+l-1,mdust+m-1)                         !GTRP135
               a2 = vza2(khgt,ilatw+i-1,ls+l-1,mdust+m-1)*upolefac                !GTRP136
               p2 = vzp2(khgt,ilatw+i-1,ls+l-1,mdust+m-1)                         !GTRP137
!---      NS wind at corners of 3-D cube                                          !GTRP138
               vm(i,l,m) = tidex_M10(v0,a1,p1,a2,p2,time)                         !GTRP139
            end do                                                                !GTRP140
         end do                                                                   !GTRP141
      end do                                                                      !GTRP142
!---    Use 3-D interpolation to get temperature, pressure, gas                   !GTRP143
!       constant, EW wind, and NS wind at given latitude, Ls, and                 !GTRP144
!       dust optical depth                                                        !GTRP145
      call threed_M10 (dlat, dls, ddust, tm, tmgcm, 0)                            !GTRP146
      call threed_M10 (dlat, dls, ddust, tday, tempday, 0)                        !GTRP147
      call threed_M10 (dlat, dls, ddust, tmax, tempmax, 0)                        !GTRP148
      call threed_M10 (dlat, dls, ddust, tmin, tempmin, 0)                        !GTRP149
      if (idaydata == 1) then                                                     !GTRP150
         call threed_M10 (dlat, dls, ddust, dmax, densmax, 1)                     !GTRP151
      else                                                                        !GTRP152
         call threed_M10 (dlat, dls, ddust, dmax, densmax, 0)                     !GTRP153
      endif                                                                       !GTRP154
      call threed_M10 (dlat, dls, ddust, dmin, densmin, 1)                        !GTRP155
      call threed_M10 (dlat, dls, ddust, pm, pmgcm, 1)                            !GTRP156
      call threed_M10 (dlat, dls, ddust, pday, presday, 1)                        !GTRP157
      call threed_M10 (dlat, dls, ddust, r0, rmgcm, 0)                            !GTRP158
      call threed_M10 (dlatw, dls, ddust, um, umgcm, 0)                           !GTRP159
      call threed_M10 (dlatw, dls, ddust, uday, uwndday, 0)                       !GTRP160
      call threed_M10 (dlatw, dls, ddust, vm, vmgcm, 0)                           !GTRP161
      call threed_M10 (dlatw, dls, ddust, vday, vwndday, 0)                       !GTRP162
!---    Compute density from temperature, pressure, and gas constant              !GTRP163
      dmgcm = pmgcm/(rmgcm*tmgcm)                                                 !GTRP164
      densday = presday/(rmgcm*tempday)                                           !GTRP165
      return                                                                      !GTRP166
      end subroutine mgcmterp_M10                                                 !GTRP167
!                                                                                 !GTRP168
!----------------------------------------------------------------------------     !IFLR  1
      integer function ifloor_M10 (x)                                             !IFLR  2
!-----------------------------------------------                                  !IFLR  3
!   M o d u l e s                                                                 !IFLR  4
!-----------------------------------------------                                  !IFLR  5
      USE vast_kind_param, ONLY:  double                                          !IFLR  6
!...                                                                              !IFLR  7
!...Switches:                                                                     !IFLR  8
      implicit none                                                               !IFLR  9
!-----------------------------------------------                                  !IFLR 10
!   D u m m y   A r g u m e n t s                                                 !IFLR 11
!-----------------------------------------------                                  !IFLR 12
      real(double) , intent(in) :: x                                              !IFLR 13
!-----------------------------------------------                                  !IFLR 14
!   L o c a l   V a r i a b l e s                                                 !IFLR 15
!-----------------------------------------------                                  !IFLR 16
      integer :: iflr                                                             !IFLR 17
!-----------------------------------------------                                  !IFLR 18
!---    Integer floor function, greatest integer <= x (provided for               !IFLR 19
!       compilers that do not have this intrinsic function)                       !IFLR 20
      iflr = idint(x)                                                             !IFLR 21
      if (iflr > x) iflr = iflr - 1                                               !IFLR 22
      ifloor_M10 = iflr                                                           !IFLR 23
      return                                                                      !IFLR 24
      end function ifloor_M10                                                     !IFLR 25
!                                                                                 !IFLR 26
!------------------------------------------------------------------------------   !MEPH  1
      subroutine marsephm_M10(xday, sunlat, sunlon, sunlsubs, radius, owlt,     & !MEPH  2
         eot)                                                                     !MEPH  3
!-----------------------------------------------                                  !MEPH  4
!   M o d u l e s                                                                 !MEPH  5
!-----------------------------------------------                                  !MEPH  6
      USE vast_kind_param, ONLY:  double                                          !MEPH  7
!---  Computes sunlat, sunlon= latitude and longitude of sub-solar                !MEPH  8
!     point on the surface, sunLsubs= areocentric longitude of the Sun            !MEPH  9
!     (Ls), radius= current orbital radius from Sun to Mars, heliolon=            !MEPH 10
!     Mars heliocentric longitude, owlt= Mars-Earth one-way light                 !MEPH 11
!     time (minutes), and EOT= equation of time (deg), calculated from            !MEPH 12
!     Julian day and time, xday.  Notes: input xday is NOT UTC, but               !MEPH 13
!     Terrestrial (Dynamical) Mars-Event Time (NOT Earth-Receive Time).           !MEPH 14
!     Mars Local Mean Solar Time (hrs ) = Local True Solar Time (hrs)             !MEPH 15
!     minus EOT (in hrs). Output is for Terrestrial (Dynamical)                   !MEPH 16
!     Mars Event Time (corresponding to input xday).                              !MEPH 17
!                                                                                 !MEPH 18
!     Equations for "moderately accurate" Mars solar time, seasonal               !MEPH 19
!     parameters, and one-way Mars-Earth light time, from Allison and             !MEPH 20
!     McEwen, Planet. Space Sci., 48, 215-235 (2000), and Allison                 !MEPH 21
!     Geophys Res. Lett., 24(16), 1967-1970 (1997).                               !MEPH 22
!                                                                                 !MEPH 23
!...                                                                              !MEPH 24
!...Switches:                                                                     !MEPH 25
!-----------------------------------------------                                  !MEPH 26
!   I n t e r f a c e   B l o c k s                                               !MEPH 27
!-----------------------------------------------                                  !MEPH 28
      use perturb_M10_I                                                           !MEPH 29
      use rescale_M10_I                                                           !MEPH 30
      use shiftdif_M10_I                                                          !MEPH 31
      implicit none                                                               !MEPH 32
!-----------------------------------------------                                  !MEPH 33
!   D u m m y   A r g u m e n t s                                                 !MEPH 34
!-----------------------------------------------                                  !MEPH 35
      real(double) , intent(in) :: xday                                           !MEPH 36
      real(double) , intent(out) :: sunlat                                        !MEPH 37
      real(double) , intent(out) :: sunlon                                        !MEPH 38
      real(double) , intent(out) :: sunlsubs                                      !MEPH 39
      real(double) , intent(out) :: radius                                        !MEPH 40
      real(double) , intent(out) :: owlt                                          !MEPH 41
      real(double) , intent(out) :: eot                                           !MEPH 42
!-----------------------------------------------                                  !MEPH 43
!   L o c a l   V a r i a b l e s                                                 !MEPH 44
!-----------------------------------------------                                  !MEPH 45
      real(double) :: pi180, dt, anomm, alphfms, alsrad, helilon, alphs, pmr,  &  !MEPH 46
         ge, re, dlat1, helone, vm, yranom, anom0, yrtrop, veqlon0, perlon0,   &  !MEPH 47
         ecc, obl, dlat2, dlat3, veqlon1, inc, anlon0, siday, rad0, ecc2, ecc3 &  !MEPH 48
         , ecc4, ecc5, ecc6, vm0, vmday0, pbs, xe, ye, xpl, ypl, zpl, eqcenter &  !MEPH 49
         , argper, trueanom, coslat                                               !MEPH 50
!-----------------------------------------------                                  !MEPH 51
      pi180 = datan(1.0D0)/45.0D0                                                 !MEPH 52
!---  Days since 2000 January 1.5                                                 !MEPH 53
      dt = xday - 2451545.0D0                                                     !MEPH 54
!                                                                                 !MEPH 55
!---------------------------------------------------------------------            !MEPH 56
!                                                                                 !MEPH 57
!---  Planetary orbit parameters                                                  !MEPH 58
!                                                                                 !MEPH 59
!     Semi-major axis (AU) = mean distance from Sun                               !MEPH 60
      rad0 = 1.52368D0                                                            !MEPH 61
!     Anomalistic year (days, perihelion-to-perihelion)                           !MEPH 62
      yranom = 686.9957D0                                                         !MEPH 63
!     Tropical year (days, for rate of fictitious mean sun)                       !MEPH 64
      yrtrop = 686.9726D0                                                         !MEPH 65
!     Mean anomaly for J2000 (degrees)                                            !MEPH 66
      anom0 = 19.387D0                                                            !MEPH 67
!     Heliocentric longitude of perihelion at J2000 (deg)                         !MEPH 68
      perlon0 = 336.0602D0                                                        !MEPH 69
!     Terms for heliocentric longitude at Ls=0 (deg)                              !MEPH 70
      veqlon0 = 85.061D0                                                          !MEPH 71
      veqlon1 = 5.5D-6                                                            !MEPH 72
!     Eccentricity and powers                                                     !MEPH 73
      ecc = 0.09340D0 + 2.477D-9*dt                                               !MEPH 74
      ecc2 = ecc**2                                                               !MEPH 75
      ecc3 = ecc2*ecc                                                             !MEPH 76
      ecc4 = ecc3*ecc                                                             !MEPH 77
      ecc5 = ecc4*ecc                                                             !MEPH 78
      ecc6 = ecc5*ecc                                                             !MEPH 79
!     Obliquity angle (radians)                                                   !MEPH 80
      obl = (25.1919D0 + 3.45D-7*dt)*pi180                                        !MEPH 81
!     Inclination (radians)                                                       !MEPH 82
      inc = (1.8497D0 - 2.23D-7*dt)*pi180                                         !MEPH 83
!     Longitude of ascending node at J2000 (deg)                                  !MEPH 84
      anlon0 = 49.5581D0                                                          !MEPH 85
!     Sidereal period of rotation (Earth days)                                    !MEPH 86
!---  Empirical correction in last digit, to agree with Horizons                  !MEPH 87
      siday = 1.025956749D0                                                       !MEPH 88
!     Heliocentric lon of prime meridian (deg) at Julian day Vmday0               !MEPH 89
      vm0 = 133.476D0                                                             !MEPH 90
!---  Empirical correction for agreement with Horizons data                       !MEPH 91
      vm0 = vm0 - 0.09746D0                                                       !MEPH 92
      vmday0 = 2451545.0D0                                                        !MEPH 93
!     Difference terms, planetocentric to planetographic lat (deg)                !MEPH 94
      dlat1 = 0.269D0                                                             !MEPH 95
      dlat2 = 0.003D0                                                             !MEPH 96
      dlat3 = 0.008D0                                                             !MEPH 97
!                                                                                 !MEPH 98
!---------------------------------------------------------------------            !MEPH 99
!                                                                                 !MEPH100
!---  Mean anomaly (radians)                                                      !MEPH101
!---  Allison & McEwen (2000) equation (16)                                       !MEPH102
      anomm = (anom0 + (360.0D0/yranom)*dt)*pi180                                 !MEPH103
!---  Right ascension of fictitious mean sun (deg)                                !MEPH104
!---  Allison & McEwen (2000) equation (17)                                       !MEPH105
      alphfms = perlon0 - veqlon0 + anom0 + (360.0D0/yrtrop)*dt                   !MEPH106
!---  Mars equation of center, A&M eqn. (4) (degrees)                             !MEPH107
      eqcenter = ((2.0D0*ecc - 0.25D0*ecc3 + (5.0D0/96.0D0)*ecc5)*dsin(anomm)   & !MEPH108
          + (1.25D0*ecc2 - (11.0D0/24.0D0)*ecc4 + (17.0D0/192.0D0)*ecc6)*dsin(  & !MEPH109
         2.0D0*anomm) + ((13.0D0/12.0D0)*ecc3 - (43.0D0/63.0D0)*ecc5)*dsin(     & !MEPH110
         3.0D0*anomm) + ((103.0D0/96.0D0)*ecc4 - (451.0D0/480.0D0)*ecc6)*dsin(  & !MEPH111
         4.0D0*anomm) + ((1097.0D0/960.0D0)*ecc5)*dsin(5.0D0*anomm) + ((        & !MEPH112
         12323.0D0/960.0D0)*ecc6)*dsin(6.0D0*anomm))/pi180                        !MEPH113
!---  True areocentric solar longitude (Ls), A&M eqns. (2) and (4)                !MEPH114
      sunlsubs = alphfms + eqcenter                                               !MEPH115
!---  Add perturbations due to Jupiter, Earth, and Venus, A&M eqns.               !MEPH116
!     (18) and (19)                                                               !MEPH117
      call perturb_M10 (dt, pbs)                                                  !MEPH118
      sunlsubs = sunlsubs + pbs                                                   !MEPH119
      call rescale_M10 (sunlsubs)                                                 !MEPH120
!---  Ls angle in radians                                                         !MEPH121
      alsrad = sunlsubs*pi180                                                     !MEPH122
!---  Sub-solar latitude of sun (planetographic solar declination),               !MEPH123
!     Allison (1997) eqn. (5) with empirical Ls and 3*Ls terms                    !MEPH124
      sunlat = dasin(dsin(obl)*dsin(alsrad))/pi180 + dlat1*dsin(alsrad) + dlat2 & !MEPH125
         *dcos(alsrad) + dlat3*dsin(3.0D0*alsrad)                                 !MEPH126
!---  Solar right ascension, un-numbered equation, A&M page 217                   !MEPH127
      alphs = datan(dcos(obl)*dtan(alsrad))/pi180                                 !MEPH128
!---  Put alphs into right quadrant                                               !MEPH129
      if (dabs(sunlsubs - alphs) > 270.0D0) then                                  !MEPH130
         alphs = alphs + 360.0D0                                                  !MEPH131
      else if (dabs(sunlsubs - alphs) > 90.0D0) then                              !MEPH132
         alphs = alphs + 180.0D0                                                  !MEPH133
      endif                                                                       !MEPH134
      call rescale_M10 (alphs)                                                    !MEPH135
!---  Mars orbital radius, Astronomical Almanac page E4                           !MEPH136
      radius = rad0*(1.0D0 - ecc2)/(1.0D0 + ecc*dcos(anomm + alsrad - alphfms*  & !MEPH137
         pi180))                                                                  !MEPH138
!---  Approximate Mars heliocentric longitude, A&M eqn, (11)                      !MEPH139
      helilon = sunlsubs + veqlon0 - veqlon1*dt - dtan(0.5D0*inc)**2*dsin(2.0D0 & !MEPH140
         *(alsrad + (veqlon0 - anlon0)*pi180))/pi180                              !MEPH141
      call rescale_M10 (helilon)                                                  !MEPH142
!---  Equation of time (deg)                                                      !MEPH143
      eot = alphfms - alphs                                                       !MEPH144
      call rescale_M10 (eot)                                                      !MEPH145
      call shiftdif_M10 (eot)                                                     !MEPH146
!---  Earth heliocentric distance and longitude, Allison eqns (20)-               !MEPH147
!     (22)                                                                        !MEPH148
      ge = (357.528D0 + 0.9856003D0*dt)*pi180                                     !MEPH149
      re = 1.00014D0 - 0.01671D0*dcos(ge) - 0.00014D0*dcos(2.0D0*ge)              !MEPH150
      helone = 100.472D0 + 0.9856474D0*dt + 1.915D0*dsin(ge) + 0.020D0*dsin(    & !MEPH151
         2.0D0*ge)                                                                !MEPH152
!---  Earth Cartesian coordinates                                                 !MEPH153
      xe = re*dcos(helone*pi180)                                                  !MEPH154
      ye = re*dsin(helone*pi180)                                                  !MEPH155
!---  Mars true anolmaly (radians)                                                !MEPH156
      trueanom = eqcenter*pi180 + anomm                                           !MEPH157
!---  Mars argument of perihelion (radians)                                       !MEPH158
      argper = (286.5016D0 + 2.92961D-5*dt)*pi180                                 !MEPH159
!---  Mars Cartesian coordinates                                                  !MEPH160
      zpl = radius*dsin(trueanom + argper)*dsin(inc)                              !MEPH161
      coslat = dsqrt(1.0D0 - (zpl/radius)**2)                                     !MEPH162
      xpl = radius*dcos((helilon + 3.82394D-5*dt)*pi180)*coslat                   !MEPH163
      ypl = radius*dsin((helilon + 3.82394D-5*dt)*pi180)*coslat                   !MEPH164
!---  One-way light time (minutes), Allison eqn.(19)                              !MEPH165
      owlt = dsqrt((xpl - xe)**2 + (ypl - ye)**2 + zpl**2)*499.005D0/60.0D0       !MEPH166
!---  Mars (Heliocentric) prime meridian, Allison eqn (11)                        !MEPH167
      vm = vm0 + (360.0D0/siday)*(xday - vmday0)                                  !MEPH168
!---  True solar time (Mars hours) at Mars prime meridian, A&M                    !MEPH169
!     page 217                                                                    !MEPH170
      pmr = (vm - alphs)/360.0D0                                                  !MEPH171
      sunlon = (pmr - dint(pmr))*360.0D0 - 180.0D0                                !MEPH172
      call rescale_M10 (sunlon)                                                   !MEPH173
      return                                                                      !MEPH174
      end subroutine marsephm_M10                                                 !MEPH175
!                                                                                 !MEPH176
!------------------------------------------------------------------------------   !MGCM  1
      subroutine marsgcm_M10(chgt, clat, clonw, als, dusttau, time, ctemp,      & !MGCM  2
         cpres, cdens, cuwin, cvwin, blwindew, blwindns, blwindvert, hpres,     & !MGCM  3
         hdens, zf, pertfact, ctopohgt, hgtasfc, careoid, tempday, presday,     & !MGCM  4
         densday, ewwnday, nswnday, bluday, blvday, tempmax, tempmin, densmax,  & !MGCM  5
         densmin, tgrnd, calbedo, icepolar, tat5m, dustoffset, requa, rpole,    & !MGCM  6
         idaydata)                                                                !MGCM  7
!-----------------------------------------------                                  !MGCM  8
!   M o d u l e s                                                                 !MGCM  9
!-----------------------------------------------                                  !MGCM 10
      USE vast_kind_param, ONLY:  double                                          !MGCM 11
      USE mgcmparm_M10_C                                                          !MGCM 12
      USE interp_M10_C                                                            !MGCM 13
      USE tgcmoffset_M10_C                                                        !MGCM 14
      USE therm_M10_C                                                             !MGCM 15
      USE parameters_M10_C                                                        !MGCM 16
!------------------------------------------------                                 !MGCM 17
!---    Uses interpolation routines to evaluate:                                  !MGCM 18
!                                                                                 !MGCM 19
!       ctemp    = temperature (K) at current position                            !MGCM 20
!       cpres    = pressure (N/m**2) at current position                          !MGCM 21
!       cdens    = density (kg/m**3) at current position                          !MGCM 22
!       cuwin    = eastward wind component (m/s) at current position              !MGCM 23
!       cvwin    = northward wind component (m/s) at current position             !MGCM 24
!       blwinew  = eastward b.l. slope wind (m/s)                                 !MGCM 25
!       blwinns  = northward b.l. slope wind (m/s)                                !MGCM 26
!       Hpres    = pressure scale height (km) at current position                 !MGCM 27
!       Hdens    = density scale height (km) at current position                  !MGCM 28
!       ZF       = height of 1.26 nbar level at current position                  !MGCM 29
!       pertfact = perturbation factor from random perturbation model             !MGCM 30
!       ctopohgt = topographic height (km) at current position                    !MGCM 31
!       careoid  = local radius (km) of MOLA 1/2 degree areoid                    !MGCM 32
!       TempDay  = Local daily average temperature (K)                            !MGCM 33
!       PresDay  = Local daily average pressure (N/m**2)                          !MGCM 34
!       DensDay  = Local daily average density (kg/m**3)                          !MGCM 35
!       EWwnDay  = Local daily average Eastward wind (m/s)                        !MGCM 36
!       NSwnDay  = Local daily average Northward wind (m/s)                       !MGCM 37
!       Tempmax  = Local daily maximum temperature (K)                            !MGCM 38
!       Tempmin  = Local daily minimum temperature (K)                            !MGCM 39
!       Densmax  = Local daily maximum density (kg/m**3)                          !MGCM 40
!       Densmin  = Local daily minimum density (kg/m**3)                          !MGCM 41
!       Tgrnd    = ground surface temperature (K)                                 !MGCM 42
!       calbedo  = surface albedo                                                 !MGCM 43
!       icepolar = polar ice indicator (0=no; 1=yes)                              !MGCM 44
!                                                                                 !MGCM 45
!       at the current height (chgt), latitude (clat), current (West)             !MGCM 46
!       longitude (clonw), for time of year given by Ls=als, and time             !MGCM 47
!       of day (time).  Interpolation is done using either boundary               !MGCM 48
!       layer or 0-80 km data from the Ames Mars General Circulation              !MGCM 49
!       model (MGCM) or from 80-170 km data from the University of                !MGCM 50
!       Michigan Mars Thermospheric General Circulation Model (MTGCM).            !MGCM 51
!                                                                                 !MGCM 52
!---    Set parameter values for number of MGCM heights (nhgt), number            !MGCM 53
!       of MTGCM heights (nhgtt), number of MGCM boundary layer levels            !MGCM 54
!       (nbl), number of MGCM latitudes (nlat), number of MTGCM lati-             !MGCM 55
!       tudes (nlatt), number of MGCM longitudes (nlon), number of dust           !MGCM 56
!       optical depths (ndust), and minimum perturbation magnitude at             !MGCM 57
!       surface (pert0)                                                           !MGCM 58
!...                                                                              !MGCM 59
!...Switches:                                                                     !MGCM 60
!-----------------------------------------------                                  !MGCM 61
!   I n t e r f a c e   B l o c k s                                               !MGCM 62
!-----------------------------------------------                                  !MGCM 63
      use ifloor_M10_I                                                            !MGCM 64
      use zlogr_M10_I                                                             !MGCM 65
      use rellips_M10_I                                                           !MGCM 66
      use twod_M10_I                                                              !MGCM 67
      use tgcmterp_M10_I                                                          !MGCM 68
      use mgcmterp_M10_I                                                          !MGCM 69
      use surfterp_M10_I                                                          !MGCM 70
      use subltchk_M10_I                                                          !MGCM 71
      use cp_M10_I                                                                !MGCM 72
      use bltp_M10_I                                                              !MGCM 73
      use slopewind_M10_I                                                         !MGCM 74
      implicit none                                                               !MGCM 75
!-----------------------------------------------                                  !MGCM 76
!   D u m m y   A r g u m e n t s                                                 !MGCM 77
!-----------------------------------------------                                  !MGCM 78
      integer , intent(out) :: icepolar                                           !MGCM 79
      integer , intent(in) :: idaydata                                            !MGCM 80
      real(double) , intent(inout)  :: chgt                                       !MGCM 81
      real(double) , intent(in)  :: clat                                          !MGCM 82
      real(double) , intent(in) :: clonw                                          !MGCM 83
      real(double) , intent(in) :: als                                            !MGCM 84
      real(double) , intent(in):: dusttau                                         !MGCM 85
      real(double) , intent(in) :: time                                           !MGCM 86
      real(double) , intent(out) :: ctemp                                         !MGCM 87
      real(double) , intent(out) :: cpres                                         !MGCM 88
      real(double) , intent(out) :: cdens                                         !MGCM 89
      real(double) , intent(out) :: cuwin                                         !MGCM 90
      real(double) , intent(out) :: cvwin                                         !MGCM 91
      real(double) , intent(out)  :: blwindew                                     !MGCM 92
      real(double) , intent(out)  :: blwindns                                     !MGCM 93
      real(double) , intent(out)  :: blwindvert                                   !MGCM 94
      real(double) , intent(out) :: hpres                                         !MGCM 95
      real(double) , intent(out) :: hdens                                         !MGCM 96
      real(double) , intent(out) :: zf                                            !MGCM 97
      real(double) , intent(out) :: pertfact                                      !MGCM 98
      real(double) , intent(inout) :: ctopohgt                                    !MGCM 99
      real(double) , intent(in) :: hgtasfc                                        !MGCM100
      real(double) , intent(inout)  :: careoid                                    !MGCM101
      real(double) , intent(out) :: tempday                                       !MGCM102
      real(double) , intent(out) :: presday                                       !MGCM103
      real(double) , intent(out) :: densday                                       !MGCM104
      real(double) , intent(out) :: ewwnday                                       !MGCM105
      real(double) , intent(out) :: nswnday                                       !MGCM106
      real(double) , intent(out) :: bluday                                        !MGCM107
      real(double) , intent(out) :: blvday                                        !MGCM108
      real(double) , intent(out)  :: tempmax                                      !MGCM109
      real(double) , intent(out)  :: tempmin                                      !MGCM110
      real(double) , intent(out) :: densmax                                       !MGCM111
      real(double) , intent(out) :: densmin                                       !MGCM112
      real(double) , intent(out) :: tgrnd                                         !MGCM113
      real(double) , intent(inout)  :: calbedo                                    !MGCM114
      real(double) , intent(out)  :: tat5m                                        !MGCM115
      real(double) , intent(in) :: dustoffset                                     !MGCM116
      real(double) , intent(in)   :: requa                                        !MGCM117
      real(double) , intent(in)   :: rpole                                        !MGCM118
!-----------------------------------------------                                  !MGCM119
!   L o c a l   P a r a m e t e r s                                               !MGCM120
!-----------------------------------------------                                  !MGCM121
      real(double), parameter :: pert0 = .02D0                                    !MGCM122
!-----------------------------------------------                                  !MGCM124
!   L o c a l   V a r i a b l e s                                                 !MGCM125
!-----------------------------------------------                                  !MGCM126
      integer :: khgt, khgtt, lhgtt, jbl, itime                                   !MGCM127
      real(double), dimension(2,2) :: offset                                      !MGCM128
      real(double) :: steplat, tsteplat, tlat1st, steplon, gz, oldrref ,        & !MGCM129
         globoffst, curoffset, tmgcm1, pmgcm1, dmgcm1, umgcm1,                  & !MGCM130
         vmgcm1, tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1  & !MGCM131
         , tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2, tday2, pday2, dday2, uday2,  & !MGCM132
         vday2, tmax2, tmin2, dmax2, dmin2, hdensc, hdensdayc, z1, z2, hpresday & !MGCM133
         , zf80, z1ofs, ofsmgcm, ofsz1, tmgcmx, pmgcmx, dmgcmx, umgcmx, vmgcmx  & !MGCM134
         , tdayx, pdayx, ddayx, udayx, vdayx, tmaxx, tminx, dmaxx, dminx,       & !MGCM135
         hden12, ofsmult1, hpres1, hdens1, z2x, dtdz, tbar, z2ofs, ofsmult2,    & !MGCM136
         ofsz2, rgas, rgasday, dhgt, r1, r2, rgas1, rgas2, z0, tcheck, tsubl,   & !MGCM137
         uhgt, factor, z5, zeval, rref, topohgt, albedo, cpoft, polefac,        & !MGCM138
         blu, blv, blw                                                            !MGCM139
!-----------------------------------------------                                  !MGCM140
      pertfact = 0.0D0                                                            !MGCM142
!---    Initialize ground surface temperature and polar ice indicator             !MGCM143
      tgrnd = 999.9D0                                                             !MGCM144
      icepolar = 99                                                               !MGCM145
!---    Insure latitude, longitude, Ls, and time of day within proper             !MGCM146
!       bounds                                                                    !MGCM147
      if (dabs(clat) > 90.0D0) stop ' Latitude error in MarsGCM_M10'              !MGCM148
      if (dabs(clonw) > 360.0D0) stop ' Longitude error: MarsGCM_M10'             !MGCM149
      if (als<0.0D0 .or. als>360.0D0) stop ' Ls error MarsGCM_M10'                !MGCM150
      if (time<0.0D0 .or. time>24.0D0) stop ' time error in MarsGCM_M10'          !MGCM151
!---    Latitude step size for MGCM and MTGCM data                                !MGCM152
      steplat = 180.0D0/(nlat - 1.0D0)                                            !MGCM153
      tsteplat = 180.0D0/nlatt                                                    !MGCM154
!---    Most southerly MTGCM latitude                                             !MGCM155
      tlat1st = (-90.0D0) + tsteplat/2.0D0                                        !MGCM156
!---    Longitude step size for MGCM boundary layer data                          !MGCM157
      steplon = 360.0D0/nlon                                                      !MGCM158
!---    MGCM height index (khgt) for current height (chgt)                        !MGCM159
      khgt = ifloor_M10(1.0D0 + chgt/5.0D0)                                       !MGCM160
!---    Insure khgt within proper limits                                          !MGCM161
      khgt = max0(1,khgt)                                                         !MGCM162
      khgt = min0(nhgt - 1,khgt)                                                  !MGCM163
!---    MGCM latitude index (ilat) from current latitude (clat)                   !MGCM164
      ilat = 1 + ifloor_M10((clat + 90.0D0)/steplat)                              !MGCM165
      ilat = min0(nlat - 1,ilat)                                                  !MGCM166
!---    MGCM wind latitude index (ilatw).  MGCM winds are offset in               !MGCM167
!       latitude by 1/2 latitude grid step.                                       !MGCM168
      ilatw = 2 + ifloor_M10((clat + 86.25D0)/steplat)                            !MGCM169
!---    Insure ilatw within proper bounds                                         !MGCM170
      ilatw = max0(2,ilatw)                                                       !MGCM171
      ilatw = min0(nlat - 1,ilatw)                                                !MGCM172
!---    MTGCM latitude index (ilatt) from current latitude (clat)                 !MGCM173
      ilatt = 1 + ifloor_M10((clat - tlat1st)/tsteplat)                           !MGCM174
!---    Insure ilatt within proper bounds                                         !MGCM175
      ilatt = max0(1,ilatt)                                                       !MGCM176
      ilatt = min0(nlatt - 1,ilatt)                                               !MGCM177
!---    MGCM boundary layer longitude index (jlon)                                !MGCM178
      jlon = ifloor_M10(clonw/steplon)                                            !MGCM179
      jlon = min0(nlon - 1,jlon)                                                  !MGCM180
!---    Time of year index (ls) from input Ls value (als)                         !MGCM181
      ls = ifloor_M10(als/30.0D0)                                                 !MGCM182
      ls = min0(11,ls)                                                            !MGCM183
!---    Dust index (mdust) for input dust optical depth (dusttau)                 !MGCM184
      if (dusttau < dust(2)) then                                                 !MGCM185
         mdust = 1                                                                !MGCM186
      else                                                                        !MGCM187
         mdust = 2                                                                !MGCM188
      endif                                                                       !MGCM189
!---    Increment of MGCM latitude (dlat) from grid point                         !MGCM190
      dlat = (clat - steplat*(ilat - 1.0D0) + 90.0D0)/steplat                     !MGCM191
!---    Increment of MTGCM latitude (dlatt) from grid point                       !MGCM192
      dlatt = (clat - tsteplat*(ilatt - 1.0D0) - tlat1st)/tsteplat                !MGCM193
!---    Insure dlatt within proper bounds near poles                              !MGCM194
      tpolefac = 1.0D0                                                            !MGCM195
      if (ilatt == 1) then                                                        !MGCM196
         tpolefac = 0.5D0                                                         !MGCM197
         if (dlatt <= 0.0D0) then                                                 !MGCM198
            dlatt = 0.0D0                                                         !MGCM199
            tpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !MGCM200
         endif                                                                    !MGCM201
      else if (ilatt >= nlat - 1) then                                            !MGCM202
         tpolefac = 0.5D0                                                         !MGCM203
         if (dlatt >= 1.0D0) then                                                 !MGCM204
            dlatt = 1.0D0                                                         !MGCM205
            tpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !MGCM206
         endif                                                                    !MGCM207
      endif                                                                       !MGCM208
!---    Increment of MGCM longitude (dlon) from grid point                        !MGCM209
      dlon = (clonw - steplon*jlon)/steplon                                       !MGCM210
!---    Increment of MGCM latitude from (offset) wind grid point                  !MGCM211
      dlatw = (clat - steplat*(ilatw - 2.0D0) + 86.25D0)/steplat                  !MGCM212
      wpolefac = 1.0D0                                                            !MGCM213
      if (ilatw == 2) then                                                        !MGCM214
         wpolefac = 0.75D0                                                        !MGCM215
         if (dlatw <= 0.0D0) then                                                 !MGCM216
            wpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !MGCM217
            dlatw = 0.0D0                                                         !MGCM218
         endif                                                                    !MGCM219
      else if (ilatw >= nlat - 1) then                                            !MGCM220
         wpolefac = 0.75D0                                                        !MGCM221
         if (dlatw >= 1.0D0) then                                                 !MGCM222
            wpolefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                        !MGCM223
            dlatw = 1.0D0                                                         !MGCM224
         endif                                                                    !MGCM225
      endif                                                                       !MGCM226
!---    Increment of solar activity (F10.7 at 1AU) for MTGCM data                 !MGCM227
      mf10 = 1                                                                    !MGCM228
      df10 = zlogr_M10(f107,f10val(mf10),'MGCM-01')/zlogr_M10(f10val(mf10+1),  &  !MGCM229
         f10val(mf10),'MGCM-02')                                                  !MGCM230
!---    Get areoid radius and topographic height at current lat, lon              !MGCM231
      call rellips_M10 (clat, clonw, careoid, chgt, gz, oldrref, ctopohgt,     &  !MGCM232
         calbedo, requa, rpole)                                                   !MGCM233
!---    Use topographic height if input height is <= -8.7 km                      !MGCM237
      if (chgt <= (-8.7D0)) chgt = ctopohgt + hgtasfc                             !MGCM238
!---    Find height index (k1st) of first 0-80 km MGCM level above                !MGCM239
!       surface topographic height                                                !MGCM240
      k1st = ifloor_M10(2.0D0 + (ctopohgt + 1.0D0)/5.0D0)                         !MGCM241
      k1st = max0(1,k1st)                                                         !MGCM242
!---    Find Ls increment (dls) from Ls "grid" on input data                      !MGCM243
      dls = (als - 30.0D0*ls)/30.0D0                                              !MGCM244
!---    Compute dust increment (ddust) from dust optical depth "grid"             !MGCM245
!       points                                                                    !MGCM246
      ddust = zlogr_M10(dusttau,dust(mdust),'MGCM-03')/zlogr_M10(dust(mdust+1), & !MGCM247
         dust(mdust),'MGCM-04')                                                   !MGCM248
!---    Insure ddust within proper range                                          !MGCM249
      if (ddust<0.0D0 .and. mdust==1) ddust = (dusttau - dust(1))/(dust(2)-dust & !MGCM250
         (1))                                                                     !MGCM251
      ddust = min(1.0D0,ddust)                                                    !MGCM252
!---    Initialize ZF = height of 1.26 nbar level (output value if                !MGCM253
!       current height < 80 km)                                                   !MGCM254
      zf = 999.0D0                                                                !MGCM255
!---    Assign MTGCM height offset from input zoffset or array offsets            !MGCM256
      globoffst = 0.0D0                                                           !MGCM257
      curoffset = 0.0D0                                                           !MGCM258
      if (ibougher == 2) then                                                     !MGCM259
         offset(1,1) = offsets(ls,mdust)                                          !MGCM260
         offset(1,2) = offsets(ls,mdust+1)                                        !MGCM261
         offset(2,1) = offsets(ls+1,mdust)                                        !MGCM262
         offset(2,2) = offsets(ls+1,mdust+1)                                      !MGCM263
         call twod_M10 (dls, ddust, offset, globoffst)                            !MGCM264
      else                                                                        !MGCM265
         call tgcmterp_M10 (1, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1, zf & !MGCM266
            , tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1,    & !MGCM267
            idaydata)                                                             !MGCM268
         call tgcmterp_M10 (2, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2, zf & !MGCM269
            , tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2, dmin2,    & !MGCM270
            idaydata)                                                             !MGCM271
         hdensc = 5.0D0/zlogr_M10(dmgcm1,dmgcm2,'MGCM-05')                        !MGCM272
         hdensdayc = 5.0D0/zlogr_M10(dday1,dday2,'MGCM-06')                       !MGCM273
         call mgcmterp_M10 (nhgt, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2  & !MGCM274
            , tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2, dmin2,    & !MGCM275
            idaydata)                                                             !MGCM276
         if (ibougher==3 .or. ibougher<2) then                                    !MGCM277
            curoffset = hdensdayc*zlogr_M10(dday2,dday1,'MGCM-07')                !MGCM278
         else                                                                     !MGCM279
            curoffset = hdensc*zlogr_M10(dmgcm2,dmgcm1,'MGCM-08')                 !MGCM280
         endif                                                                    !MGCM281
      endif                                                                       !MGCM282
      select case (ibougher)                                                      !MGCM283
      case (:0)                                                                   !MGCM284
         hgtoffset = zoffset                                                      !MGCM285
      case (1)                                                                    !MGCM286
         hgtoffset = zoffset + 1.0D0*dcos(2.0D0*datan(1.0D0)*als/45.0D0)          !MGCM287
      case (2)                                                                    !MGCM288
         hgtoffset = globoffst                                                    !MGCM289
      case default                                                                !MGCM290
         hgtoffset = curoffset                                                    !MGCM291
      end select                                                                  !MGCM292
!---    Add height offset due to dust storm                                       !MGCM293
      hgtoffset = hgtoffset + dustoffset                                          !MGCM294
!---    MTGCM height index (khgtt) for current height                             !MGCM295
      khgtt = ifloor_M10((chgt - hgtoffset - 75.0D0)/5.0D0)                       !MGCM296
!---    Insure khgtt within proper limits                                         !MGCM297
      khgtt = max0(1,khgtt)                                                       !MGCM298
      lhgtt = 1                                                                   !MGCM299
      if (khgtt==1 .and. hgtoffset<(-4.0D0)) then                                 !MGCM300
         khgtt = 2                                                                !MGCM301
         lhgtt = 2                                                                !MGCM302
      endif                                                                       !MGCM303
      khgtt = min0(nhgtt - 1,khgtt)                                               !MGCM304
!---    Initialize MGCM height offset to zero                                     !MGCM305
      ofszl = 0.0D0                                                               !MGCM306
!---    Use MTGCM interpolation if height >= 80 km                                !MGCM307
      if (chgt >= 80.0D0 + hgtoffset + 5.0D0*(lhgtt - 1.0D0)) then                !MGCM308
!---      Get temperature, pressure, density, and wind components at              !MGCM309
!         height indexes above and below current height                           !MGCM310
         call tgcmterp_M10 (khgtt, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1 & !MGCM311
            , zf, tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1 & !MGCM312
            , idaydata)                                                           !MGCM313
         call tgcmterp_M10 (khgtt + 1, time, tmgcm2, pmgcm2, dmgcm2, umgcm2,    & !MGCM314
            vmgcm2, zf, tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2  & !MGCM315
            , dmin2, idaydata)                                                    !MGCM316
!---      Height grid points above and below current height                       !MGCM317
         z1 = 75.0D0 + 5.0D0*khgtt + hgtoffset                                    !MGCM318
         z2 = 80.0D0 + 5.0D0*khgtt + hgtoffset                                    !MGCM319
!---      Apply MTGCM height offset to ZF altitude                                !MGCM320
         zf = zf + hgtoffset                                                      !MGCM321
!---      Pressure and density scale heights                                      !MGCM322
         hpres = (z2 - z1)/zlogr_M10(pmgcm1,pmgcm2,'MGCM-09')                     !MGCM323
         hpresday = (z2 - z1)/zlogr_M10(pday1,pday2,'MGCM-10')                    !MGCM324
         hdens = (z2 - z1)/zlogr_M10(dmgcm1,dmgcm2,'MGCM-11')                     !MGCM325
         ofszl = hgtoffset                                                        !MGCM326
!---    Use MGCM interpolation at 75 km and MTGCM interpolation at 80             !MGCM327
!       km if height between 75 and 80 km                                         !MGCM328
      else if (chgt >= 75.0D0) then                                               !MGCM329
!---      Get temperature, pressure, density, and wind components at              !MGCM330
!         heights above and below current height                                  !MGCM331
         call mgcmterp_M10 (khgt, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1  & !MGCM332
            , tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1,    & !MGCM333
            idaydata)                                                             !MGCM334
         call tgcmterp_M10 (lhgtt, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2 & !MGCM335
            , zf80, tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2,     & !MGCM336
            dmin2, idaydata)                                                      !MGCM337
         z1 = 75.0D0                                                              !MGCM338
         z2 = 80.0D0 + hgtoffset + 5.0D0*(lhgtt - 1.0D0)                          !MGCM339
!---      Apply 'equivalent' multiplier for offset between 75 & 80 km             !MGCM340
         if (ibougher<=1 .or. dustoffset>0.0D0) then                              !MGCM341
            z1ofs = 60.0D0                                                        !MGCM342
            ofsmgcm = hgtoffset - curoffset                                       !MGCM343
            if (ibougher > 1) ofsmgcm = dustoffset                                !MGCM344
            ofsz1 = ofsmgcm*(z1 - z1ofs)/(z2 - z1ofs)                             !MGCM345
            call mgcmterp_M10 (khgt + 1, time, tmgcmx, pmgcmx, dmgcmx, umgcmx,  & !MGCM346
               vmgcmx, tdayx, pdayx, ddayx, udayx, vdayx, tmaxx, tminx, dmaxx,  & !MGCM347
               dminx, idaydata)                                                   !MGCM348
            hden12 = 5.0D0/dlog(dmgcm1/dmgcmx)                                    !MGCM349
            ofsmult1 = dexp(ofsz1/hden12)                                         !MGCM350
!---        Local MGCM height offset                                              !MGCM351
            ofszl = ofsmgcm*(chgt - z1ofs)/(z2 - z1ofs)                           !MGCM352
            pmgcm1 = pmgcm1*ofsmult1                                              !MGCM353
            dmgcm1 = dmgcm1*ofsmult1                                              !MGCM354
            pday1 = pday1*ofsmult1                                                !MGCM355
            dday1 = dday1*ofsmult1                                                !MGCM356
            dmaxa = dmaxa*ofsmult1                                                !MGCM357
            dmin1 = dmin1*ofsmult1                                                !MGCM358
         endif                                                                    !MGCM359
!---      Pressure and density scale heights (km)                                 !MGCM360
         hpres = (z2 - z1)/zlogr_M10(pmgcm1,pmgcm2,'MGCM-12')                     !MGCM361
         hpresday = (z2 - z1)/zlogr_M10(pday1,pday2,'MGCM-13')                    !MGCM362
         hdens = (z2 - z1)/zlogr_M10(dmgcm1,dmgcm2,'MGCM-14')                     !MGCM363
!---    Use surfterp_M10 routine if height within boundary layer                  !MGCM364
      else if (chgt <= ctopohgt + dzbl(nbl)) then                                 !MGCM365
!---      Set index for surface layer data                                        !MGCM366
         jbl = 1                                                                  !MGCM367
         if (chgt >= ctopohgt + dzbl(2)) jbl = 2                                  !MGCM368
!---      Get temperature, pressure, density, and wind components at              !MGCM369
!         heights above and below current height                                  !MGCM370
         call surfterp_M10 (jbl + 1, time, tmgcm2, pmgcm2, dmgcm2, umgcm2,      & !MGCM371
            vmgcm2, hpres, hdens, ctopohgt, tday2, pday2, dday2, uday2, vday2,  & !MGCM372
            hpresday, tmax2, tmin2, dmax2, dmin2, tat5m, idaydata)                !MGCM373
         call surfterp_M10 (jbl, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1,  & !MGCM374
            hpres1, hdens1, ctopohgt, tday1, pday1, dday1, uday1, vday1,        & !MGCM375
            hpresday, tmax1, tmin1, dmaxa, dmin1, tat5m, idaydata)                !MGCM376
!---      Heights at two boundary layer levels                                    !MGCM377
         z1 = ctopohgt + dzbl(jbl)                                                !MGCM378
         z2 = ctopohgt + dzbl(jbl+1)                                              !MGCM379
!---      Get Temperature at 1st MGCM height above BL, for computing              !MGCM380
!         density scale height                                                    !MGCM381
         call mgcmterp_M10 (k1st, time, tmgcmx, pmgcmx, dmgcmx, umgcmx, vmgcmx  & !MGCM382
            , tdayx, pdayx, ddayx, udayx, vdayx, tmaxx, tminx, dmaxx, dminx,    & !MGCM383
            idaydata)                                                             !MGCM384
!---      Temperature gradient for density scale height calculation               !MGCM385
         z2x = 5.0D0*(k1st - 1.0D0)                                               !MGCM386
         dtdz = (tmgcmx - tmgcm1)/(z2x - z1)                                      !MGCM387
         if (chgt <= ctopohgt) dtdz = 0.0D0                                       !MGCM388
!---      Average layer temperature for density scale height                      !MGCM389
         tbar = (tmgcm1 + tmgcm2)/2.0D0                                           !MGCM390
!---      Density scale height from pressure scale height and                     !MGCM391
!         temperature gradient                                                    !MGCM392
         hdens = hpres/(1.0D0 + (hpres/tbar)*dtdz)                                !MGCM393
!---      Perturbation factor = surface value                                     !MGCM394
         pertfact = pert0                                                         !MGCM395
!---    Use MGCMterp_M10 routine if height above boundary layer levels            !MGCM396
!        and height <= 75 km                                                      !MGCM397
      else if (chgt >= 5.0D0*(k1st - 1.0D0)) then                                 !MGCM398
!---      Get temperature, pressure, density, and wind components at              !MGCM399
!         heights above and below current height                                  !MGCM400
         call mgcmterp_M10 (khgt, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1  & !MGCM401
            , tday1, pday1, dday1, uday1, vday1, tmax1, tmin1, dmaxa, dmin1,    & !MGCM402
            idaydata)                                                             !MGCM403
         call mgcmterp_M10 (khgt + 1, time, tmgcm2, pmgcm2, dmgcm2, umgcm2,     & !MGCM404
            vmgcm2, tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2,     & !MGCM405
            dmin2, idaydata)                                                      !MGCM406
!---      Heights at grid points above and below current level                    !MGCM407
         z1 = 5.0D0*(khgt - 1.0D0)                                                !MGCM408
         z2 = 5.0D0*khgt                                                          !MGCM409
!---      Apply 'equivalent' multiplier for offset below 75 km                    !MGCM410
         if (ibougher<=1 .or. dustoffset>0.0D0) then                              !MGCM411
            z1ofs = 60.0D0                                                        !MGCM412
            z2ofs = 80.0D0 + hgtoffset + 5.0D0*(lhgtt - 1.0D0)                    !MGCM413
            ofsmgcm = hgtoffset - curoffset                                       !MGCM414
            if (ibougher > 1) ofsmgcm = dustoffset                                !MGCM415
            if (z1 <= z1ofs) then                                                 !MGCM416
               ofsmult1 = 1.0D0                                                   !MGCM417
            else                                                                  !MGCM418
               hden12 = 5.0D0/dlog(dmgcm1/dmgcm2)                                 !MGCM419
               ofsz1 = ofsmgcm*(z1 - z1ofs)/(z2ofs - z1ofs)                       !MGCM420
               ofsmult1 = dexp(ofsz1/hden12)                                      !MGCM421
            endif                                                                 !MGCM422
            if (z2 <= z1ofs) then                                                 !MGCM423
               ofsmult2 = 1.0D0                                                   !MGCM424
            else                                                                  !MGCM425
               hden12 = 5.0D0/dlog(dmgcm1/dmgcm2)                                 !MGCM426
               ofsz2 = ofsmgcm*(z2 - z1ofs)/(z2ofs - z1ofs)                       !MGCM427
               ofsmult2 = dexp(ofsz2/hden12)                                      !MGCM428
            endif                                                                 !MGCM429
!---        Local MGCM height offset                                              !MGCM430
            if (chgt > z1ofs) ofszl = ofsmgcm*(chgt - z1ofs)/(z2ofs - z1ofs)      !MGCM431
            pmgcm1 = pmgcm1*ofsmult1                                              !MGCM432
            dmgcm1 = dmgcm1*ofsmult1                                              !MGCM433
            pmgcm2 = pmgcm2*ofsmult2                                              !MGCM434
            dmgcm2 = dmgcm2*ofsmult2                                              !MGCM435
            pday1 = pday1*ofsmult1                                                !MGCM436
            dday1 = dday1*ofsmult1                                                !MGCM437
            dmaxa = dmaxa*ofsmult1                                                !MGCM438
            dmin1 = dmin1*ofsmult1                                                !MGCM439
            pday2 = pday2*ofsmult2                                                !MGCM440
            dday2 = dday2*ofsmult2                                                !MGCM441
            dmax2 = dmax2*ofsmult2                                                !MGCM442
            dmin2 = dmin2*ofsmult2                                                !MGCM443
         endif                                                                    !MGCM444
!---      Pressure and density scale heights (km)                                 !MGCM445
         hpres = (z2 - z1)/zlogr_M10(pmgcm1,pmgcm2,'MGCM-15')                     !MGCM446
         hpresday = (z2 - z1)/zlogr_M10(pday1,pday2,'MGCM-16')                    !MGCM447
         hdens = (z2 - z1)/zlogr_M10(dmgcm1,dmgcm2,'MGCM-17')                     !MGCM448
!---    Use surfterp_M10 at top of boundary layer and MGCMterp_M10 at             !MGCM449
!       1st level above boundary layer if height between boundary                 !MGCM450
!       layer and height index k1st                                               !MGCM451
      else                                                                        !MGCM452
!---      Get temperature, pressure, density, and wind components at              !MGCM453
!         heights above and below current height                                  !MGCM454
         call surfterp_M10 (nbl, time, tmgcm1, pmgcm1, dmgcm1, umgcm1, vmgcm1,  & !MGCM455
            hpres, hdens, ctopohgt, tday1, pday1, dday1, uday1, vday1, hpresday & !MGCM456
            , tmax1, tmin1, dmaxa, dmin1, tat5m, idaydata)                        !MGCM457
         call mgcmterp_M10 (k1st, time, tmgcm2, pmgcm2, dmgcm2, umgcm2, vmgcm2  & !MGCM458
            , tday2, pday2, dday2, uday2, vday2, tmax2, tmin2, dmax2, dmin2,    & !MGCM459
            idaydata)                                                             !MGCM460
!---      Heights at grid points above and below current level                    !MGCM461
         z1 = ctopohgt + dzbl(nbl)                                                !MGCM462
         z2 = 5.0D0*(k1st - 1.0D0)                                                !MGCM463
!---      Temperature gradient and mean temperature for density scale             !MGCM464
!         height calculation                                                      !MGCM465
         dtdz = (tmgcm2 - tmgcm1)/(z2 - z1)                                       !MGCM466
         tbar = (tmgcm1 + tmgcm2)/2.0D0                                           !MGCM467
!---      Density scale height from pressure scale height and                     !MGCM468
!         temperature gradient                                                    !MGCM469
         hdens = hpres/(1.0D0 + (hpres/tbar)*dtdz)                                !MGCM470
      endif                                                                       !MGCM471
!---    Get gas constant from pressure, density, and temperature                  !MGCM472
      if (chgt <= ctopohgt) then                                                  !MGCM473
         rgas = pmgcm1/(dmgcm1*tmgcm1)                                            !MGCM474
         rgasday = pday1/(dday1*tday1)                                            !MGCM475
         dhgt = (ctopohgt - z1)/(z2 - z1)                                         !MGCM476
      else                                                                        !MGCM477
         dhgt = (chgt - z1)/(z2 - z1)                                             !MGCM478
         r1 = pmgcm1/(dmgcm1*tmgcm1)                                              !MGCM479
         r2 = pmgcm2/(dmgcm2*tmgcm2)                                              !MGCM480
         rgas1 = pday1/(dday1*tday1)                                              !MGCM481
         rgas2 = pday2/(dday2*tday2)                                              !MGCM482
         rgas = r1 + dhgt*(r2 - r1)                                               !MGCM483
         rgasday = rgas1 + dhgt*(rgas2 - rgas1)                                   !MGCM484
      endif                                                                       !MGCM485
!---    Use logarithmic wind and temperature profiles (with surface               !MGCM486
!       roughness z0) if height below lowest boundary layer level                 !MGCM487
      if (chgt < ctopohgt + dzbl(2)) then                                         !MGCM488
!---      Convert surface roughness to km                                         !MGCM489
         z0 = zwsfc/1000.0D0                                                      !MGCM490
!---      Save ground surface temperature for output                              !MGCM491
         tgrnd = tmgcm1                                                           !MGCM492
!---      Consistent with Ames MGCM, use z0 = 0.01 cm (1.0e-7 km) if              !MGCM493
!         over ice (T <= CO2 sublimation temperature + 5K)                        !MGCM494
         tcheck = tmgcm1                                                          !MGCM495
         call subltchk_M10 (tcheck, pmgcm2, tsubl)                                !MGCM496
!---      If surface temperature near sublimation point, set polar ice            !MGCM497
!           indicator on (= 1) and re-set surface roughness                       !MGCM498
         icepolar = 0                                                             !MGCM499
         if (tmgcm1 <= tsubl + 5.0D0) then                                        !MGCM500
            z0 = 1.0D-7                                                           !MGCM501
            icepolar = 1                                                          !MGCM502
         endif                                                                    !MGCM503
         uhgt = chgt - ctopohgt                                                   !MGCM504
         uhgt = dmax1(z0,uhgt)                                                    !MGCM505
!---      Compute logarithmic boundary layer shape factor for surface             !MGCM506
!         to lowest boundary layer level                                          !MGCM507
         factor = zlogr_M10(uhgt,z0,'MGCM-18')/zlogr_M10(dzbl(2),z0,'MGCM-19')    !MGCM508
!---      Apply wind factor; assume no-slip wind condition at surface             !MGCM509
         cuwin = umgcm2*factor                                                    !MGCM510
         cvwin = vmgcm2*factor                                                    !MGCM511
         ewwnday = uday2*factor                                                   !MGCM512
         nswnday = vday2*factor                                                   !MGCM513
!---      Set up parameters to evaluate temperature boundary layer                !MGCM514
!         Convert heights to meters for input to bltp_M10 subroutine              !MGCM515
         z5 = dzbl(2)*1000.0D0                                                    !MGCM516
         zeval = uhgt*1000.0D0                                                    !MGCM517
!---      Get value of local gravity                                              !MGCM518
         call rellips_M10 (clat, clonw, rref, chgt, gz, oldrref, topohgt,       & !MGCM519
            albedo, requa, rpole)                                                 !MGCM520
!---      Use Ames MGCM boundary layer model for current temperature              !MGCM521
!         Get specific heat at constant pressure                                  !MGCM522
         cpoft = cp_M10(tmgcm2)                                                   !MGCM523
         call bltp_M10 (gz, cpoft, tmgcm1, z5, tmgcm2, umgcm2, vmgcm2, zeval,   & !MGCM524
            factor, ctemp)                                                        !MGCM525
!---      Use Ames MGCM boundary layer model for daily avg temperature            !MGCM526
         cpoft = cp_M10(tday2)                                                    !MGCM527
         call bltp_M10 (gz, cpoft, tday1, z5, tday2, uday2, vday2, zeval,       & !MGCM528
            factor, tempday)                                                      !MGCM529
!---      Use Ames MGCM boundary layer model for daily max temperature            !MGCM530
         cpoft = cp_M10(tmax2)                                                    !MGCM531
         call bltp_M10 (gz, cpoft, tmax1, z5, tmax2, uday2, vday2, zeval,       & !MGCM532
            factor, tempmax)                                                      !MGCM533
!---      Use Ames MGCM boundary layer model for daily min temperature            !MGCM534
         cpoft = cp_M10(tmin2)                                                    !MGCM535
         call bltp_M10 (gz, cpoft, tmin1, z5, tmin2, uday2, vday2, zeval,       & !MGCM536
            factor, tempmin)                                                      !MGCM537
!---      Pressure at current position from pressure scale height                 !MGCM538
         cpres = pmgcm2*dexp((z2 - chgt)/hpres)                                   !MGCM539
         presday = pday2*dexp((z2 - chgt)/hpresday)                               !MGCM540
!---      Density at current position from gas law                                !MGCM541
         cdens = cpres/(rgas*ctemp)                                               !MGCM542
         densday = presday/(rgasday*tempday)                                      !MGCM543
         densmin = 9999.0D0                                                       !MGCM544
         densmax = -9999.0D0                                                      !MGCM545
         if (idaydata > 0) then                                                   !MGCM546
!---      Daily maximum and minimum density                                       !MGCM547
            densmin = densday*(dmin1/dday1 + factor*(dmin2/dday2 - dmin1/dday1) & !MGCM548
               )                                                                  !MGCM549
            densmax = densday*(dmaxa/dday1 + factor*(dmax2/dday2 - dmaxa/dday1) & !MGCM550
               )                                                                  !MGCM551
         endif                                                                    !MGCM552
!---    Use linear height interpolation if above logarithmic                      !MGCM553
!       surface layer                                                             !MGCM554
      else                                                                        !MGCM555
         dhgt = (chgt - z1)/(z2 - z1)                                             !MGCM556
         cuwin = umgcm1 + dhgt*(umgcm2 - umgcm1)                                  !MGCM557
         cvwin = vmgcm1 + dhgt*(vmgcm2 - vmgcm1)                                  !MGCM558
         ewwnday = uday1 + dhgt*(uday2 - uday1)                                   !MGCM559
         nswnday = vday1 + dhgt*(vday2 - vday1)                                   !MGCM560
!---      Interpolate temperature to current height                               !MGCM561
         ctemp = tmgcm1 + dhgt*(tmgcm2 - tmgcm1)                                  !MGCM562
         tempday = tday1 + dhgt*(tday2 - tday1)                                   !MGCM563
         tempmax = tmax1 + dhgt*(tmax2 - tmax1)                                   !MGCM564
         tempmin = tmin1 + dhgt*(tmin2 - tmin1)                                   !MGCM565
!---      Pressure at current position from pressure scale height                 !MGCM566
         cpres = pmgcm2*dexp((z2 - chgt)/hpres)                                   !MGCM567
         presday = pday2*dexp((z2 - chgt)/hpresday)                               !MGCM568
!---      Density at current position from gas law                                !MGCM569
         cdens = cpres/(rgas*ctemp)                                               !MGCM570
         densday = presday/(rgasday*tempday)                                      !MGCM571
         densmin = 9999.0D0                                                       !MGCM572
         densmax = -9999.0D0                                                      !MGCM573
         if (idaydata > 0) then                                                   !MGCM574
!---      Daily maximum and minimum density                                       !MGCM575
            densmin = densday*(dmin1/dday1 + dhgt*(dmin2/dday2 - dmin1/dday1))    !MGCM576
            densmax = densday*(dmaxa/dday1 + dhgt*(dmax2/dday2 - dmaxa/dday1))    !MGCM577
         endif                                                                    !MGCM578
      endif                                                                       !MGCM579
      if (chgt < ctopohgt + 0.5D0) then                                           !MGCM580
         if (dabs(clat) >= 85.0D0) then                                           !MGCM581
            polefac = 1.0D0 - (dabs(clat) - 85.0D0)/5.0D0                         !MGCM582
            cpres = polefac*cpres + (1.0D0 - polefac)*presday                     !MGCM583
            cdens = polefac*cdens + (1.0D0 - polefac)*densday                     !MGCM584
            densmin = 9999.0D0                                                    !MGCM585
            densmax = -9999.0D0                                                   !MGCM586
            if (idaydata > 0) then                                                !MGCM587
               densmax = polefac*densmax + (1.0D0 - polefac)*densday              !MGCM588
               densmin = polefac*densmin + (1.0D0 - polefac)*densday              !MGCM589
            endif                                                                 !MGCM590
         endif                                                                    !MGCM591
      endif                                                                       !MGCM592
!---    Set specific bogus values of pressure or density scale heights            !MGCM593
!       are out of range                                                          !MGCM594
      hpres = dmax1(-9.99D0,hpres)                                                !MGCM595
      hpres = min(99.99D0,hpres)                                                  !MGCM596
      hdens = dmax1(-9.99D0,hdens)                                                !MGCM597
      hdens = min(99.99D0,hdens)                                                  !MGCM598
!---    Compute perturbation factor, unless it has already been set               !MGCM614
      if (pertfact < pert0) then                                                  !MGCM615
!---      Perturbation factor from new, simplified model                          !MGCM616
        If (chgt < 100.0d0)Then                                                   !MGCM616a
          pertfact = 0.30D0*dexp((chgt - 100.0D0)/35.0D0)                         !MGCM617
          If (pertfact < pert0)pertfact = pert0                                   !MGCM618
        Else                                                                      !MGCM619
          pertfact = 0.30d0 + 0.5d-2*(chgt-100.0d0)                               !MGCM620
          If (pertfact > 0.45d0)pertfact = 0.45d0                                 !MGCM620a
        Endif                                                                     !MGCM620b
      endif                                                                       !MGCM621
!---    Get slope winds (0 below surface and > 4.5 km above surface)              !MGCM622
      call slopewind_M10 (clat, clonw, chgt, time, cuwin, cvwin, blwindew,      & !MGCM623
         blwindns, blwindvert)                                                    !MGCM624
!---    Compute daily average slope winds                                         !MGCM625
      bluday = 0.0D0                                                              !MGCM626
      blvday = 0.0D0                                                              !MGCM627
      if (idaydata > 0) then                                                      !MGCM628
         do itime = 0, 22, 2                                                      !MGCM629
            call slopewind_M10 (clat, clonw, chgt, dble(itime), cuwin, cvwin,   & !MGCM630
               blu, blv, blw)                                                     !MGCM631
            bluday = bluday + blu                                                 !MGCM632
            blvday = blvday + blv                                                 !MGCM633
         end do                                                                   !MGCM634
         bluday = bluday/12.0D0                                                   !MGCM635
         blvday = blvday/12.0D0                                                   !MGCM636
      endif                                                                       !MGCM637
      return                                                                      !MGCM638
      end subroutine marsgcm_M10                                                  !MGCM639
!                                                                                 !MGCM640
!------------------------------------------------------------------------------   !PTRB  1
      subroutine perturb_M10(dt, pbs)                                             !PTRB  2
!-----------------------------------------------                                  !PTRB  3
!   M o d u l e s                                                                 !PTRB  4
!-----------------------------------------------                                  !PTRB  5
      USE vast_kind_param, ONLY:  double                                          !PTRB  6
!---  Mars Ls perturbations from Jupiter, Earth, and Venus, Table 5               !PTRB  7
!     and eqn (18) of Allison and McEwen, Planet. Space Sci., 48, 215-            !PTRB  8
!     235 (2000). dt is time (days) after J2000 Terrestrial Time.                 !PTRB  9
!...                                                                              !PTRB 10
!...Switches:                                                                     !PTRB 11
      implicit none                                                               !PTRB 12
!-----------------------------------------------                                  !PTRB 13
!   D u m m y   A r g u m e n t s                                                 !PTRB 14
!-----------------------------------------------                                  !PTRB 15
      real(double) , intent(in) :: dt                                             !PTRB 16
      real(double) , intent(out) :: pbs                                           !PTRB 17
!-----------------------------------------------                                  !PTRB 18
!   L o c a l   V a r i a b l e s                                                 !PTRB 19
!-----------------------------------------------                                  !PTRB 20
      integer :: i                                                                !PTRB 21
      real(double), dimension(7) :: a, tau, phi                                   !PTRB 22
      real(double) :: pi180, per                                                  !PTRB 23
!-----------------------------------------------                                  !PTRB 24
!     Table 5 Data: amplitudes (deg), periods (J yrs), phases (deg)               !PTRB 25
      data a/ 0.007D0, 0.006D0, 0.004D0, 0.004D0, 0.002D0, 0.002D0, 0.002D0/      !PTRB 26
      data tau/ 2.2353D0, 2.7543D0, 1.1177D0, 15.7866D0, 2.1354D0, 2.4694D0,   &  !PTRB 27
         32.8493D0/                                                               !PTRB 28
      data phi/ 49.909D0, 168.173D0, 191.837D0, 21.736D0, 15.704D0, 95.528D0,  &  !PTRB 29
         49.095D0/                                                                !PTRB 30
      pi180 = datan(1.0D0)/45.0D0                                                 !PTRB 31
      per = (360.0D0/365.25D0)*pi180                                              !PTRB 32
      pbs = 0.0D0                                                                 !PTRB 33
      do i = 1, 7                                                                 !PTRB 34
         pbs = pbs + a(i)*dcos(per*dt/tau(i)+phi(i)*pi180)                        !PTRB 35
      end do                                                                      !PTRB 36
      return                                                                      !PTRB 37
      end subroutine perturb_M10                                                  !PTRB 38
!                                                                                 !PTRB 39
!-----------------------------------------------------------------------------    !PPND  1
      REAL(KIND(0.0D0)) FUNCTION PPND_M10 (P, IFAULT)                             !PPND  2
!-----------------------------------------------                                  !PPND  3
!   M o d u l e s                                                                 !PPND  4
!-----------------------------------------------                                  !PPND  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !PPND  6
!                                                                                 !PPND  7
!     Algorithm AS 111 Appl. Statist. (1977) Vol. 26, p. 118                      !PPND  8
!                                                                                 !PPND  9
!     Produces normal deviate corresponding to lower tail area of p.              !PPND 10
!     Returns ifault = 1 in input p >= 1 or <= 0, ifault = 0                      !PPND 11
!     otherwise.  If ifault = 1, PPND_M10 value is set to 0.                      !PPND 12
!     Single precision version with error epsilon = 2 ** (-31).                   !PPND 13
!     For double precision version, change REAL to DOUBLE PRECISION               !PPND 14
!     in the FUNCTION statement and the declaration of variables;                 !PPND 15
!     change E0 to D0 in the DATA statements and change ABS, ALOG                 !PPND 16
!     and SQRT to DABS, DLOG and DSQRT in the assignment statements.              !PPND 17
!     The hash sums are the sums of the moduli of the coefficients.               !PPND 18
!     They have no inherent meanings, but are included for use in                 !PPND 19
!     checking transpositions.                                                    !PPND 20
!                                                                                 !PPND 21
!...Switches:                                                                     !PPND 23
      IMPLICIT NONE                                                               !PPND 24
!-----------------------------------------------                                  !PPND 25
!   D u m m y   A r g u m e n t s                                                 !PPND 26
!-----------------------------------------------                                  !PPND 27
      INTEGER , INTENT(OUT) :: IFAULT                                             !PPND 28
      REAL(DOUBLE) , INTENT(IN) :: P                                              !PPND 29
!-----------------------------------------------                                  !PPND 30
!   L o c a l   V a r i a b l e s                                                 !PPND 31
!-----------------------------------------------                                  !PPND 32
      REAL(DOUBLE) :: ZERO, HALF, ONE, SPLIT, A0, A1, A2, A3, B1, B2, B3, B4,   & !PPND 33
         C0, C1, C2, C3, D1, D2, Q, R                                             !PPND 34
!-----------------------------------------------                                  !PPND 35
!                                                                                 !PPND 36
      DATA ZERO, HALF, ONE, SPLIT/ 0.0D0, 0.5D0, 1.0D0, 0.42D0/                   !PPND 37
!                                                                                 !PPND 38
      DATA A0/ 2.50662823884D0/                                                   !PPND 39
      DATA A1/  - 18.61500062529D0/                                               !PPND 40
      DATA A2/ 41.39119773534D0/                                                  !PPND 41
      DATA A3/  - 25.44106049637D0/                                               !PPND 42
      DATA B1/  - 8.47351093090D0/                                                !PPND 43
      DATA B2/ 23.08336743743D0/                                                  !PPND 44
      DATA B3/  - 21.06224101826D0/                                               !PPND 45
      DATA B4/ 3.13082909833D0/                                                   !PPND 46
!                                                                                 !PPND 47
!     Hash sum for a & b = 143.70383558076                                        !PPND 48
!                                                                                 !PPND 49
      DATA C0/  - 2.78718931138D0/                                                !PPND 50
      DATA C1/  - 2.29796479134D0/                                                !PPND 51
      DATA C2/ 4.85014127135D0/                                                   !PPND 52
      DATA C3/ 2.32121276858D0/                                                   !PPND 53
      DATA D1/ 3.54388924762D0/                                                   !PPND 54
      DATA D2/ 1.63706781897D0/                                                   !PPND 55
!                                                                                 !PPND 56
!     Hash sum for c & d = 17.43746520924                                         !PPND 57
!                                                                                 !PPND 58
!                                                                                 !PPND 59
      IFAULT = 0                                                                  !PPND 60
      Q = P - HALF                                                                !PPND 61
      IF (DABS(Q) <= SPLIT) THEN                                                  !PPND 62
         R = Q*Q                                                                  !PPND 63
         PPND_M10 = Q*(((A3*R + A2)*R + A1)*R + A0)/((((B4*R + B3)*R + B2)*R +  & !PPND 64
            B1)*R + ONE)                                                          !PPND 65
         RETURN                                                                   !PPND 66
      ENDIF                                                                       !PPND 67
      R = P                                                                       !PPND 68
      IF (Q > ZERO) R = ONE - P                                                   !PPND 69
      IF (R >= ZERO) THEN                                                         !PPND 70
         R = DSQRT((-DLOG(R)))                                                    !PPND 71
         PPND_M10 = (((C3*R + C2)*R + C1)*R + C0)/((D2*R + D1)*R + ONE)           !PPND 72
         IF (Q < ZERO) PPND_M10 = -PPND_M10                                       !PPND 73
         RETURN                                                                   !PPND 74
      ENDIF                                                                       !PPND 75
      IFAULT = 1                                                                  !PPND 76
      PPND_M10 = ZERO                                                             !PPND 77
      RETURN                                                                      !PPND 78
      END FUNCTION PPND_M10                                                       !PPND 79
!                                                                                 !PPND 80
!-------------------------------------------------------------------------------  !PRSE  1
      subroutine prseas_M10(lsun, lat, pr)                                        !PRSE  2
!-----------------------------------------------                                  !PRSE  3
!   M o d u l e s                                                                 !PRSE  4
!-----------------------------------------------                                  !PRSE  5
      USE vast_kind_param, ONLY:  double                                          !PRSE  6
!...                                                                              !PRSE  7
!...Switches:                                                                     !PRSE  8
      implicit none                                                               !PRSE  9
!-----------------------------------------------                                  !PRSE 10
!   D u m m y   A r g u m e n t s                                                 !PRSE 11
!-----------------------------------------------                                  !PRSE 12
      real(double) , intent(in) :: lsun                                           !PRSE 13
      real(double) , intent(in) :: lat                                            !PRSE 14
      real(double) , intent(out) :: pr                                            !PRSE 15
!-----------------------------------------------                                  !PRSE 16
!   L o c a l   V a r i a b l e s                                                 !PRSE 17
!-----------------------------------------------                                  !PRSE 18
      real(double) :: lat2, a11, a12, a21, a22, phi11, phi12, phi21, phi22,     & !PRSE 19
         pi180, a1, a2, phi1, phi2                                                !PRSE 20
!-----------------------------------------------                                  !PRSE 21
!---  Relative seasonal pressure variation on reference ellipsoid                 !PRSE 22
      data a11, a12/ 0.0847194D0,  - 0.570405D-5/                                 !PRSE 23
      data a21, a22/ 0.0690599D0,  - 0.132689D-5/                                 !PRSE 24
      data phi11, phi12/ 304.041D0, 0.0080602D0/                                  !PRSE 25
      data phi21, phi22/ 61.362D0, 0.0016533D0/                                   !PRSE 26
      pi180 = datan(1.0D0)/45.0D0                                                 !PRSE 27
      lat2 = lat**2                                                               !PRSE 28
!---  a1, a2 = amplitudes of cos(Ls) and cos(2*Ls) terms                          !PRSE 29
      a1 = a11 + a12*lat2                                                         !PRSE 30
      a2 = a21 + a22*lat2                                                         !PRSE 31
!---  phi1, phi2 = phases of cos(Ls) and cos(2*Ls) terms                          !PRSE 32
      phi1 = phi11 + phi12*lat2                                                   !PRSE 33
      phi2 = phi21 + phi22*lat2                                                   !PRSE 34
!---  Relative variation in pressure on reference ellipsoid, due to               !PRSE 35
!     latitude and time (Ls) variations                                           !PRSE 36
      pr = 1.0D0 + a1*dcos(pi180*(lsun - phi1)) + a2*dcos(2.0D0*pi180*(lsun -   & !PRSE 37
         phi2))                                                                   !PRSE 38
      return                                                                      !PRSE 39
      end subroutine prseas_M10                                                   !PRSE 40
!                                                                                 !PRSE 41
!-----------------------------------------------------------------------------    !QRHT  1
      real(kind(0.0d0)) function qrhtp_M10 (rh, t, p)                             !QRHT  2
!-----------------------------------------------                                  !QRHT  3
!   M o d u l e s                                                                 !QRHT  4
!-----------------------------------------------                                  !QRHT  5
      USE vast_kind_param, ONLY:  double                                          !QRHT  6
!     Specific humidity q (g/kg) versus RH (0-1), temperature (K), and            !QRHT  7
!     pressure (mb) (Savijarvi, Contr. Atmos. Phys., 64, 103, 1991)               !QRHT  8
!                                                                                 !QRHT  9
!     Saturation water vapor pressure es vs temperature T                         !QRHT 10
!...                                                                              !QRHT 11
!...Switches:                                                                     !QRHT 12
      implicit none                                                               !QRHT 13
!-----------------------------------------------                                  !QRHT 14
!   D u m m y   A r g u m e n t s                                                 !QRHT 15
!-----------------------------------------------                                  !QRHT 16
      real(double) , intent(in) :: rh                                             !QRHT 17
      real(double) , intent(in) :: t                                              !QRHT 18
      real(double) , intent(in) :: p                                              !QRHT 19
!-----------------------------------------------                                  !QRHT 20
!   L o c a l   V a r i a b l e s                                                 !QRHT 21
!-----------------------------------------------                                  !QRHT 22
      real(double) :: es, esmax                                                   !QRHT 23
!-----------------------------------------------                                  !QRHT 24
      es = 6.1135D0*dexp(22.542D0*(t - 273.16D0)/(t + 0.32D0))                    !QRHT 25
      esmax = p/1.59D0                                                            !QRHT 26
      es = min(esmax,es)                                                          !QRHT 27
      qrhtp_M10 = 1000.0D0*rh*0.407D0*es/(p - 0.59D0*es)                          !QRHT 28
      return                                                                      !QRHT 29
      end function qrhtp_M10                                                      !QRHT 30
!                                                                                 !QRHT 31
!----------------------------------------------------------------------------     !RAND  1
      REAL(KIND(0.0D0)) FUNCTION RANDOM_M10 (L)                                   !RAND  2
!-----------------------------------------------                                  !RAND  3
!   M o d u l e s                                                                 !RAND  4
!-----------------------------------------------                                  !RAND  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !RAND  6
      USE RANDCOM_M10_C                                                           !RAND  7
!...                                                                              !RAND  8
!...Switches:                                                                     !RAND  9
      IMPLICIT NONE                                                               !RAND 10
!-----------------------------------------------                                  !RAND 11
!   D u m m y   A r g u m e n t s                                                 !RAND 12
!-----------------------------------------------                                  !RAND 13
      INTEGER , INTENT(OUT) :: L                                                  !RAND 14
!-----------------------------------------------                                  !RAND 15
!   L o c a l   V a r i a b l e s                                                 !RAND 16
!-----------------------------------------------                                  !RAND 17
      REAL(DOUBLE) :: ONE, ZERO                                                   !RAND 18
!-----------------------------------------------                                  !RAND 19
!                                                                                 !RAND 20
!     Algorithm AS 183 Appl. Statist. (1982) Vol. 31, p.188                       !RAND 21
!                                                                                 !RAND 22
!     Returns a pseudo-random number rectangularly distributed                    !RAND 23
!     between 0 and 1.                                                            !RAND 24
!                                                                                 !RAND 25
!     IX, IY and IZ should be set to integer values between                       !RAND 26
!     1 and 30,000 before first entry.                                            !RAND 27
!                                                                                 !RAND 28
!     Integer arithmetic up to 30323 is required.                                 !RAND 29
!                                                                                 !RAND 30
!     Returns L = 0 unless random = 0 or random = 1, in which                     !RAND 31
!     case L = 1                                                                  !RAND 32
!                                                                                 !RAND 33
      DATA ONE, ZERO/ 1.0D0, 0.0D0/                                               !RAND 34
!     IX = 171 * Mod(IX, 177) -  2 * (IX / 177)                                   !RAND 35
!     IY = 172 * Mod(IY, 176) - 35 * (IY / 176)                                   !RAND 36
!     IZ = 170 * Mod(IZ, 178) - 63 * (IZ / 178)                                   !RAND 37
!                                                                                 !RAND 38
!     If (IX  <  0) IX = IX + 30269                                               !RAND 39
!     If (IY  <  0) IY = IY + 30307                                               !RAND 40
!     If (IZ  <  0) IZ = IZ + 30323                                               !RAND 41
!                                                                                 !RAND 42
!     If integer arithmetic up to 5,212,632 is not available,                     !RAND 43
!     the preceding 6 statements may be used instead of the following 3           !RAND 44
!                                                                                 !RAND 45
      IX = MOD(171*IX,30269)                                                      !RAND 46
      IY = MOD(172*IY,30307)                                                      !RAND 47
      IZ = MOD(170*IZ,30323)                                                      !RAND 48
!                                                                                 !RAND 49
      RANDOM_M10 = DMOD(DBLE(IX)/30269.0D0 + DBLE(IY)/30307.0D0 + DBLE(IZ)/    &  !RAND 50
         30323.0D0,ONE)                                                           !RAND 51
      L = 0                                                                       !RAND 52
      IF (RANDOM_M10<=ZERO .OR. RANDOM_M10>=ONE) L = 1                            !RAND 53
      RETURN                                                                      !RAND 54
      END FUNCTION RANDOM_M10                                                     !RAND 55
!                                                                                 !RAND 56
!-------------------------------------------------------------------------------  !RDMG  1
      SUBROUTINE READMGCM_M10(GCMDIR, VERSION)                                    !RDMG  2
!-----------------------------------------------                                  !RDMG  3
!   M o d u l e s                                                                 !RDMG  4
!-----------------------------------------------                                  !RDMG  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !RDMG  6
      USE MGCMPARM_M10_C                                                          !RDMG  7
      USE MGCMDATA_M10_C                                                          !RDMG  8
      USE parameters_M10_C                                                        !RDMG  9
!-----------------------------------------------                                  !RDMG 10
!---    Reads NASA Ames Mars General Circulation Model (MGCM) 0-80 km             !RDMG 11
!       data (in binary format) and loads into data arrays for common             !RDMG 12
!       MGCMdata                                                                  !RDMG 13
!       GCMDIR is directory name where MGCM data resides                          !RDMG 14
!...                                                                              !RDMG 15
!...Switches:                                                                     !RDMG 16
      IMPLICIT NONE                                                               !RDMG 17
!-----------------------------------------------                                  !RDMG 18
!   D u m m y   A r g u m e n t s                                                 !RDMG 19
!-----------------------------------------------                                  !RDMG 20
      CHARACTER(LEN=99) , INTENT(IN) :: GCMDIR                                    !RDMG 21
      CHARACTER , INTENT(IN) :: VERSION                                           !RDMG 22
!-----------------------------------------------                                  !RDMG 23
!   L o c a l   P a r a m e t e r s                                               !RDMG 24
!-----------------------------------------------                                  !RDMG 25
      CHARACTER(LEN = 11), PARAMETER :: SYSFORM = 'unformatted'                   !RDMG 26
!-----------------------------------------------                                  !RDMG 27
!   L o c a l   V a r i a b l e s                                                 !RDMG 28
!-----------------------------------------------                                  !RDMG 29
      INTEGER :: LASTLS, ILATSTEP, LENDIR, M, LSEA, LS, LAT, I, K, ILS, IHGT      !RDMG 30
      REAL(DOUBLE), DIMENSION(3) :: DFA0, DFA1, DFA2, DFA3, DFA4, DFA5, DFA6,   & !RDMG 31
         DFA7, DFB0, DFB1, DFB2, DFB3, DFB4, DFB5, DFB6, DFB7, DFC1, DFC2, DFC3   !RDMG 32
      REAL(DOUBLE), DIMENSION(12) :: SINLS                                        !RDMG 33
      REAL(DOUBLE) :: PI180, XLAT, YLAT, DFA, DFB, DFM, ZZ, DFC                   !RDMG 34
!-----------------------------------------------                                  !RDMG 35
!---    Set parameter for form= in binary file open statement                     !RDMG 36
!---    Factors for Map Year 0 density and pressure to better agree               !RDMG 37
!         with Map Years 1 and 2                                                  !RDMG 38
      DATA DFA0/ 1.03050D+00, 1.01386E+00, 1.2029D+0/                             !RDMG 39
      DATA DFA1/ -6.81901D-04, -1.52668E-04, 0.0000D+0/                           !RDMG 40
      DATA DFA2/ 6.67896D-06, 5.78641E-06, 4.5266D-5/                             !RDMG 41
      DATA DFA3/ 1.69052D-02, 5.57000E-03, 0.0000D+0/                             !RDMG 42
      DATA DFA4/ 2.83632D-04, -8.23194E-05, 0.0000D+0/                            !RDMG 43
      DATA DFA5/ 4.11891D-06, -7.11778E-06, 0.0000D+0/                            !RDMG 44
      DATA DFA6/ -1.05497D-01, 0.00000D+0, 0.0000D+0/                             !RDMG 45
      DATA DFA7/ 9.17778D-05, 0.00000D+0, 0.0000D+0/                              !RDMG 46
      DATA DFB0/ -3.91632D+00, -1.63780E+00, 18.1260D+0/                          !RDMG 47
      DATA DFB1/ 1.02837D-01, 6.76088E-02, 0.0000D+0/                             !RDMG 48
      DATA DFB2/ -2.73027D-04, -3.23646E-05,  -9.5731D-3/                         !RDMG 49
      DATA DFB3/ 1.02129D+00, 8.20808E-02, 0.0000D+0/                             !RDMG 50
      DATA DFB4/ -3.11446D-02, -4.21906E-02, 0.0000D+0/                           !RDMG 51
      DATA DFB5/ -5.95152D-04, 1.97186E-04, 0.0000D+0/                            !RDMG 52
      DATA DFB6/ 8.26562D+00, 0.00000D+0, 0.0000D+0/                              !RDMG 53
      DATA DFB7/ -3.37582D-02, 0.00000D+0, 0.0000D+0/                             !RDMG 54
      DATA DFC1/ -0.1307D0, -0.1657D0,  -0.3931D0/                                !RDMG 55
      DATA DFC2/ 1.6074D0, 1.6981D0, 2.9640D0/                                    !RDMG 56
      DATA DFC3/ -3.7984D0, -4.1688D0,  -5.6292D0/                                !RDMG 57
      PI180 = DATAN(1.0D0)/45.0D0                                                 !RDMG 58
!                                                                                 !RDMG 59
!---    Initialize last Ls value processed to 0                                   !RDMG 60
      LASTLS = 0                                                                  !RDMG 61
!---    Set ilatstep = latitude step size x 10                                    !RDMG 62
      ILATSTEP = 1800/(NLAT - 1)                                                  !RDMG 63
!---    Compute string length for directory name                                  !RDMG 64
      LENDIR = Len_Trim(GCMDIR)                                                   !RDMG 65
      IF (LENDIR<1 .OR. LENDIR>99) LENDIR = 99                                    !RDMG 66
!---    Step through all dust optical depths                                      !RDMG 67
      DO M = 1, NDUST                                                             !RDMG 68
!---      Open MGCM input files for temperature, pressure, and density            !RDMG 69
         OPEN(32, FILE=GCMDIR(1:LENDIR)//'tpdlo'//DUSTC(M)//VERSION//'.bin',   &  !RDMG 70
            FORM=SYSFORM, STATUS='old', POSITION='asis')                          !RDMG 71
!---      Open MGCM input files for wind components                               !RDMG 72
         OPEN(33, FILE=GCMDIR(1:LENDIR)//'uvlo'//DUSTC(M)//VERSION//'.bin',    &  !RDMG 73
            FORM=SYSFORM, STATUS='old', POSITION='asis')                          !RDMG 74
!---      Step through all Ls values                                              !RDMG 75
         DO LSEA = 30, 360, 30                                                    !RDMG 76
            LS = LSEA/30                                                          !RDMG 77
            SINLS(LS) = DSIN(PI180*LSEA)                                          !RDMG 78
!---      Step through all latitude grid points                                   !RDMG 79
            DO LAT = -900, 900, ILATSTEP                                          !RDMG 80
               XLAT = LAT/10.0D0                                                  !RDMG 81
               I = 1 + (LAT + 900)/ILATSTEP                                       !RDMG 82
!---      Step through all height levels                                          !RDMG 83
               DO K = NHGT, 1, -1                                                 !RDMG 84
!---        Read (binary) tide coefficients for temperature, pressure,            !RDMG 85
!           and density                                                           !RDMG 86
                  READ (32, END=99) ILS, IHGT, YLAT, TZA0(K,I,LS,M), TZA1(K,I,  & !RDMG 87
                     LS,M), TZP1(K,I,LS,M), TZA2(K,I,LS,M), TZP2(K,I,LS,M),     & !RDMG 88
                     PZA0(K,I,LS,M), PZA1(K,I,LS,M), PZP1(K,I,LS,M), PZA2(K,I,  & !RDMG 89
                     LS,M), PZP2(K,I,LS,M), DZA0(K,I,LS,M)                        !RDMG 90
                  IF (ILS /= LSEA) STOP ' Bad tpd Ls'                             !RDMG 91
                  IF (IHGT /= 5*(K - 1)) STOP ' Bad tpd Height'                   !RDMG 92
                  IF (dabs(YLAT-XLAT)>0.0d0) STOP ' Bad tpd Latitude'             !RDMG 93
!---        Adjust pressure and density to agree with Map Year 2                  !RDMG 94
                  DFA = DFA0(M) + DFA1(M)*XLAT + DFA2(M)*XLAT**2 + (DFA3(M)+    & !RDMG 95
                     DFA4(M)*XLAT+DFA5(M)*XLAT**2)*SINLS(LS) + (DFA6(M)+DFA7(M) & !RDMG 96
                     *XLAT)*SINLS(LS)**2                                          !RDMG 97
                  DFB = DFB0(M) + DFB1(M)*XLAT + DFB2(M)*XLAT**2 + (DFB3(M)+    & !RDMG 98
                     DFB4(M)*XLAT+DFB5(M)*XLAT**2)*SINLS(LS) + (DFB6(M)+DFB7(M) & !RDMG 99
                     *XLAT)*SINLS(LS)**2                                          !RDMG100
                  DFM = DFA + DFB*DZA0(K,I,LS,M)                                  !RDMG101
                  ZZ = (K - 1.0D0)/20.0D0                                         !RDMG102
                  DFC = 1.0D0 + DFC1(M)*ZZ + DFC2(M)*ZZ**2 + DFC3(M)*ZZ**3        !RDMG103
                  DFC = DMAX1(0.5D0,DFC)                                          !RDMG104
                  PZA0(K,I,LS,M) = PZA0(K,I,LS,M)*DFM/DFC                         !RDMG105
                  DZA0(K,I,LS,M) = DZA0(K,I,LS,M)*DFM/DFC                         !RDMG106
!---        Read (binary) tide coefficients for wind components                   !RDMG107
                  READ (33, END=99) ILS, IHGT, YLAT, UZA0(K,I,LS,M), UZA1(K,I,  & !RDMG108
                     LS,M), UZP1(K,I,LS,M), UZA2(K,I,LS,M), UZP2(K,I,LS,M),     & !RDMG109
                     VZA0(K,I,LS,M), VZA1(K,I,LS,M), VZP1(K,I,LS,M), VZA2(K,I,  & !RDMG110
                     LS,M), VZP2(K,I,LS,M)                                        !RDMG111
                  IF (ILS /= LSEA) STOP ' Bad uv Ls'                              !RDMG112
!---        Reset value of last Ls processed                                      !RDMG113
                  LASTLS = ILS                                                    !RDMG114
                  IF (IHGT /= 5*(K - 1)) STOP ' Bad uv Height'                    !RDMG115
                  IF (dabs(YLAT-XLAT)<=0.0d0) CYCLE                               !RDMG116
                  STOP ' Bad uv Latitude'                                         !RDMG117
               END DO                                                             !RDMG118
            END DO                                                                !RDMG119
         END DO                                                                   !RDMG120
!---      Set data for Ls = 0 to data for Ls = 360                                !RDMG121
         TZA0(:NHGT,:NLAT,0,M) = TZA0(:NHGT,:NLAT,12,M)                           !RDMG122
         TZA1(:NHGT,:NLAT,0,M) = TZA1(:NHGT,:NLAT,12,M)                           !RDMG123
         TZP1(:NHGT,:NLAT,0,M) = TZP1(:NHGT,:NLAT,12,M)                           !RDMG124
         TZA2(:NHGT,:NLAT,0,M) = TZA2(:NHGT,:NLAT,12,M)                           !RDMG125
         TZP2(:NHGT,:NLAT,0,M) = TZP2(:NHGT,:NLAT,12,M)                           !RDMG126
         PZA0(:NHGT,:NLAT,0,M) = PZA0(:NHGT,:NLAT,12,M)                           !RDMG127
         PZA1(:NHGT,:NLAT,0,M) = PZA1(:NHGT,:NLAT,12,M)                           !RDMG128
         PZP1(:NHGT,:NLAT,0,M) = PZP1(:NHGT,:NLAT,12,M)                           !RDMG129
         PZA2(:NHGT,:NLAT,0,M) = PZA2(:NHGT,:NLAT,12,M)                           !RDMG130
         PZP2(:NHGT,:NLAT,0,M) = PZP2(:NHGT,:NLAT,12,M)                           !RDMG131
         DZA0(:NHGT,:NLAT,0,M) = DZA0(:NHGT,:NLAT,12,M)                           !RDMG132
         UZA0(:NHGT,:NLAT,0,M) = UZA0(:NHGT,:NLAT,12,M)                           !RDMG133
         UZA1(:NHGT,:NLAT,0,M) = UZA1(:NHGT,:NLAT,12,M)                           !RDMG134
         UZP1(:NHGT,:NLAT,0,M) = UZP1(:NHGT,:NLAT,12,M)                           !RDMG135
         UZA2(:NHGT,:NLAT,0,M) = UZA2(:NHGT,:NLAT,12,M)                           !RDMG136
         UZP2(:NHGT,:NLAT,0,M) = UZP2(:NHGT,:NLAT,12,M)                           !RDMG137
         VZA0(:NHGT,:NLAT,0,M) = VZA0(:NHGT,:NLAT,12,M)                           !RDMG138
         VZA1(:NHGT,:NLAT,0,M) = VZA1(:NHGT,:NLAT,12,M)                           !RDMG139
         VZP1(:NHGT,:NLAT,0,M) = VZP1(:NHGT,:NLAT,12,M)                           !RDMG140
         VZA2(:NHGT,:NLAT,0,M) = VZA2(:NHGT,:NLAT,12,M)                           !RDMG141
         VZP2(:NHGT,:NLAT,0,M) = VZP2(:NHGT,:NLAT,12,M)                           !RDMG142
!---    Close input files to re-use same unit number for next dust                !RDMG143
!       value                                                                     !RDMG144
         CLOSE(32)                                                                !RDMG145
         CLOSE(33)                                                                !RDMG146
         CYCLE                                                                    !RDMG147
!---    Terminate if not all Ls values have been processed                        !RDMG148
   99    CONTINUE                                                                 !RDMG149
         IF (LASTLS == 360) CYCLE                                                 !RDMG150
         STOP ' Incomplete 0-80 km MGCM data'                                     !RDMG151
      END DO                                                                      !RDMG152
      RETURN                                                                      !RDMG153
      END SUBROUTINE READMGCM_M10                                                 !RDMG154
!                                                                                 !RDMG155
!------------------------------------------------------------------------------   !RDSF  1
      subroutine readsurf_M10(gcmdir, version)                                    !RDSF  2
!-----------------------------------------------                                  !RDSF  3
!   M o d u l e s                                                                 !RDSF  4
!-----------------------------------------------                                  !RDSF  5
      USE vast_kind_param, ONLY:  double                                          !RDSF  6
      USE surfdata_M10_C                                                          !RDSF  7
      USE mgcmparm_M10_C                                                          !RDSF  8
      USE parameters_M10_C                                                        !RDSF  9
!-----------------------------------------------                                  !RDSF 10
!---    Reads NASA Ames Mars General Circulation Model (MGCM) surface             !RDSF 11
!       data (in binary format) and loads into data arrays for common             !RDSF 12
!       surfdata                                                                  !RDSF 13
!       GCMDIR is directory name where MGCM data resides                          !RDSF 14
!...                                                                              !RDSF 15
!...Switches:                                                                     !RDSF 16
      implicit none                                                               !RDSF 17
!-----------------------------------------------                                  !RDSF 18
!   D u m m y   A r g u m e n t s                                                 !RDSF 19
!-----------------------------------------------                                  !RDSF 20
      character(len=99) , intent(in) :: gcmdir                                    !RDSF 21
      character , intent(in) :: version                                           !RDSF 22
!-----------------------------------------------                                  !RDSF 23
!   L o c a l   P a r a m e t e r s                                               !RDSF 24
!-----------------------------------------------                                  !RDSF 25
      character(len=11), parameter :: sysform = 'unformatted'                     !RDSF 26
!-----------------------------------------------                                  !RDSF 27
!   L o c a l   V a r i a b l e s                                                 !RDSF 28
!-----------------------------------------------                                  !RDSF 29
      integer :: lastls, ilatstep, lendir, m, lsea, ls, lat, i, k, lon, ils,    & !RDSF 30
         jlon                                                                     !RDSF 31
      real(double) :: xlat, ylat                                                  !RDSF 32
!-----------------------------------------------                                  !RDSF 33
!---    Set parameter for form= in binary file open statement                     !RDSF 34
!                                                                                 !RDSF 35
!---    Initialize last Ls value processed to 0                                   !RDSF 36
      lastls = 0                                                                  !RDSF 37
!---    Set ilatstep = latitude step size x 10                                    !RDSF 38
      ilatstep = 1800/(nlat - 1)                                                  !RDSF 39
!---    Compute string length for directory name                                  !RDSF 40
      lendir = Len_Trim(gcmdir)                                                   !RDSF 41
      if (lendir<1 .or. lendir>99) lendir = 99                                    !RDSF 42
!---    Step through all dust optical depths                                      !RDSF 43
      do m = 1, ndust                                                             !RDSF 44
!---      Open surface data files for surface level                               !RDSF 45
         open(33, file=gcmdir(1:lendir)//'sfc00'//dustc(m)//version//'.bin',    & !RDSF 46
            form=sysform, status='old', position='asis')                          !RDSF 47
!---      Open surface data files for 5 meter level above surface                 !RDSF 48
         open(34, file=gcmdir(1:lendir)//'sfc05'//dustc(m)//version//'.bin',    & !RDSF 49
            form=sysform, status='old', position='asis')                          !RDSF 50
!---      Open surface data files for 30 meter level above surface                !RDSF 51
         open(35, file=gcmdir(1:lendir)//'sfc30'//dustc(m)//version//'.bin',    & !RDSF 52
            form=sysform, status='old', position='asis')                          !RDSF 53
!---      Step through all Ls values                                              !RDSF 54
         do lsea = 30, 360, 30                                                    !RDSF 55
            ls = lsea/30                                                          !RDSF 56
!---      Step through all latitudes                                              !RDSF 57
            do lat = -900, 900, ilatstep                                          !RDSF 58
               xlat = lat/10.0D0                                                  !RDSF 59
               i = 1 + (lat + 900)/ilatstep                                       !RDSF 60
!---      Step through all boundary layer levels                                  !RDSF 61
               do k = 1, nbl                                                      !RDSF 62
!---      Step through all longitudes                                             !RDSF 63
                  if (k == 1) then                                                !RDSF 64
                     do lon = nlon, 1, -1                                         !RDSF 65
!---        Read (binary) tide coefficients for temperature and wind              !RDSF 66
!           components at all boundary layer levels                               !RDSF 67
                        read (32 + k, end=99) ils, ylat, jlon, tsa0(k,i,lon,ls, & !RDSF 68
                           m), tsa1(k,i,lon,ls,m), tsp1(k,i,lon,ls,m), tsa2(k,i & !RDSF 69
                           ,lon,ls,m), tsp2(k,i,lon,ls,m)                         !RDSF 70
!---         Assume surface wind = 0 (no slip condition)                          !RDSF 71
                        usa0(k,i,lon,ls,m) = 0.0D0                                !RDSF 72
                        usa1(k,i,lon,ls,m) = 0.0D0                                !RDSF 73
                        usp1(k,i,lon,ls,m) = 0.0D0                                !RDSF 74
                        usa2(k,i,lon,ls,m) = 0.0D0                                !RDSF 75
                        usp2(k,i,lon,ls,m) = 0.0D0                                !RDSF 76
                        vsa0(k,i,lon,ls,m) = 0.0D0                                !RDSF 77
                        vsa1(k,i,lon,ls,m) = 0.0D0                                !RDSF 78
                        vsp1(k,i,lon,ls,m) = 0.0D0                                !RDSF 79
                        vsa2(k,i,lon,ls,m) = 0.0D0                                !RDSF 80
                        vsp2(k,i,lon,ls,m) = 0.0D0                                !RDSF 81
                        if (ils /= lsea) stop ' Bad surface Ls'                   !RDSF 82
!---        Reset value of last Ls processed                                      !RDSF 83
                        lastls = ils                                              !RDSF 84
                        if (dabs(ylat-xlat)>0.0d0) stop ' Bad surface Latitude'   !RDSF 85
                        if (jlon == 9*lon) cycle                                  !RDSF 86
                        stop ' Bad surface Longitude'                             !RDSF 87
                     end do                                                       !RDSF 88
                  else                                                            !RDSF 89
                     do lon = nlon, 1, -1                                         !RDSF 90
!---         Assume surface wind = 0 (no slip condition)                          !RDSF 91
                        read (32 + k, end=99) ils, ylat, jlon, tsa0(k,i,lon,ls, & !RDSF 92
                           m), tsa1(k,i,lon,ls,m), tsp1(k,i,lon,ls,m), tsa2(k,i & !RDSF 93
                           ,lon,ls,m), tsp2(k,i,lon,ls,m), usa0(k,i,lon,ls,m),  & !RDSF 94
                           usa1(k,i,lon,ls,m), usp1(k,i,lon,ls,m), usa2(k,i,lon & !RDSF 95
                           ,ls,m), usp2(k,i,lon,ls,m), vsa0(k,i,lon,ls,m), vsa1 & !RDSF 96
                           (k,i,lon,ls,m), vsp1(k,i,lon,ls,m), vsa2(k,i,lon,ls, & !RDSF 97
                           m), vsp2(k,i,lon,ls,m)                                 !RDSF 98
                        if (ils /= lsea) stop ' Bad surface Ls'                   !RDSF 99
!---        Reset value of last Ls processed                                      !RDSF100
                        lastls = ils                                              !RDSF101
                        if (dabs(ylat-xlat)>0.0d0) stop ' Bad surface Latitude'   !RDSF102
                        if (jlon == 9*lon) cycle                                  !RDSF103
                        stop ' Bad surface Longitude'                             !RDSF104
                     end do                                                       !RDSF105
                  endif                                                           !RDSF106
               end do                                                             !RDSF107
            end do                                                                !RDSF108
         end do                                                                   !RDSF109
!---      Set all values at Ls=0 to values at Ls=360                              !RDSF110
         tsa0(:nbl,:nlat,1:nlon,0,m) = tsa0(:nbl,:nlat,1:nlon,12,m)               !RDSF111
         tsa1(:nbl,:nlat,1:nlon,0,m) = tsa1(:nbl,:nlat,1:nlon,12,m)               !RDSF112
         tsp1(:nbl,:nlat,1:nlon,0,m) = tsp1(:nbl,:nlat,1:nlon,12,m)               !RDSF113
         tsa2(:nbl,:nlat,1:nlon,0,m) = tsa2(:nbl,:nlat,1:nlon,12,m)               !RDSF114
         tsp2(:nbl,:nlat,1:nlon,0,m) = tsp2(:nbl,:nlat,1:nlon,12,m)               !RDSF115
         usa0(:nbl,:nlat,1:nlon,0,m) = usa0(:nbl,:nlat,1:nlon,12,m)               !RDSF116
         usa1(:nbl,:nlat,1:nlon,0,m) = usa1(:nbl,:nlat,1:nlon,12,m)               !RDSF117
         usp1(:nbl,:nlat,1:nlon,0,m) = usp1(:nbl,:nlat,1:nlon,12,m)               !RDSF118
         usa2(:nbl,:nlat,1:nlon,0,m) = usa2(:nbl,:nlat,1:nlon,12,m)               !RDSF119
         usp2(:nbl,:nlat,1:nlon,0,m) = usp2(:nbl,:nlat,1:nlon,12,m)               !RDSF120
         vsa0(:nbl,:nlat,1:nlon,0,m) = vsa0(:nbl,:nlat,1:nlon,12,m)               !RDSF121
         vsa1(:nbl,:nlat,1:nlon,0,m) = vsa1(:nbl,:nlat,1:nlon,12,m)               !RDSF122
         vsp1(:nbl,:nlat,1:nlon,0,m) = vsp1(:nbl,:nlat,1:nlon,12,m)               !RDSF123
         vsa2(:nbl,:nlat,1:nlon,0,m) = vsa2(:nbl,:nlat,1:nlon,12,m)               !RDSF124
         vsp2(:nbl,:nlat,1:nlon,0,m) = vsp2(:nbl,:nlat,1:nlon,12,m)               !RDSF125
!           Set all values at Lon=0 to values at Lon=360                          !RDSF126
         tsa0(:nbl,:nlat,0,:12,m) = tsa0(:nbl,:nlat,nlon,:12,m)                   !RDSF127
         tsa1(:nbl,:nlat,0,:12,m) = tsa1(:nbl,:nlat,nlon,:12,m)                   !RDSF128
         tsp1(:nbl,:nlat,0,:12,m) = tsp1(:nbl,:nlat,nlon,:12,m)                   !RDSF129
         tsa2(:nbl,:nlat,0,:12,m) = tsa2(:nbl,:nlat,nlon,:12,m)                   !RDSF130
         tsp2(:nbl,:nlat,0,:12,m) = tsp2(:nbl,:nlat,nlon,:12,m)                   !RDSF131
         usa0(:nbl,:nlat,0,:12,m) = usa0(:nbl,:nlat,nlon,:12,m)                   !RDSF132
         usa1(:nbl,:nlat,0,:12,m) = usa1(:nbl,:nlat,nlon,:12,m)                   !RDSF133
         usp1(:nbl,:nlat,0,:12,m) = usp1(:nbl,:nlat,nlon,:12,m)                   !RDSF134
         usa2(:nbl,:nlat,0,:12,m) = usa2(:nbl,:nlat,nlon,:12,m)                   !RDSF135
         usp2(:nbl,:nlat,0,:12,m) = usp2(:nbl,:nlat,nlon,:12,m)                   !RDSF136
         vsa0(:nbl,:nlat,0,:12,m) = vsa0(:nbl,:nlat,nlon,:12,m)                   !RDSF137
         vsa1(:nbl,:nlat,0,:12,m) = vsa1(:nbl,:nlat,nlon,:12,m)                   !RDSF138
         vsp1(:nbl,:nlat,0,:12,m) = vsp1(:nbl,:nlat,nlon,:12,m)                   !RDSF139
         vsa2(:nbl,:nlat,0,:12,m) = vsa2(:nbl,:nlat,nlon,:12,m)                   !RDSF140
         vsp2(:nbl,:nlat,0,:12,m) = vsp2(:nbl,:nlat,nlon,:12,m)                   !RDSF141
!---      Close input file units                                                  !RDSF142
         close(33)                                                                !RDSF143
         close(34)                                                                !RDSF144
         close(35)                                                                !RDSF145
         cycle                                                                    !RDSF146
!---      Terminate if not all Ls values processed                                !RDSF147
   99    continue                                                                 !RDSF148
         if (lastls == 360) cycle                                                 !RDSF149
         stop ' Incomplete surface GCM data'                                      !RDSF150
      end do                                                                      !RDSF151
      return                                                                      !RDSF152
      end subroutine readsurf_M10                                                 !RDSF153
!                                                                                 !RDSF154
!-------------------------------------------------------------------------------  !RDTG  1
      subroutine readtgcm_M10(gcmdir, version)                                    !RDTG  2
!-----------------------------------------------                                  !RDTG  3
!   M o d u l e s                                                                 !RDTG  4
!-----------------------------------------------                                  !RDTG  5
      USE vast_kind_param, ONLY:  double                                          !RDTG  6
      USE tgcmdata_M10_C                                                          !RDTG  7
      USE mgcmparm_M10_C                                                          !RDTG  8
      USE parameters_M10_C                                                        !RDTG  9
!-----------------------------------------------                                  !RDTG 10
!---    Reads University of Michigan Mars Thermospheric General Circu-            !RDTG 11
!       lation Model (MTGCM) data (in binary format) and loads into               !RDTG 12
!       data arrays for common TGCMdata                                           !RDTG 13
!       GCMDIR is directory name where MTGCM data resides                         !RDTG 14
!...                                                                              !RDTG 15
!...Switches:                                                                     !RDTG 16
      implicit none                                                               !RDTG 17
!-----------------------------------------------                                  !RDTG 18
!   D u m m y   A r g u m e n t s                                                 !RDTG 19
!-----------------------------------------------                                  !RDTG 20
      character(len=99) , intent(in) :: gcmdir                                    !RDTG 21
      character , intent(in) :: version                                           !RDTG 22
!-----------------------------------------------                                  !RDTG 23
!   L o c a l   P a r a m e t e r s                                               !RDTG 24
!-----------------------------------------------                                  !RDTG 25
      character(len=11), parameter :: sysform = 'unformatted'                     !RDTG 26
!-----------------------------------------------                                  !RDTG 27
!   L o c a l   V a r i a b l e s                                                 !RDTG 28
!-----------------------------------------------                                  !RDTG 29
      integer :: lastls, ilatstep, ilat1, ilat2, lendir, n, m, lsea, ls, lat, i & !RDTG 30
         , k, ils, ihgt , k1                                                      !RDTG 30a
      real(double) :: xlat, ylat, zfdust, FitLat, zm80, DMG, FitHgt, ZFX ,      & !RDTG 31
           zlat, Hf80(0:6,0:12,3),Chf(0:6), AC(0:8), BC(0:8), sls, cls, pi180 , & !RDTG 31a
           Acalc, Bcalc, phi, phi2                                                !RDTG 31b
!-----------------------------------------------                                  !RDTG 31c
      Data AC/-2.8476225d0,5.1614397d0,11.808264d0, 1.5427184d0,8.2490367d0,    & !RDTG 31d
         -6.7993216d0,-7.6310943d0,-7.1398243d0,-11.384270d0/                     !RDTG 31e
      Data BC/4.7110391d0,-6.9787011d0,-19.723706d0,-2.5946215d0,-13.403260d0,  & !RDTG 31f
        6.8682539d0,15.810774d0,9.5193664d0,19.050826d0/                          !RDTG 31g
  Data Hf80/1.310D+0,-1.7173D-3,2.2086D-4,1.1335D-6,-4.7842D-8,-1.402D-10,      & !RDTG 32
   1.9581D-12,1.2855D+0,2.5407D-3,1.114D-8,-3.3227D-6,5.6398D-8,4.1167D-10,     & !RDTG 32a
   -7.4666D-12,1.2491D+0,3.5334D-3,-8.0617D-5,-3.5076D-6,8.6122D-8,4.0803D-10,  & !RDTG 32b
   -9.809D-12,1.1595D+0,1.9913D-3,2.8612D-5,-2.5338D-6,4.403D-8,3.1054D-10,     & !RDTG 32c
   -5.8952D-12,1.1046D+0,3.3547D-3,-1.4369D-6,-3.588D-6,6.7749D-8,3.359D-10,    & !RDTG 32d
   -7.5981D-12,1.2877D+0,4.6463D-3,-1.7076D-4,-4.6047D-6,1.2693D-7,4.6269D-10,  & !RDTG 32e
   -1.3291D-11,1.2717D+0,-2.5201D-4,1.4159D-4,-2.8328D-7,8.3218D-9,5.5739D-12,  & !RDTG 32g
   -3.3071D-12,1.3287D+0,-1.1228D-3,1.4243D-4,7.3074D-7,-2.0977D-8,-1.4763D-10, & !RDTG 32h
   -1.3986D-13,1.3602D+0,-1.9213D-3,4.4605D-5,-2.4831D-7,-2.0548D-9,-9.2112D-12,& !RDTG 32i
   -4.961D-13,1.3292D+0,-2.0575D-3,3.0917D-5,-6.1131D-10,8.5858D-9,-6.742D-11,  & !RDTG 32j
   -1.9742D-12,1.2491D+0,-2.6908D-3,6.523D-5,9.6963D-7,2.313D-8,-1.7627D-10,    & !RDTG 32k
   -4.1092D-12,1.3873D+0,-1.8219D-3,6.3232D-5,-1.9586D-8,-1.2456D-8,-4.4703D-11,& !RDTG 32l
   -1.4046D-13,1.310D+0,-1.7173D-3,2.2086D-4,1.1335D-6,-4.7842D-8,-1.402D-10,   & !RDTG 32n
   1.9581D-12,1.0691D+0,-2.2073D-3,2.0158D-4,2.4884D-6,3.8384D-8,-2.094D-10,    & !RDTG 32o
   -8.8724D-12,1.2242D+0,4.0856D-3,9.6936D-5,-5.5182D-6,6.5142D-8,6.7364D-10,   & !RDTG 32p
   -1.0937D-11,1.2203D+0,2.5951D-3,1.0236D-4,-2.4933D-6,3.2277D-8,2.9808D-10,   & !RDTG 32q
   -5.2909D-12,1.2738D+0,3.5809D-4,5.7095D-5,-5.9205D-7,1.1426D-8,-3.6894D-11,  & !RDTG 32r
   -6.3835D-13,1.2828D+0,1.7465D-3,6.3646D-5,-2.0767D-6,2.4885D-8,1.4811D-10,   & !RDTG 32s
   -2.5337D-12,1.2416D+0,3.0057D-3,1.4008D-4,-3.7762D-6,2.4889D-8,5.1451D-10,   & !RDTG 33a
   -6.1947D-12,1.2061D+0,-3.3591D-4,2.8237D-4,-6.3895D-7,-5.5637D-9,3.0563D-11, & !RDTG 33b
  -4.7434D-12,1.2602D+0,7.0066D-4,2.6984D-4,1.3802D-6,-3.9678D-8,-2.809D-10,    & !RDTG 33c
   2.5941D-13,1.2806D+0,-6.809D-4,1.7769D-4,5.8416D-7,-1.436D-8,-1.4034D-10,    & !RDTG 33d
   -7.310D-13,8.4267D-1,-1.583D-3,7.8998D-5,1.5957D-6,4.1777D-8,2.7419D-10,     & !RDTG 33e
   -9.1151D-13,9.1483D-1,-4.4468D-3,7.3244D-5,4.7337D-6,7.2785D-8,-3.7216D-10,  & !RDTG 33f
   -7.8868D-12,1.1589D+0,-1.0558D-3,2.2047D-4,1.6263D-6,-2.2203D-8,-3.0259D-10, & !RDTG 33h
   -1.0585D-12,1.0691D+0,-2.2073D-3,2.0158D-4,2.4884D-6,3.8384D-8,-2.094D-10,   & !RDTG 33i
   -8.8724D-12,1.3214D+0,-4.1666D-4,3.7428D-4,-1.172D-7,-2.3338D-8,3.5305D-11,  & !RDTG 33j
   -3.2754D-12,1.1866D+0,7.2828D-3,3.0029D-4,-9.4224D-6,8.9523D-8,1.168D-9,     & !RDTG 33k
   -1.6491D-11,1.3759D+0,3.0491D-3,1.7077D-4,-2.5388D-6,2.5061D-8,2.4012D-10,   & !RDTG 33l
   -3.7566D-12,1.2639D+0,2.2375D-3,7.6136D-5,-1.7501D-6,3.9701D-8,-1.3015D-10,  & !RDTG 33m
   -1.3353D-12,1.3175D+0,1.0165D-3,6.5074D-5,-1.3655D-6,4.5156D-8,-2.1937D-10,  & !RDTG 33o
   -1.2701D-12,1.2192D+0,7.6473D-3,1.2184D-4,-8.6728D-6,1.3123D-7,7.5499D-10,   & !RDTG 33p
  -1.4887D-11,1.440D+0,5.3844D-4,2.5964D-4,-1.5433D-6,1.5967D-8,6.5136D-11,     & !RDTG 33q
  -5.900D-12,1.4992D+0,3.2545D-4,4.0814D-4,-3.1428D-7,-9.129D-8,-1.196D-10,     & !RDTG 33r
   5.628D-12,9.375D-1,-9.1505D-3,1.013D-4,8.7482D-6,1.3768D-7,-8.4349D-10,      & !RDTG 33s
   -1.6221D-11,1.0283D+0,6.4878D-4,1.8831D-4,-1.9329D-6,-2.0171D-8,8.8323D-10,  & !RDTG 33t
   8.6919D-12,9.6642D-1,-1.9405D-3,1.2465D-4,5.3758D-7,3.1163D-8,4.6631D-10,    & !RDTG 33v
   1.3538D-12,1.166D+0,-3.2232D-3,4.0286D-4,3.7747D-6,-9.7224D-9,-5.6825D-10,   & !RDTG 33w
   -4.0148D-12,1.3214D+0,-4.1666D-4,3.7428D-4,-1.172D-7,-2.3338D-8,3.5305D-11,  & !RDTG 33x
   -3.2754D-12/                                                                   !RDTG 33y
!-------------------------------------------------------------------------------- !RDTG 33z
!                                                                                 !RDTG 34 
!---    Set parameter for form= in binary file open statement                     !RDTG 34a
!                                                                                 !RDTG 35
      pi180 = dAtan(1.0d0)/45.0d0                                                 !RDTG 35a
!---    Initialize last Ls value processed to 0                                   !RDTG 36
      lastls = 0                                                                  !RDTG 37
!---    Set ilatstep = latitude step size x 10                                    !RDTG 38
      ilatstep = 1800/nlatt                                                       !RDTG 39
!---    Set initial and final latitudes (x 10) for stepping                       !RDTG 40
      ilat1 = (-900) + ilatstep/2                                                 !RDTG 41
      ilat2 = 900 - ilatstep/2                                                    !RDTG 42
!---    Compute string length for directory name                                  !RDTG 43
      lendir = Len_Trim(gcmdir)                                                   !RDTG 44
      if (lendir<1 .or. lendir>99) lendir = 99                                    !RDTG 45
!---    Step through all solar activity levels                                    !RDTG 46
      do n = 1, nf10                                                              !RDTG 47
         open(34, file=gcmdir(1:lendir)//'zfht'//solact(n)//version//'.txt',    & !RDTG 48
            status='old', position='asis')                                        !RDTG 49
!---    Step through all dust optical depths                                      !RDTG 50
         do m = 1, ndust                                                          !RDTG 51
!---      Open MTGCM data files for temperature, pressure, and density            !RDTG 52
            open(32, file=gcmdir(1:lendir)//'tpd'//solact(n)//dustc(m)//version & !RDTG 53
               //'.bin', form=sysform, status='old', position='asis')             !RDTG 54
!---      Open MTGCM data files for wind components                               !RDTG 55
            open(33, file=gcmdir(1:lendir)//'uv'//solact(n)//dustc(m)//version  & !RDTG 56
               //'.bin', form=sysform, status='old', position='asis')             !RDTG 57
!---      Step through all Ls values                                              !RDTG 58
            do lsea = 30, 360, 30                                                 !RDTG 59
               ls = lsea/30                                                       !RDTG 60
               sls = dSin(pi180*lsea)                                             !RDTG 60a
               cls = dCos(pi180*lsea)                                             !RDTG 60b
!---      Step through all latitudes                                              !RDTG 61
               do lat = ilat1, ilat2, ilatstep                                    !RDTG 62
                  xlat = lat/10.0D0                                               !RDTG 63
                  zlat = xlat                                                     !RDTG 65
                  If (zlat < -82.5d0)zlat = -82.5d0                               !RDTG 66
                  If (zlat > 82.5d0)zlat = 82.5d0                                 !RDTG 67
                  Do 10 k = 0,6                                                   !RDTG 68
                    Chf(k) = Hf80(k,ls,m)                                         !RDTG 69
  10              Enddo                                                           !RDTG 70
                  FitLat = Chf(0) + Chf(1)*zlat + Chf(2)*zlat**2 +            &   !RDTG 71
                    Chf(3)*zlat**3 + Chf(4)*zlat**4 + Chf(5)*zlat**5 +        &   !RDTG 71a
                    Chf(6)*zlat**6                                                !RDTG 71b
                  If (FitLat < 0.5d0)FitLat = 0.5d0                               !RDTG 71c
                  i = 1 + (lat-ilat1)/ilatstep                                    !RDTG 72
!---              Step through all heights                                        !RDTG 73
                  Do k = 1,nhgtt                                                  !RDTG 74
!---                Read (binary) tide coefficients for temperature, pressure,    !RDTG 75
!                   and density                                                   !RDTG 76
                    Read(32,End=99)ils,ihgt,ylat,TTA0(k,i,ls,m,n),         &      !RDTG 77
                    TTA1(k,i,ls,m,n),TTP1(k,i,ls,m,n),TTA2(k,i,ls,m,n),    &      !RDTG 78
                    TTP2(k,i,ls,m,n),PTA0(k,i,ls,m,n),PTA1(k,i,ls,m,n),    &      !RDTG 79
                    PTP1(k,i,ls,m,n),PTA2(k,i,ls,m,n),PTP2(k,i,ls,m,n),    &      !RDTG 80
                    DTA0(k,i,ls,m,n),DTA1(k,i,ls,m,n),DTP1(k,i,ls,m,n),    &      !RDTG 81
                    DTA2(k,i,ls,m,n),DTP2(k,i,ls,m,n)                             !RDTG 82
                    DMG = DTA0(k,i,ls,m,n)                                        !RDTG 82a
                    zm80 = 5.0d0*(k-1)/100.0d0                                    !RDTG 82b
                    If (zm80 > 0.55d0)zm80 = 0.55d0                               !RDTG 83
                    phi = zlat/100.0d0                                            !RDTG 84
                    phi2 = phi**2                                                 !RDTG 85            
                    Acalc = AC(0) + AC(1)*phi + AC(2)*phi2 + AC(3)*sls +   &      !RDTG 86
                      AC(4)*cls + AC(5)*phi*sls + AC(6)*phi*cls +          &      !RDTG 87
                      AC(7)*phi2*sls + AC(8)*phi2*cls                             !RDTG 88
                    Bcalc = BC(0) + BC(1)*phi + BC(2)*phi2 + BC(3)*sls +   &      !RDTG 89
                      BC(4)*cls + BC(5)*phi*sls + BC(6)*phi*cls +          &      !RDTG 90
                      BC(7)*phi2*sls + BC(8)*phi2*cls                             !RDTG 91
                    FitHgt = (1.0d0 + Acalc*zm80 + Bcalc*zm80**2)*0.9883d0        !RDTG 92
                    If (FitHgt < 0.5d0)FitHgt = 0.5d0                             !RDTG 93
                    DTA0(k,i,ls,m,n)=DMG*FitLat*FitHgt                            !RDTG 96
                    PTA0(k,i,ls,m,n)=PTA0(k,i,ls,m,n)*FitLat*FitHgt               !RDTG 97
                    if (ils /= lsea) stop ' Bad tpd Ls'                           !RDTG 98 
                    if (ihgt /= 80 + 5*(k - 1)) stop ' Bad tpd Height'            !RDTG 99
                    if (dabs(ylat-xlat)>0.0d0) stop ' Bad tpd Latitude'           !RDTG100
!---                Read (binary) tide coefficients for wind components           !RDTG101
                    read (33, end=99) ils, ihgt, ylat, uta0(k,i,ls,m,n),        & !RDTG102
                       uta1(k,i,ls,m,n), utp1(k,i,ls,m,n), uta2(k,i,ls,m,n),    & !RDTG103
                       utp2(k,i,ls,m,n), vta0(k,i,ls,m,n), vta1(k,i,ls,m,n),    & !RDTG104
                       vtp1(k,i,ls,m,n), vta2(k,i,ls,m,n), vtp2(k,i,ls,m,n)       !RDTG105
                    if (ils /= lsea) stop ' Bad uv Ls'                            !RDTG106
!---                Reset last Ls value processed                                 !RDTG107
                    lastls = ils                                                  !RDTG108
                    if (ihgt /= 80 + 5*(k - 1)) stop ' Bad uv Height'             !RDTG109
                    if (dabs(ylat-xlat)<=0.0d0) cycle                             !RDTG110
                    stop ' Bad uv Latitude'                                       !RDTG111
                  end do                                                          !RDTG112
!---              Read tide coefficient data for ZF=height of 1.26 nbar level     !RDTG113
                  read (34, *, end=99) zfdust, ils, ylat, zfa0(i,ls,m,n), zfa1( & !RDTG114
                     i,ls,m,n), zfp1(i,ls,m,n), zfa2(i,ls,m,n), zfp2(i,ls,m,n)    !RDTG115
                  if (dabs(zfdust-dust(m))>0.0d0) stop ' Bad ZF dust value'       !RDTG116
                  if (ils /= lsea) stop ' Bad ZF Ls'                              !RDTG117
                  k1 = Int((zfa0(i,ls,m,n)-75.0d0)/5.0d0)                         !RDTG117a
                  ZFX = zfa0(i,ls,m,n)                                            !RDTG117b
                  zfa0(i,ls,m,n) = ZFX +5.0d0*dLog(FitLat)/                     & !RDTG117c
                    dLog(DTA0(k1,i,ls,m,n)/DTA0(k1+1,i,ls,m,n))                   !RDTG117d
                  if (dabs(ylat-xlat)<=0.0d0) cycle                               !RDTG118
                  stop ' Bad ZF Latitude'                                         !RDTG119
               end do                                                             !RDTG120
            end do                                                                !RDTG121
!---      Set all values at Ls=0 to values at Ls=360                              !RDTG122
            zfa0(:nlatt,0,m,n) = zfa0(:nlatt,12,m,n)                              !RDTG123
            zfa1(:nlatt,0,m,n) = zfa1(:nlatt,12,m,n)                              !RDTG124
            zfp1(:nlatt,0,m,n) = zfp1(:nlatt,12,m,n)                              !RDTG125
            zfa2(:nlatt,0,m,n) = zfa2(:nlatt,12,m,n)                              !RDTG126
            zfp2(:nlatt,0,m,n) = zfp2(:nlatt,12,m,n)                              !RDTG127
            tta0(:nhgtt,:nlatt,0,m,n) = tta0(:nhgtt,:nlatt,12,m,n)                !RDTG128
            tta1(:nhgtt,:nlatt,0,m,n) = tta1(:nhgtt,:nlatt,12,m,n)                !RDTG129
            ttp1(:nhgtt,:nlatt,0,m,n) = ttp1(:nhgtt,:nlatt,12,m,n)                !RDTG130
            tta2(:nhgtt,:nlatt,0,m,n) = tta2(:nhgtt,:nlatt,12,m,n)                !RDTG131
            ttp2(:nhgtt,:nlatt,0,m,n) = ttp2(:nhgtt,:nlatt,12,m,n)                !RDTG132
            pta0(:nhgtt,:nlatt,0,m,n) = pta0(:nhgtt,:nlatt,12,m,n)                !RDTG133
            pta1(:nhgtt,:nlatt,0,m,n) = pta1(:nhgtt,:nlatt,12,m,n)                !RDTG134
            ptp1(:nhgtt,:nlatt,0,m,n) = ptp1(:nhgtt,:nlatt,12,m,n)                !RDTG135
            pta2(:nhgtt,:nlatt,0,m,n) = pta2(:nhgtt,:nlatt,12,m,n)                !RDTG136
            ptp2(:nhgtt,:nlatt,0,m,n) = ptp2(:nhgtt,:nlatt,12,m,n)                !RDTG137
            dta0(:nhgtt,:nlatt,0,m,n) = dta0(:nhgtt,:nlatt,12,m,n)                !RDTG138
            dta1(:nhgtt,:nlatt,0,m,n) = dta1(:nhgtt,:nlatt,12,m,n)                !RDTG139
            dtp1(:nhgtt,:nlatt,0,m,n) = dtp1(:nhgtt,:nlatt,12,m,n)                !RDTG140
            dta2(:nhgtt,:nlatt,0,m,n) = dta2(:nhgtt,:nlatt,12,m,n)                !RDTG141
            dtp2(:nhgtt,:nlatt,0,m,n) = dtp2(:nhgtt,:nlatt,12,m,n)                !RDTG142
            uta0(:nhgtt,:nlatt,0,m,n) = uta0(:nhgtt,:nlatt,12,m,n)                !RDTG143
            uta1(:nhgtt,:nlatt,0,m,n) = uta1(:nhgtt,:nlatt,12,m,n)                !RDTG144
            utp1(:nhgtt,:nlatt,0,m,n) = utp1(:nhgtt,:nlatt,12,m,n)                !RDTG145
            uta2(:nhgtt,:nlatt,0,m,n) = uta2(:nhgtt,:nlatt,12,m,n)                !RDTG146
            utp2(:nhgtt,:nlatt,0,m,n) = utp2(:nhgtt,:nlatt,12,m,n)                !RDTG147
            vta0(:nhgtt,:nlatt,0,m,n) = vta0(:nhgtt,:nlatt,12,m,n)                !RDTG148
            vta1(:nhgtt,:nlatt,0,m,n) = vta1(:nhgtt,:nlatt,12,m,n)                !RDTG149
            vtp1(:nhgtt,:nlatt,0,m,n) = vtp1(:nhgtt,:nlatt,12,m,n)                !RDTG150
            vta2(:nhgtt,:nlatt,0,m,n) = vta2(:nhgtt,:nlatt,12,m,n)                !RDTG151
            vtp2(:nhgtt,:nlatt,0,m,n) = vtp2(:nhgtt,:nlatt,12,m,n)                !RDTG152
!---    Close input file unit numbers                                             !RDTG153
            close(32)                                                             !RDTG154
            close(33)                                                             !RDTG155
            cycle                                                                 !RDTG156
!---    Terminate if not all Ls values processed                                  !RDTG157
   99       continue                                                              !RDTG158
            if (lastls == 360) cycle                                              !RDTG159
            stop ' Incomplete 80-170 km MTGCM data'                               !RDTG160
         end do                                                                   !RDTG161
         close(34)                                                                !RDTG162
      end do                                                                      !RDTG163
      return                                                                      !RDTG164
      end subroutine readtgcm_M10                                                 !RDTG165
!                                                                                 !RDTG166
!-------------------------------------------------------------------------------  !RLPS  1
      SUBROUTINE RELLIPS_M10(LAT, LONW, RREF, Z, GZ, OLDRREF, CTOPOHGT, CALBEDO & !RLPS  2
         , REQUA, RPOLE)                                                          !RLPS  3
!-----------------------------------------------                                  !RLPS  4
!   M o d u l e s                                                                 !RLPS  5
!-----------------------------------------------                                  !RLPS  6
      USE vast_kind_param, ONLY:  DOUBLE                                          !RLPS  7
!...                                                                              !RLPS  8
!...Switches:                                                                     !RLPS  9
!-----------------------------------------------                                  !RLPS 10
!   I n t e r f a c e   B l o c k s                                               !RLPS 11
!-----------------------------------------------                                  !RLPS 12
      USE topoareo_M10_I                                                          !RLPS 13
      IMPLICIT NONE                                                               !RLPS 14
!-----------------------------------------------                                  !RLPS 15
!   D u m m y   A r g u m e n t s                                                 !RLPS 16
!-----------------------------------------------                                  !RLPS 17
      REAL(DOUBLE) , INTENT(IN) :: LAT                                            !RLPS 18
      REAL(DOUBLE) , INTENT(IN)  :: LONW                                          !RLPS 19
      REAL(DOUBLE) , INTENT(INOUT)  :: RREF                                       !RLPS 20
      REAL(DOUBLE) , INTENT(IN) :: Z                                              !RLPS 21
      REAL(DOUBLE) , INTENT(OUT) :: GZ                                            !RLPS 22
      REAL(DOUBLE) , INTENT(OUT) :: OLDRREF                                       !RLPS 23
      REAL(DOUBLE) , INTENT(INOUT)  :: CTOPOHGT                                   !RLPS 24
      REAL(DOUBLE) , INTENT(INOUT)  :: CALBEDO                                    !RLPS 25
      REAL(DOUBLE) , INTENT(IN) :: REQUA                                          !RLPS 26
      REAL(DOUBLE) , INTENT(IN) :: RPOLE                                          !RLPS 27
!-----------------------------------------------                                  !RLPS 28
!   L o c a l   P a r a m e t e r s                                               !RLPS 29
!-----------------------------------------------                                  !RLPS 30
      REAL(DOUBLE), PARAMETER :: J2 = 0.001958616128D0                            !RLPS 31
      REAL(DOUBLE), PARAMETER :: RADEG = 57.29577958D0                            !RLPS 32
      REAL(DOUBLE), PARAMETER :: OMEGA = 0.004061250D0/RADEG                      !RLPS 33
      REAL(DOUBLE), PARAMETER :: GM = 4.2828314258D7                              !RLPS 34
!-----------------------------------------------                                  !RLPS 35
!   L o c a l   V a r i a b l e s                                                 !RLPS 36
!-----------------------------------------------                                  !RLPS 37
      INTEGER :: IAU                                                              !RLPS 38
      REAL(DOUBLE) :: A, B, C, AB, TLAT, XX, YY, RZ, P2                           !RLPS 39
!-----------------------------------------------                                  !RLPS 40
!---  J2 = coefficient of 1st non-spherical gravity term                          !RLPS 41
!---  User-selectable inputs for equatorial and polar ellipsoid radii             !RLPS 42
      A = REQUA                                                                   !RLPS 43
      B = REQUA                                                                   !RLPS 44
      C = RPOLE                                                                   !RLPS 45
!---  Mean equatorial radius for ellipsoid                                        !RLPS 46
      AB = DSQRT(A*B)                                                             !RLPS 47
      TLAT = DTAN(LAT/RADEG)                                                      !RLPS 48
!---  XX, YY = squares of x, y components of local ellipsoid radius               !RLPS 49
      XX = (AB*C)**2/(C**2 + (AB*TLAT)**2)                                        !RLPS 50
      YY = XX*TLAT**2                                                             !RLPS 51
!---  Reference ellipsoid radius                                                  !RLPS 52
      OLDRREF = DSQRT(XX + YY)                                                    !RLPS 53
!---  Get MOLA areoid (Rref)                                                      !RLPS 54
      IAU = 2000                                                                  !RLPS 55
      CALL TOPOAREO_M10 (LAT, LONW, RREF, CTOPOHGT, CALBEDO, IAU)                 !RLPS 56
!---  Rz = total radius to current height z                                       !RLPS 57
      RZ = RREF + Z                                                               !RLPS 58
!---  Acceleration of gravity including J2 and centrifugal terms                  !RLPS 59
      P2 = 1.5D0*DSIN(LAT/RADEG)**2 - 0.5D0                                       !RLPS 60
      GZ = (GM/RZ**2)*(1.0D0 - 3.0D0*J2*(AB/RZ)**2*P2)                            !RLPS 61
      GZ = GZ - 1000.0D0*RZ*(OMEGA*DCOS(LAT/RADEG))**2                            !RLPS 62
      RETURN                                                                      !RLPS 63
      END SUBROUTINE RELLIPS_M10                                                  !RLPS 64
!                                                                                 !RLPS 65
!------------------------------------------------------------------------------   !RSCL  1
      subroutine rescale_M10(x)                                                   !RSCL  2
!-----------------------------------------------                                  !RSCL  3
!   M o d u l e s                                                                 !RSCL  4
!-----------------------------------------------                                  !RSCL  5
      USE vast_kind_param, ONLY:  double                                          !RSCL  6
!...                                                                              !RSCL  7
!...Switches:                                                                     !RSCL  8
      implicit none                                                               !RSCL  9
!-----------------------------------------------                                  !RSCL 10
!   D u m m y   A r g u m e n t s                                                 !RSCL 11
!-----------------------------------------------                                  !RSCL 12
      real(double) , intent(inout) :: x                                           !RSCL 13
!-----------------------------------------------                                  !RSCL 14
!   L o c a l   V a r i a b l e s                                                 !RSCL 15
!-----------------------------------------------                                  !RSCL 16
!-----------------------------------------------                                  !RSCL 17
!---  Puts x into range 0 - 360                                                   !RSCL 18
      x = x/360.0D0 - dint(x/360.0D0) + 1.0D0                                     !RSCL 19
      x = (x - dint(x))*360.0D0                                                   !RSCL 20
      return                                                                      !RSCL 21
      end subroutine rescale_M10                                                  !RSCL 22
!                                                                                 !RSCL 23
!------------------------------------------------------------------------------   !SHFD  1
      subroutine shiftdif_M10(x)                                                  !SHFD  2
!-----------------------------------------------                                  !SHFD  3
!   M o d u l e s                                                                 !SHFD  4
!-----------------------------------------------                                  !SHFD  5
      USE vast_kind_param, ONLY:  double                                          !SHFD  6
!...                                                                              !SHFD  7
!...Switches:                                                                     !SHFD  8
      implicit none                                                               !SHFD  9
!-----------------------------------------------                                  !SHFD 10
!   D u m m y   A r g u m e n t s                                                 !SHFD 11
!-----------------------------------------------                                  !SHFD 12
      real(double) , intent(inout) :: x                                           !SHFD 13
!-----------------------------------------------                                  !SHFD 14
!   L o c a l   V a r i a b l e s                                                 !SHFD 15
!-----------------------------------------------                                  !SHFD 16
!-----------------------------------------------                                  !SHFD 17
!---  Shifts difference x to be +/- and close to 0.0                              !SHFD 18
      if (x > 180.0D0) then                                                       !SHFD 19
         x = x - 360.0D0                                                          !SHFD 20
      else if (x < (-180.0D0)) then                                               !SHFD 21
         x = x + 360.0D0                                                          !SHFD 22
      endif                                                                       !SHFD 23
      return                                                                      !SHFD 24
      end subroutine shiftdif_M10                                                 !SHFD 25
!                                                                                 !SHFD 26
!------------------------------------------------------------------------------   !SLWN  1
      subroutine slopewind_M10(xlat, xlonw, hgtmola, time, umean, vmean, uew,  &  !SLWN  2
         vns, wvert)                                                              !SLWN  3
!-----------------------------------------------                                  !SLWN  4
!   M o d u l e s                                                                 !SLWN  5
!-----------------------------------------------                                  !SLWN  6
      USE vast_kind_param, ONLY:  double                                          !SLWN  7
!---    Analytical slope winds solution from Ye, Segal, and Pielke,               !SLWN  8
!       J. Atmos. Sci., 47, 612, 1990.  (hereafter YSP)                           !SLWN  9
!                                                                                 !SLWN 10
!       Inputs:                                                                   !SLWN 11
!         xlat, xlonW = planeto-centric latitude, longitude(West), deg            !SLWN 12
!         hgtMOLA = planeto-centric height above MOLA areoid (km)                 !SLWN 13
!         time    = local solar time (Mars hours)                                 !SLWN 14
!       Outputs:                                                                  !SLWN 15
!         uew, vns = eastward, northward wind components (from MOLA               !SLWN 16
!           slopes in eastward and northward directions)                          !SLWN 17
!                                                                                 !SLWN 18
!...                                                                              !SLWN 19
!...Switches:                                                                     !SLWN 20
!-----------------------------------------------                                  !SLWN 21
!   I n t e r f a c e   B l o c k s                                               !SLWN 22
!-----------------------------------------------                                  !SLWN 23
      use topoareo_M10_I                                                          !SLWN 24
      implicit none                                                               !SLWN 25
!-----------------------------------------------                                  !SLWN 26
!   D u m m y   A r g u m e n t s                                                 !SLWN 27
!-----------------------------------------------                                  !SLWN 28
      real(double) , intent(in)  :: xlat                                          !SLWN 29
      real(double) , intent(in)  :: xlonw                                         !SLWN 30
      real(double) , intent(in) :: hgtmola                                        !SLWN 31
      real(double) , intent(in) :: time                                           !SLWN 32
      real(double) , intent(in) :: umean                                          !SLWN 33
      real(double) , intent(in) :: vmean                                          !SLWN 34
      real(double) , intent(out) :: uew                                           !SLWN 35
      real(double) , intent(out) :: vns                                           !SLWN 36
      real(double) , intent(out) :: wvert                                         !SLWN 37
!-----------------------------------------------                                  !SLWN 38
!   L o c a l   V a r i a b l e s                                                 !SLWN 39
!-----------------------------------------------                                  !SLWN 40
      integer :: iau                                                              !SLWN 41
      real(double) :: pi180, cosfact, h, rref, ctopohgt, calbedo, hgtsfc, c0,  &  !SLWN 42
         qslam, xi, xlatp, xlatm, xlonp, xlonm, rp, topop, rm, topom, alphay,  &  !SLWN 43
         alphayw, alphax, alphaxw, fupslp, fcross, omega, f, wb, b, p, b1, b2, &  !SLWN 44
         b3, b4, zfactor                                                          !SLWN 45
!-----------------------------------------------                                  !SLWN 46
      pi180 = datan(1.0D0)/45.0D0                                                 !SLWN 47
      iau = 2000                                                                  !SLWN 48
!---    Cosine factor for diurnal time dependence                                 !SLWN 49
      cosfact = dcos(15.0D0*pi180*(time - 15.0D0))                                !SLWN 50
!---    Assumed diurnal variation in BL height                                    !SLWN 51
      h = 3.5D0 + cosfact                                                         !SLWN 52
!---    Get MOLA areoid (Rref, km)                                                !SLWN 53
      call topoareo_M10 (xlat, xlonw, rref, ctopohgt, calbedo, iau)               !SLWN 54
!---    Get height above surface                                                  !SLWN 55
      hgtsfc = hgtmola - ctopohgt                                                 !SLWN 56
!---    Zero slope winds at and below surface and above 4.5 km                    !SLWN 57
      if (hgtsfc<=0.0 .or. hgtsfc>=h) then                                        !SLWN 58
         uew = 0.0D0                                                              !SLWN 59
         vns = 0.0D0                                                              !SLWN 60
         wvert = 0.0D0                                                            !SLWN 61
         return                                                                   !SLWN 62
      else                                                                        !SLWN 63
!---      Set typical values of parameters from YSP                               !SLWN 64
         c0 = 0.07D0                                                              !SLWN 65
!---      Assumed diurnal variation in magnitude of Qs                            !SLWN 66
         qslam = 550.0D0 + 150.0D0*cosfact                                        !SLWN 67
!---      Normalized height above surface                                         !SLWN 68
         xi = hgtsfc/h                                                            !SLWN 69
!---      Get northward slope (alphay) from MOLA                                  !SLWN 70
         xlatp = xlat + 0.25D0                                                    !SLWN 71
         xlatm = xlat - 0.25D0                                                    !SLWN 72
         xlonp = xlonw                                                            !SLWN 73
         xlonm = xlonw                                                            !SLWN 74
         if (xlatp > 90.0D0) then                                                 !SLWN 75
            xlatp = 180.0D0 - xlatp                                               !SLWN 76
            xlonp = xlonp + 180.0D0                                               !SLWN 77
            if (xlonp > 360.0D0) xlonp = xlonp - 360.0D0                          !SLWN 78
         endif                                                                    !SLWN 79
         if (xlatm < (-90.0D0)) then                                              !SLWN 80
            xlatm = (-180.0D0) - xlatm                                            !SLWN 81
            xlonm = xlonm + 180.0D0                                               !SLWN 82
            if (xlonm > 360.0D0) xlonm = xlonm - 360.0D0                          !SLWN 83
         endif                                                                    !SLWN 84
         call topoareo_M10 (xlatp, xlonp, rp, topop, calbedo, iau)                !SLWN 85
         call topoareo_M10 (xlatm, xlonm, rm, topom, calbedo, iau)                !SLWN 86
         alphay = (topop - topom)/(0.5D0*pi180*rref)                              !SLWN 87
         alphayw = alphay                                                         !SLWN 88
!---      Limit slope to ~ 3 degrees (and use sin(alpha) ~ alpha)                 !SLWN 89
         if (dabs(alphay) > 0.0525D0) alphay = dsign(0.0525D0,alphay)             !SLWN 90
!---      Get eastward slope (alphax) from MOLA                                   !SLWN 91
         if (dabs(xlat) > 89.75D0) then                                           !SLWN 92
            alphax = 0.0D0                                                        !SLWN 93
         else                                                                     !SLWN 94
            xlonp = xlonw - 0.25D0                                                !SLWN 95
            xlonm = xlonw + 0.25D0                                                !SLWN 96
            if (xlonp < 0.0D0) xlonp = xlonp + 360.0D0                            !SLWN 97
            if (xlonm > 360.0D0) xlonm = xlonm - 360.0D0                          !SLWN 98
            call topoareo_M10 (xlat, xlonp, rp, topop, calbedo, iau)              !SLWN 99
            call topoareo_M10 (xlat, xlonm, rm, topom, calbedo, iau)              !SLWN100
            alphax = (topop - topom)/(0.5D0*pi180*dcos(pi180*xlat)*rref)          !SLWN101
            alphaxw = alphax                                                      !SLWN102
!---        Limit slope to ~ 3 degrees (and use sin(alpha) ~ alpha)               !SLWN103
            if (dabs(alphax) > 0.0525D0) alphax = dsign(0.0525D0,alphax)          !SLWN104
         endif                                                                    !SLWN105
!---      For xlat ~ 0 use YSP equation (12)                                      !SLWN106
         if (dabs(xlat) < 0.01D0) then                                            !SLWN107
            fupslp = (qslam/c0)*((2.0D0 + xi**2)/3.0D0 - xi)*xi                   !SLWN108
            fcross = 0.0D0                                                        !SLWN109
         else                                                                     !SLWN110
!---        For xlat ne 0, use YSP equations (13) and (14)                        !SLWN111
            omega = 7.0777D-5                                                     !SLWN112
            f = 2.0D0*omega*dsin(pi180*xlat)                                      !SLWN113
            wb = 2.0D0*qslam/(1000.0D0*h*dabs(f))                                 !SLWN114
            b = dsqrt(1000.0D0*dabs(f)*h/(2.0D0*c0))                              !SLWN115
            p = dexp((-4.0D0*b)) - 2.0D0*dexp((-2.0D0*b))*dcos(2.0D0*b) + 1.0D0   !SLWN116
            b1 = dexp((-b*xi))                                                    !SLWN117
            b2 = dexp((-b*(2.0D0 + xi)))                                          !SLWN118
            b3 = dexp((-b*(2.0D0 - xi)))                                          !SLWN119
            b4 = dexp((-b*(4.0D0 - xi)))                                          !SLWN120
!---        Upslope wind factor (YSP equation 13)                                 !SLWN121
            fupslp = wb*((b1 - b4)*dsin(b*xi) + (b2 - b3)*dsin(b*(2.0D0 - xi))) & !SLWN122
               /p                                                                 !SLWN123
!---        Cross-slope wind factor (YSP equation 14)                             !SLWN124
            fcross = (f/dabs(f))*wb*(xi - 1.0D0 + ((b1 + b4)*dcos(b*xi) - (b2   & !SLWN125
                + b3)*dcos(b*(2.0D0 - xi)))/p)                                    !SLWN126
         endif                                                                    !SLWN127
!---      Eastward and northward wind components from up-slope and                !SLWN128
!         cross-slope components and eastward and northward slopes                !SLWN129
         uew = fupslp*alphax + fcross*alphay                                      !SLWN130
         vns = fupslp*alphay - fcross*alphax                                      !SLWN131
      endif                                                                       !SLWN132
!---    Compute assumed time-of-day dependence on wind components                 !SLWN133
      uew = uew*cosfact                                                           !SLWN134
      vns = vns*dsin(15.0D0*pi180*(time - 11.0D0))                                !SLWN135
      zfactor = 1.0D0                                                             !SLWN136
      if (xi >= 0.5D0) zfactor = 0.5D0*(1.0D0 + dcos(pi180*360.0D0*(xi - 0.5D0) & !SLWN137
         ))                                                                       !SLWN138
      wvert = zfactor*(alphaxw*(umean + uew) + alphayw*(vmean + vns))             !SLWN139
      return                                                                      !SLWN140
      end subroutine slopewind_M10                                                !SLWN141
!                                                                                 !SLWN142
!------------------------------------------------------------------------------   !SPEC  1
      subroutine species_M10(hgt, xlat, als, zbase, amz, dens, pres, temp, iup  & !SPEC  2
         , fmol, fmass, fmolh2o, fmassh2o)                                        !SPEC  3
!-----------------------------------------------                                  !SPEC  4
!   M o d u l e s                                                                 !SPEC  5
!-----------------------------------------------                                  !SPEC  6
      USE vast_kind_param, ONLY:  double                                          !SPEC  7
!                                                                                 !SPEC  8
!---  Computes species concentrations as mole (or volume) fraction,               !SPEC  9
!     mass fraction, and number density (#/m**3).  Average mole                   !SPEC 10
!     fraction and isotope ratio data are taken from Kieffer et al.,              !SPEC 11
!     editors, "Mars" (1992) and Tables A-5 and  A-6 of NASA/TM-2001-             !SPEC 12
!     210935 (2001).                                                              !SPEC 13
!                                                                                 !SPEC 14
!     Notes:                                                                      !SPEC 15
!                                                                                 !SPEC 16
!     (1) Below 80 km, Mars MGCM assumes pure CO2 atmosphere (for                 !SPEC 17
!     which molecular weight would be 44.01).  In this height range,              !SPEC 18
!     molecular weight computed from the perfect gas law relation                 !SPEC 19
!     M = R0 * rho * T / p would give values close to, but not exactly            !SPEC 20
!     this value.  Deviations would be caused by the fact that the                !SPEC 21
!     ratio of the averages is not the same as the average of the                 !SPEC 22
!     ratio, i.e.                                                                 !SPEC 23
!          Avg(rho) * Avg(T) / Avg(p)     /=     Avg( rho * T / p)                !SPEC 24
!                                                                                 !SPEC 25
!     (2) Below 80 km, this subroutine computes species concentrations            !SPEC 26
!     and resultant average molecular weight by assumptions given in              !SPEC 27
!     note (3). Therefore average molecular weight given by this                  !SPEC 28
!     subroutine is not exactly the same as average molecular weight              !SPEC 29
!     computed from the perfect gas law [as given in note (1)].                   !SPEC 30
!                                                                                 !SPEC 31
!     (3) Below 80 km, this subroutine computes species concentrations            !SPEC 32
!     by assuming atmospheric mass density from MGCM is correct,                  !SPEC 33
!     but species concentrations are calculated using the following               !SPEC 34
!     assumptions: (a) Average dry-atmosphere mole fractions (fbar),              !SPEC 35
!     are assumed. (b) Mole fractions are adjusted for seasonal                   !SPEC 36
!     variation of mass of CO2 in the atmosphere, due to freezing and             !SPEC 37
!     sublimation of CO2 at the poles. (c) Only the partial pressure              !SPEC 38
!     of CO2 is assumed to vary seasonally, not the partial pressures             !SPEC 39
!     of other constituents, which are assumed not to freeze out of               !SPEC 40
!     the atmosphere. However, mole fractions of all species vary                 !SPEC 41
!     seasonally due to this effect. (d) Seasonal variation of                    !SPEC 42
!     total pressure is taken from subroutine PRSEAS_M10, which was               !SPEC 43
!     used in the original Stewart thermosphere model, and was                    !SPEC 44
!     employed in Mars-GRAM up through version 3.8.  (e) Water vapor              !SPEC 45
!     concentration is added to the dry atmosphere by assuming relative           !SPEC 46
!     humidity = 20% (computed by function qrhtp, the same as used in             !SPEC 47
!     marsrad.f calculations).                                                    !SPEC 48
!                                                                                 !SPEC 49
!     (4) Between 80 km and the base of the Stewart thermosphere (at              !SPEC 50
!     altitude zbase), a combination of information is used from MTGCM            !SPEC 51
!     and the modified Stewart model. MTGCM data used in Mars-GRAM do             !SPEC 52
!     not include calculated mole fractions or number densities.  Mole            !SPEC 53
!     fractions between 80 km and zbase height are computed from the              !SPEC 54
!     following assumptions: (a) Mole fractions for N2, Ar, and O2 are            !SPEC 55
!     assumed to be the same as their value at 80 km (from methods                !SPEC 56
!     described in note (3). (b) Mole fractions for He, H2, H, and H2O            !SPEC 57
!     are assumed to be zero. (c) Mole fractions for N2, Ar, O2, and CO           !SPEC 58
!     are assumed to vary linearly from their values at 80 km [from               !SPEC 59
!     method in note (3)] to their values at zbase altitude (from the             !SPEC 60
!     Stewart model). (d) Mole fractions for CO2 and O are computed               !SPEC 61
!     from two constraint conditions - (i) Sum of the mole fractions              !SPEC 62
!     must be 1.0, and (ii) Input molecular weight (AMz, from MTGCM               !SPEC 63
!     value) is preserved.                                                        !SPEC 64
!                                                                                 !SPEC 65
!     (5) From height zbase up until MTGCM data runs out, a combination           !SPEC 66
!     of information is used from MTGCM and the modified Stewart model.           !SPEC 67
!     In this height range, the following assumptions are used: (a)               !SPEC 68
!     Mole fractions for constituents other than CO2 and O are taken              !SPEC 69
!     from the modified Stewart model, (b) Molecular weight from MTGCM            !SPEC 70
!     is assumed, (c)  Mole fractions for CO2 and O are computed from             !SPEC 71
!     two constraint conditions - (i) Sum of the mole fractions must              !SPEC 72
!     be 1.0, and (ii) Input molecular weight (AMz, from MTGCM value)             !SPEC 73
!     is preserved.                                                               !SPEC 74
!                                                                                 !SPEC 75
!     (6) Above the top altitude for which MTGCM data are available,              !SPEC 76
!     the same methodology is used as in note (5), except that the                !SPEC 77
!     input value for molecular weight (AMz) is taken directly from               !SPEC 78
!     the modified Stewart model.                                                 !SPEC 79
!                                                                                 !SPEC 80
!...Switches:                                                                     !SPEC 82
!-----------------------------------------------                                  !SPEC 83
!   I n t e r f a c e   B l o c k s                                               !SPEC 84
!-----------------------------------------------                                  !SPEC 85
      use prseas_M10_I                                                            !SPEC 86
      use qrhtp_M10_I                                                             !SPEC 87
      implicit none                                                               !SPEC 88
!-----------------------------------------------                                  !SPEC 89
!   D u m m y   A r g u m e n t s                                                 !SPEC 90
!-----------------------------------------------                                  !SPEC 91
      integer , intent(in) :: iup                                                 !SPEC 92
      real(double) , intent(in) :: hgt                                            !SPEC 93
      real(double) , intent(in)  :: xlat                                          !SPEC 94
      real(double) , intent(in)  :: als                                           !SPEC 95
      real(double) , intent(in) :: zbase                                          !SPEC 96
      real(double) , intent(inout) :: amz                                         !SPEC 97
      real(double) , intent(in) :: dens                                           !SPEC 98
      real(double) , intent(in) :: pres                                           !SPEC 99
      real(double) , intent(in)  :: temp                                          !SPEC100
      real(double) , intent(out) :: fmolh2o                                       !SPEC101
      real(double) , intent(out) :: fmassh2o                                      !SPEC102
      real(double) , intent(out) :: fmol(0:8)                                     !SPEC103
      real(double) , intent(out) :: fmass(0:8)                                    !SPEC104
!-----------------------------------------------                                  !SPEC105
!   L o c a l   V a r i a b l e s                                                 !SPEC106
!-----------------------------------------------                                  !SPEC107
      integer :: i                                                                !SPEC108
      real(double), dimension(0:8) :: amx, fbar, xnd                              !SPEC109
      real(double) :: avn, amh2o, fn2in, farin, fo2in, fcoin, xndh2o, pr,      &  !SPEC110
         fmoltot, hgtfact, fsum, fmsum, d, totnd, sum                             !SPEC111
!-----------------------------------------------                                  !SPEC112
!---  Molecular weights computed using isotope ratios applicable for              !SPEC113
!     Mars atmosphere. See Kieffer et al., editors, "Mars" (1992) and             !SPEC114
!     Table A-6 of NASA/TM-2001-210935 (2001)                                     !SPEC115
      data amx/ 44.00903D0, 28.01781D0, 39.96093D0, 31.99799D0, 28.01003D0,    &  !SPEC116
         15.99900D0, 4.00260D0, 2.01746D0, 1.00873D0/                             !SPEC117
!---  Assumed average mole fractions for dry Mars atmosphere below 80             !SPEC118
!     km altitude.  See Kieffer et al., editors, "Mars" (1992) and                !SPEC119
!     Table A-5 of NASA/TM-2001-210935 (2001)                                     !SPEC120
      data fbar/ 0.9537D0, 0.0275D0, 0.0165D0, 0.0013D0, 0.0010D0, 4*0.0D0/       !SPEC121
!---  Avaogadro's number (number per kg-mole)                                     !SPEC122
      avn = 6.02214D26                                                            !SPEC123
!---  Molecular weight for water vapor                                            !SPEC124
      amh2o = 18.01646D0                                                          !SPEC125
!---  Store input values of N2, Ar, O2, and CO mole fraction (= values            !SPEC126
!     at height zbase if input height is between 80 km and zbase)                 !SPEC127
      fn2in = fmol(1)                                                             !SPEC128
      farin = fmol(2)                                                             !SPEC129
      fo2in = fmol(3)                                                             !SPEC130
      fcoin = fmol(4)                                                             !SPEC131
!---  Assume dry atmosphere, unless modified for heights up to 80 km              !SPEC132
      fmassh2o = 0.0D0                                                            !SPEC133
      fmolh2o = 0.0D0                                                             !SPEC134
      xndh2o = 0.0D0                                                              !SPEC135
      if (hgt <= zbase) then                                                      !SPEC136
         call prseas_M10 (als, xlat, pr)                                          !SPEC137
         fmol(0) = 1.0D0 - (1.0D0 - fbar(0))/pr                                   !SPEC138
         fmoltot = fmol(0)                                                        !SPEC139
         do i = 1, 8                                                              !SPEC140
            fmol(i) = fbar(i)/pr                                                  !SPEC141
            fmoltot = fmoltot + fmol(i)                                           !SPEC142
         end do                                                                   !SPEC143
!---    Calculations for heights up to 80 km                                      !SPEC144
         if (hgt <= 80.0D0) then                                                  !SPEC145
!---      Molecular weight of dry atmosphere                                      !SPEC146
            amz = dot_product(fmol(:8),amx(:8))                                   !SPEC147
!---      Water vapor mass mixing ratio (kg-water/kg-dry atmosphere)              !SPEC148
            fmassh2o = qrhtp_M10(0.2D0,temp,pres/100.0D0)/1000.0D0                !SPEC149
!---      Water vapor mole fraction                                               !SPEC150
            fmolh2o = amz*fmassh2o/amh2o                                          !SPEC151
!---      Rescale total mole fraction                                             !SPEC152
            fmoltot = fmoltot + fmolh2o                                           !SPEC153
!---      Reclculate mole fractions and molecular weight of moist                 !SPEC154
!         atmosphere                                                              !SPEC155
            fmol(:8) = fmol(:8)/fmoltot                                           !SPEC156
            amz = dot_product(fmol(:8),amx(:8))                                   !SPEC157
            amz = amz + fmolh2o*amh2o                                             !SPEC158
!---      Water vapor number density                                              !SPEC159
            xndh2o = fmolh2o*dens*avn/amz                                         !SPEC160
!---    Calculations for heights between 80 km and zbase altitude                 !SPEC161
         else                                                                     !SPEC162
!---      Assume N2, Ar, O2, and CO mole fractions varies linearly                !SPEC163
!         between 80 km and zbase altitude                                        !SPEC164
            hgtfact = (hgt - 80.0D0)/(zbase - 80.0D0)                             !SPEC165
            fmol(1) = fmol(1) + (fn2in - fmol(1))*hgtfact                         !SPEC166
            fmol(2) = fmol(2) + (farin - fmol(2))*hgtfact                         !SPEC167
            fmol(3) = fmol(3) + (fo2in - fmol(3))*hgtfact                         !SPEC168
            fmol(4) = fmol(4) + (fcoin - fmol(4))*hgtfact                         !SPEC169
!---      Solve for CO2 and O mole fractions using the following                  !SPEC170
!         assumptions: (a) sum of mole fractions must be 1.0, and                 !SPEC171
!         (b) input value of molecular weight (AMz) is preserved.                 !SPEC172
!---      Set up two simultaneous linear equations from these                     !SPEC173
!         assumptions.                                                            !SPEC174
            fsum = 1.0D0 - fmol(1) - fmol(2) - fmol(3) - fmol(4) - fmol(6) -    & !SPEC175
               fmol(7) - fmol(8)                                                  !SPEC176
            fmsum = amz - fmol(1)*amx(1) - fmol(2)*amx(2) - fmol(3)*amx(3) -    & !SPEC177
               fmol(4)*amx(4) - fmol(6)*amx(6) - fmol(7)*amx(7) - fmol(8)*amx(8 & !SPEC178
               )                                                                  !SPEC179
            d = amx(5) - amx(0)                                                   !SPEC180
!---      Re-computed mole fractions for CO2 and O from two linear                !SPEC181
!         constraint equations                                                    !SPEC182
            fmol(0) = (fsum*amx(5)-fmsum)/d                                       !SPEC183
            fmol(5) = (fmsum - amx(0)*fsum)/d                                     !SPEC184
            if (fmol(5) < 0.0D0) then                                             !SPEC185
               fmol(5) = 0.0D0                                                    !SPEC186
               fmol(0) = fsum                                                     !SPEC187
               amz = fmol(0)*amx(0) + amz - fmsum                                 !SPEC188
            endif                                                                 !SPEC189
         endif                                                                    !SPEC190
!---  Calculations for heights above zbase altitude                               !SPEC191
      else                                                                        !SPEC192
!---    Solve for CO2 and O mole fractions using the following                    !SPEC193
!       assumptions: (a) sum of mole fractions must be 1.0, and                   !SPEC194
!       (b) input value of molecular weight (AMz) is preserved.                   !SPEC195
!---    Set up two simultaneous linear equations from these                       !SPEC196
!       assumptions.                                                              !SPEC197
         fsum = 1.0D0 - fmol(1) - fmol(2) - fmol(3) - fmol(4) - fmol(6) - fmol( & !SPEC198
            7) - fmol(8)                                                          !SPEC199
         fmsum = amz - fmol(1)*amx(1) - fmol(2)*amx(2) - fmol(3)*amx(3) - fmol( & !SPEC200
            4)*amx(4) - fmol(6)*amx(6) - fmol(7)*amx(7) - fmol(8)*amx(8)          !SPEC201
         d = amx(5) - amx(0)                                                      !SPEC202
!---    Re-computed mole fractions for CO2 and O from two linear                  !SPEC203
!       constraint equations                                                      !SPEC204
         fmol(0) = (fsum*amx(5)-fmsum)/d                                          !SPEC205
         fmol(5) = (fmsum - amx(0)*fsum)/d                                        !SPEC206
         if (fmol(5) < 0.0D0) then                                                !SPEC207
            fmol(5) = 0.0D0                                                       !SPEC208
            fmol(0) = fsum                                                        !SPEC209
            amz = fmol(0)*amx(0) + amz - fmsum                                    !SPEC210
         endif                                                                    !SPEC211
         if (fmol(0) < 0.0D0) then                                                !SPEC212
            fmol(0) = 0.0D0                                                       !SPEC213
            fmol(5) = fsum                                                        !SPEC214
            amz = fmol(5)*amx(5) + amz - fmsum                                    !SPEC215
         endif                                                                    !SPEC216
      endif                                                                       !SPEC217
!---  Calculation of number densities and mass fractions (applicable              !SPEC218
!     for all height ranges)                                                      !SPEC219
      totnd = 0.0D0                                                               !SPEC220
      sum = 0.0D0                                                                 !SPEC221
!---  Species number densities and total number density                           !SPEC222
      do i = 0, 8                                                                 !SPEC223
         xnd(i) = fmol(i)*dens*avn/amz                                            !SPEC224
         totnd = totnd + xnd(i)                                                   !SPEC225
         sum = sum + xnd(i)*amx(i)                                                !SPEC226
      end do                                                                      !SPEC227
!---  Add water vapor number density to total                                     !SPEC228
      totnd = totnd + xndh2o                                                      !SPEC229
!---  Compute mass fraction in % and rescale mole fraction to %                   !SPEC230
      sum = sum + xndh2o*amh2o                                                    !SPEC231
      do i = 0,8                                                                  !SPEC232
        fmass(i) = 100.0D0*xnd(i)*amx(i)/sum                                      !SPEC233
        fmol(i) = 100.0D0*fmol(i)                                                 !SPEC234
      enddo                                                                       !SPEC235
      if (iup > 0) then                                                           !SPEC236
!---    Write species concentration data to LIST.txt file                         !SPEC237
         write (iup, 40) (xnd(i),i=0,4), (fmass(i),i=0,4), (fmol(i),i=0,4)        !SPEC238
!---    Water vapor not included above 80 km                                      !SPEC239
         if (hgt > 80.0D0) then                                                   !SPEC240
            write (iup, 50) (xnd(i),i=5,8), totnd, (fmass(i),i=5,8), amz, (fmol & !SPEC241
               (i),i=5,8)                                                         !SPEC242
         else                                                                     !SPEC243
!---      Water vapor included below 80 km                                        !SPEC244
            fmassh2o = 100.0D0*fmassh2o                                           !SPEC245
            fmolh2o = 100.0D0*fmolh2o                                             !SPEC246
            write (iup, 60) xndh2o, (xnd(i),i=5,7), totnd, fmassh2o, (fmass(i), & !SPEC247
               i=5,7), amz, fmolh2o, (fmol(i),i=5,7)                              !SPEC248
         endif                                                                    !SPEC249
      endif                                                                       !SPEC250
   40 format(' CO2=',1p,e9.3,' N2=',e9.3,' Ar=',e9.3,' O2=',e9.3,' CO=',e9.3,   & !SPEC251
         ' #/m**3'/,0p,f14.3,4f13.3,' % by mass'/,f14.3,4f13.3,' % by volume')    !SPEC252
   50 format('   O=',1p,e9.3,' He=',e9.3,' H2=',e9.3,'  H=',e9.3,'   Total=',   & !SPEC253
         e9.3,' #/m**3'/,0p,f14.3,3f13.3,' % by mass  ','MolWgt=',f6.3,/,f14.3, & !SPEC254
         3f13.3,' % volume (mole) fraction')                                      !SPEC255
   60 format(' H2O=',1p,e9.3,'  O=',e9.3,' He=',e9.3,' H2=',e9.3,'   Total=',   & !SPEC256
         e9.3,' #/m**3'/,0p,f14.3,3f13.3,' % by mass  ','MolWgt=',f6.3,/,f14.3, & !SPEC257
         3f13.3,' % volume (mole) fraction')                                      !SPEC258
      return                                                                      !SPEC259
      end subroutine species_M10                                                  !SPEC260
!                                                                                 !SPEC261
!------------------------------------------------------------------------------   !STCK  1
      subroutine subltchk_M10(temp, pres, tsubl)                                  !STCK  2
!-----------------------------------------------                                  !STCK  3
!   M o d u l e s                                                                 !STCK  4
!-----------------------------------------------                                  !STCK  5
      USE vast_kind_param, ONLY:  double                                          !STCK  6
!---  Assure temperatures greater than sublimation point for CO2.                 !STCK  7
!     Coefficients from Kieffer et al. eds. "Mars" (Univ. Arizona                 !STCK  8
!     Press book) 1992, page 959.                                                 !STCK  9
!     Inputs are temp = temperature (K), pres = pressure (N/m**2)                 !STCK 10
!...                                                                              !STCK 11
!...Switches:                                                                     !STCK 12
      implicit none                                                               !STCK 13
!-----------------------------------------------                                  !STCK 14
!   D u m m y   A r g u m e n t s                                                 !STCK 15
!-----------------------------------------------                                  !STCK 16
      real(double) , intent(inout) :: temp                                        !STCK 17
      real(double) , intent(in) :: pres                                           !STCK 18
      real(double) , intent(out) :: tsubl                                         !STCK 19
!-----------------------------------------------                                  !STCK 20
!   L o c a l   V a r i a b l e s                                                 !STCK 21
!-----------------------------------------------                                  !STCK 22
!-----------------------------------------------                                  !STCK 23
      tsubl = 3182.48D0/(23.3494D0 - dlog(pres/100.0D0))                          !STCK 24
      temp = dmax1(tsubl,temp)                                                    !STCK 25
      return                                                                      !STCK 26
      end subroutine subltchk_M10                                                 !STCK 27
!                                                                                 !STCK 28
!------------------------------------------------------------------------------   !STRP  1
      subroutine surfterp_M10(khgt, time, tmgcm, pmgcm, dmgcm, umgcm, vmgcm,   &  !STRP  2
         hpres, hdens, ctopohgt, tempday, presday, densday, uwndday, vwndday,  &  !STRP  3
         hpres0, tempmax, tempmin, densmax, densmin, tat5m, idaydata)             !STRP  4
!-----------------------------------------------                                  !STRP  5
!   M o d u l e s                                                                 !STRP  6
!-----------------------------------------------                                  !STRP  7
      USE vast_kind_param, ONLY:  double                                          !STRP  8
      USE surfdata_M10_C                                                          !STRP  9
      USE mgcmdata_M10_C                                                          !STRP 10
      USE interp_M10_C                                                            !STRP 11
      USE mgcmparm_M10_C                                                          !STRP 12
      USE parameters_M10_C                                                        !STRP 13
!-----------------------------------------------                                  !STRP 14
!---    Interpolates Ames Mars General Circulation Model (MGCM) surface           !STRP 15
!       data to a given latitude, longitude, time of year (Ls), and               !STRP 16
!       dust optical depth, for a given height index (khgt) and time of           !STRP 17
!       day (time).                                                               !STRP 18
!       Some input data is provided by the Common "Interp".                       !STRP 19
!---    Set parameter values for number of heights, boundary layer                !STRP 20
!       levels, latitudes, longitudes, and number of dust optical depth           !STRP 21
!       values                                                                    !STRP 22
!...                                                                              !STRP 23
!...Switches:                                                                     !STRP 24
!-----------------------------------------------                                  !STRP 25
!   I n t e r f a c e   B l o c k s                                               !STRP 26
!-----------------------------------------------                                  !STRP 27
      use tidex_M10_I                                                             !STRP 28
      use fourd_M10_I                                                             !STRP 29
      use tidey_M10_I                                                             !STRP 30
      use threed_M10_I                                                            !STRP 31
      use zlogr_M10_I                                                             !STRP 32
      implicit none                                                               !STRP 33
!-----------------------------------------------                                  !STRP 34
!   D u m m y   A r g u m e n t s                                                 !STRP 35
!-----------------------------------------------                                  !STRP 36
      integer , intent(in) :: khgt                                                !STRP 37
      integer , intent(in) :: idaydata                                            !STRP 38
      real(double) , intent(in)  :: time                                          !STRP 39
      real(double) , intent(out)  :: tmgcm                                        !STRP 40
      real(double) , intent(out) :: pmgcm                                         !STRP 41
      real(double) , intent(out) :: dmgcm                                         !STRP 42
      real(double) , intent(out)  :: umgcm                                        !STRP 43
      real(double) , intent(out)  :: vmgcm                                        !STRP 44
      real(double) , intent(out) :: hpres                                         !STRP 45
      real(double) , intent(out) :: hdens                                         !STRP 46
      real(double) , intent(in) :: ctopohgt                                       !STRP 47
      real(double) , intent(out)  :: tempday                                      !STRP 48
      real(double) , intent(out) :: presday                                       !STRP 49
      real(double) , intent(out) :: densday                                       !STRP 50
      real(double) , intent(out)  :: uwndday                                      !STRP 51
      real(double) , intent(out)  :: vwndday                                      !STRP 52
      real(double) , intent(out) :: hpres0                                        !STRP 53
      real(double) , intent(out)  :: tempmax                                      !STRP 54
      real(double) , intent(out)  :: tempmin                                      !STRP 55
      real(double) , intent(out) :: densmax                                       !STRP 56
      real(double) , intent(out) :: densmin                                       !STRP 57
      real(double) , intent(inout) :: tat5m                                       !STRP 58
!-----------------------------------------------                                  !STRP 59
!   L o c a l   V a r i a b l e s                                                 !STRP 60
!-----------------------------------------------                                  !STRP 61
      integer :: i, j, l, m, itime, k1h                                           !STRP 62
      real(double), dimension(2,2,2,2) :: tm, um, vm, ts0                         !STRP 63
      real(double), dimension(2,2,2) :: tz1, pz1, rz0, pzh, pzh1, tz0, pz0, dzh & !STRP 64
         , dzh1                                                                   !STRP 65
      real(double), dimension(2,2,2,2) :: tday, uday, vday, tmax, tmin            !STRP 66
      real(double), dimension(2,2,2) :: pmax, pmin, t1max, t1min                  !STRP 67
      real(double) :: polefac, upolefac, t0, a1t, p1t, a2t, p2t, xtime, ttime,  & !STRP 68
         u0, a1, p1, a2, p2, v0, tszero, p0, ptime, t1time, d0, pzk1h, pzk1h1,  & !STRP 69
         dzk1h, dzk1h1, hdens0, tzero, pzero, tbar0, rzero, t1st, p1st, p1max,  & !STRP 70
         p1min, tmax1, tmin1, tbar, height, z1st, hpmin, hpmax                    !STRP 71
!-----------------------------------------------                                  !STRP 72
!---    MGCM surface data arrays                                                  !STRP 73
!---    MGCM 0-80 km data arrays                                                  !STRP 74
!---    Establish MGCM surface values at corners of a 4-dimensional               !STRP 75
!       cube in latitude-longitude-Ls-dust space, at a given height               !STRP 76
!       index (khgt) and time of day (time)                                       !STRP 77
      do i = 1, 2                                                                 !STRP 78
         polefac = 1.0D0                                                          !STRP 79
         upolefac = 1.0D0                                                         !STRP 80
         if (ilat == 1) then                                                      !STRP 81
            polefac = i - 1.0D0                                                   !STRP 82
         else if (ilat == nlat - 1) then                                          !STRP 83
            polefac = 2.0D0 - i                                                   !STRP 84
         endif                                                                    !STRP 85
         if (ilatw == 2) then                                                     !STRP 86
            if (i == 1) upolefac = wpolefac                                       !STRP 87
         else if (ilatw == nlat - 1) then                                         !STRP 88
            if (i == 2) upolefac = wpolefac                                       !STRP 89
         endif                                                                    !STRP 90
         do j = 1, 2                                                              !STRP 91
            do l = 1, 2                                                           !STRP 92
               do m = 1, 2                                                        !STRP 93
!---      Daily mean temperature at level khgt                                    !STRP 94
                  t0 = tsa0(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1)              !STRP 95
                  tday(i,j,l,m) = t0                                              !STRP 96
!---      Temperature tide amplitudes and phases                                  !STRP 97
                  a1t = tsa1(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1)*polefac     !STRP 98
                  p1t = tsp1(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP 99
                  a2t = tsa2(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1)*polefac     !STRP100
                  p2t = tsp2(khgt,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP101
!---      Temperature at corners of 4-D cube                                      !STRP102
                  tm(i,j,l,m) = tidex_M10(t0,a1t,p1t,a2t,p2t,time)                !STRP103
!---      Daily mean temperature at surface                                       !STRP104
                  ts0(i,j,l,m) = tsa0(1,ilat+i-1,jlon+j-1,ls+l-1,mdust+m-1)       !STRP105
!---      Max and Min temperatures at corners of 4-D cube                         !STRP106
                  tmax(i,j,l,m) = -9999.0D0                                       !STRP107
                  tmin(i,j,l,m) = 9999.0D0                                        !STRP108
                  if (idaydata > 0) then                                          !STRP109
                     do itime = 0, 23                                             !STRP110
                        xtime = float(itime)                                      !STRP111
                        ttime = tidex_M10(t0,a1t,p1t,a2t,p2t,xtime)               !STRP112
                        tmax(i,j,l,m) = dmax1(ttime,tmax(i,j,l,m))                !STRP113
                        tmin(i,j,l,m) = min(ttime,tmin(i,j,l,m))                  !STRP114
                     end do                                                       !STRP115
                  endif                                                           !STRP116
!---      Daily mean EW wind at level khgt                                        !STRP117
                  u0 = usa0(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP118
                  uday(i,j,l,m) = u0                                              !STRP119
!---      EW wind tide coefficient amplitudes and phases                          !STRP120
                  a1 = usa1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)*upolefac    !STRP121
                  p1 = usp1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP122
                  a2 = usa2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)*upolefac    !STRP123
                  p2 = usp2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP124
!---      EW wind at corners of 4-D cube                                          !STRP125
                  um(i,j,l,m) = tidex_M10(u0,a1,p1,a2,p2,time)                    !STRP126
!---      Daily mean NS wind at level khgt                                        !STRP127
                  v0 = vsa0(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP128
                  vday(i,j,l,m) = v0                                              !STRP129
!---      NS wind coefficient amplitudes and phases                               !STRP130
                  a1 = vsa1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)*upolefac    !STRP131
                  p1 = vsp1(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP132
                  a2 = vsa2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)*upolefac    !STRP133
                  p2 = vsp2(khgt,ilatw+i-1,jlon+j-1,ls+l-1,mdust+m-1)             !STRP134
!---      NS wind at corners of 4-D cube                                          !STRP135
                  vm(i,j,l,m) = tidex_M10(v0,a1,p1,a2,p2,time)                    !STRP136
               end do                                                             !STRP137
            end do                                                                !STRP138
         end do                                                                   !STRP139
      end do                                                                      !STRP140
!---    Use 4-D interpolation to get temperature, EW wind, NS wind,               !STRP141
!       and daily mean surface temperature at given latitude,                     !STRP142
!       longitude, Ls, and dust optical depth                                     !STRP143
      call fourd_M10 (dlat, dlon, dls, ddust, tm, tmgcm, 0)                       !STRP144
      call fourd_M10 (dlatw, dlon, dls, ddust, um, umgcm, 0)                      !STRP145
      call fourd_M10 (dlatw, dlon, dls, ddust, vm, vmgcm, 0)                      !STRP146
      call fourd_M10 (dlat, dlon, dls, ddust, ts0, tszero, 0)                     !STRP147
      call fourd_M10 (dlat, dlon, dls, ddust, tday, tempday, 0)                   !STRP148
      call fourd_M10 (dlat, dlon, dls, ddust, tmax, tempmax, 0)                   !STRP149
      call fourd_M10 (dlat, dlon, dls, ddust, tmin, tempmin, 0)                   !STRP150
      call fourd_M10 (dlatw, dlon, dls, ddust, uday, uwndday, 0)                  !STRP151
      call fourd_M10 (dlatw, dlon, dls, ddust, vday, vwndday, 0)                  !STRP152
!---    k1h = height index just below k1st                                        !STRP153
      k1h = k1st - 1                                                              !STRP154
      k1h = max0(1,k1h)                                                           !STRP155
!---    Establish MGCM values at height levels k1h, k1st and corners of           !STRP156
!       a 3-dimensional cube in latitude-Ls-dust space, at given time             !STRP157
!       of day (time)                                                             !STRP158
      do i = 1, 2                                                                 !STRP159
         polefac = 1.0D0                                                          !STRP160
         if (ilat == 1) then                                                      !STRP161
            polefac = i - 1.0D0                                                   !STRP162
         else if (ilat == nlat - 1) then                                          !STRP163
            polefac = 2.0D0 - i                                                   !STRP164
         endif                                                                    !STRP165
         do l = 1, 2                                                              !STRP166
            do m = 1, 2                                                           !STRP167
!---      Daily average pressure and density at level k1h                         !STRP168
               pzh(i,l,m) = pza0(k1h,ilat+i-1,ls+l-1,mdust+m-1)                   !STRP169
               dzh(i,l,m) = dza0(k1h,ilat+i-1,ls+l-1,mdust+m-1)                   !STRP170
               pzh1(i,l,m) = pza0(k1h+1,ilat+i-1,ls+l-1,mdust+m-1)                !STRP171
               dzh1(i,l,m) = dza0(k1h+1,ilat+i-1,ls+l-1,mdust+m-1)                !STRP172
!---      Pressure tide coefficient amplitudes and phases                         !STRP173
               p0 = pza0(k1st,ilat+i-1,ls+l-1,mdust+m-1)                          !STRP174
               a1 = pza1(k1st,ilat+i-1,ls+l-1,mdust+m-1)*polefac                  !STRP175
               p1 = pzp1(k1st,ilat+i-1,ls+l-1,mdust+m-1)                          !STRP176
               a2 = pza2(k1st,ilat+i-1,ls+l-1,mdust+m-1)*polefac                  !STRP177
               p2 = pzp2(k1st,ilat+i-1,ls+l-1,mdust+m-1)                          !STRP178
!---      Pressure values at corners of 3-D cube                                  !STRP179
               pz1(i,l,m) = tidey_M10(p0,a1,p1,a2,p2,time)                        !STRP180
!---      Daily average pressure at level k1st                                    !STRP181
               pz0(i,l,m) = p0                                                    !STRP182
!---      Level k1st Pressure at corners of 3-D cube                              !STRP183
               pmax(i,l,m) = -9999.0D0                                            !STRP184
               pmin(i,l,m) = 9999.0D0                                             !STRP185
               if (idaydata > 0) then                                             !STRP186
                  do itime = 0, 23                                                !STRP187
                     xtime = float(itime)                                         !STRP188
                     ptime = tidey_M10(p0,a1,p1,a2,p2,xtime)                      !STRP189
                     pmax(i,l,m) = dmax1(ptime,pmax(i,l,m))                       !STRP190
                     pmin(i,l,m) = min(ptime,pmin(i,l,m))                         !STRP191
                  end do                                                          !STRP192
               endif                                                              !STRP193
!---      Temperature tide coefficient amplitudes and phases                      !STRP194
               t0 = tza0(k1st,ilat+i-1,ls+l-1,mdust+m-1)                          !STRP195
               a1 = tza1(k1st,ilat+i-1,ls+l-1,mdust+m-1)*polefac                  !STRP196
               p1 = tzp1(k1st,ilat+i-1,ls+l-1,mdust+m-1)                          !STRP197
               a2 = tza2(k1st,ilat+i-1,ls+l-1,mdust+m-1)*polefac                  !STRP198
               p2 = tzp2(k1st,ilat+i-1,ls+l-1,mdust+m-1)                          !STRP199
!---      temperature values at corners of 3-D cube                               !STRP200
               tz1(i,l,m) = tidex_M10(t0,a1,p1,a2,p2,time)                        !STRP201
!---      Level k1st Temperature at corners of 3-D cube                           !STRP202
               t1max(i,l,m) = -9999.0D0                                           !STRP203
               t1min(i,l,m) = 9999.0D0                                            !STRP204
               if (idaydata > 0) then                                             !STRP205
                  do itime = 0, 23                                                !STRP206
                     xtime = float(itime)                                         !STRP207
                     t1time = tidex_M10(t0,a1,p1,a2,p2,xtime)                     !STRP208
                     t1max(i,l,m) = dmax1(t1time,t1max(i,l,m))                    !STRP209
                     t1min(i,l,m) = min(t1time,t1min(i,l,m))                      !STRP210
                  end do                                                          !STRP211
               endif                                                              !STRP212
!---      Daily average temperature at level k1st                                 !STRP213
               tz0(i,l,m) = t0                                                    !STRP214
!---      Daily average density at level k1st                                     !STRP215
               d0 = dza0(k1st,ilat+i-1,ls+l-1,mdust+m-1)                          !STRP216
!---      Gas constant from pressure, density, and temperature                    !STRP217
               rz0(i,l,m) = 190.0D0                                               !STRP218
               if (dabs(t0)<=0.0D0 .or. dabs(d0)<=0.0D0) cycle                    !STRP219
               rz0(i,l,m) = p0/(t0*d0)                                            !STRP220
            end do                                                                !STRP221
         end do                                                                   !STRP222
      end do                                                                      !STRP223
!---    Do 3-D interpolation on pressure                                          !STRP224
      call threed_M10 (dlat, dls, ddust, pzh, pzk1h, 1)                           !STRP225
      call threed_M10 (dlat, dls, ddust, pzh1, pzk1h1, 1)                         !STRP226
!---    Daily average pressure scale height                                       !STRP227
      hpres0 = 5.0D0/zlogr_M10(pzk1h,pzk1h1,'STRP-01')                            !STRP228
!---    Do 3-D interpolation on density                                           !STRP229
      call threed_M10 (dlat, dls, ddust, dzh, dzk1h, 1)                           !STRP230
      call threed_M10 (dlat, dls, ddust, dzh1, dzk1h1, 1)                         !STRP231
!---    Daily average density scale height                                        !STRP232
      hdens0 = 5.0D0/zlogr_M10(dzk1h,dzk1h1,'STRP-02')                            !STRP233
!---    Do 3-D interpolation on daily mean temperature                            !STRP234
      call threed_M10 (dlat, dls, ddust, tz0, tzero, 0)                           !STRP235
      call threed_M10 (dlat, dls, ddust, pz0, pzero, 1)                           !STRP236
!---    Daily average layer mean temperature                                      !STRP237
      tbar0 = (tzero + tszero)/2.0D0                                              !STRP238
!---    Do 3-D interpolation on gas constant                                      !STRP239
      call threed_M10 (dlat, dls, ddust, rz0, rzero, 0)                           !STRP240
!---    Do 3-D interpolation on temperature and pressure                          !STRP241
      call threed_M10 (dlat, dls, ddust, tz1, t1st, 0)                            !STRP242
      call threed_M10 (dlat, dls, ddust, pz1, p1st, 1)                            !STRP243
!---    Do 3-D interpolation on max,min pressure at level k1st                    !STRP244
      if (idaydata == 1) then                                                     !STRP245
         call threed_M10 (dlat, dls, ddust, pmax, p1max, 1)                       !STRP246
      else                                                                        !STRP247
         call threed_M10 (dlat, dls, ddust, pmax, p1max, 0)                       !STRP248
      endif                                                                       !STRP249
      call threed_M10 (dlat, dls, ddust, pmin, p1min, 1)                          !STRP250
!---    Do 3-D interpolation on max,min temperature at level k1st                 !STRP251
      call threed_M10 (dlat, dls, ddust, t1max, tmax1, 0)                         !STRP252
      call threed_M10 (dlat, dls, ddust, t1min, tmin1, 0)                         !STRP253
!---    Density from gas law                                                      !STRP254
!---      d1st = p1st/(rzero*t1st)                                                !STRP255
!---    Layer mean temperature at current time                                    !STRP256
      if (khgt==2 .and. tat5m<=0.0D0) tat5m = tmgcm                               !STRP257
      tbar = (t1st + tat5m)/2.0D0                                                 !STRP258
!---    Pressure scale height and density scale height at current time            !STRP259
      hpres = hpres0*tbar/tbar0                                                   !STRP260
      hdens = hdens0*tbar/tbar0                                                   !STRP261
!---    Adjust pressure to height level, using pressure scale height              !STRP262
      height = ctopohgt + dzbl(khgt)                                              !STRP263
      z1st = 5.0D0*(k1st - 1.0D0)                                                 !STRP264
      pmgcm = p1st*dexp((z1st - height)/hpres)                                    !STRP265
      presday = pzero*dexp((z1st - height)/hpres0)                                !STRP266
!---    Compute density from gas law, using pressure and temperature              !STRP267
      dmgcm = pmgcm/(rzero*tmgcm)                                                 !STRP268
      densday = presday/(rzero*tempday)                                           !STRP269
      densmax = -9999.0D0                                                         !STRP270
      densmin = 9999.0D0                                                          !STRP271
      if (idaydata > 0) then                                                      !STRP272
!---    Daily maximum and minimum density                                         !STRP273
         hpmin = hpres0*0.5D0*(tmax1 + tempmax)/tbar0                             !STRP274
         hpmax = hpres0*0.5D0*(tmax1 + tempmin)/tbar0                             !STRP275
         p1max = p1max*dexp((z1st - height)/hpmax)                                !STRP276
         p1min = p1min*dexp((z1st - height)/hpmin)                                !STRP277
         densmax = densday*(p1max/presday)*(tempday/tempmin)                      !STRP278
         densmin = densday*(p1min/presday)*(tempday/tempmax)                      !STRP279
      endif                                                                       !STRP280
      return                                                                      !STRP281
      end subroutine surfterp_M10                                                 !STRP282
!                                                                                 !STRP283
!-------------------------------------------------------------------------------  !STW2  1
      SUBROUTINE STEWART2_M10(RAUI, LAT, LON, LST, TOTALPRZ, TZ, TOTALMDZ, CHGT & !STW2  2
         , RSTAR, H, MOLWTG, SIGMA, IU0, SUNLAT, DELTATEX, TINF, TF, ZF, HRHO,  & !STW2  3
         REQUA, RPOLE, TGRAD)                                                     !STW2  4
!-----------------------------------------------                                  !STW2  5
!   M o d u l e s                                                                 !STW2  6
!-----------------------------------------------                                  !STW2  7
      USE vast_kind_param, ONLY:  DOUBLE                                          !STW2  8
      USE THERM_M10_C                                                             !STW2  9
!-----------------------------------------------                                  !STW2 10
!                                                                                 !STW2 11
!---  TIME-DEPENDENT MARS ATMOSPHERE MODEL, FORTRAN VERSION OF PROGRAM            !STW2 12
!     BY IAN STEWART, LABORATORY FOR ATMOSPHERIC AND SPACE PHYSICS,               !STW2 13
!     UNIV. OF COLORADO. FINAL REPORT JPL PO # NQ-802429                          !STW2 14
!--------------------------------------------------------------------             !STW2 15
!...                                                                              !STW2 16
!...Switches:                                                                     !STW2 17
!-----------------------------------------------                                  !STW2 18
!   I n t e r f a c e   B l o c k s                                               !STW2 19
!-----------------------------------------------                                  !STW2 20
      USE escalc_M10_I                                                            !STW2 21
      USE rellips_M10_I                                                           !STW2 22
      USE thermpar_M10_I                                                          !STW2 23
      USE thermos_M10_I                                                           !STW2 24
      IMPLICIT NONE                                                               !STW2 25
!-----------------------------------------------                                  !STW2 26
!   D u m m y   A r g u m e n t s                                                 !STW2 27
!-----------------------------------------------                                  !STW2 28
      INTEGER , INTENT(IN)  :: IU0                                                !STW2 29
      REAL(DOUBLE) , INTENT(IN) :: RAUI                                           !STW2 30
      REAL(DOUBLE) , INTENT(IN)  :: LAT                                           !STW2 31
      REAL(DOUBLE) , INTENT(IN)  :: LON                                           !STW2 32
      REAL(DOUBLE) , INTENT(IN)  :: LST                                           !STW2 33
      REAL(DOUBLE) , INTENT(INOUT) :: TOTALPRZ                                    !STW2 34
      REAL(DOUBLE) , INTENT(INOUT)  :: TZ                                         !STW2 35
      REAL(DOUBLE) , INTENT(INOUT)  :: TOTALMDZ                                   !STW2 36
      REAL(DOUBLE) , INTENT(IN)  :: CHGT                                          !STW2 37
      REAL(DOUBLE) , INTENT(IN) :: RSTAR                                          !STW2 38
      REAL(DOUBLE) , INTENT(OUT) :: H                                             !STW2 39
      REAL(DOUBLE) , INTENT(OUT)  :: MOLWTG                                       !STW2 40
      REAL(DOUBLE) , INTENT(IN)  :: SIGMA                                         !STW2 41
      REAL(DOUBLE) , INTENT(IN)  :: SUNLAT                                        !STW2 42
      REAL(DOUBLE) , INTENT(IN) :: DELTATEX                                       !STW2 43
      REAL(DOUBLE) , INTENT(OUT)  :: TINF                                         !STW2 44
      REAL(DOUBLE) , INTENT(IN)  :: TF                                            !STW2 45
      REAL(DOUBLE) , INTENT(IN)  :: ZF                                            !STW2 46
      REAL(DOUBLE) , INTENT(OUT) :: HRHO                                          !STW2 47
      REAL(DOUBLE) , INTENT(IN)   :: REQUA                                        !STW2 48
      REAL(DOUBLE) , INTENT(IN)   :: RPOLE                                        !STW2 49
      REAL(DOUBLE) , INTENT(OUT)   :: TGRAD                                       !STW2 50
!-----------------------------------------------                                  !STW2 51
!   L o c a l   V a r i a b l e s                                                 !STW2 52
!-----------------------------------------------                                  !STW2 53
      INTEGER :: FLAG                                                             !STW2 54
      REAL(DOUBLE), DIMENSION(0:11) :: ES                                         !STW2 55
      REAL(DOUBLE), DIMENSION(0:8) :: PRZ, NDZ, MDZ, FMOLX                        !STW2 56
      REAL(DOUBLE) :: ZZF, TOTALNDZ, RAU, FBAR, FBARR, RREF, RF, GZ, OLDRREF,   & !STW2 57
         CTOPOHGT, ALBEDO, TINF0, TF0, ZF0, SCALE, DMDZ, ROG                      !STW2 58
!-----------------------------------------------                                  !STW2 59
!---  ES ARE STD DEVIATIONS FROM NOMINAL VALUES                                   !STW2 60
!---  0,2,....10  LONG - TERM                                                     !STW2 61
!---  1,3.....,11  SHORT-TERM                                                     !STW2 62
!---  FOR   FBAR,TINF,FOXY,AOXY,ZF,DZDUST                                         !STW2 63
!---  RETURNS TEMP,#DENS,MDENS,PRESSURE                                           !STW2 64
      FLAG = 0                                                                    !STW2 65
      CALL ESCALC_M10 (STDL, SIGMA, ES)                                           !STW2 66
!---  DEVIATIONS FROM NOMINAL VALUES                                              !STW2 67
      FBAR = F107*DEXP(ES(0))                                                     !STW2 68
!---  3 MONTH RUNNING MEAN OF 10.7 CM SOLAR FLUX                                  !STW2 69
!---  IN UNITS OF 1.0E-22 W/CM**2                                                 !STW2 70
      RAU = RAUI                                                                  !STW2 71
!---  Convert solar 10.7 cm flux to Mars position                                 !STW2 72
      FBARR = FBAR/RAU**2                                                         !STW2 73
      CALL RELLIPS_M10 (LAT, LON, RREF, CHGT, GZ, OLDRREF, CTOPOHGT, ALBEDO,    & !STW2 74
         REQUA, RPOLE)                                                            !STW2 75
!---  Evaluate the basic parameters for the thermosphere model                    !STW2 76
      CALL THERMPAR_M10 (RAU, FBARR, LAT, LST, SUNLAT, TINF0, TF0, ZF0, SCALE)    !STW2 77
      IF (FLAG > 0) WRITE (IU0, *) ' RREF, RAU, GZ = ', RREF, RAU, GZ             !STW2 78
!---  Height above base of thermosphere                                           !STW2 79
      ZZF = CHGT - ZF                                                             !STW2 80
      IF (FLAG > 0) WRITE (IU0, *) ' ZF,CHGT,ZZF = ', ZF, CHGT, ZZF               !STW2 81
      RF = RREF + ZF                                                              !STW2 82
!---  Exospheric temperature                                                      !STW2 83
      TINF = TINF0*DEXP(ES(2)+ES(3)) + DELTATEX                                   !STW2 84
      IF (FLAG > 0) THEN                                                          !STW2 85
         WRITE (IU0, *) ' '                                                       !STW2 86
         WRITE (IU0, *) 'FROM PROC. DRAG-- ZF,RF,TINF,TF = '                      !STW2 87
         WRITE (IU0, 160) ZF, RF, TINF, TF                                        !STW2 88
  160    FORMAT(F7.2,F8.2,3F7.2)                                                  !STW2 89
         WRITE (IU0, *) ' '                                                       !STW2 90
      ENDIF                                                                       !STW2 91
      CALL THERMOS_M10 (FLAG, ES, TINF, TF, LAT, LON, LST, ZF, RF, ZZF,         & !STW2 92
         TOTALPRZ, TOTALNDZ, TZ, MOLWTG, PRZ, NDZ, MDZ, TOTALMDZ, IU0, SCALE,   & !STW2 93
         TGRAD, DMDZ, REQUA, RPOLE, FMOLX)                                        !STW2 94
      IF (DABS(SIGMA) < 0.01D0) THEN                                              !STW2 95
         FMOL(:8) = FMOLX(:8)                                                     !STW2 96
      ENDIF                                                                       !STW2 97
!---  SCALE HEIGHT, km                                                            !STW2 98
      ROG = RSTAR/(1000.0D0*MOLWTG*GZ)                                            !STW2 99
      H = ROG*TZ                                                                  !STW2100
      HRHO = H/(1.0D0 + TGRAD*ROG - (H/MOLWTG)*DMDZ)                              !STW2101
!---  Convert pressure to N/m**2                                                  !STW2102
      TOTALPRZ = TOTALPRZ*1.0E5                                                   !STW2103
!---  Convert density to kg/m**3                                                  !STW2104
      TOTALMDZ = TOTALMDZ*1000.0D0                                                !STW2105
      RETURN                                                                      !STW2106
      END SUBROUTINE STEWART2_M10                                                 !STW2107
!                                                                                 !STW2108
!------------------------------------------------------------------------------   !THRD  1
      subroutine threed_M10(dx, dy, dz, array, value, lint)                       !THRD  2
!-----------------------------------------------                                  !THRD  3
!   M o d u l e s                                                                 !THRD  4
!-----------------------------------------------                                  !THRD  5
      USE vast_kind_param, ONLY:  double                                          !THRD  6
!---    3-Dimensional linear interpolation within a 1x1x1 cube (x,y,z)            !THRD  7
!       of Array(2,2,2) to position dx,dy,dz (all 0-1).                           !THRD  8
!       Value is value of interpolated output.                                    !THRD  9
!...                                                                              !THRD 10
!...Switches:                                                                     !THRD 11
      implicit none                                                               !THRD 12
!-----------------------------------------------                                  !THRD 13
!   D u m m y   A r g u m e n t s                                                 !THRD 14
!-----------------------------------------------                                  !THRD 15
      integer , intent(in) :: lint                                                !THRD 16
      real(double) , intent(in) :: dx                                             !THRD 17
      real(double) , intent(in) :: dy                                             !THRD 18
      real(double) , intent(in) :: dz                                             !THRD 19
      real(double) , intent(out) :: value                                         !THRD 20
      real(double) , intent(in) :: array(2,2,2)                                   !THRD 21
!-----------------------------------------------                                  !THRD 22
!   L o c a l   V a r i a b l e s                                                 !THRD 23
!-----------------------------------------------                                  !THRD 24
      real(double) :: dxp, dyp, dzp                                               !THRD 25
!-----------------------------------------------                                  !THRD 26
!---    Complementary displacements in x,y,z                                      !THRD 27
      dxp = 1.0D0 - dx                                                            !THRD 28
      dyp = 1.0D0 - dy                                                            !THRD 29
      dzp = 1.0D0 - dz                                                            !THRD 30
      if (lint /= 1) then                                                         !THRD 31
!---      3-D interpolated Value from Array                                       !THRD 32
         value = dxp*dyp*dzp*array(1,1,1) + dxp*dyp*dz*array(1,1,2) + dxp*dy*   & !THRD 33
            dzp*array(1,2,1) + dx*dyp*dzp*array(2,1,1) + dxp*dy*dz*array(1,2,2) & !THRD 34
             + dx*dyp*dz*array(2,1,2) + dx*dy*dzp*array(2,2,1) + dx*dy*dz*array & !THRD 35
            (2,2,2)                                                               !THRD 36
      else                                                                        !THRD 37
!---      3-D interpolated Value from log of Array                                !THRD 38
         value = dexp(dxp*dyp*dzp*dlog(array(1,1,1))+dxp*dyp*dz*dlog(array(1,1, & !THRD 39
            2))+dxp*dy*dzp*dlog(array(1,2,1))+dx*dyp*dzp*dlog(array(2,1,1))+dxp & !THRD 40
            *dy*dz*dlog(array(1,2,2))+dx*dyp*dz*dlog(array(2,1,2))+dx*dy*dzp*   & !THRD 41
            dlog(array(2,2,1))+dx*dy*dz*dlog(array(2,2,2)))                       !THRD 42
      endif                                                                       !THRD 43
      return                                                                      !THRD 44
      end subroutine threed_M10                                                   !THRD 45
!                                                                                 !THRD 46
!------------------------------------------------------------------------------   !THRM  1
      SUBROUTINE THERMOS_M10(FLAG, ES, TINF, TF, LAT, LON, LST, ZF, RF, ZZF,   &  !THRM  2
         TOTALPRZ, TOTALNDZ, TZ, MOLWTG, PRZ, NDZ, MDZ, TOTALMDZ, IU0, SCALE,  &  !THRM  3
         TGRAD, DMDZ, REQUA, RPOLE, FMOL)                                         !THRM  4
!-----------------------------------------------                                  !THRM  5
!   M o d u l e s                                                                 !THRM  6
!-----------------------------------------------                                  !THRM  7
      USE vast_kind_param, ONLY:  DOUBLE                                          !THRM  8
!...                                                                              !THRM  9
!...Switches:                                                                     !THRM 10
!-----------------------------------------------                                  !THRM 11
!   I n t e r f a c e   B l o c k s                                               !THRM 12
!-----------------------------------------------                                  !THRM 13
      USE rellips_M10_I                                                           !THRM 14
      USE subltchk_M10_I                                                          !THRM 15
      IMPLICIT NONE                                                               !THRM 16
!-----------------------------------------------                                  !THRM 17
!   D u m m y   A r g u m e n t s                                                 !THRM 18
!-----------------------------------------------                                  !THRM 19
      INTEGER , INTENT(IN) :: FLAG                                                !THRM 20
      INTEGER , INTENT(IN) :: IU0                                                 !THRM 21
      REAL(DOUBLE) , INTENT(IN) :: TINF                                           !THRM 22
      REAL(DOUBLE) , INTENT(IN) :: TF                                             !THRM 23
      REAL(DOUBLE) , INTENT(IN)  :: LAT                                           !THRM 24
      REAL(DOUBLE) , INTENT(IN)  :: LON                                           !THRM 25
      REAL(DOUBLE) , INTENT(IN) :: LST                                            !THRM 26
      REAL(DOUBLE) , INTENT(IN)  :: ZF                                            !THRM 27
      REAL(DOUBLE) , INTENT(IN) :: RF                                             !THRM 28
      REAL(DOUBLE) , INTENT(IN) :: ZZF                                            !THRM 29
      REAL(DOUBLE) , INTENT(OUT)  :: TOTALPRZ                                     !THRM 30
      REAL(DOUBLE) , INTENT(OUT) :: TOTALNDZ                                      !THRM 31
      REAL(DOUBLE) , INTENT(OUT)  :: TZ                                           !THRM 32
      REAL(DOUBLE) , INTENT(OUT) :: MOLWTG                                        !THRM 33
      REAL(DOUBLE) , INTENT(OUT) :: TOTALMDZ                                      !THRM 34
      REAL(DOUBLE) , INTENT(IN) :: SCALE                                          !THRM 35
      REAL(DOUBLE) , INTENT(OUT) :: TGRAD                                         !THRM 36
      REAL(DOUBLE) , INTENT(OUT) :: DMDZ                                          !THRM 37
      REAL(DOUBLE) , INTENT(IN)  :: REQUA                                         !THRM 38
      REAL(DOUBLE) , INTENT(IN)  :: RPOLE                                         !THRM 39
      REAL(DOUBLE) , INTENT(IN) :: ES(0:11)                                       !THRM 40
      REAL(DOUBLE) , INTENT(INOUT) :: PRZ(0:8)                                    !THRM 41
      REAL(DOUBLE) , INTENT(INOUT) :: NDZ(0:8)                                    !THRM 42
      REAL(DOUBLE) , INTENT(INOUT) :: MDZ(0:8)                                    !THRM 43
      REAL(DOUBLE) , INTENT(OUT) :: FMOL(0:8)                                     !THRM 44
!-----------------------------------------------                                  !THRM 45
!   L o c a l   V a r i a b l e s                                                 !THRM 46
!-----------------------------------------------                                  !THRM 47
      INTEGER :: I, J, K                                                          !THRM 48
      REAL(DOUBLE) :: BK, GF, PRESSF, P1BAR, AO, FO, PFHE, PFH2, PFH, RATIO,    & !THRM 49
         RREF, YSC                                                                !THRM 50
      REAL(DOUBLE), DIMENSION(0:8) :: M, HH, XHH, DM, XDM, FF, XFF                !THRM 51
      REAL(DOUBLE) :: RADEG, AMUMASS, OLDRREF, CTOPOHGT, ALBEDO, EXPS, SMOLWTG  & !THRM 52
         , STOTPR, SYSC, STZ, SPRZI, SPRZJ, TSUBL                                 !THRM 53
!-----------------------------------------------                                  !THRM 54
!---  RETURNS TEMP & COMPOSITION VS ALTITUDE ABOVE ZF                             !THRM 55
!---  ES (EPS*SIG)   DEVIATIONS FROM NOMINAL VALUES                               !THRM 56
!---  0,2,....,10 LONG-TERM                                                       !THRM 57
!---  1,3,....11   SHORT-TERM                                                     !THRM 58
!---  FOR   FBAR, TINF, FOXY, ADXY,ZF,DZDUST                                      !THRM 59
!---  ZZF  = INPUT ALTITUDES ABOVE ZF                                             !THRM 60
!---  TZ  = TEMPERATURE VS ALTITUDE, DEG K                                        !THRM 61
!---  NDZ= # DENSITY VS ALTITUDE, #/cc                                            !THRM 62
!---  MDZ= MASS DENSITY VS ALTITUDE, gm/cc                                        !THRM 63
!---  PRZ= PRESSURE VS ALTITUDE, BARS                                             !THRM 64
!---  FLAG=    0  CALCULATIONS ONLY                                               !THRM 65
!---  FLAG=   1 PRINT DEBUG OUTPUT                                                !THRM 66
      RADEG = 57.29577958D0                                                       !THRM 67
!     RADEG = DEGREES/RADIAN                                                      !THRM 68
      BK = 1.38065D-16                                                            !THRM 69
      AMUMASS = 1.66054D-24                                                       !THRM 70
!---  CONSTANT FOR NUMBER DENSITY CALCULATIONS                                    !THRM 71
      CALL RELLIPS_M10 (LAT, LON, RREF, ZF, GF, OLDRREF, CTOPOHGT, ALBEDO,      & !THRM 72
         REQUA, RPOLE)                                                            !THRM 73
      GF = 100.0D0*GF                                                             !THRM 74
!---  ACC. OF GRAVITY AT ZF                                                       !THRM 75
!---  PRESSF = 1.24E-9   (as originally in Stewart's model)                       !THRM 76
      PRESSF = 1.26D-9                                                            !THRM 77
!---  1.26E-3 dynes/cm**2 = 1.26 nbar AT ZF, BASE OF THERMOSPHERE                 !THRM 78
!---  P1BAR = 1.013E6   (as originally in Stewart's model)                        !THRM 79
      P1BAR = 1.0D6                                                               !THRM 80
!---  EARTH SURFACE PRESSURE                                                      !THRM 81
      AO = 0.18D0*(1.0D0 + ES(7))                                                 !THRM 82
!---  AO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT                        !THRM 83
      FO = 0.01D0*DEXP(ES(4)+ES(5))                                               !THRM 84
!---  FO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT I                      !THRM 85
!---  Molecular weights using Mars isotopic ratios                                !THRM 86
      M(0) = 44.0090D0                                                            !THRM 87
!---  CO2 MOLECULAR WEIGHT                                                        !THRM 88
      M(1) = 28.0178D0                                                            !THRM 89
!---  N2                                                                          !THRM 90
      M(2) = 39.9609D0                                                            !THRM 91
!---  ARGON                                                                       !THRM 92
      M(3) = 31.9980D0                                                            !THRM 93
!---  MOLECULAR OXYGEN                                                            !THRM 94
      M(4) = 28.0100D0                                                            !THRM 95
!---  CARBON MONOXIDE                                                             !THRM 96
      M(5) = 15.9990D0                                                            !THRM 97
!---  ATOMIC OXYGEN                                                               !THRM 98
      M(6) = 4.00260D0                                                            !THRM 99
!---  HELIUM                                                                      !THRM100
      M(7) = 2.01746D0                                                            !THRM101
!---  MOLECULAR HYDROGEN                                                          !THRM102
      M(8) = 1.00873D0                                                            !THRM103
!---  ATOMIC HYDROGEN MOLECULAR WEIGHT                                            !THRM104
      DM(0) = AMUMASS*M(0)                                                        !THRM105
!---  CO2 MOLECULAR MASS                                                          !THRM106
      DM(1) = AMUMASS*M(1)                                                        !THRM107
!---  N2 MOLECULAR MASS                                                           !THRM108
      DM(2) = AMUMASS*M(2)                                                        !THRM109
!---  ARGON MOLECULAR MASS                                                        !THRM110
      DM(3) = AMUMASS*M(3)                                                        !THRM111
!---  02 MOLECULAR MASS                                                           !THRM112
      DM(4) = AMUMASS*M(4)                                                        !THRM113
!---  CARBON MONOXIDE MOLECULAR MASS                                              !THRM114
      DM(5) = AMUMASS*M(5)                                                        !THRM115
!---  ATOMIC OXYGEN MOLECULAR MASS                                                !THRM116
      XDM(0) = AMUMASS*M(6)                                                       !THRM117
!---  HELIUM MOLECULAR MASS                                                       !THRM118
      XDM(1) = AMUMASS*M(7)                                                       !THRM119
!---  H2 MOLECULAR MASS                                                           !THRM120
      XDM(2) = AMUMASS*M(8)                                                       !THRM121
!---  H   MOLECULAR MASS                                                          !THRM122
!---  THE FOLLOWING IS THE COMPOSITION OF THE HETEROSPHERE                        !THRM123
      FF(0) = 0.932D0                                                             !THRM124
!---  CO2                                                                         !THRM125
      FF(1) = 0.027D0                                                             !THRM126
!---  NITROGEN                                                                    !THRM127
      FF(2) = 0.016D0                                                             !THRM128
!---  ARGON                                                                       !THRM129
      FF(3) = 0.002D0                                                             !THRM130
!---  MOLECULAR OXYGEN                                                            !THRM131
      FF(4) = 0.013D0                                                             !THRM132
!---  CARBON MONOXIDE                                                             !THRM133
      FF(5) = 0.010D0                                                             !THRM134
!---  ATOMIC OXYGEN                                                               !THRM135
      FF(5) = FO*(1.0D0 - AO*DSIN(15.0D0*LST/RADEG)*DCOS(LAT/RADEG))              !THRM136
      PFHE = 3.3D-16*TINF                                                         !THRM137
!---  EXOBASE (ZF) HELIUM PARTIAL PRESSURE                                        !THRM138
      PFH2 = 2.4D-15                                                              !THRM139
      IF (TINF <= 330.0D0) THEN                                                   !THRM140
!---  EXOBASE (ZF) H2 PARTIAL PRESSURE                                            !THRM141
         PFH = 5.2D-16*TINF*DEXP((-TINF/70.0D0))                                  !THRM142
!---  EXOBASE (ZF) H PARTIAL PRESSURE                                             !THRM143
      ELSE                                                                        !THRM144
         RATIO = 1440.0D0/TINF                                                    !THRM145
         PFH = 5.8D-18*DSQRT(TINF)*DEXP(RATIO)/(1.0D0 + RATIO)                    !THRM146
!---    EXOBASE (ZF) H PARTIAL PRESSURE                                           !THRM147
      ENDIF                                                                       !THRM148
      XFF(0) = PFHE/PRESSF                                                        !THRM149
      XFF(1) = PFH2/PRESSF                                                        !THRM150
      XFF(2) = PFH/PRESSF                                                         !THRM151
      MOLWTG = 0.0D0                                                              !THRM152
      TOTALPRZ = 0.0D0                                                            !THRM153
      TOTALMDZ = 0.0D0                                                            !THRM154
      YSC = ZZF*RF/(RF + ZZF)                                                     !THRM155
      EXPS = DEXP((-YSC/SCALE))                                                   !THRM156
      TZ = TINF - (TINF - TF)*EXPS                                                !THRM157
!---  Parameters for gradient of molecular weight                                 !THRM158
      SMOLWTG = 0.0D0                                                             !THRM159
      STOTPR = 0.0D0                                                              !THRM160
      SYSC = (ZZF + 1.0D0)*RF/(RF + ZZF + 1.0D0)                                  !THRM161
      STZ = TINF - (TINF - TF)*DEXP((-SYSC/SCALE))                                !THRM162
!---  Temperature gradient, K/km                                                  !THRM163
      TGRAD = (TINF - TF)*(RF + YSC)/(SCALE*(RF + ZZF))*EXPS                      !THRM164
      DO I = 0, 5                                                                 !THRM165
         HH(I) = BK*TINF/(GF*DM(I))/1.0D5                                         !THRM166
!---    SCALE HEIGHT                                                              !THRM167
         PRZ(I) = PRESSF*FF(I)*DEXP((-YSC/HH(I))-(SCALE/HH(I))*DLOG(TZ/TF))       !THRM168
         NDZ(I) = P1BAR*PRZ(I)/(BK*TZ)                                            !THRM169
!---    NUMBER DENSITY HEAVY GASES                                                !THRM170
         MDZ(I) = NDZ(I)*DM(I)                                                    !THRM171
         TOTALMDZ = TOTALMDZ + MDZ(I)                                             !THRM172
         TOTALPRZ = TOTALPRZ + PRZ(I)                                             !THRM173
         MOLWTG = MOLWTG + PRZ(I)*M(I)                                            !THRM174
!---    Molecular weight 1 km higher                                              !THRM175
         SPRZI = PRESSF*FF(I)*DEXP((-SYSC/HH(I))-(SCALE/HH(I))*DLOG(STZ/TF))      !THRM176
         STOTPR = STOTPR + SPRZI                                                  !THRM177
         SMOLWTG = SMOLWTG + SPRZI*M(I)                                           !THRM178
      END DO                                                                      !THRM179
      DO J = 0, 2                                                                 !THRM180
         XHH(J) = BK*TINF/(GF*XDM(J))/1.0D5                                       !THRM181
!---    SCALE HEIGHT                                                              !THRM182
         PRZ(J+6) = PRESSF*XFF(J)*DEXP((-YSC/XHH(J))-(SCALE/XHH(J))*DLOG(TZ/TF) & !THRM183
            )                                                                     !THRM184
         NDZ(J+6) = P1BAR*PRZ(J+6)/(BK*TZ)                                        !THRM185
!---    NUMBER DENSITY LIGHT GASES                                                !THRM186
         MDZ(J+6) = NDZ(J+6)*XDM(J)                                               !THRM187
         TOTALMDZ = TOTALMDZ + MDZ(J+6)                                           !THRM188
         TOTALPRZ = TOTALPRZ + PRZ(J+6)                                           !THRM189
         MOLWTG = MOLWTG + PRZ(J+6)*M(J+6)                                        !THRM190
!---    Molecular weight 1 km higher                                              !THRM191
         SPRZJ = PRESSF*XFF(J)*DEXP((-SYSC/XHH(J))-(SCALE/XHH(J))*DLOG(STZ/TF))   !THRM192
         STOTPR = STOTPR + SPRZJ                                                  !THRM193
         SMOLWTG = SMOLWTG + SPRZJ*M(J+6)                                         !THRM194
      END DO                                                                      !THRM195
!---  Check that temperature >= sublimation temperature                           !THRM196
      CALL SUBLTCHK_M10 (TZ, TOTALPRZ, TSUBL)                                     !THRM197
      MOLWTG = MOLWTG/TOTALPRZ                                                    !THRM198
!---  Change in molecular weight over 1 km                                        !THRM199
      DMDZ = SMOLWTG/STOTPR - MOLWTG                                              !THRM200
      TOTALNDZ = P1BAR*TOTALPRZ/(BK*TZ)                                           !THRM201
      IF (FLAG > 0) THEN                                                          !THRM202
         WRITE (IU0, 220) ZZF, TZ, MOLWTG, TOTALPRZ, TOTALNDZ, (NDZ(K),K=0,3),  & !THRM203
            TOTALMDZ, (NDZ(K),K=4,8)                                              !THRM204
  220    FORMAT(2F6.1,F5.1,6E10.3,/,17X,6E10.3)                                   !THRM205
         WRITE (IU0, *) ' '                                                       !THRM206
      ENDIF                                                                       !THRM207
      FMOL(:8) = NDZ(:8)/TOTALNDZ                                                 !THRM208
      RETURN                                                                      !THRM209
      END SUBROUTINE THERMOS_M10                                                  !THRM210
!                                                                                 !THRM211
!------------------------------------------------------------------------------   !TIDX  1
      real(kind(0.0d0)) function tidex_M10 (a0, a1, phi1, a2, phi2, t)            !TIDX  2
!-----------------------------------------------                                  !TIDX  3
!   M o d u l e s                                                                 !TIDX  4
!-----------------------------------------------                                  !TIDX  5
      USE vast_kind_param, ONLY:  double                                          !TIDX  6
!---    Tide value at local solar time t, from mean value A0, amplitude           !TIDX  7
!       A1 and phase phi1 of 24-hour period component, and amplitude A2           !TIDX  8
!       and phase phi2 of 12-hour period component.  Amplitudes A1 and            !TIDX  9
!       A2 are in same physical units as mean term A0.  Phases are in             !TIDX 10
!       hours of local solar time.                                                !TIDX 11
!...                                                                              !TIDX 12
!...Switches:                                                                     !TIDX 13
      implicit none                                                               !TIDX 14
!-----------------------------------------------                                  !TIDX 15
!   D u m m y   A r g u m e n t s                                                 !TIDX 16
!-----------------------------------------------                                  !TIDX 17
      real(double) , intent(in) :: a0                                             !TIDX 18
      real(double) , intent(in) :: a1                                             !TIDX 19
      real(double) , intent(in) :: phi1                                           !TIDX 20
      real(double) , intent(in) :: a2                                             !TIDX 21
      real(double) , intent(in) :: phi2                                           !TIDX 22
      real(double) , intent(in) :: t                                              !TIDX 23
!-----------------------------------------------                                  !TIDX 24
!   L o c a l   V a r i a b l e s                                                 !TIDX 25
!-----------------------------------------------                                  !TIDX 26
      real(double) :: pi                                                          !TIDX 27
!-----------------------------------------------                                  !TIDX 28
      pi = 4.0D0*datan(1.0D0)                                                     !TIDX 29
      tidex_M10 = a0 + a1*dcos(pi*(t - phi1)/12.0D0) + a2*dcos(pi*(t - phi2)/   & !TIDX 30
         6.0D0)                                                                   !TIDX 31
      return                                                                      !TIDX 32
      end function tidex_M10                                                      !TIDX 33
!                                                                                 !TIDX 34
!------------------------------------------------------------------------------   !TIDY  1
      REAL(KIND(0.0D0)) FUNCTION TIDEY_M10 (A0, A1, PHI1, A2, PHI2, T)            !TIDY  2
!-----------------------------------------------                                  !TIDY  3
!   M o d u l e s                                                                 !TIDY  4
!-----------------------------------------------                                  !TIDY  5
      USE vast_kind_param, ONLY:  DOUBLE                                          !TIDY  6
!---    Tide value at local solar time t, from mean value A0, amplitude           !TIDY  7
!       A1 and phase phi1 of 24-hour period component, and amplitude A2           !TIDY  8
!       and phase phi2 of 12-hour period component.  Amplitudes A1 and            !TIDY  9
!       A2 are in relative units (% of mean term A0).  Phases are in              !TIDY 10
!       hours of local solar time.                                                !TIDY 11
!...                                                                              !TIDY 12
!...Switches:                                                                     !TIDY 13
      IMPLICIT NONE                                                               !TIDY 14
!-----------------------------------------------                                  !TIDY 15
!   D u m m y   A r g u m e n t s                                                 !TIDY 16
!-----------------------------------------------                                  !TIDY 17
      REAL(DOUBLE) , INTENT(IN) :: A0                                             !TIDY 18
      REAL(DOUBLE) , INTENT(IN) :: A1                                             !TIDY 19
      REAL(DOUBLE) , INTENT(IN) :: PHI1                                           !TIDY 20
      REAL(DOUBLE) , INTENT(IN) :: A2                                             !TIDY 21
      REAL(DOUBLE) , INTENT(IN) :: PHI2                                           !TIDY 22
      REAL(DOUBLE) , INTENT(IN) :: T                                              !TIDY 23
!-----------------------------------------------                                  !TIDY 24
!   L o c a l   V a r i a b l e s                                                 !TIDY 25
!-----------------------------------------------                                  !TIDY 26
      REAL(DOUBLE) :: PI                                                          !TIDY 27
!-----------------------------------------------                                  !TIDY 28
      PI = 4.0D0*DATAN(1.0D0)                                                     !TIDY 29
      TIDEY_M10 = A0*(1.0D0 + (A1*DCOS(PI*(T - PHI1)/12.0D0) + A2*DCOS(PI*(T -  & !TIDY 30
         PHI2)/6.0D0))/100.0D0)                                                   !TIDY 31
      TIDEY_M10 = DMAX1(0.1D0*A0,TIDEY_M10)                                       !TIDY 32
      RETURN                                                                      !TIDY 33
      END FUNCTION TIDEY_M10                                                      !TIDY 34
!                                                                                 !TIDY 35
!-------------------------------------------------------------------------------  !TOPO  1
      subroutine topoareo_M10(clat, clonwin, careoid, ctopohgt, calbedo, iau)     !TOPO  2
!-----------------------------------------------                                  !TOPO  3
!   M o d u l e s                                                                 !TOPO  4
!-----------------------------------------------                                  !TOPO  5
      USE vast_kind_param, ONLY:  double                                          !TOPO  6
      USE terhgt_M10_C                                                            !TOPO  7
!...                                                                              !TOPO  8
!...Switches:                                                                     !TOPO  9
!-----------------------------------------------                                  !TOPO 10
!   I n t e r f a c e   B l o c k s                                               !TOPO 11
!-----------------------------------------------                                  !TOPO 12
      use ifloor_M10_I                                                            !TOPO 13
      use twod_M10_I                                                              !TOPO 14
      implicit none                                                               !TOPO 15
!-----------------------------------------------                                  !TOPO 16
!   G l o b a l   P a r a m e t e r s                                             !TOPO 17
!-----------------------------------------------                                  !TOPO 18
!-----------------------------------------------                                  !TOPO 19
!   D u m m y   A r g u m e n t s                                                 !TOPO 20
!-----------------------------------------------                                  !TOPO 21
      integer , intent(in) :: iau                                                 !TOPO 22
      real(double) , intent(in) :: clat                                           !TOPO 23
      real(double) , intent(in) :: clonwin                                        !TOPO 24
      real(double) , intent(inout)  :: careoid                                    !TOPO 25
      real(double) , intent(inout)  :: ctopohgt                                   !TOPO 26
      real(double) , intent(inout)  :: calbedo                                    !TOPO 27
!-----------------------------------------------                                  !TOPO 28
!   L o c a l   V a r i a b l e s                                                 !TOPO 29
!-----------------------------------------------                                  !TOPO 30
      integer :: n1lat, n2lat, ilatmt, jlonmt, n3lat, n4lat, ilatalb, jlonalb     !TOPO 31
      real(double), dimension(2,2) :: topohgt, areoid, albint                     !TOPO 32
      real(double) :: clonw, stepmolalat, stepmolalon, stepmola, dlatmt, dlonmt & !TOPO 33
         , stepalblat, stepalblon, stepalb, dlatalb, dlonalb                      !TOPO 34
!-----------------------------------------------                                  !TOPO 35
!---    Convert back to IAU 1991 rotation coordinates, if necessary               !TOPO 36
      clonw = clonwin                                                             !TOPO 37
      if (iau == 2000) then                                                       !TOPO 38
         clonw = clonw + 0.238D0                                                  !TOPO 39
         if (clonw > 360.0D0) clonw = clonw - 360.0D0                             !TOPO 40
      endif                                                                       !TOPO 41
!---    Latitude, Longitude steps sizes for MOLA data                             !TOPO 42
      stepmolalat = 180.0D0/(nmtlat - 1.0D0)                                      !TOPO 43
      stepmolalon = 360.0D0/(nmtlon - 1.0D0)                                      !TOPO 44
      n1lat = nint(1000/stepmolalat)                                              !TOPO 45
      n2lat = 90*n1lat + 500                                                      !TOPO 46
!---    Compute MOLA latitude index and lat increment from gridpoint              !TOPO 47
      stepmola = stepmolalat                                                      !TOPO 48
      if (clat < (-90.0D0) + stepmolalat/2.0D0) then                              !TOPO 49
         stepmola = stepmolalat/2.0D0                                             !TOPO 50
         ilatmt = 0                                                               !TOPO 51
         dlatmt = (clat + 90.0D0)/stepmola                                        !TOPO 52
      else if (clat > 90.0D0 - stepmolalat/2.0D0) then                            !TOPO 53
         stepmola = stepmolalat/2.0D0                                             !TOPO 54
         ilatmt = nmtlat - 1                                                      !TOPO 55
         dlatmt = (clat - 90.0D0 + stepmola)/stepmola                             !TOPO 56
      else                                                                        !TOPO 57
         ilatmt = (ifloor_M10(clat*n1lat) + n2lat)/1000                           !TOPO 58
         dlatmt = (clat + 90.0D0)/stepmolalat - (ilatmt - 0.5D0)                  !TOPO 59
      endif                                                                       !TOPO 60
      ilatmt = min0(nmtlat - 1,ilatmt)                                            !TOPO 61
      ilatmt = max0(0,ilatmt)                                                     !TOPO 62
!---    Compute MOLA longitude index and lon increment from gridpoint             !TOPO 63
      jlonmt = ifloor_M10((clonw + stepmolalon/2.0D0)/stepmolalon)                !TOPO 64
      jlonmt = min0(nmtlon - 1,jlonmt)                                            !TOPO 65
      jlonmt = max0(0,jlonmt)                                                     !TOPO 66
      dlonmt = (clonw + stepmolalon/2.0D0 - stepmolalon*jlonmt)/stepmolalon       !TOPO 67
!---    Topographic heights at corners of 2-D square grid points                  !TOPO 68
      topohgt(1,1) = topomola(ilatmt,jlonmt)                                      !TOPO 69
      topohgt(1,2) = topomola(ilatmt,jlonmt+1)                                    !TOPO 70
      topohgt(2,1) = topomola(ilatmt+1,jlonmt)                                    !TOPO 71
      topohgt(2,2) = topomola(ilatmt+1,jlonmt+1)                                  !TOPO 72
!---    Areoid radius at corners of 2-D square grid                               !TOPO 73
      areoid(1,1) = areorad(ilatmt,jlonmt)                                        !TOPO 74
      areoid(1,2) = areorad(ilatmt,jlonmt+1)                                      !TOPO 75
      areoid(2,1) = areorad(ilatmt+1,jlonmt)                                      !TOPO 76
      areoid(2,2) = areorad(ilatmt+1,jlonmt+1)                                    !TOPO 77
!---    Use 2-D interpolation to get topographic height at current                !TOPO 78
!       position                                                                  !TOPO 79
      call twod_M10 (dlatmt, dlonmt, topohgt, ctopohgt)                           !TOPO 80
!---    Use 2-D interpolation to get areoid radius at current position            !TOPO 81
      call twod_M10 (dlatmt, dlonmt, areoid, careoid)                             !TOPO 82
!---    Latitude, Longitude steps sizes for albedo data                           !TOPO 83
      stepalblat = 180.0D0/(nalblat - 1.0D0)                                      !TOPO 84
      stepalblon = 360.0D0/(nalblon - 1.0D0)                                      !TOPO 85
      n3lat = nint(1000/stepalblat)                                               !TOPO 86
      n4lat = 90*n3lat + 500                                                      !TOPO 87
!---    Compute albedo latitude index and lat increment from gridpoint            !TOPO 88
      stepalb = stepalblat                                                        !TOPO 89
      if (clat < (-90.0D0) + stepalblat/2.0D0) then                               !TOPO 90
         stepalb = stepalblat/2.0D0                                               !TOPO 91
         ilatalb = 0                                                              !TOPO 92
         dlatalb = (clat + 90.0D0)/stepalb                                        !TOPO 93
      else if (clat > 90.0D0 - stepalblat/2.0D0) then                             !TOPO 94
         stepalb = stepalblat/2.0D0                                               !TOPO 95
         ilatalb = nalblat - 1                                                    !TOPO 96
         dlatalb = (clat - 90.0D0 + stepalb)/stepalb                              !TOPO 97
      else                                                                        !TOPO 98
         ilatalb = (ifloor_M10(clat*n3lat) + n4lat)/1000                          !TOPO 99
         dlatalb = (clat + 90.0D0)/stepalblat - (ilatalb - 0.5D0)                 !TOPO100
      endif                                                                       !TOPO101
      ilatalb = min0(nalblat - 1,ilatalb)                                         !TOPO102
      ilatalb = max0(0,ilatalb)                                                   !TOPO103
!---    Compute albedo longitude index and lon increment from gridpoint           !TOPO104
      jlonalb = ifloor_M10((clonw + stepalblon/2.0D0)/stepalblon)                 !TOPO105
      jlonalb = min0(nalblon - 1,jlonalb)                                         !TOPO106
      jlonalb = max0(0,jlonalb)                                                   !TOPO107
      dlonalb = (clonw + stepalblon/2.0D0 - stepalblon*jlonalb)/stepalblon        !TOPO108
!---    Albedo at corners of 2-D square grid points                               !TOPO109
      albint(1,1) = albedo(ilatalb,jlonalb)                                       !TOPO110
      albint(1,2) = albedo(ilatalb,jlonalb+1)                                     !TOPO111
      albint(2,1) = albedo(ilatalb+1,jlonalb)                                     !TOPO112
      albint(2,2) = albedo(ilatalb+1,jlonalb+1)                                   !TOPO113
!---    Use 2-D interpolation to get albedo at current position                   !TOPO114
      call twod_M10 (dlatalb, dlonalb, albint, calbedo)                           !TOPO115
      return                                                                      !TOPO116
      end subroutine topoareo_M10                                                 !TOPO117
!                                                                                 !TOPO118
!------------------------------------------------------------------------------   !TPAR  1
      SUBROUTINE THERMPAR_M10(RAU, FBARR, LAT, LST, SUNLAT, TINF0, TF0, ZF0,   &  !TPAR  2
         SCALE)                                                                   !TPAR  3
!-----------------------------------------------                                  !TPAR  4
!   M o d u l e s                                                                 !TPAR  5
!-----------------------------------------------                                  !TPAR  6
      USE vast_kind_param, ONLY:  DOUBLE                                          !TPAR  7
!...                                                                              !TPAR  8
!...Switches:                                                                     !TPAR  9
      IMPLICIT NONE                                                               !TPAR 10
!-----------------------------------------------                                  !TPAR 11
!   D u m m y   A r g u m e n t s                                                 !TPAR 12
!-----------------------------------------------                                  !TPAR 13
      REAL(DOUBLE) , INTENT(IN) :: RAU                                            !TPAR 14
      REAL(DOUBLE) , INTENT(IN) :: FBARR                                          !TPAR 15
      REAL(DOUBLE) , INTENT(IN) :: LAT                                            !TPAR 16
      REAL(DOUBLE) , INTENT(IN) :: LST                                            !TPAR 17
      REAL(DOUBLE) , INTENT(IN) :: SUNLAT                                         !TPAR 18
      REAL(DOUBLE) , INTENT(OUT) :: TINF0                                         !TPAR 19
      REAL(DOUBLE) , INTENT(OUT) :: TF0                                           !TPAR 20
      REAL(DOUBLE) , INTENT(OUT) :: ZF0                                           !TPAR 21
      REAL(DOUBLE) , INTENT(OUT) :: SCALE                                         !TPAR 22
!-----------------------------------------------                                  !TPAR 23
!   L o c a l   V a r i a b l e s                                                 !TPAR 24
!-----------------------------------------------                                  !TPAR 25
      REAL(DOUBLE) :: LATMAX, PI180, TBAR, TAVG, T1, T2, POLEAMP, CPHI, ZBAR,  &  !TPAR 26
         FACTLAT, ZAVG, A1, A2                                                    !TPAR 27
!-----------------------------------------------                                  !TPAR 28
      DATA LATMAX/ 25.4D0/                                                        !TPAR 29
!                                                                                 !TPAR 30
!--------------------------------------------------------------------             !TPAR 31
!                                                                                 !TPAR 32
!---  Thermospheric parameters, revised from the original Stewart                 !TPAR 33
!     parameterizations:                                                          !TPAR 34
!     SMA = 1.523691                                                              !TPAR 35
!     ZF0 = 124.4 * (SMA/RAU)                                                     !TPAR 36
!     TINF0 = 4.11 * (11.0 + FBARR)                                               !TPAR 37
!     TF0 = 170.0 * (SMA/RAU)                                                     !TPAR 38
!     SCALE = TF0 / 9.20                                                          !TPAR 39
!                                                                                 !TPAR 40
!     The new parameterizations are based on four data sets from the              !TPAR 41
!     University of Michigan Mars Thermospheric General Circulation               !TPAR 42
!     Model (MTGCM), cases MGS97L, MGS98L, MANC00, and MGS97E. For                !TPAR 43
!     a description of the MTGCM model and its output, see Bougher,               !TPAR 44
!     et al., Journal of Geophysical Research, vol. 95 (B9), pp.                  !TPAR 45
!     14,811 - 14,827, August 30, 1990.                                           !TPAR 46
!                                                                                 !TPAR 47
!-------------------------------------------------------------------              !TPAR 48
!                                                                                 !TPAR 49
!     Inputs:                                                                     !TPAR 50
!       RAU    = orbital position radius (AU)                                     !TPAR 51
!       FBARR  = 10.7 cm solar flux at Mars position                              !TPAR 52
!       lat    = latitude for evaluation of parameters (degrees)                  !TPAR 53
!       LST    = local solar time (Mars hours) at evaluation point                !TPAR 54
!       sunlat = latitude of sun (degrees)                                        !TPAR 55
!     Outputs:                                                                    !TPAR 56
!       TINF0  = Exospheric temperature (K)                                       !TPAR 57
!       TF0    = Temperature at base of thermosphere (K)                          !TPAR 58
!       ZF0    = Height of base of thermosphere (km)                              !TPAR 59
!       SCALE  = Scale height for temperature variations (km)                     !TPAR 60
!                                                                                 !TPAR 61
!     Output values are un-corrected for Stewart (ES array) variations,           !TPAR 62
!     pressure and dust effects.  These factors are accounted for in              !TPAR 63
!     the STEWART2_M10 subroutine.  Adjustment factor deltaTEX is added           !TPAR 64
!     after computation of exospheric temperature.                                !TPAR 65
!--------------------------------------------------------------------             !TPAR 66
!                                                                                 !TPAR 67
!---  Degrees to radians conversion factor                                        !TPAR 68
      PI180 = DATAN(1.0D0)/45.0D0                                                 !TPAR 69
!---  Global mean exospheric temperature (K) versus 10.7 cm flux                  !TPAR 70
      TBAR = 156.3D0 + 0.9427D0*FBARR                                             !TPAR 71
!---  Zonal average exospheric temperature (K) versus latitude                    !TPAR 72
      TAVG = TBAR*(1.0D0 + 1.369D-4*SUNLAT*LAT)                                   !TPAR 73
!---  Phase angles (hours) for local solar time variation                         !TPAR 74
      T1 = 13.2D0 - 0.00119D0*SUNLAT*LAT                                          !TPAR 75
      T2 = 9.4D0 - 0.00231D0*SUNLAT*LAT                                           !TPAR 76
!---  Amplitude factor for local solar time variation                             !TPAR 77
      POLEAMP = 1.0D0                                                             !TPAR 78
      IF (DABS(LAT) > 85.0D0) POLEAMP = 1.0D0 - (DABS(LAT) - 85.0D0)/5.0D0        !TPAR 79
      CPHI = DCOS(PI180*(LAT + SUNLAT)/(1.0D0 + LATMAX/90.0D0))*POLEAMP           !TPAR 80
!---  Exospheric temperature (K) versus local solar time                          !TPAR 81
      TINF0 = TAVG*(1.0D0 + 0.22D0*CPHI*DCOS(PI180*15.0D0*(LST - T1)) + 0.04D0* & !TPAR 82
         CPHI*DCOS(PI180*30.0D0*(LST - T2)))                                      !TPAR 83
!---  Global mean height of thermosphere base (km)                                !TPAR 84
      ZBAR = 197.94D0 - 49.058D0*RAU                                              !TPAR 85
!---  Latitude variation factor                                                   !TPAR 86
      FACTLAT = (SUNLAT/LATMAX)*(LAT/77.5D0)**3                                   !TPAR 87
!---  Zonal average base height (km) versus latitude                              !TPAR 88
      ZAVG = ZBAR + 4.3D0*FACTLAT                                                 !TPAR 89
!---  Amplitudes for local solar time variation                                   !TPAR 90
      A1 = (1.5D0 - DCOS(PI180*4.0D0*LAT))*POLEAMP                                !TPAR 91
      A2 = (2.3D0*DCOS(PI180*(LAT + 0.5D0*SUNLAT))**3)*POLEAMP                    !TPAR 92
!---  Phase angles (hours) for local solar time variation                         !TPAR 93
      T1 = 16.2D0 - (SUNLAT/LATMAX)*DATAN(PI180*10.0D0*LAT)                       !TPAR 94
      T2 = 11.5D0                                                                 !TPAR 95
!---  Base height of thermosphere (km) versus local solar time                    !TPAR 96
      ZF0 = ZAVG + A1*DCOS(PI180*15.0D0*(LST - T1)) + A2*DCOS(PI180*30.0D0*(LST & !TPAR 97
          - T2))                                                                  !TPAR 98
!---  Global mean temperature (K) at thermosphere base, versus FBARR              !TPAR 99
      TBAR = 113.7D0 + 0.5791D0*FBARR                                             !TPAR100
!---  Zonal average temperature at thermosphere base (K) vs. latitude             !TPAR101
      TAVG = TBAR*(1.0D0 + 0.186D0*FACTLAT)                                       !TPAR102
!---  Amplitudes for local solar time variation                                   !TPAR103
      A1 = (0.06D0 - 0.05D0*DCOS(PI180*4.0D0*LAT))*POLEAMP                        !TPAR104
      A2 = (0.1D0*DCOS(PI180*(LAT + 0.5D0*SUNLAT))**3)*POLEAMP                    !TPAR105
!---  Phase angles (hours) for local solar time variation                         !TPAR106
      T1 = 17.5D0 - 2.5D0*(SUNLAT/LATMAX)*DATAN(PI180*10.0D0*LAT)                 !TPAR107
      T2 = 10.0D0 + 2.0D0*(LAT/77.5D0)**2                                         !TPAR108
!---  Thermosphere base temperature (K) versus local solar time                   !TPAR109
      TF0 = TAVG*(1.0D0 + A1*DCOS(PI180*15.0D0*(LST - T1)) + A2*DCOS(PI180*     & !TPAR110
         30.0D0*(LST - T2)))                                                      !TPAR111
!---  Global mean scale height (km) of thermospheric temperature                  !TPAR112
      SCALE = 8.38D0 + 0.09725D0*FBARR                                            !TPAR113
!---  Zonal average temperature scale height (km) vs. latitude                    !TPAR114
      SCALE = SCALE*(1.14D0 - 0.18D0*DCOS(PI180*LAT))                             !TPAR115
      RETURN                                                                      !TPAR116
      END SUBROUTINE THERMPAR_M10                                                 !TPAR117
!                                                                                 !TPAR118
!------------------------------------------------------------------------------   !TTRP  1
      subroutine tgcmterp_M10(khgtt, time, ttgcm, ptgcm, dtgcm, utgcm, vtgcm,   & !TTRP  2
         zf, tempday, presday, densday, uwndday, vwndday, tempmax, tempmin,     & !TTRP  3
         densmax, densmin, idaydata)                                              !TTRP  4
!-----------------------------------------------                                  !TTRP  5
!   M o d u l e s                                                                 !TTRP  6
!-----------------------------------------------                                  !TTRP  7
      USE vast_kind_param, ONLY:  double                                          !TTRP  8
      USE tgcmdata_M10_C                                                          !TTRP  9
      USE interp_M10_C                                                            !TTRP 10
      USE parameters_M10_C                                                        !TTRP 11
!-----------------------------------------------                                  !TTRP 12
!---    Interpolates University of Michigan Mars Thermospheric General            !TTRP 13
!       Circulation Model (MTGCM) data to a given latitude, time of               !TTRP 14
!       year (Ls), and dust optical depth, for a given height index               !TTRP 15
!       (khgtt) and time of day (time).                                           !TTRP 16
!       Some input data is provided by the Common "Interp".                       !TTRP 17
!---    Set parameter values for number of heights (nhgtt), number                !TTRP 18
!       of latitudes (nlatt), and number of dust optical depth values             !TTRP 19
!...                                                                              !TTRP 20
!...Switches:                                                                     !TTRP 21
!-----------------------------------------------                                  !TTRP 22
!   I n t e r f a c e   B l o c k s                                               !TTRP 23
!-----------------------------------------------                                  !TTRP 24
      use tidex_M10_I                                                             !TTRP 25
      use tidey_M10_I                                                             !TTRP 26
      use fourd_M10_I                                                             !TTRP 27
      implicit none                                                               !TTRP 28
!-----------------------------------------------                                  !TTRP 29
!   D u m m y   A r g u m e n t s                                                 !TTRP 30
!-----------------------------------------------                                  !TTRP 31
      integer , intent(in) :: khgtt                                               !TTRP 32
      integer , intent(in) :: idaydata                                            !TTRP 33
      real(double) , intent(in) :: time                                           !TTRP 34
      real(double) , intent(out)  :: ttgcm                                        !TTRP 35
      real(double) , intent(out)  :: ptgcm                                        !TTRP 36
      real(double) , intent(out)  :: dtgcm                                        !TTRP 37
      real(double) , intent(out)  :: utgcm                                        !TTRP 38
      real(double) , intent(out)  :: vtgcm                                        !TTRP 39
      real(double) , intent(out)  :: zf                                           !TTRP 40
      real(double) , intent(out)  :: tempday                                      !TTRP 41
      real(double) , intent(out)  :: presday                                      !TTRP 42
      real(double) , intent(out) :: densday                                       !TTRP 43
      real(double) , intent(out)  :: uwndday                                      !TTRP 44
      real(double) , intent(out)  :: vwndday                                      !TTRP 45
      real(double) , intent(out)  :: tempmax                                      !TTRP 46
      real(double) , intent(out)  :: tempmin                                      !TTRP 47
      real(double) , intent(out)  :: densmax                                      !TTRP 48
      real(double) , intent(out)  :: densmin                                      !TTRP 49
!-----------------------------------------------                                  !TTRP 50
!   L o c a l   V a r i a b l e s                                                 !TTRP 51
!-----------------------------------------------                                  !TTRP 52
      integer :: i, l, m, n, itime                                                !TTRP 53
      real(double), dimension(2,2,2,2) :: tt, pt, ut, vt, r0, dt, zt, tday,     & !TTRP 54
         pday, uday, vday, tmax, tmin, dmax, dmin                                 !TTRP 55
      real(double) :: polefac, t0, a1, p1, a2, p2, xtime, ttime, p0, a1p, p1p,  & !TTRP 56
         a2p, p2p, d0, dtime, u0, v0, z0, rtgcm                                   !TTRP 57
!-----------------------------------------------                                  !TTRP 58
!---    MTGCM 80-170 km data arrays for interpolation                             !TTRP 59
!---    Establish MTGCM values at corners of a 4-dimensional cube in              !TTRP 60
!       latitude-Ls-dust-F107 space, at the given height index (khgtt),           !TTRP 61
!       and time of day (time)                                                    !TTRP 62
      do i = 1, 2                                                                 !TTRP 63
         polefac = 1.0D0                                                          !TTRP 64
         if (ilatt == 1) then                                                     !TTRP 65
            if (i == 1) polefac = tpolefac                                        !TTRP 66
         else if (ilatt == nlatt - 1) then                                        !TTRP 67
            if (i == 2) polefac = tpolefac                                        !TTRP 68
         endif                                                                    !TTRP 69
         do l = 1, 2                                                              !TTRP 70
            do m = 1, 2                                                           !TTRP 71
               do n = 1, 2                                                        !TTRP 72
!---      Daily mean temperature                                                  !TTRP 73
                  t0 = tta0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP 74
                  tday(i,l,m,n) = t0                                              !TTRP 75
!---      Temperature tide amplitudes and phases                                  !TTRP 76
                  a1 = tta1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP 77
                  p1 = ttp1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP 78
                  a2 = tta2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP 79
                  p2 = ttp2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP 80
!---      Temperature at corners of 3-D cube                                      !TTRP 81
                  tt(i,l,m,n) = tidex_M10(t0,a1,p1,a2,p2,time)                    !TTRP 82
!---      Max and Min temperatures at corners of 3-D cube                         !TTRP 83
                  tmax(i,l,m,n) = -9999.0D0                                       !TTRP 84
                  tmin(i,l,m,n) = 9999.0D0                                        !TTRP 85
                  if (idaydata > 0) then                                          !TTRP 86
                     do itime = 0, 23                                             !TTRP 87
                        xtime = float(itime)                                      !TTRP 88
                        ttime = tidex_M10(t0,a1,p1,a2,p2,xtime)                   !TTRP 89
                        tmax(i,l,m,n) = dmax1(ttime,tmax(i,l,m,n))                !TTRP 90
                        tmin(i,l,m,n) = min(ttime,tmin(i,l,m,n))                  !TTRP 91
                     end do                                                       !TTRP 92
                  endif                                                           !TTRP 93
!---      Daily mean pressure                                                     !TTRP 94
                  p0 = pta0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP 95
                  pday(i,l,m,n) = p0                                              !TTRP 96
!---      Pressure tide amplitudes and phases                                     !TTRP 97
                  a1p = pta1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac   !TTRP 98
                  p1p = ptp1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)           !TTRP 99
                  a2p = pta2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac   !TTRP100
                  p2p = ptp2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)           !TTRP101
!---      Pressure at corners of 3-D cube                                         !TTRP102
                  pt(i,l,m,n) = tidey_M10(p0,a1p,p1p,a2p,p2p,time)                !TTRP103
!---      Daily mean density                                                      !TTRP104
                  d0 = dta0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP105
!---      Density tide amplitudes and phases                                      !TTRP106
                  a1 = dta1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP107
                  p1 = dtp1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP108
                  a2 = dta2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP109
                  p2 = dtp2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP110
!---      Density at corners of 3-D cube                                          !TTRP111
                  dt(i,l,m,n) = tidey_M10(d0,a1,p1,a2,p2,time)                    !TTRP112
!---      Max and Min densities at corners of 3-D cube                            !TTRP113
                  dmax(i,l,m,n) = -9999.0D0                                       !TTRP114
                  dmin(i,l,m,n) = 9999.0D0                                        !TTRP115
                  if (idaydata > 0) then                                          !TTRP116
                     do itime = 0, 23                                             !TTRP117
                        xtime = float(itime)                                      !TTRP118
                        dtime = tidey_M10(d0,a1,p1,a2,p2,xtime)                   !TTRP119
                        dmax(i,l,m,n) = dmax1(dtime,dmax(i,l,m,n))                !TTRP120
                        dmin(i,l,m,n) = min(dtime,dmin(i,l,m,n))                  !TTRP121
                     end do                                                       !TTRP122
                  endif                                                           !TTRP123
!---      Gas constant from pressure, density, and temperature                    !TTRP124
                  r0(i,l,m,n) = 190.0D0                                           !TTRP125
                  if (dabs(t0)>0.0D0 .and. dabs(d0)>0.0D0) r0(i,l,m,n)=p0/(t0*d0) !TTRP126
!---      Daily mean EW wind                                                      !TTRP127
                  u0 = uta0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP128
                  uday(i,l,m,n) = u0                                              !TTRP129
!---      EW wind tide amplitudes and phases                                      !TTRP130
                  a1 = uta1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP131
                  p1 = utp1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP132
                  a2 = uta2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP133
                  p2 = utp2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP134
!---      EW wind at corners of 3-D cube                                          !TTRP135
                  ut(i,l,m,n) = tidex_M10(u0,a1,p1,a2,p2,time)                    !TTRP136
!---      Daily mean NS wind                                                      !TTRP137
                  v0 = vta0(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP138
                  vday(i,l,m,n) = v0                                              !TTRP139
!---      NS wind tide amplitudes and phases                                      !TTRP140
                  a1 = vta1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP141
                  p1 = vtp1(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP142
                  a2 = vta2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac    !TTRP143
                  p2 = vtp2(khgtt,ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)            !TTRP144
!---      NS wind at corners of 3-D cube                                          !TTRP145
                  vt(i,l,m,n) = tidex_M10(v0,a1,p1,a2,p2,time)                    !TTRP146
!---      Tide amplitudes and phases for ZF=height of 1.26 nbar level             !TTRP147
                  z0 = zfa0(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)                  !TTRP148
                  a1 = zfa1(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac          !TTRP149
                  p1 = zfp1(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)                  !TTRP150
                  a2 = zfa2(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)*polefac          !TTRP151
                  p2 = zfp2(ilatt+i-1,ls+l-1,mdust+m-1,mf10+n-1)                  !TTRP152
!---      ZF values at corners of 3-D cube                                        !TTRP153
                  zt(i,l,m,n) = tidex_M10(z0,a1,p1,a2,p2,time)                    !TTRP154
               end do                                                             !TTRP155
            end do                                                                !TTRP156
         end do                                                                   !TTRP157
      end do                                                                      !TTRP158
!---    Use 4-D interpolation to get temperature, pressure, density,              !TTRP159
!       gas constant, EW wind, NS wind, and ZF height at given lati-              !TTRP160
!       tude, Ls, dust optical depth, and solar activity                          !TTRP161
      call fourd_M10 (dlatt, dls, ddust, df10, tt, ttgcm, 0)                      !TTRP162
      call fourd_M10 (dlatt, dls, ddust, df10, tday, tempday, 0)                  !TTRP163
      call fourd_M10 (dlatt, dls, ddust, df10, tmax, tempmax, 0)                  !TTRP164
      call fourd_M10 (dlatt, dls, ddust, df10, tmin, tempmin, 0)                  !TTRP165
      if (idaydata == 1) then                                                     !TTRP166
         call fourd_M10 (dlatt, dls, ddust, df10, dmax, densmax, 1)               !TTRP167
      else                                                                        !TTRP168
         call fourd_M10 (dlatt, dls, ddust, df10, dmax, densmax, 0)               !TTRP169
      endif                                                                       !TTRP170
      call fourd_M10 (dlatt, dls, ddust, df10, dmin, densmin, 1)                  !TTRP171
      call fourd_M10 (dlatt, dls, ddust, df10, pt, ptgcm, 1)                      !TTRP172
      call fourd_M10 (dlatt, dls, ddust, df10, pday, presday, 1)                  !TTRP173
      call fourd_M10 (dlatt, dls, ddust, df10, dt, dtgcm, 1)                      !TTRP174
      call fourd_M10 (dlatt, dls, ddust, df10, r0, rtgcm, 0)                      !TTRP175
!---    Daily density from gas constant                                           !TTRP176
      densday = presday/(rtgcm*tempday)                                           !TTRP177
      call fourd_M10 (dlatt, dls, ddust, df10, ut, utgcm, 0)                      !TTRP178
      call fourd_M10 (dlatt, dls, ddust, df10, uday, uwndday, 0)                  !TTRP179
      call fourd_M10 (dlatt, dls, ddust, df10, vt, vtgcm, 0)                      !TTRP180
      call fourd_M10 (dlatt, dls, ddust, df10, vday, vwndday, 0)                  !TTRP181
      call fourd_M10 (dlatt, dls, ddust, df10, zt, zf, 0)                         !TTRP182
      return                                                                      !TTRP183
      end subroutine tgcmterp_M10                                                 !TTRP184
!                                                                                 !TTRP185
!------------------------------------------------------------------------------   !TWOD  1
      subroutine twod_M10(dx, dy, array, value)                                   !TWOD  2
!-----------------------------------------------                                  !TWOD  3
!   M o d u l e s                                                                 !TWOD  4
!-----------------------------------------------                                  !TWOD  5
      USE vast_kind_param, ONLY:  double                                          !TWOD  6
!---    2-Dimensional linear interpolation within a 1x1 cube (x,y) of             !TWOD  7
!       Array(2,2) to position dx,dy (both 0-1).                                  !TWOD  8
!       Value is value of interpolated output.                                    !TWOD  9
!...                                                                              !TWOD 10
!...Switches:                                                                     !TWOD 11
      implicit none                                                               !TWOD 12
!-----------------------------------------------                                  !TWOD 13
!   D u m m y   A r g u m e n t s                                                 !TWOD 14
!-----------------------------------------------                                  !TWOD 15
      real(double) , intent(in) :: dx                                             !TWOD 16
      real(double) , intent(in) :: dy                                             !TWOD 17
      real(double) , intent(out) :: value                                         !TWOD 18
      real(double) , intent(in) :: array(2,2)                                     !TWOD 19
!-----------------------------------------------                                  !TWOD 20
!   L o c a l   V a r i a b l e s                                                 !TWOD 21
!-----------------------------------------------                                  !TWOD 22
      real(double) :: dxp, dyp                                                    !TWOD 23
!-----------------------------------------------                                  !TWOD 24
!---    Complementary displacements in x,y,z                                      !TWOD 25
      dxp = 1.0D0 - dx                                                            !TWOD 26
      dyp = 1.0D0 - dy                                                            !TWOD 27
!---    Do 2-D linear interpolation to get Value from Array                       !TWOD 28
      value = dxp*dyp*array(1,1) + dxp*dy*array(1,2) + dx*dyp*array(2,1) + dx*  & !TWOD 29
         dy*array(2,2)                                                            !TWOD 30
      return                                                                      !TWOD 31
      end subroutine twod_M10                                                     !TWOD 32
!                                                                                 !TWOD 33
!------------------------------------------------------------------------------   !WAVE  1
      subroutine wavelon_M10(lonew, wlon, clat, height, day, wavepert)            !WAVE  2
!-----------------------------------------------                                  !WAVE  3
!   M o d u l e s                                                                 !WAVE  4
!-----------------------------------------------                                  !WAVE  5
      USE vast_kind_param, ONLY:  double                                          !WAVE  6
      USE wavecoef_M10_C                                                          !WAVE  7
!---  Inputs                                                                      !WAVE  8
!       LonEW:  0 if West Longitude phases, 1 if East Longitude phases            !WAVE  9
!       Wlon:   Current West Longitude (degrees)                                  !WAVE 10
!       CLat:   Current Latitude (degrees)                                        !WAVE 11
!       Height: Current altitude (km)                                             !WAVE 12
!     Output:                                                                     !WAVE 13
!       wavepert: relative perturbation due to wave model                         !WAVE 14
!                                                                                 !WAVE 15
!...                                                                              !WAVE 16
!...Switches:                                                                     !WAVE 17
      implicit none                                                               !WAVE 18
!-----------------------------------------------                                  !WAVE 19
!   D u m m y   A r g u m e n t s                                                 !WAVE 20
!-----------------------------------------------                                  !WAVE 21
      integer , intent(in) :: lonew                                               !WAVE 22
      real(double) , intent(in) :: wlon                                           !WAVE 23
      real(double) , intent(in) :: clat                                           !WAVE 24
      real(double) , intent(in) :: height                                         !WAVE 25
      real(double) , intent(in) :: day                                            !WAVE 26
      real(double) , intent(out) :: wavepert                                      !WAVE 27
!-----------------------------------------------                                  !WAVE 28
!   L o c a l   V a r i a b l e s                                                 !WAVE 29
!-----------------------------------------------                                  !WAVE 30
      real(double) :: scale, heightfact, pi180, dd, phi1, phi2, phi3, dphi1dt,  & !WAVE 31
         dphi2dt, dphi3dt, polefact                                               !WAVE 32
!-----------------------------------------------                                  !WAVE 33
!---  Scale for height variation of wave perturbations below 100 km               !WAVE 34
      scale = wscale                                                              !WAVE 35
      heightfact = 1.0D0                                                          !WAVE 36
!---  Assume exponential variation with height for waves below 100 km             !WAVE 37
      if (height < 100.0D0) heightfact = dexp((height - 100.0D0)/scale)           !WAVE 38
      pi180 = datan(1.0D0)/45.0D0                                                 !WAVE 39
      dd = day - wavedate                                                         !WAVE 40
      phi1 = wavephi1                                                             !WAVE 41
      phi2 = wavephi2                                                             !WAVE 42
      phi3 = wavephi3                                                             !WAVE 43
      dphi1dt = phi1dot                                                           !WAVE 44
      dphi2dt = phi2dot                                                           !WAVE 45
      dphi3dt = phi3dot                                                           !WAVE 46
!---  Convert phases to West longitude if LonEW = 1                               !WAVE 47
      if (lonew == 1) then                                                        !WAVE 48
         phi1 = 360.0D0 - phi1                                                    !WAVE 49
         phi2 = 360.0D0 - phi2                                                    !WAVE 50
         phi3 = 360.0D0 - phi3                                                    !WAVE 51
         dphi1dt = -phi1dot                                                       !WAVE 52
         dphi2dt = -phi2dot                                                       !WAVE 53
         dphi3dt = -phi3dot                                                       !WAVE 54
      endif                                                                       !WAVE 55
      if (wavedate <= 0.0D0) then                                                 !WAVE 56
         dd = 0.0D0                                                               !WAVE 57
         dphi1dt = 0.0D0                                                          !WAVE 58
         dphi2dt = 0.0D0                                                          !WAVE 59
         dphi3dt = 0.0D0                                                          !WAVE 60
      endif                                                                       !WAVE 61
!---  Relative perturbation factor due to wave model                              !WAVE 62
      wavepert = (wavea0 + wavea1*dcos(pi180*(wlon - phi1 - dphi1dt*dd)) +      & !WAVE 63
         wavea2*dcos(2.0D0*pi180*(wlon - phi2 - dphi2dt*dd)) + wavea3*dcos(     & !WAVE 64
         3.0D0*pi180*(wlon - phi3 - dphi3dt*dd)) - 1.0D0)*heightfact              !WAVE 65
!---  Insure wave perturbation goes to (WaveA0-1.)*Heightfact at poles            !WAVE 66
      if (dabs(clat) >= 85.0D0) then                                              !WAVE 67
         polefact = dcos(18.0D0*pi180*(dabs(clat) - 85.0D0))**2                   !WAVE 68
         wavepert = (wavepert - (wavea0 - 1.0D0)*heightfact)*polefact + (wavea0 & !WAVE 69
             - 1.0D0)*heightfact                                                  !WAVE 70
      endif                                                                       !WAVE 71
!---  Insure wave perturbation within proper limits                               !WAVE 72
      wavepert = dmax1(-0.9D0,wavepert)                                           !WAVE 73
      wavepert = min(9.0D0,wavepert)                                              !WAVE 74
      return                                                                      !WAVE 75
      end subroutine wavelon_M10                                                  !WAVE 76
!                                                                                 !WAVE 77
!------------------------------------------------------------------------------   !ZLGR  1
      real(kind(0.0d0)) function zlogr_M10 (znumer, zdenom, label)                !ZLGR  2
!-----------------------------------------------                                  !ZLGR  3
!   M o d u l e s                                                                 !ZLGR  4
!-----------------------------------------------                                  !ZLGR  5
      USE vast_kind_param, ONLY:  double                                          !ZLGR  6
!...                                                                              !ZLGR  7
!...Switches:                                                                     !ZLGR  8
      implicit none                                                               !ZLGR  9
!-----------------------------------------------                                  !ZLGR 10
!   D u m m y   A r g u m e n t s                                                 !ZLGR 11
!-----------------------------------------------                                  !ZLGR 12
      real(double) , intent(in) :: znumer                                         !ZLGR 13
      real(double) , intent(in) :: zdenom                                         !ZLGR 14
      character(len=7) , intent(in) :: label                                      !ZLGR 15
!-----------------------------------------------                                  !ZLGR 16
!   L o c a l   V a r i a b l e s                                                 !ZLGR 17
!-----------------------------------------------                                  !ZLGR 18
!-----------------------------------------------                                  !ZLGR 19
!---    Log of znumer/zdenom with error message for znumer, zdenom <= 0           !ZLGR 20
      if (znumer<=0.0D0 .or. zdenom<=0.0D0) then                                  !ZLGR 21
         zlogr_M10 = 1.0D0                                                        !ZLGR 22
         write (*, *) ' Log error at ', label, znumer, zdenom                     !ZLGR 23
      else                                                                        !ZLGR 24
         zlogr_M10 = dlog(znumer/zdenom)                                          !ZLGR 25
      endif                                                                       !ZLGR 26
      return                                                                      !ZLGR 27
      end function zlogr_M10                                                      !ZLGR 28
!                                                                                 !ZLGR 29
!------------------------------------------------------------------------------   !RTES  1
      subroutine readtes_M10(datadir, version)                                    !RTES  2
!-----------------------------------------------                                  !RTES  3
!   M o d u l e s                                                                 !RTES  4
!-----------------------------------------------                                  !RTES  5
      USE vast_kind_param, ONLY:  double                                          !RTES  6
      USE tesdust_M10_C                                                           !RTES  7
!...                                                                              !RTES  8
!...Switches:                                                                     !RTES  9
      implicit none                                                               !RTES 10
!-----------------------------------------------                                  !RTES 11
!   G l o b a l   P a r a m e t e r s                                             !RTES 12
!-----------------------------------------------                                  !RTES 13
!-----------------------------------------------                                  !RTES 14
!   D u m m y   A r g u m e n t s                                                 !RTES 15
!-----------------------------------------------                                  !RTES 16
      character(len=99) , intent(in) :: datadir                                   !RTES 17
      character , intent(in) :: version                                           !RTES 18
!-----------------------------------------------                                  !RTES 19
!   L o c a l   P a r a m e t e r s                                               !RTES 20
!-----------------------------------------------                                  !RTES 21
      character(len=11), parameter :: sysform = 'unformatted'                     !RTES 22
!-----------------------------------------------                                  !RTES 23
!   L o c a l   V a r i a b l e s                                                 !RTES 24
!-----------------------------------------------                                  !RTES 25
      integer :: iunit, lendir                                                    !RTES 26
!-----------------------------------------------                                  !RTES 27
!---     Set parameter for "form=" in binary file Open statement                  !RTES 28
      iunit = 67                                                                  !RTES 29
!---     Compute character string length of DATADIR path name                     !RTES 30
      lendir = Len_Trim(datadir)                                                  !RTES 31
      if (lendir<1 .or. lendir>99) lendir = 99                                    !RTES 32
!---     Open and read TES dust optical depths vs year, Ls, lat, lon              !RTES 33
      open(iunit, file=datadir(1:lendir)//'TESdust'//version//'.bin', form=     & !RTES 34
         sysform, status='old', position='asis')                                  !RTES 35
      read (iunit) testau                                                         !RTES 36
      close(iunit)                                                                !RTES 37
      return                                                                      !RTES 38
      end subroutine readtes_M10                                                  !RTES 39
!                                                                                 !RTES 40
!------------------------------------------------------------------------------   !TESD  1
      subroutine tesod_M10(mapyear, xls, ylat, zlon, dustod)                      !TESD  2
!-----------------------------------------------                                  !TESD  3
!   M o d u l e s                                                                 !TESD  4
!-----------------------------------------------                                  !TESD  5
      USE vast_kind_param, ONLY:  double                                          !TESD  6
      USE tesdust_M10_C                                                           !TESD  7
      USE parameters_M10_C                                                        !TESD  8
!-----------------------------------------------                                  !TESD  9
!---     Interpolate TES optical depths vs Ls, lat and lon for given              !TESD 10
!        TES mapping year                                                         !TESD 11
!---     Set parameter values for number of years, Ls's, lats and lons            !TESD 12
!...                                                                              !TESD 13
!...Switches:                                                                     !TESD 14
!-----------------------------------------------                                  !TESD 15
!   I n t e r f a c e   B l o c k s                                               !TESD 16
!-----------------------------------------------                                  !TESD 17
      use ifloor_M10_I                                                            !TESD 18
      use threed_M10_I                                                            !TESD 19
      implicit none                                                               !TESD 20
!-----------------------------------------------                                  !TESD 21
!   D u m m y   A r g u m e n t s                                                 !TESD 22
!-----------------------------------------------                                  !TESD 23
      integer , intent(in) :: mapyear                                             !TESD 24
      real(double) , intent(in) :: xls                                            !TESD 25
      real(double) , intent(in) :: ylat                                           !TESD 26
      real(double) , intent(in) :: zlon                                           !TESD 27
      real(double) , intent(out)  :: dustod                                       !TESD 28
!-----------------------------------------------                                  !TESD 29
!   L o c a l   V a r i a b l e s                                                 !TESD 30
!-----------------------------------------------                                  !TESD 31
      integer :: jlat1, jlat2, ils1, ils2, jlon1, jlon2                           !TESD 32
      real(double), dimension(2,2,2) :: array                                     !TESD 33
      real(double) :: steplat, steplon, stepls, halfstep, ylat1, dlat, xls1,    & !TESD 34
         dls, dlon                                                                !TESD 35
!-----------------------------------------------                                  !TESD 36
!---     Set latitude, longitude and Ls data step intervals                       !TESD 37
      steplat = 180.0D0/(nteslat - 1.0D0)                                         !TESD 38
      steplon = 360.0D0/nteslon                                                   !TESD 39
      stepls = 360.0D0/ntesls                                                     !TESD 40
      halfstep = stepls/2.0D0                                                     !TESD 41
!---     Find lat interpolation index values jlat1, jlat2                         !TESD 42
      jlat1 = idint((ylat + 90.0D0 + steplat)/steplat)                            !TESD 43
      if (jlat1 >= nteslat) jlat1 = nteslat - 1                                   !TESD 44
      jlat2 = jlat1 + 1                                                           !TESD 45
      ylat1 = (-(90.0D0 + steplat)) + steplat*jlat1                               !TESD 46
!---     Increment in lat within interpolation box                                !TESD 47
      dlat = (ylat - ylat1)/steplat                                               !TESD 48
      ils1 = idint((xls + halfstep)/stepls)                                       !TESD 49
      xls1 = stepls*ils1 - halfstep                                               !TESD 50
!---     Find Ls interpolation index values ils1, ils2                            !TESD 51
      ils2 = ils1 + 1                                                             !TESD 52
!---     Increment in Ls within interpolation box                                 !TESD 53
      dls = (xls - xls1)/stepls                                                   !TESD 54
!---     Adjust Ls index values near Ls = 0 or 360                                !TESD 55
      if (ils1==0 .or. ils1==72) then                                             !TESD 56
         ils1 = 72                                                                !TESD 57
         ils2 = 1                                                                 !TESD 58
      endif                                                                       !TESD 59
!---     Find lon interpolation index jlon                                        !TESD 60
      jlon1 = ifloor_M10(zlon/steplon)                                            !TESD 61
      jlon1 = min0(nteslon - 1,jlon1)                                             !TESD 62
      jlon2 = jlon1 + 1                                                           !TESD 63
      dlon = (zlon - steplon*jlon1)/steplon                                       !TESD 64
!---     Fill 3-d array box for interpolation                                     !TESD 65
      array(1,1,1) = testau(mapyear,ils1,jlat1,jlon1)                             !TESD 66
      array(1,1,2) = testau(mapyear,ils1,jlat1,jlon2)                             !TESD 67
      array(1,2,1) = testau(mapyear,ils1,jlat2,jlon1)                             !TESD 68
      array(1,2,2) = testau(mapyear,ils1,jlat2,jlon2)                             !TESD 69
      array(2,1,1) = testau(mapyear,ils2,jlat1,jlon1)                             !TESD 70
      array(2,1,2) = testau(mapyear,ils2,jlat1,jlon2)                             !TESD 71
      array(2,2,1) = testau(mapyear,ils2,jlat2,jlon1)                             !TESD 72
      array(2,2,2) = testau(mapyear,ils2,jlat2,jlon2)                             !TESD 73
!---     Do 3-D interpolation on Ls, lat and lon                                  !TESD 74
      call threed_M10 (dls, dlat, dlon, array, dustod, 0)                         !TESD 75
      return                                                                      !TESD 76
      end subroutine tesod_M10                                                    !TESD 77
!------------------------------------------------------------------------------   !TESD 78
