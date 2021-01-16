                  DESCRIPTION OF MARS-GRAM 2010 SPECIAL FEATURES 
                     NOT FULLY COVERED IN OTHER README FILES


			1.  COORDINATE TRANSFORMATIONS

EAST VERSUS WEST LONGITUDE

Input parameter LonEW allows the user to select whether input/output longitudes
are East positive (LonEW=1) or West positive (LonEW=0).  Most internal 
calculations in Mars-GRAM use West Longitude.  LonEW determines whether longitudes
in several input files are positive East or West.  These include the NAMELIST,
trajectory, and auxiliary profile files (see README3.txt).  Files for MGCM 
surface data, TES mapping years 1 & 2 dust optical depth data, and surface 
albedo have West longitude positive, while the MOLA topography data file has 
East longitude positive (See README3.txt).  Mars-GRAM automatically converts
input and output to the longitude convention specified by LonEW.  Plot 
variables NVARX or NVARY = 15 allow output longitudes to be expressed in the  
range -180 to +180 degrees, instead of 0 to 360 degrees (See file README1.txt).
Mars-GRAM 2010 uses the IAU 2000 definition of prime meridian, which differs
from IAU 1991 definition, used in Mars-GRAM 2001, by about 0.24 degrees. This
difference effectively shifts all longitudes by a corresponding amount.  The
input MOLA data set is still in IAU 1991 coordinates, but the necessary shift
is made automatically within Mars-GRAM 2010.


ALTITUDE COORDINATES

In Mars-GRAM, the zero datum level for measuring altitude (corresponding to
mean sea level on Earth) can be either: (1) a reference ellipsoid, specified
by given values for equatorial radius (input requa) and polar radius (input
rpole), or (2) a constant potential surface (areoid), given in the MOLA data 
file at 1/2 by 1/2 degree latitude resolution (see file README3.txt). Choice 
between these two altitude systems is made by setting the value of input 
parameter MOLAhgts (1 for MOLA areoid, 0 for reference ellipsoid). The value
of MOLAhgts controls interpretation of altitudes given in the NAMELIST, 
trajectory, and auxiliary profile files.  Most internal calculations in
Mars-GRAM are done in the MOLA height coordinate system.  All heights in
MGCM and MTGCM input data files are in the MOLA system.  Output altitudes,
as determined by values of NVARX or NVARY, can also be expressed as height
above the MOLA topographic surface (height above ground level; See file
README1.txt).  If using heights above the reference ellipsoid, these may be
expressed in either planeto-centric or planeto-graphic coordinate systems
(see following section).  Heights in the MOLA system must be planeto-centric
(See file README1.txt).  Differences between planeto-centric and planeto-
graphic heights are usually no more than a few meters at all altitudes of
interest.

NOTE:  If input heights are > 3000 km, they are interpreted as 
       planetocentric radius values (km).


PLANETO-CENTRIC VERSUS PLANETO-GRAPHIC LATITUDE

Planeto-centric latitude of a given point of interest is defined as the angle 
between the equatorial plane and the radius vector from the center of mass of 
the planet to the given point.  Latitude for MOLA areoid and topography is
given in the planeto-centric system.  Most internal calculations in Mars-GRAM
2010 are done in the planeto-centric system. For altitudes relative to a given
reference ellipsoid (see previous section), latitudes can alternatively be
expressed in the planeto-graphic system.  For Earth, the planeto-centric and
planeto-graphic systems are also referred to as geocentric and geographic (or
geodetic) coordinates, respectively.  Planeto-graphic latitude for a given
point of interest is defined as the angle between the equatorial plane and
the vector that is tangent to the reference ellipsoid, and which passes through 
the given point.  Transformation from a given planeto-graphic latitude and 
altitude to planeto-centric latitude and altitude (above the reference 
ellipsoid) is done with subroutine GeodettoGeocent, which uses an analytical 
method given on page K12 of "The Astronomical Almanac".  For transformation 
from a given planeto-centric latitude and altitude to planeto-graphic latitude 
and altitude (above the reference ellipsoid), it is common to use an iterative
method also given on page K12 of "The Astronomical Almanac".  However, 
Mars-GRAM uses an analytical solution for this transformation, adapted from
the Polish version of K.M.Borkowski, Bull. Geod. vol 63, pp.50-56 (1989). 


                        2. MARS EPHEMERIS ROUTINE
                           
A more accurate Mars ephemeris routine is introduced, based on equations for 
"moderately accurate" Mars solar time, seasonal parameters, and one-way Mars-
Earth light time, from Allison and McEwen, Planet. Space Sci., 48, 215-235 
(2000), and Allison, Geophys Res. Lett., 24(16), 1967-1970 (1997).  Mars
perturbations, from effects of Jupiter, Earth, and Venus, are computed from
Table 5 and eqn (18) of Allison and McEwen.  Additional perturbation terms
were derived and added, determined from empirical fits to JPL "Horizons"
ephemeris generator output.  The IAU 2000 definition for prime meridian is
used. This new ephemeris routine computes areocentric longitude of the Sun (Ls) 
and solar longitude to an RMS accuracy of about 0.004 degrees.  RMS accuracy of 
local true solar time (LTST) is approximately 1 second.



                           3. TIME TRANSFORMATIONS

Mars-GRAM 2010 options allow time to be expressed in either Terrestrial 
(Dynamical) Time (TT) or Coordinated Universal Time (UTC) and in either Mars-
Event Time (MET) or Earth-Receive Time (ERT).  Earlier Mars-GRAM versions
used UTC and ERT as the time basis.  Accurate ephemeris calculations are best
done in TT, which progresses at a uniform rate of 86,400 SI seconds per mean
solar day.  UTC accounts for variations in Earth's actual rotation rate, and
includes occasional addition of "leap seconds" to maintain this consistency.
The Mars-GRAM ephemeris routine assumes conversion from TT to UTC to be
given by

    TT  =  UTC  +  (64.184 + 95 * dt + 35 * dt**2)   (in seconds)

where dt = (YEAR - 2000)/100.

MET is the time that would be measured by a clock on a spacecraft at Mars.
ERT is the time at which a signal, propagated at a given MET, would be received
at Earth.  These differ by the one-way-light time (OWLT), calculated from
the speed of light and computed Earth-Mars distance, using the ephemeris
routine.  ERT and MET are therefore related by

    ERT = MET + OWLT           or         MET = ERT - OWLT



              4. SURFACE LAYER TEMPERATURE AND WIND PROFILES

Input files provide MGCM temperature data at the topographic surface and 
temperature and wind data at five meters above the surface (see README3.txt).
Profiles of temperature between the surface and 5-meter level are computed by 
subroutine bltp, using methods from the NASA Ames MGCM, as described by 
Haberle et al., Jour. Geophys. Res. 104(E4), 8957-8974, 1999.  Subroutine bltp 
uses an iterative technique and input parameters:  
                                
   gz = local acceleration of gravity (m/s**2) 
                 
   Cp = specific heat at constant pressure (joules kg**-1 K**-1)

   Tg = temperature (K) at ground surface
                       
   z5 = first MGCM height level (nominally 5 m) 
                
   T5 = temperature (K) at first MGCM level (nominally 5 m)
      
   U5 = zonal wind (m/s) at first MGCM level    
                 
   V5 = meridional wind (m/s) at first MGCM level 
                
   zeval  = height (m) at which to evaluate temperature 
         
   factor = factor for calculations = Ln(zeval/z0)/Ln(5/z0),
            where z0 is surface roughness parameter (m),

Subroutine bltp computes output value for Tempz = temperature (K) at height 
zeval. The iteration process first computes Richardson number (Ri) from

    Ri = (gz*Sqrt(Fh)/thet5)*((thet5 - Tg)/(1 + Sqrt(Fh)))*z5/(U5**2 + V5**2)

where thet5 is potential temperature at 5 m, from 

    thet5 = T5 + gz*z5/Cp   
       
It then computes temperature from

    Tempz = Tg +(thet5 - Tg)*(1 + Sqrt(Fh)*factor)/(1 + Sqrt(Fh)) - gz*zeval/Cp 
   
with stability parameter Sqrt(Fh) given by

    Sqrt(Fh) = (1 - 16*Ri)**0.25                           if Ri < 0 

or
    Sqrt(Fh) =(1 + 15*Ri/Sqrt(1 + 5*Ri))**(-0.5)           if Ri >= 0

The iterative process continues until convergence is reached, or for 10
iterations, whichever comes first.  Winds u(z) and v(z) at height z are
given by

    u(z) = U5 * Ln(z/z0) / Ln(z5/z0)
       
    v(z) = V5 * Ln(z/z0) / Ln(z5/z0)
       
where the surface roughness parameter z0 is taken to be 10**-2 meters if
the surface is not ice-covered, or 10**-4 meters if it is ice-covered.
Winds are taken to be zero for heights at or below z0.



                     5. "SUB-SURFACE" CONDITIONS

Topographic height information at 1/2 by 1/2 degree resolution is provided by
the MOLA data input (see README3.txt). Topographic heights at locations
between MOLA grid points are determined by 2-D interpolation in latitude and
longitude.  Occasionally it may be desired to evaluate "sub-surface" 
conditions, i.e. conditions at an altitude that is below the nominal height
of the interpolated topography.  For example, one may wish to estimate 
conditions at a rover position within a valley that is smaller than the MOLA 
grid resolution, and whose true topographic surface altitude is below that 
obtained by interpolation of the MOLA 1/2 degree data.  If the user inputs a 
"sub-surface" altitude (i.e. a height below local interpolated MOLA topographic 
height), then temperature is assumed to be the same as at the MOLA interpolated 
surface altitude, and winds are taken to be zero.  Pressure and density at the 
"sub-surface" altitude are evaluated using a pressure scale height consistent 
with near-surface conditions, and from the perfect gas law relationship. To 
evaluate conditions at a fixed altitude above the MOLA interpolated topographic
surface, use input parameter hgtasfcm, together with an input height that is
less than -10 km (used as a key to activate the hgtasfcm parameter).


                           6. SLOPE WINDS

Effects of terrain on the near-surface MGCM data are limited, due to the
relative course resolution (7.5 deg latitude by 9 degrees longitude) for
the MGCM data (see README3.txt).  Near-surface MGCM winds are augmented
(between 0 and ~ 4.5 km above the topographic surface) by using subroutine
slopewind to compute slope-dependent winds, using equations (12), (13), and
(14) from Ye, Segal, and Pielke, J. Atmos. Sci., 47, 612, 1990.  Terrain
slopes, at a resolution of 1/2 by 1/2 degree, are computed by evaluating
gradients of MOLA topographic height for the MOLA grid containing the
latitude-longitude of interest.  The method of Ye et al. provides an
analytical (algebraic) relationship to compute winds as a function of
altitude and terrain slope for daytime, thermally-driven, upslope winds.
The slopewind routine assumes a diurnal pattern of slope wind variation with 
time of day.  Peak daytime winds are assumed to occur at 15 hours local solar
time  Peak nighttime winds, due to downslope drainage flows, are assumed to
occur at 3 hours local solar time.  Lighter, cross-slope flows occur between 
these two times.  Vertical component slope winds are computed.  These are
proportional to the terrain slope, to the horizontal slope winds, and have an
assumed variation with altitude, which gives maximum vertical winds near
the middle of the slope wind altitude region, and zero vertical winds near
the surface and near the top of the slope wind altitude region.


                      7. SPECIES CONCENTRATIONS

Mars-GRAM 2010 subroutine "species" computes species concentrations as mole (or 
volume) fraction, mass fraction, and number density (#/m**3).  Average mole 
fraction and isotope ratio data are taken from Kieffer et al., editors, "Mars" 
(1992) and Tables A-5 and  A-6 of NASA/TM-2001-210935 (2001).  Below 80 km, 
NASA Ames Mars MGCM assumes a pure CO2 atmosphere (for which molecular weight 
would be 44.01).  In this height range, molecular weight computed from the 
perfect gas law relation [ M = R0 * rho * T / p ] would give values close to,  
but not exactly this value.  Deviations would be caused by the fact that the 
ratio of the averages is not the same as the average of the ratio, i.e.

    Avg(rho) * Avg(T) / Avg(p)    is not equal to     Avg( rho * T / p)    

Below 80 km, the "species" subroutine computes species concentrations and 
resultant average molecular weight by assumptions given below. Average 
molecular weight given by this subroutine is not exactly the same as average 
molecular weight computed from the perfect gas law.        
 
Below 80 km, the "species" subroutine computes species concentrations by 
assuming that atmospheric mass density from MGCM is correct.  Species 
concentrations are calculated using the following assumptions: 

    (a) Average dry-atmosphere mole fractions (fbar), are assumed.
 
    (b) Mole fractions are adjusted for seasonal variation of mass of CO2 in 
        the atmosphere, due to freezing and sublimation of CO2 at the poles.
  
    (c) Only the partial pressure of CO2 is assumed to vary seasonally, not 
        the partial pressures of other constituents, which are assumed not to 
        freeze out of the atmosphere. However, mole fractions of all species 
        vary seasonally due to this effect. 

    (d) Seasonal variation of total pressure is taken from subroutine PRSEAS, 
        which was used in the original Stewart thermosphere model, and was 
        employed in Mars-GRAM up through version 3.8. 
 
    (e) Water vapor concentration is added to the dry atmosphere by assuming 
        relative humidity = 20% (computed by function qrhtp, the same as used 
        in  marsrad.f calculations).       
    
Between 80 km and the base of the Stewart thermosphere (at altitude zbase), a 
combination of information is used from MTGCM and the modified Stewart model. 
MTGCM data used in Mars-GRAM do not include calculated mole fractions or number 
densities.  Mole fractions between 80 km and zbase height are computed from the   
following assumptions: 

    (a) Mole fractions for N2, Ar, and O2 are assumed to be the same as their 
        value at 80 km (from methods described above). 

    (b) Mole fractions for He, H2, H, and H2O are assumed to be zero. 

    (c) Mole fractions for N2, Ar, O2, and CO are assumed to vary linearly 
        from their values at 80 km [from method described above] to their 
        values at zbase altitude (from the Stewart model). 

    (d) Mole fractions for CO2 and O are computed from two constraint 
        conditions - (i) Sum of the mole fractions must be 1.0, and (ii) 
        Input molecular weight (AMz, from MTGCM value) is preserved.   
     
From height zbase up until MTGCM data runs out, a combination of information 
is used from MTGCM and the modified Stewart model.  In this height range, the 
following assumptions are used: 

    (a) Mole fractions for constituents other than CO2 and O are taken    
        from the modified Stewart model,
 
    (b) Molecular weight from MTGCM is assumed, 

    (c) Mole fractions for CO2 and O are computed from two constraint 
        conditions - (i) Sum of the mole fractions must be 1.0, and (ii) 
        Input molecular weight (AMz, from MTGCM value) is preserved.   
    
Above the top altitude for which MTGCM data are available, the same methodology 
is used as at immediately lower altitudes, except that the input value for 
molecular weight (AMz) is taken directly from the modified Stewart model. 


                    8. STATIONARY OR TRAVELING WAVES

Two methods are available for adjusting MTGCM and MGCM data from values as 
provided in the input files: wave multiplier adjustment and height offset
adjustment.  Wave multiplier adjustment, as the name implies, is a multipli-
cative adjustment factor, used to account for effects of stationary or 
traveling waves on MTGCM and MGCM values (See discussion in file README5.txt).
The wave multiplier may be as simple as a constant multiplier term (WaveA0), 
used to increase or decrease MTGCM and MGCM values from those provided in the 
input data files.  File README5.txt describes the mathematical form of the 
more general wave multiplier.

Wave multipliers are used to adjust both density and pressure values, but
temperatures are left unchanged.  This procedure preserves both the perfect
gas law (p = rho * R * T) and hydrostatic balance ( dp/dz = - rho * g) as in 
the original data.  Consider effects of a wave multiplier W, used to convert 
pressure p and density rho to new values p' and rho'.  We have

    rho'  =  W * rho  ,        p'   =  W * p        and        T' = T

Therefore

    p'  =  W * p  =  W * rho *R * T  =  rho' * R * T' 

which preserves the perfect gas law.  If W is independent of height z, then

    dp'/dz  =  W * dp/dz  =  -W * rho * g  = -rho' * g

which preserves the hydrostatic relationship.  Since wave multiplier W
changes pressure values, it also changes ZF, the height of the 1.26 nbar 
pressure level.  This adjustment is given by

    ZF'  =  ZF  + H(ZF) * Ln(W)             

where H(ZF) is pressure scale height at the ZF altitude level.


                          9. HEIGHT OFFSET

In addition to wave multiplier adjustment, MTGCM and MGCM data can also be 
adjusted by applying a height offset.  In contrast to wave multiplier
adjustment, which shifts density and pressure by a multiplier (see previous
section), height offset adjustment shifts the altitude at which density,
pressure AND temperature are evaluated.  For a given height offset deltaz,
new values p', rho', and T', for pressure p, density rho, and temperature T 
are evaluated by

    p'(z)  =  p(z - deltaz) 
 
    rho'(z)  =  rho(z - deltaz)   and 

    T'(z)  =  T(z - deltaz)

Positive values of deltaz cause an increase in density and pressure, while
negative values of deltaz cause a decrease in density and pressure.  ZF,
the height of the 1.26 nbar pressure level is also shifted by deltaz,
according to

    ZF'  =  ZF  + deltaz

Height offsets can be used for two purposes: (1) to control the smoothness of 
the transition at 80 km altitude between MGCM data and MTGCM data, or (2) as
another means (besides wave multipliers) to adjust MTGCM data for better
agreement with observations, such as obtained during aerobraking operations.
Height offset options are controlled by input parameters zoffset and ibougher.
Parameter zoffset (which may be positive, negative, or zero) provides a
constant term (in km), to be used in evaluating height offsets. Parameter
ibougher has the following meaning:

ibougher For MTGCM Height Offset Use:       For MGCM Height Offset Use:
-------- ----------------------------  ----------------------------------------------
   0               zoffset              [zoffset - daily avg local offset]*f(z)
   1          zoffset - A*Sin(Ls)       [zoffset - A*Sin(Ls) - daily avg offset]*f(z)
   2     global average height offset                    0
   3      daily average local offset                     0
   4       current offset = local                        0
           offset at current time
            
where f(z) varies linearly from 0 at 60 km to 1 at 80 km, and f(z) = 0 for all
altitudes below 60 km.  Values of global average height offset are given in 
file hgtoffst.dat (See description in file README3.txt).  Daily average local 
height offset, deltazday, is computed from

    deltazday  =  Hrhoday * Ln(rhodayMGCM/rhodayMTGCM)

where Hrhoday is density scale height computed from daily average MTGCM data 
between 80 and 85 km, and densities rhodayMGCM and rhodayMTGCM are daily 
average densities from unmodified MGCM and MTGCM data at 80 km.  Local height 
offset at current time is computed from

    deltaz  =  Hrho * Ln(rhoMGCM/rhoMTGCM)
    
where Hrho is local density scale height, computed from MTGCM data between 80 
and 85 km, and densities rhoMGCM and rhoMTGCM are local densities from 
unmodified MGCM and MTGCM data at 80 km.  Amplitude A for the seasonal 
variation in height offset is 2.5 km for Mars-GRAM 2001 data and 0.5 km for TES
mapping year 1 and 2 data.  Options ibougher = 2, 3, or 4 provide three 
different ways of height-shifting the MTGCM data (leaving the MGCM data 
unchanged), yielding three different degrees of smoothness of the MGCM-MTGCM 
transition at 80 km.  Options ibougher = 0 or 1 provide two methods for height-
shifting the MTGCM data to obtain better agreement with observational data, 
with corresponding height shifts of MGCM data, given in the table above, 
to insure smooth MGCM-MTGCM transition at 80 km altitude.  MGCM height offset 
factor f(z) makes these MGCM height offsets gradually diminish with altitude, 
until no MGCM height shift is applied at or below 60 km altitude.

If the dust storm model feature is used with Mars-GRAM 2001 input data, 
additional height offset, stormdz, is calculated by subroutine DUSTFACT.  Dust
storm height offset varies with time (Ls angle) according to the DUSTFACT
model for build-up and decay of dust storms (controlled by input parameters
ALS0, ALSDUR, and INTENS).  If a local/regional dust storm is being simulated,
spatial variation of stormdz is also controlled by input parameters dustlat,
dustlon, and radmax.


                   10. TES MAPPING YEAR 1 AND 2 DATA

File README3.txt describes characteristics of MGCM and MTGCM data files for
original Mars-GRAM 2001 data and for TES mapping years 1 and 2 data.  Input
parameter MapYear, from the NAMELIST input file, controls which of these data 
sets to use.  MapYear=0 means use Mars-GRAM 2001 data files, with dust
characteristics controlled by input parameters Dusttau, Dustmin, Dustmax,
Dustnu, Dustdiam, and Dustdens.  If MapYear = 0 and the dust storm simulation
model is used, then parameters ALS0, ALSDUR, INTENS, RADMAX, DUSTLAT, and
DUSTLON  are also used (in subroutine DUSTFACT).  If a MapYear value of 1 or 2
is input, then TES mapping year 1 or 2 data are used, and dust optical
depth variations with season, latitude, and longitude are as observed by 
the TES instrument on Mars Global Surveyor.  If MapYear = 1 or 2, then input 
values of Dusttau, Dustmin, Dustmax, and dust storm model parameters are
ignored. Characteristics of the file giving TES optical depth data are provided 
in file README3.txt.  Approximate dates for beginning of TES mapping years are:

TES Mapping Year	Starting Date	 Approx. Ls Range
----------------	-------------	 -----------------
       1		 ~Apr 1, 1999    115 -> 360 -> 115
       2                 ~Feb 1, 2001    115 -> 360 -> 115
       3		 ~Jan 1, 2003    115 -> 360 -> 115
       4		~Nov 15, 2004    115 -> 360 -> 115


                  11. OPTIONAL AUXILIARY PROFILE INPUT

As an option, data read from an auxiliary profile may be used to replace
data from the MGCM and MTGCM data files.  This option is controlled by
setting parameters profile, profnear, and proffar in the NAMELIST input
file.  Parameter profile gives the file name containing the profile data
values.  Parameter profnear is the latitude-longitude radius (in degrees) 
within which weight for the auxiliary profile is 1.0.  Parameter proffar
is the latitude-longitude radius (in degrees) beyond which weight for the
auxiliary profile is 0.0.  A weighting factor for the profile data (profwgt),
having values between 0 and 1, is applied between radii proffar and profnear.
Mean conditions are as given in the profile file if the desired point is within
a lat-lon radius of profnear from the profile lat-lon at the given altitude; 
mean conditions are as given by the original MGCM or MTGCM data if the desired 
point is beyond a lat-lon radius of proffar from the lat-lon of the profile 
at the given altitude.  If profnear = 0, then profile data are NOT used.
The profile weight factor (profwgt) for the auxiliary profile also varies 
between 0 at the first profile altitude level and 1 at the second profile 
altitude level (and between 1 at the next-to-last profile altitude level and 
0 at the last profile altitude level).  First and second profile points (and 
next-to-last and last profile points) should therefore be selected widely 
enough apart in altitude that a smooth transition can occur as profwgt changes 
form 0 to 1 near these profile end points.  Characteristics and format of the 
auxiliary profile input file are given in file README3.txt.


                   12. SOLAR AND INFRARED RADIATION

See discussion in file README4.txt for details of how to use Mars-GRAM to
generate data to be used in auxiliary program marsrad, for computation of
solar and infrared fluxes at the top and bottom of the atmosphere.


                13. WAVE MODEL FILES (FOR AEROBRAKING)

Three spacecraft (Mars Global Surveyor, Mars Odyssey, and Mars Reconnaissance
Orbiter) have performed aerobraking procedures at Mars.  In all three cases,
substantial orbit-to-orbit density variations were observed, even though each
successive periapsis pass occurred at essentially the same latitude and local
solar time (with only the longitude being different from pass-to-pass).  Mars-
GRAM includes an optional "longitude-dependent" wave (LDW) model to facilitate 
representing such variations.  Wave model parameters may be input from
the NAMELIST input file.  optionally, a sequence of wave model parameters
may be read in from a "WaveFile".

The WaveFile gives time (seconds from start time noted above), mean value,
and amplitudes and phases of wave-1 (period = 360 degrees longitude),
wave-2 (period = 180 degrees longitude) and wave-3 (period = 120 degrees
longitude) longitude-dependent wave components.  With the following
notation for amplitudes and phases:

  W0    = Diurnal mean value of longitude-dependent wave (LDW)

  d0    = Julian day for primary peak(s) of LDW traveling component

  W1    = Amplitude of the wave-1 LDW component

  Wphi1 = Phase (degrees) of the wave-1 LDW component

  phi1dot = rate of movement of wave-1 LDW peak

  W2    = Amplitude of the wave-2 LDW component

  Wphi2 = Phase (degrees) of the wave-2 LDW component

  phi2dot = rate of movement of wave-2 LDW peak

  W3    = Amplitude of the wave-3 LDW component

  Wphi3 = Phase (degrees) of the wave-3 LDW component

  phi3dot = rate of movement of wave-3 LDW peak

  d     = Julian day at which LDW is evaluated


Longitude-dependent wave multiplier values are computed in Mars-GRAM 2010
from the relation

    Wave = W0 + W1*Cos((pi/180)*(Lon - Wphi1 - phi1dot*(d - d0)))
              + W2*Cos(2*(pi/180)*(Lon - Wphi2 - phi2dot*(d - d0)))
              + W3*Cos(3*(pi/180)*(Lon - Wphi3 - phi3dot*(d - d0)))

where Lon is longitude (East or West, as controlled by input parameter
LonEW).  Note that phases Wphi2 and Wphi3 are defined differently
in Mars-GRAM 2010 than they were in Mars-GRAM 2001 (with wave-2 and
wave-3 phases in Mars-GRAM 2010 being, respectively, 1/2 and 1/3 times
their Mars-GRAM 2001 values).  This is the phase definition that was used
in Mars-GRAM 2000, and lets the traveling phase Wphix + phixdot*(d - d0) be
the same as longitude of the moving peak of wave-x component.

Wave multiplier scale parameter Wscale, from the NAMELIST input file, 
determines variation of the wave multiplier for altitudes z below 100 km, 
namely

    Wave(z) = 1 + [ Wave(100) - 1 ] Exp[(z - 100)/Wscale]

See further discussion in section 2.3 of the Mars-GRAM 2001 report NASA/TM-
2001-210961.  Default value for Wscale is 20 km.

