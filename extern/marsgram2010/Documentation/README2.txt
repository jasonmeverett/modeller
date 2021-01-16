      DUMMY TRAJECTORY PROGRAMS (dumytraj_M10.f and multtraj_M10.f)

With earlier versions of Mars-GRAM a dummy trajectory program,
marstraj.f, was supplied.  Beginning with Mars-GRAM version 3.8, an
alternate version of a (double precision) dummy trajectory calculating
program (dumytraj.f) was included.  Although similar in general function
to the original marstraj.f code, some details of dumytraj.f are different:

(1)  In the original marstraj.f, interaction with Mars-GRAM was via calls
to three subroutines -

    Call Setup(...)
    Call Randinit(...)
    Call Datastep(...)

These three subroutines are part of the Mars-GRAM 2010 code and are
automatically available to be called whenever the Mars-GRAM 2010 code
(marssubs_M10.f90 and setup_M10.f90) is linked to the user's main trajectory
driver program. If you already have a trajectory program built like this,
with calls to Setup, Randinit, and Datastep it might be easily modified to
incorporate Mars-GRAM 2010 subroutines without using the approach taken
in dumytraj_M10.f90. Note, however, that the number of arguments in these
subroutines has changed, so appropriate modifications in your trajectory
programs must be made.

(2)  In dumytraj_M10.f90, interaction with Mars-GRAM 2010 is via three calls
to one "wrapper" subroutine (named Marstraj_M10 and provided as file 
wrapper_M10.f90), but with different values of three control parameters (isetup, 
jmonte, and istep) -

    Call Marstraj_M10(...)      with isetup = 1
    Call Marstraj_M10(...)      with isetup = 0, jmonte > 0, istep = 0
    Call Marstraj_M10(...)      with isetup = 0, jmonte = 0, istep > 0

where isetup = 1 triggers the call to the Setup subroutine, jmonte > 0
triggers the call to the reinitialization process (including the call to
the Randinit subroutine), and istep = 1 to MAXNUM is a counter for steps
along the trajectory (with a call to the Datastep subroutine at each step).
Marstraj_M10 is a subroutine in the dumytraj_M10.f90 code, and must be included
(along with the basic Mars-GRAM code setup_M10.f90 and marssubs_M10.f90) as a
subroutine in the user's calling trajectory program.

(3)  In the original marstraj.f dummy trajectory main code, transfer of 
double precision (trajectory) variables to and from single precision (Mars-
GRAM) variables was assumed to be done within the user's main trajectory 
code.  In the dumytraj_M10.f90 code this transfer is handled within the 
Marstraj_M10 subroutine (which must be included as a subroutine in the user's 
trajectory program).

(4)  In the original marstraj.f dummy trajectory main code, (single
precision) values of position increments (DELHGT, DELLAT, and DELLON) were
presumed to be calculated within the user's main trajectory program.  In
the dumytraj_M10.f90 code, input variables to the Marstraj_M10 subroutine are 
current and next (double precision) position values (height, latitude, and
longitude) and the position increments to be passed to the Datastep
subroutine (increments of height, latitude, and longitude) are computed
within the Marstraj_M10 subroutine.

Regardless of which dummy trajectory code you decide to use as your
starting model from which to build the interface to Mars-GRAM 2010 for your
own trajectory code, it is worthwhile to read the comments embedded in the
dumytraj_M10.f90 code.  These comments give more explicit descriptions of the
functions that are being performed.  They also provide better hints
about what to do if you are using predictor-corrector (or other) trajectory
approaches that require mid-point corrections along trajectory steps and/or
the use of density variations that occur within each trajectory step.

Another feature of dumytraj_M10.f90 is that it allows high precision
Mars ephemeris values for sun latitude, longitude, and Ls angle to be
passed from the trajectory program for use by Mars-GRAM 2010 subroutines.

The multiple perturbation version (multtraj_M10.f90) allows different
realizations of perturbation profiles to be generated on a single trajectory
run, for example to test sensitivity of a guidance algorithm to different
"gain" levels.

For technical, programmatic or policy questions, please contact

Hilary Justh
NASA MSFC EV44
Marshall Space Flight Center, AL 35812
phone: (256)-544-3694
fax:   (256)-544-3060
e-mail: Hilary.L.Justh@nasa.gov

