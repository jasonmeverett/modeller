/* ------------------------------------------------------------------------------------
 
         ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗     ███████╗██████╗ 
         ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║     ██╔════╝██╔══██╗
         ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║     █████╗  ██████╔╝
         ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║     ██╔══╝  ██╔══██╗
         ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████╗███████╗██║  ██║
         ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝

    This example expands on the previous example but the idea is to start constructing
    up a more functional based library for thrusters, gimbals, tanks, variable-mass
    simulation setups, etc. In this example, a full 6DOF vehicle is implemented with both
    a liquid oxygen and liquid hydrogen tank. There are no slosh effects in the vehicle 
    although that is definitely one of the next things to implement (before flex and aero).
    There also needs to be an option to variably change the vehicle tank's moment of 
    inertia as a function of mass - perhaps building up interpolation logic here would
    be helpful. Ideally it would be great to use ASSET for this, but there'd have to be
    a smart enough way to alleviate overhead if the functions are defined on the Python 
    side.

    Also in this example the vehicle components will be fully defined on the Python side.
    The whole point of this simulation database is to allow for models with C++ efficiency
    to be exposed and constructed on the Python side, so you need to start building up
    that capability. Objects that are constructed on the Python side should ideally be
    constructed as pointers so we don't have unnecessary memory overhead with copying 
    memory. Need to look into Pybind11 documentation to see if items by default are copied
    by a pointer or not.

    There exists a world (again, not tied to the name!). The world is the baseline that
    contains everything (it contains frames, it contains time, it contains mass and matter,
    it contains forces, etc.). The world has one definitive baseline frame defined that
    represents the true "inertial" frame. If operating with the SPICE engine, then custom
    motions can be applied to new additional frames by defining 1) what SPICE frame represents
    the world frame, and 2) the relationship between the world frame and the new frame
    that you would like to define. So for the example of a lunar case, the world inertial
    frame could potentially be a lunar-centered J2000 frame (EME2000). Then, a new frame
    can be defined with a custom motion attached to it that represents the Lunar-fixed 
    principal axes frame: by passing the frame into to the SPICE engine, the SPICE engine
    can return the corresponding mobod with proper degrees of freedom that represents 
    that frame you just made (well, return a pointer to it, and then maybe have SPICE maintain
    a mapping of all the corresponding frames?).


*/