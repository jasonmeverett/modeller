
#include <Modeller/SpiceInterface.hpp>

/**
 * @brief Prescribed position of the SpiceFrameMotion class. This class requires a ball 
 * joint and therefore the nq will be 4 here for a quaternion. TODO: Add some checks
 * here that "nq" equals the expected result of 4.
 * 
 * @param s 
 * @param nq 
 * @param q 
 */
void Modeller::Spice::SpiceFrameMotionImpl::calcPrescribedPosition(const State &s, int nq, Real *q) const 
{
    SpiceDouble rotmat[3][3];
    SpiceDouble et = this->SE->getET0() + s.getTime();
    pxform_c(this->baseFrame.c_str(), this->targetFrame.c_str(), et, rotmat);
    m2q_c(rotmat, q);

    return;
}

/**
 * @brief The derivative of the function above. TODO: There's some stuff in here (skew matrix, quat
 * multiplication, etc.) that should eventually be pulled out.
 * 
 * @param s 
 * @param nq 
 * @param qdot 
 */
void Modeller::Spice::SpiceFrameMotionImpl::calcPrescribedPositionDot(const State &s, int nq, Real *qdot) const 
{

    /* Configure some starting SPICE variables. */
    SpiceDouble av[3];                  // Placeholder for current Angular Velocity
    SpiceDouble quat[4];                // Placeholder for Current Quaternion
    SpiceDouble rotmat[3][3];           // Placehodler for current Rotation Matrix
    SpiceDouble xfmat[6][6];            // Placeholder for current State transformation matrix including velocity xform
    SpiceDouble et = this->SE->getET0() + s.getTime();  // Current ET
    
    /** The following three lines do the following:
     * 1) Calculates the transformation matrix between the base and target frames,
     * 2) Converts that transformation matrix into a rotation matrix (DCM) and angular velocity, and
     * 3) Converts the rotation matrix into a quaternion.
     */
    sxform_c(this->baseFrame.c_str(), this->targetFrame.c_str(), et, xfmat);
    xf2rav_c(xfmat, rotmat, av);
    m2q_c(rotmat, quat);

    /**
     * @brief The below code computes Qdot = 1/2 * Omega(omeg) * q.
     * 
     * TODO: It would be great to include some conversions for this.
     * 
     */
    SpiceDouble avs[4][4] = {
        {0,     -av[0],     -av[1],     -av[2]},
        {av[0],     0,      av[2],     -av[1]},
        {av[1],  -av[2],    0,          av[0]},
        {av[2],  av[1],     -av[0],         0}
    };

    qdot[0] = 0.5*(avs[0][0]*quat[0] + avs[0][1]*quat[1] + avs[0][2]*quat[3] + avs[0][3]*quat[3]);
    qdot[1] = 0.5*(avs[1][0]*quat[0] + avs[1][1]*quat[1] + avs[1][2]*quat[3] + avs[1][3]*quat[3]);
    qdot[2] = 0.5*(avs[2][0]*quat[0] + avs[2][1]*quat[1] + avs[2][2]*quat[3] + avs[2][3]*quat[3]);
    qdot[3] = 0.5*(avs[3][0]*quat[0] + avs[3][1]*quat[1] + avs[3][2]*quat[3] + avs[3][3]*quat[3]);

    return;

}


/**
 * @brief Gonna assume this is just the time-scale of how slow the quaternions are for this use case,
 * but we seem to be okay with leaving this blank. Should look into analysis of quaternion drift
 * on planetary scales and see if approximations can be used for computational efficiency?
 * 
 * @param s 
 * @param nq 
 * @param qdotdot 
 */
void Modeller::Spice::SpiceFrameMotionImpl::calcPrescribedPositionDotDot(const State &s, int nq, Real *qdotdot) const  { }