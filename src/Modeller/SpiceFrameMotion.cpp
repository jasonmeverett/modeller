
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

    // /** Convert av. */
    // SpiceDouble avNew[3];
    // mxv_c(rotmat, av, avNew);
    // av[0] = avNew[0];
    // av[1] = avNew[1];
    // av[2] = avNew[2];

    Vec4 qdot_new = Rotation::convertAngVelToQuaternionDot(
        Vec4(quat[0], quat[1], quat[2], quat[3]), 
        Vec3(av[0], av[1], av[2])
    );
    qdot[0] = qdot_new[0];
    qdot[1] = qdot_new[1];
    qdot[2] = qdot_new[2];
    qdot[3] = qdot_new[3];

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