#include <Modeller/rotations.hpp>
using namespace Modeller::Utility;


/**
 * @brief Convert Euler values into a quaternion. Although the 
 * input to the function is in roll-pitch-yaw, the actual 
 * Euler sequence used is a yaw-pitch-roll.
 * 
 * @param Eul 
 * @return Quaternion_<Real> 
 */
Quaternion_<Real> Modeller::Utility::Euler321ToQuaternion(Vec3 Eul)
{
    double roll     = Eul[0];
    double pitch    = Eul[1];
    double yaw      = Eul[2];

    // Abbreviations for the various angular functions
    double cy = cos(yaw * 0.5);
    double sy = sin(yaw * 0.5);
    double cp = cos(pitch * 0.5);
    double sp = sin(pitch * 0.5);
    double cr = cos(roll * 0.5);
    double sr = sin(roll * 0.5);

    Quaternion_<Real> q;
    q[0] = cr * cp * cy + sr * sp * sy;
    q[1] = sr * cp * cy - cr * sp * sy;
    q[2] = cr * sp * cy + sr * cp * sy;
    q[3] = cr * cp * sy - sr * sp * cy;

    return q;
}