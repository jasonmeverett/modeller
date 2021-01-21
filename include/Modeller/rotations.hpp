#pragma once

#include "Simbody.h"

#include <Modeller/consts.hpp>

namespace Modeller
{
    namespace Utility
    {
        using namespace SimTK;

        /**
         * @brief Euler 321 to Quat pulled from Wiki.
         * https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
         * 
         * @param Eul 
         * @return Quaternion_<Real> 
         */
        Quaternion_<Real> Euler321ToQuaternion(Vec3 Eul);
    }
}