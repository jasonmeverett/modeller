/**
 * @file forces.hpp
 * @author your name (you@domain.com)
 * @brief 
 * @version 0.1
 * @date 2021-01-16
 * 
 * @copyright Copyright (c) 2021
 * 
 */

#pragma once

#include "Simbody.h"

#include <Modeller/consts.hpp>

namespace Modeller
{
    namespace Forces
    {
        using namespace SimTK;
        using Modeller::Constants::PI;

        class ConstantForceAlongXAxisImpl : public Force::Custom::Implementation
        {
        
        public:
            ConstantForceAlongXAxisImpl(MobilizedBody mobod, double magnitude)
                : m_mobod(mobod), magnitude(magnitude) {}

            virtual void calcForce( 
                const State&                state,
                Vector_<SpatialVec>&        bodyForcesInG,
                Vector_<Vec3>&              particleForcesInG,
                Vector&                     mobilityForces
            ) const override 
            {
                // Apply the force.
                m_mobod.applyForceToBodyPoint(
                    state,
                    Vec3(0),
                    m_mobod.getBodyRotation(state) * this->magnitude * Vec3(1,0,0),
                    bodyForcesInG
                );
            }

            virtual Real calcPotentialEnergy(const State& state) const override
            {
                return 0;
            }


        protected:
            MobilizedBody m_mobod;
            double magnitude;

        };


        class ConstantForceAlongXAxis : public Force::Custom 
        {
        public:
            ConstantForceAlongXAxis( GeneralForceSubsystem& forces, MobilizedBody mobod, Real magnitude ) 
            : Force::Custom( forces, new ConstantForceAlongXAxisImpl( mobod, magnitude ) ) {}
        };  

    }
}
