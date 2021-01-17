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

#include "Simbody.h"

#include <Modeller/consts.hpp>

namespace Modeller
{
    namespace Forces
    {
        using namespace SimTK;
        using Modeller::Constants::PI;

        /**
         * @brief Basic sinusoidal force implementation
         * 
         */
        class SinusoidalForceImpl : public SimTK::Force::Custom::Implementation
        {
        public:
            SinusoidalForceImpl(MobilizedBody mobod, Real amp, Real freq_Hz, Real phase_rad, Vec3 axis) : 
                m_mobod(mobod), 
                amp(amp), 
                freq_Hz(freq_Hz), 
                phase_rad(phase_rad), 
                axis(axis.normalize()) {}

            /**
             * @brief Override of foce calculation. Provides a sinusoidal force to our mobod.
             * 
             * @param state 
             * @param bodyForcesInG 
             * @param particleForcesInG 
             * @param mobilityForces 
             */
            virtual void calcForce( 
                const State&                state,
                Vector_<SpatialVec>&        bodyForcesInG,
                Vector_<Vec3>&              particleForcesInG,
                Vector&                     mobilityForces
            ) const override 
            {
                // Where to apply the force in the body.
                Vec3 bodyPoint = {0,0,0};

                // Current force.
                Real t = state.getTime();
                Vec3 force = this->amp * sin(2*PI*t/this->freq_Hz + this->phase_rad) * this->axis;

                // Apply the force.
                m_mobod.applyForceToBodyPoint(
                    state,
                    bodyPoint,
                    force,
                    bodyForcesInG
                );
            }


            /**
             * @brief Override for calculating potential energy
             * 
             * @param state 
             * @return Real 
             */
            virtual Real calcPotentialEnergy(const State& state) const override
            {
                return 0;
            }


        protected:
            SimTK::MobilizedBody m_mobod;
            Real amp;
            Real freq_Hz;
            Real phase_rad;
            Vec3 axis;

        };

        class SinusoidalForce : public Force::Custom 
        {
        public:
            SinusoidalForce(
                GeneralForceSubsystem&  forces,
                MobilizedBody           mobod,
                Real                    amp,
                Real                    freq_Hz,
                Real                    phase_rad, 
                Vec3                    axis 
            ) : Force::Custom(
                forces,
                new SinusoidalForceImpl(
                    mobod,
                    amp,
                    freq_Hz,
                    phase_rad,
                    axis
                )
            ) {}
        };  

    }
}
