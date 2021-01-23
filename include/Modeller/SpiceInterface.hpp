#pragma once

#include "Simbody.h"

#include <string>

#include "SpiceUsr.h"

namespace Modeller
{
    namespace Spice
    {
        using namespace SimTK;

        /**
         * @brief The SPICE engine. Lives (as for now, for lack
         * of thinking about this more currently) as an equal 
         * to the world. I think it would be cool to eventually
         * have multiple different SPICE engines communicating with
         * one another but again, that's future work.
         * 
         */
        class SpiceEngine {
        public:
            SpiceEngine();

            /**
             * @brief Load in a specific "furnsh" kernel file. 
             * 
             * @param kernelLoadFile 
             */
            void loadKernelFile(std::string kernelLoadFile);

            /**
             * @brief Set the SPICE frame ID that corresponds
             * to the ground frame of the matter subsystem.
             * 
             * @param groundFrame 
             */
            void setGroundFrame(std::string groundFrame);

            /**
             * @brief Set the planetary body that represents
             * the ground matter subsystem. Not sure if this
             * should be required for SPICE engine use. TODO:
             * think this one out.
             * 
             * @param groundBody 
             */
            void setGroundBody(std::string groundBody);
            
            /**
             * @brief Set the SPICE Epoch that corresponds
             * to the "zero" point of a State acting within
             * subsystems. This version uses SPICE's versatile
             * "str2et" function and is pretty lenient.
             * 
             * @param etstr 
             */
            void setStateZeroEpoch(std::string etstr);

            /**
             * @brief Get the ET0 value of the simulation.
             * 
             * @return double 
             */
            double getET0(){return this->ET0;}

        protected:
            std::string m_kernelLoadFile;
            std::string groundFrame;
            std::string groundBody;
            bool kernelFileLoaded;
            bool groundFrameSet;
            bool groundBodySet;
            bool zeroEpochSet;
            double ET0;
            

        };

        /**
         * @brief Implementation of the SpiceFrameMotion class. Users shouldn't
         * particularly call this unless you know what you're doing.
         * 
         */
        class SpiceFrameMotionImpl : public Motion::Custom::Implementation
        {
        public: 
            SpiceFrameMotionImpl(
                SpiceEngine * SE,
                std::string baseFrame,
                std::string targetFrame
            ) : SE(SE), baseFrame(baseFrame), targetFrame(targetFrame) {}

            Motion::Level getLevel(const State&) const override {
                return Motion::Level::Position;
            }

            void calcPrescribedPosition(const State &s, int nq, Real *q) const override;
            void calcPrescribedPositionDot(const State &s, int nq, Real *qdot) const override;
            void calcPrescribedPositionDotDot(const State &s, int nq, Real *qdotdot) const override;

        protected:
            SpiceEngine * SE;
            std::string baseFrame;
            std::string targetFrame;

        };


        /**
         * @brief SpiceFrameMotion class. Users should call this.
         * 
         */
        class SpiceFrameMotion : public Motion::Custom
        {
        public:
            SpiceFrameMotion(
                MobilizedBody mobod,
                SpiceEngine * SE,
                std::string baseFrame,
                std::string targetFrame
            ) : Motion::Custom(mobod, new SpiceFrameMotionImpl(
                SE,
                baseFrame,
                targetFrame
            )) {}
        };

        
    }
}