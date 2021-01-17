#include "Simbody.h"

#include <Modeller/output.hpp>

namespace Modeller
{
    namespace Core
    {
        using namespace SimTK;


        /**
         * @brief Generic world structure that represents a space within which simulations
         * can occur. Previous version of Modeller only allow for one concurrent simulation
         * and that is the methodology kept here as well. TODO: Is it worth it to construct
         * multiple simulations at once?
         * 
         * There exists a world.
         * Within the world, objects and systems exist.
         * Simulations can be ran within the world, using world objects.
         * Simulation time may be different from world time (did not incorporate in other versions of code)
         * 
         * In a single-simulation model:
         *      - Simulations can pause world time, stop world time (but I don't like that)
         *      - Simulation can't spawn world objects but can modify?
         */
        class World {
        public:

            World();

            const MultibodySystem& getSystem() const {return m_system;}
            const State& getDefaultState()     const {return m_system.getDefaultState();}

            void constructSystem();
            MultibodySystem                 m_system;
            SimbodyMatterSubsystem          m_matter;
            GeneralForceSubsystem           m_forces;
            

            
        };

    
        World * GetWorld(); 
    }
}

