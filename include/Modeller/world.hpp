#include "Simbody.h"

#include <Modeller/output.hpp>

namespace Modeller
{
    namespace Core
    {
        using namespace SimTK;



        struct World {

            World();

            const MultibodySystem& getSystem() const {return m_system;}
            const State& getDefaultState()     const {return m_system.getDefaultState();}

            void constructSystem();
            MultibodySystem                 m_system;
            SimbodyMatterSubsystem          m_matter;
            GeneralForceSubsystem           m_forces;
            
            Modeller::Output::DataBin       db;
        };

    
        World * GetWorld(); 
    }
}

