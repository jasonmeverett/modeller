#include <Modeller/consts.hpp>
#include <Modeller/output.hpp>


#include "simbody/Simbody.h"


namespace Modeller
{
    namespace Core
    {
        using namespace SimTK;

        class Simulation
        {
        public:
            Simulation() : db(new Modeller::Output::DataBase()) {}
            Simulation(Modeller::Output::DataBase * db) : db(db) {}

            void Initialize();
            void Run();

            /**
             * @brief Get current simulation time
             * 
             * @return double 
             */
            double getTime(){ return this->time; }


        protected:
            double time;
            bool complete;

            Integrator * integrator;
            Modeller::Output::DataBase * db;

        };
    }
}