#pragma once

#include <Modeller/consts.hpp>
#include <Modeller/output.hpp>


#include "Simbody.h"

#include <pybind11/pybind11.h>
namespace py = pybind11;


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

            Modeller::Output::DataBase * getDataBase(){ return this->db; }


            static void Build(py::module & m)
            {
                py::class_<Simulation>(m, "Simulation")
                    .def(py::init<>())
                    .def("getDataBase", &Simulation::getDataBase)
                    .def_readonly("complete", &Simulation::complete);
            }

            /// TODO: Implement
            void Initialize();
            void Run();
            double getTime(){ return this->time; }

        protected:
            double time;
            bool complete;

            Integrator * integrator;
            Modeller::Output::DataBase * db;

        };
    }
}