#pragma once

#include <pybind11/pybind11.h>
namespace py = pybind11;

#include "Simbody.h"
#include <vector>

namespace Modeller
{
    namespace Output
    {
        using namespace SimTK;

        class DataSet
        {
        public:
            DataSet() {}

            std::string getName() { return this->name; }
            std::vector<std::vector<double>> getData() { return this->data; }

            static void Build(py::module& m)
            {
                py::class_<DataSet>(m, "DataSet")
                    .def(py::init<>())
                    .def("getName", &DataSet::getName)
                    .def("getData", &DataSet::getData);
            }

        protected:
            std::string name;
            std::vector<std::vector<double>> data;

        };


        class DataBase
        {
        public:
            DataBase() {}
            


        protected:
            std::vector<DataSet*> datasets;
        };


        class DataLogger : public PeriodicEventReporter 
        {
        public:
            DataLogger(Real interval, DataSet * ds) : PeriodicEventReporter(interval), ds(ds) {}

            void handleEvent(const State& s) const override {
                
            }


        protected:
            DataSet* ds;

        };
    }
}
