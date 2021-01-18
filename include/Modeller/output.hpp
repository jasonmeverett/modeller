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
            DataSet(std::string name) : name(name) {}

            std::string getName() { return this->name; }
            std::vector<std::vector<double>> getData() { return this->data; }
            void addDataEntry(std::vector<double> dataEntry)
            { 
                this->data.push_back(dataEntry); 
            }

            static void Build(py::module& m)
            {
                py::class_<DataSet>(m, "DataSet")
                    .def(py::init<std::string>())
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
            void addDataSet(DataSet& ds) { this->datasets.push_back(&ds); }
            void addDataSet(DataSet * ds){ this->datasets.push_back(ds); }
            DataSet* getDataSetByName(std::string name);

            static void Build(py::module& m)
            {
                py::class_<DataBase>(m, "DataBase")
                    .def(py::init<>())
                    .def("getDataSetByName", &DataBase::getDataSetByName);
            }

        protected:
            std::vector<DataSet*> datasets;
        };
    }
}
