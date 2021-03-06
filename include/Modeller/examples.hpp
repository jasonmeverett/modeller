#pragma once

// Modeller includes
#include <Modeller/output.hpp>
#include <Modeller/simulation.hpp>

// Pybind.
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/stl_bind.h>
#include <pybind11/functional.h>
namespace py = pybind11;

namespace Modeller
{
    namespace Examples
    {
        std::vector<std::vector<double>> Run_Ex1(py::dict cfg);
        
        Modeller::Core::Simulation Run_Ex2(py::dict cfg);

        Modeller::Core::Simulation Run_Ex3(py::dict cfg);

        Modeller::Core::Simulation Run_Ex4(py::dict cfg);

        Modeller::Core::Simulation Run_Ex5(py::dict cfg);

        Modeller::Core::Simulation Run_Ex6(py::dict cfg);

        Modeller::Core::Simulation Run_Ex7(py::dict cfg);

        Modeller::Core::Simulation Run_Ex8(py::dict cfg);

        Modeller::Core::Simulation Run_Ex9(py::dict cfg);
    }
}