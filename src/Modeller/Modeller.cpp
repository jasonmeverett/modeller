#include <Modeller/examples.hpp>

// Pybind.
#include <pybind11/pybind11.h>
namespace py = pybind11;


PYBIND11_MODULE(Modeller, m)
{   
    // Examples list
    auto sm_examples = m.def_submodule("examples", "Modeller examples");
    sm_examples.def("Run_Ex1", &Modeller::Examples::Run_Ex1);

    
}
