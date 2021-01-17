#include <Modeller/Modeller.hpp>


PYBIND11_MODULE(Modeller, m)
{   
    // Examples list
    auto sm_examples = m.def_submodule("examples", "Modeller examples");
    sm_examples.def("Run_Ex1", &Modeller::Examples::Run_Ex1);
    sm_examples.def("Run_Ex2", &Modeller::Examples::Run_Ex2);

    // Build extra modules
    Modeller::Output::DataSet::Build(m);
    
}
