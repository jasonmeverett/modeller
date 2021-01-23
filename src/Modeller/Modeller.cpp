#include <Modeller/Modeller.hpp>


PYBIND11_MODULE(Modeller, m)
{   
    // Examples list
    auto sm_examples = m.def_submodule("examples", "Modeller examples");
    sm_examples.def("Run_Ex1", &Modeller::Examples::Run_Ex1);
    sm_examples.def("Run_Ex2", &Modeller::Examples::Run_Ex2);
    sm_examples.def("Run_Ex3", &Modeller::Examples::Run_Ex3);
    sm_examples.def("Run_Ex4", &Modeller::Examples::Run_Ex4);
    sm_examples.def("Run_Ex5", &Modeller::Examples::Run_Ex5);
    sm_examples.def("Run_Ex6", &Modeller::Examples::Run_Ex6);
    sm_examples.def("Run_Ex7", &Modeller::Examples::Run_Ex7);
    sm_examples.def("Run_Ex8", &Modeller::Examples::Run_Ex8);

    // Build extra modules
    Modeller::Output::DataSet::Build(m);
    Modeller::Output::DataBase::Build(m);
    Modeller::Core::Simulation::Build(m);
}
