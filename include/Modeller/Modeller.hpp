


// Pull in all Modeller includes.
#include <Modeller/examples.hpp>
#include <Modeller/consts.hpp>
#include <Modeller/world.hpp>
#include <Modeller/output.hpp>
#include <Modeller/forces.hpp>
#include <Modeller/simulation.hpp>

using namespace Modeller::Constants;
using namespace Modeller::Output;
using namespace Modeller::Core;
using namespace Modeller::Forces;

// Simbody.
#include "Simbody.h"
using namespace SimTK;


// Pybind.
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/stl_bind.h>
#include <pybind11/functional.h>
namespace py = pybind11;
