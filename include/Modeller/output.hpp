#pragma once

#include "Simbody.h"
#include <vector>

namespace Modeller
{
    namespace Output
    {
        using namespace SimTK;

        struct DataBin
        {
            std::vector<std::vector<std::vector<double>>> DataSets;
        };
    }
}
