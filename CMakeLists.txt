# ------------------------------------------------------------------------------------
#
#
#         ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗     ███████╗██████╗ 
#         ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║     ██╔════╝██╔══██╗
#         ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║     █████╗  ██████╔╝
#         ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║     ██╔══╝  ██╔══██╗
#         ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████╗███████╗██║  ██║
#         ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝
#
# 
#                                        v0.1
#
# ------------------------------------------------------------------------------------


cmake_minimum_required(VERSION 3.10) 
project(Modeller VERSION 0.1 LANGUAGES C CXX Fortran)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_BUILD_TYPE Debug)
 
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
set(PYTHON_EXECUTABLE "/miniconda3/envs/modeller/bin/python")

find_package(Simbody REQUIRED)
add_subdirectory(extern/pybind11)
add_subdirectory(extern/googletest)
add_subdirectory(extern/marsgram2010)

pybind11_add_module(Modeller
    src/Modeller/Modeller.cpp
    src/Modeller/world.cpp 
    src/Modeller/forces.cpp
    src/Modeller/output.cpp
    src/Modeller/rotations.cpp
    src/Modeller/SpiceEngine.cpp
    src/Modeller/SpiceFrameMotion.cpp

    src/examples/example1.cpp 
    src/examples/example2.cpp
    src/examples/example3.cpp
    src/examples/example4.cpp
    src/examples/example5.cpp
    src/examples/example6.cpp
    src/examples/example7.cpp
    src/examples/example8.cpp
)

target_include_directories(Modeller PUBLIC 
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/extern/pybind11/include
    ${CMAKE_SOURCE_DIR}/extern/marsgram2010/Code
    ${Simbody_INCLUDE_DIR}
    $ENV{SPICE_INCDIR}
) 

target_link_libraries(Modeller PUBLIC 
    SimTKcommon 
    SimTKmath 
    SimTKsimbody
    $ENV{SPICE_LIBDIR}/cspice.a
    $ENV{SPICE_LIBDIR}/csupport.a
    MarsGRAM2010
)