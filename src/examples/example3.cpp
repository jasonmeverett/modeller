//     ------------------------------------------------------------------------------------
//     
//     
//             ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗     ███████╗██████╗ 
//             ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║     ██╔════╝██╔══██╗
//             ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║     █████╗  ██████╔╝
//             ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║     ██╔══╝  ██╔══██╗
//             ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████╗███████╗██║  ██║
//             ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝
//
//                                          Ex2
//                                          ---
//                        Double pendulum w/ springs, dampers, etc.
//     
//     ------------------------------------------------------------------------------------



// Include Modeller.
#include <Modeller/Modeller.hpp>
#include <string>


namespace Ex3
{
    class Ex3Drawer : public SimTK::DecorationGenerator {
    public:
        Ex3Drawer() {}

        void generateDecorations(const State& state, Array_<DecorativeGeometry>& geometry) override  {
            DecorativeArrow a1;
            a1.setStartPoint(Vec3(0));
            a1.setEndPoint(Vec3(1));
            geometry.push_back(a1);
        }
    };
}


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
void Modeller::Examples::Run_Ex3(py::dict cfg)
{
    using namespace Ex3;

    GetWorld()->m_system.setUseUniformBackground(true);
    Visualizer viz(GetWorld()->m_system);
    viz.setMode(Visualizer::Mode::PassThrough);
    GetWorld()->m_system.addEventReporter(new Visualizer::Reporter(viz, 0.01));
    viz.addDecorationGenerator(new Ex3Drawer());

    // Initialize the system and state.
    State state = GetWorld()->m_system.realizeTopology();

    // Simulate for 20 seconds.
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(1e-8);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(state);
    ts.stepTo(100);

    return;
}