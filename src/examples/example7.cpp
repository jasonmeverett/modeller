//     ------------------------------------------------------------------------------------
//     
//             ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗     ███████╗██████╗ 
//             ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║     ██╔════╝██╔══██╗
//             ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║     █████╗  ██████╔╝
//             ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║     ██╔══╝  ██╔══██╗
//             ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████╗███████╗██║  ██║
//             ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝
//     
//     ------------------------------------------------------------------------------------

// Include Modeller.
#include <Modeller/Modeller.hpp>

// Helper macros for readability
#define PCD(x) py::cast<double>(x)
#define PCB(x) py::cast<bool>(x)

// Global namespace for Example1 code.
namespace Ex7
{
    /**
     * @brief Custom data logger for the example 1 problem.
     * 
     * This class allows the user to set a mobilized body to monitor.
     * 
     */
    class Ex7Logger : public PeriodicEventReporter 
    {
    public:

        Ex7Logger(Real interval, DataSet* ds) : PeriodicEventReporter(interval), ds(ds) { }

        void handleEvent(const State& s) const override {
            std::cout << s.getTime() << "\n";
        }

    protected:

        DataSet* ds;

    };   
}



/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
Modeller::Core::Simulation Modeller::Examples::Run_Ex7(py::dict cfg)
{
    using namespace Ex7;

    /* Configure the SPICE engine. TODO: eventually add in a 
     * stronger programmatic pairing between World and SpiceEngine.
     * Spice should 100% not be a requirement for simulation, and 
     * both should be able to switch back and forth simultaneously.
     */
    SpiceEngine * SE = new SpiceEngine();
    SE->loadKernelFile("/modeller/data/SPICE/furnsh_ex7.txt");
    SE->setGroundFrame("J2000");
    SE->setGroundBody("EARTH");
    SE->setStateZeroEpoch("2021 Jan 21 21:21:21.000 UTC");

    std::vector<std::string> framesToSimulate = { "IAU_EARTH","IAU_MARS","IAU_MOON" };
    std::vector<MobilizedBody::Ball> mobods;

    for(std::string frame : framesToSimulate) {
        SpiceFrameMotion(
            MobilizedBody::Ball(
                GetWorld()->m_matter.Ground(),
                Body::Massless()
            ),
            SE, "J2000", frame
        );
    }

    if(PCB(cfg["use_viz"])) {
        GetWorld()->m_system.setUseUniformBackground(true);
        Visualizer viz(GetWorld()->m_system);
        GetWorld()->m_system.addEventReporter(
            new Visualizer::Reporter( viz,  1000 ) );
        viz.setBackgroundColor(Vec3(0));
        viz.setSystemUpDirection(ZAxis);
    }

    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);
    DataSet ds("output");
    sim.getDataBase()->addDataSet(&ds);
    GetWorld()->m_system.addEventReporter(new Ex7Logger(1000, &ds));

    State s = GetWorld()->m_system.realizeTopology();

    RungeKutta3Integrator integ(GetWorld()->m_system);
    integ.setAccuracy(1e-6);
    integ.setMaximumStepSize(1.0);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(s);
    ts.stepTo(5.0 * spd_c());

    return sim;
}