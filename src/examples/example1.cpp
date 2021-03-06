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
//                              A simple acceleration oscillator.
//     
//     ------------------------------------------------------------------------------------


// Include Modeller.
#include <Modeller/Modeller.hpp>


// Logging output.
std::vector<std::vector<double>> ex1_output;


// Global namespace for Example1 code.
namespace Ex1
{
    /* Simple acceleration oscillator. */
    class AccelOscillator : public Motion::Custom::Implementation
    {
    public:

        // Constructors
        AccelOscillator(double amp, double freq) : amp(amp), freq(freq), phase(0) {}
        AccelOscillator(double a, double f, double p) : amp(a), freq(f), phase(p) {}

        // Required override to let Simbody know what level we are prescribing.
        Motion::Level getLevel(const State&) const override {
            return Motion::Level::Acceleration;
        }

        // Prescribe the acceleration profile
        void calcPrescribedAcceleration(const State &s, int nu, Real *udot) const override  {
            Real tt = s.getTime();
            udot[0] = this->amp * cos(2*PI*tt/this->freq + this->phase);
            return;
        }

    protected:
        // Class members
        double amp;
        double freq;
        double phase;

    };

    /* Custom data logger for the example 1 problem. */
    class MyDataLogger : public PeriodicEventReporter 
    {
    public:

        // Upon construction, clear the output.
        MyDataLogger(Real interval) : PeriodicEventReporter(interval) {
            ex1_output.clear();
        }

        void handleEvent(const State& s) const override {
            GetWorld()->m_system.realize(s, Stage::Acceleration);

            std::vector<double> out_row;
            out_row.push_back(s.getTime());
            out_row.push_back(m_mobod.getBodyOriginLocation(s)[0]);
            out_row.push_back(m_mobod.getBodyOriginVelocity(s)[0]);
            out_row.push_back(m_mobod.getBodyOriginAcceleration(s)[0]);

            // Push the output row to the total ex1 output vector.
            ex1_output.push_back(out_row);
        }

        void set_body_to_monitor(MobilizedBody mb) { this->m_mobod = mb; }

    protected:

        // Local reference to the mobilized body.
        SimTK::MobilizedBody m_mobod;

    };   
}



/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
std::vector<std::vector<double>> Modeller::Examples::Run_Ex1(py::dict cfg)
{
    using namespace Ex1;

    // Pull in all dictionary parameters from python.
    double amp          = py::cast<double>(cfg["amplitude"]);
    double freq         = 1.0 / py::cast<double>(cfg["frequency_Hz"]);
    double output_freq  = py::cast<double>(cfg["data_output_frequency"]);
    double integ_acc    = py::cast<double>(cfg["integ_accuracy"]);
    double total_time   = py::cast<double>(cfg["total_sim_time"]);

    // Create a placeholder rigid-body
    Body::Rigid sphereBody (
        MassProperties(
            1.0,            // 1 kg
            Vec3(0.0),      // CoM wrt body = 0
            Inertia(1.0)    // Unit sphere inertia
        )
    );

    // Create mobod.
    MobilizedBody::Slider sphere(
        GetWorld()->m_matter.Ground(),
        Transform(),
        sphereBody,
        Transform()
    );

    // Create custom motion
    Motion::Custom myMotion(sphere, new AccelOscillator(amp, freq));

    // Create a new data logger
    Ex1::MyDataLogger logger(output_freq);
    logger.set_body_to_monitor(sphere);

    // Add the logger as an event reporter
    GetWorld()->m_system.addEventReporter(&logger);

    // Done with system changes. Realize topology.
    State s = GetWorld()->m_system.realizeTopology();

    // Configure the integrator.
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(integ_acc);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(s);

    // Run!
    ts.stepTo(total_time);

    return ex1_output;
}