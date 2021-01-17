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
//                                A force-based oscillator.
//     
//     ------------------------------------------------------------------------------------



// Include Modeller.
#include <Modeller/Modeller.hpp>


namespace Ex2
{
    /**
     * @brief Generic struct to hold pendulum data.
     * 
     */
    struct Pendulum
    {
        double endpoint_mass; 
        double length;
        double inital_angular_rate;
    };


    /**
     * @brief Data logger class.
     * 
     */
    class Ex2DataLogger : public PeriodicEventReporter 
    {
    public:

        // Upon construction, clear the output.
        Ex2DataLogger(Real interval, DataSet* ds, MobilizedBody &p1, MobilizedBody &p2) : 
            PeriodicEventReporter(interval), 
            ds(ds),
            m_p1(p1), 
            m_p2(p2) { }

        void handleEvent(const State& s) const override {
            GetWorld()->m_system.realize(s, Stage::Position);

            std::vector<double> out_row;
            out_row.push_back(s.getTime());
            out_row.push_back(m_p1.getBodyOriginLocation(s)[0]);
            out_row.push_back(m_p1.getBodyOriginLocation(s)[1]);
            out_row.push_back(m_p2.getBodyOriginLocation(s)[0]);
            out_row.push_back(m_p2.getBodyOriginLocation(s)[1]);

            this->ds->addDataEntry(out_row);
        }


    protected:

        // Local reference to the mobilized body.
        const SimTK::MobilizedBody &m_p1;
        const SimTK::MobilizedBody &m_p2;
        DataSet* ds;

    };   
}


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
DataSet Modeller::Examples::Run_Ex2(py::dict cfg)
{
    using namespace Ex2;

    // Initialize pendulums.
    Pendulum p1;
    Pendulum p2;

    auto cp1 = cfg["pendulum1"];
    auto cp2 = cfg["pendulum2"];
    p1.endpoint_mass        = py::cast<double>(cp1["endpoint_mass_kg"]);
    p1.length               = py::cast<double>(cp1["length_m"]);
    p1.inital_angular_rate  = py::cast<double>(cp1["initial_angular_rate_deg_s"]) * PI / 180.0;
    p2.endpoint_mass        = py::cast<double>(cp2["endpoint_mass_kg"]);
    p2.length               = py::cast<double>(cp2["length_m"]);
    p2.inital_angular_rate  = py::cast<double>(cp2["initial_angular_rate_deg_s"]) * PI / 180.0;
    bool useViz             = py::cast<bool>(cfg["use_viz"]);
    double simTime          = py::cast<double>(cfg["sim_time"]);
    double data_rate        = py::cast<double>(cfg["output_data_rate"]);

    // Define the system.
    MultibodySystem system;
    SimbodyMatterSubsystem matter(system);
    GeneralForceSubsystem forces(system);
    Force::Gravity gravity(forces, matter, -YAxis, 9.8);

    // Describe mass and visualization properties for a generic body.
    Body::Rigid p1body(MassProperties(p1.endpoint_mass, Vec3(0), UnitInertia(1)));
    p1body.addDecoration(Transform(), DecorativeSphere(0.1));
    Body::Rigid p2body(MassProperties(p2.endpoint_mass, Vec3(0), UnitInertia(1)));
    p2body.addDecoration(Transform(), DecorativeSphere(0.1));

    // Create the moving (mobilized) bodies of the pendulum.
    MobilizedBody::Pin pendulum1(matter.Ground(), Transform(Vec3(0)), p1body, Transform(Vec3(0, p1.length, 0)));
    MobilizedBody::Pin pendulum2(pendulum1, Transform(Vec3(0)), p2body, Transform(Vec3(0, p2.length, 0)));

    // Set up visualization.
    if(useViz)
    {
        double update_rate = py::cast<double>(cfg["viz_update_rate"]);
        system.setUseUniformBackground(true);
        Visualizer viz(system);
        viz.setMode(Visualizer::Mode::PassThrough);
        system.addEventReporter(new Visualizer::Reporter(viz, update_rate));
    }

    // Create Data Set
    DataSet ds ("Ex2_output");
    system.addEventReporter(new Ex2::Ex2DataLogger(data_rate, &ds, pendulum1, pendulum2));

    // Initialize the system and state.
    State state = system.realizeTopology();
    pendulum1.setRate(state, p1.inital_angular_rate);
    pendulum2.setRate(state, p2.inital_angular_rate);

    // Simulate for 20 seconds.
    RungeKuttaMersonIntegrator integ(system);
    TimeStepper ts(system, integ);
    ts.initialize(state);
    ts.stepTo(simTime);

    return ds;
}