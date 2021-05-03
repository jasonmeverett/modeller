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
            GetWorld()->m_system.realize(s, Stage::Dynamics);

            std::vector<double> out_row;
            out_row.push_back(s.getTime());
            out_row.push_back(m_p1.getBodyOriginLocation(s)[0]);
            out_row.push_back(m_p1.getBodyOriginLocation(s)[1]);
            out_row.push_back(m_p2.getBodyOriginLocation(s)[0]);
            out_row.push_back(m_p2.getBodyOriginLocation(s)[1]);
            out_row.push_back(GetWorld()->m_system.calcEnergy(s));
            out_row.push_back(GetWorld()->m_system.calcKineticEnergy(s));
            out_row.push_back(GetWorld()->m_system.calcPotentialEnergy(s));

            this->ds->addDataEntry(out_row);
        }


    protected:

        // Local reference to the mobilized body.
        const SimTK::MobilizedBody &m_p1;
        const SimTK::MobilizedBody &m_p2;
        DataSet* ds;

    };   


    class Ex2Printer : public PeriodicEventReporter 
    {
    public:
        Ex2Printer(Real interval) : PeriodicEventReporter(interval){ }
        void handleEvent(const State& s) const override {
            std::cout << "Time: " << s.getTime() << std::endl;
        }
    };   
}


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
Modeller::Core::Simulation Modeller::Examples::Run_Ex2(py::dict cfg)
{
    using namespace Ex2;

    // Input from Python
    Pendulum p1;
    Pendulum p2;
    auto cp1 = cfg["pendulum1"];
    auto cp2 = cfg["pendulum2"];
    p1.endpoint_mass        = py::cast<double>(cp1["endpoint_mass_kg"]);
    p1.length               = py::cast<double>(cp1["length_m"]);
    p1.inital_angular_rate  = py::cast<double>(cp1["initial_angular_rate_deg_s"]) * PI / 180.0;
    double cd1              = py::cast<double>(cp1["damping_coeff"]);
    p2.endpoint_mass        = py::cast<double>(cp2["endpoint_mass_kg"]);
    p2.length               = py::cast<double>(cp2["length_m"]);
    p2.inital_angular_rate  = py::cast<double>(cp2["initial_angular_rate_deg_s"]) * PI / 180.0;
    double cd2              = py::cast<double>(cp2["damping_coeff"]);
    double simTime          = py::cast<double>(cfg["sim_time"]);
    double data_rate        = py::cast<double>(cfg["output_data_rate"]);
    std::string ds_name     = py::cast<std::string>(cfg["output_dataset_name"]);
    double integ_acc        = py::cast<double>(cfg["integ_accuracy"]);
    bool use_grav           = py::cast<bool>(cfg["gravity"]);

    // Initialize simulation
    SetWorld(new Modeller::Core::World());
    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);

    // Add gravity to the world (doesn't come w/ grav by default!)
    if(use_grav) {
        Force::Gravity gravity(GetWorld()->m_forces, GetWorld()->m_matter, -YAxis, 9.8);
    }


    // Describe mass and visualization properties for a generic body.
    Body::Rigid p1body(MassProperties(p1.endpoint_mass, Vec3(0), UnitInertia(1)));
    Body::Rigid p2body(MassProperties(p2.endpoint_mass, Vec3(0), UnitInertia(1)));

    // Create the moving (mobilized) bodies of the pendulum.
    MobilizedBody::BendStretch pendulum1(
        GetWorld()->m_matter.Ground(), 
        Transform(Rotation(-PI/2.0 , Vec3(0,0,1))), 
        p1body, 
        Transform(Rotation(-PI/2.0 , Vec3(0,0,1)))
    );

    MobilizedBody::BendStretch pendulum2(
        pendulum1,                     
        Transform(Rotation(-PI/2.0 , Vec3(0,0,1))), 
        p2body, 
        Transform(Rotation(-PI/2.0 , Vec3(0,0,1)))
    );

    // Add spring force.
    Force::MobilityLinearSpring spring1( GetWorld()->m_forces, pendulum1, 1, 50.0, 1.0 );
    Force::MobilityLinearSpring spring2( GetWorld()->m_forces, pendulum2, 1, 50.0, 1.0 );

    // Add some damping to the joints.
    Force::MobilityLinearDamper damper1( GetWorld()->m_forces, pendulum1, 0, cd1 );
    Force::MobilityLinearDamper damper2( GetWorld()->m_forces, pendulum2, 0, cd2 );

    // add damping to the springs themselves.
    Force::MobilityLinearDamper damper3( GetWorld()->m_forces, pendulum1, 1, 1.0);
    Force::MobilityLinearDamper damper4( GetWorld()->m_forces, pendulum2, 1, 1.0);

    // Create Data Set and create the logger class for that dataset.
    DataSet * ds = new DataSet(ds_name);
    sim.getDataBase()->addDataSet(ds);

    // Add the reporter that will modify the database with information.
    GetWorld()->m_system.addEventReporter(new Ex2::Ex2DataLogger(data_rate, ds, pendulum1, pendulum2));
    GetWorld()->m_system.addEventReporter(new Ex2Printer(1));

    // Initialize the system and state.
    State state = GetWorld()->m_system.realizeTopology();
    pendulum1.setOneQ(state, 1, 1.0);
    pendulum2.setOneQ(state, 1, 1.0);
    pendulum1.setOneU(state, 0, p1.inital_angular_rate);
    pendulum2.setOneU(state, 0, p2.inital_angular_rate);
    pendulum1.setOneU(state, 1, 5.0);

    // Simulate.
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(integ_acc);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(state);
    ts.stepTo(simTime);

    return sim;
}