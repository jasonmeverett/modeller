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



    /**
     * @brief decoration generator for Ex2.
     * 
     */
    class Ex2Drawer : public SimTK::DecorationGenerator
    {
    public:
        Ex2Drawer(MobilizedBody m1, MobilizedBody m2, Force::MobilityLinearSpring s1, Force::MobilityLinearSpring s2) : m1(m1), m2(m2), s1(s1), s2(s2) {}

        void generateDecorations(const State& state, 
            Array_<DecorativeGeometry>& geometry) override 
        {
            GetWorld()->m_system.realize(state, Stage::Dynamics);

            // Force - manual
            double q1 = m1.getOneQ(state, 1);
            double dq1 = q1 - s1.getQZero(state);
            double force1 = -1.0 * s1.getStiffness(state) * dq1;
            double q2 = m2.getOneQ(state, 1);
            double dq2 = q2 - s2.getQZero(state);
            double force2 = -1.0 * s2.getStiffness(state) * dq2;

            // Force - pull out (NOT WORKING)
            // double force1 = GetWorld()->m_system.getMobilityForces(state, Stage::Dynamics)[s1.getForceIndex()];
            // double force2 = GetWorld()->m_system.getMobilityForces(state, Stage::Dynamics)[s2.getForceIndex()];
            

            // Set lines
            DecorativeLine line1, line2;
            line1.setPoint2(m1.getBodyOriginLocation(state));
            line2.setTransform(m1.getBodyTransform(state));
            line2.setPoint2(m2.findBodyOriginLocationInAnotherBody(state, m1));
            line1.setLineThickness(3.0);
            line2.setLineThickness(3.0);
            line1.setColor( force1 > 0.0 ? Vec3(fabs(force1)/30.0,0,0) : Vec3(0,fabs(force1)/30.0,fabs(force1)/30.0) );
            line2.setColor( force2 > 0.0 ? Vec3(fabs(force2)/30.0,0,0) : Vec3(0,fabs(force2)/30.0,fabs(force2)/30.0) );
            geometry.push_back(line1);
            geometry.push_back(line2);

            // Set texts
            DecorativeText text_title, text_sp1, text_sp2;
            text_title.setText("Sim Time: " + std::to_string(state.getTime()) + " sec");
            text_title.setIsScreenText(true);
            text_title.setColor(Vec3(0,0,1));
            text_sp1.setText(std::to_string(force1) + " N");
            text_sp1.setTransform(0.5*(m1.getBodyOriginLocation(state)));
            text_sp1.setScale(0.1);
            text_sp2.setText(std::to_string(force2) + " N");
            text_sp2.setTransform(0.5*(m1.getBodyOriginLocation(state) + m2.getBodyOriginLocation(state)));
            text_sp2.setScale(0.1);
            geometry.push_back( text_title );
            geometry.push_back( text_sp1 );
            geometry.push_back( text_sp2 );
        }

    protected:
        MobilizedBody m1;
        MobilizedBody m2;
        Force::MobilityLinearSpring s1;
        Force::MobilityLinearSpring s2;

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
    bool useViz             = py::cast<bool>(cfg["use_viz"]);
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
    p1body.addDecoration(Transform(), DecorativeSphere(0.1));
    Body::Rigid p2body(MassProperties(p2.endpoint_mass, Vec3(0), UnitInertia(1)));
    p2body.addDecoration(Transform(), DecorativeSphere(0.1));

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
    Force::MobilityLinearSpring spring1(
        GetWorld()->m_forces,
        pendulum1,
        1,
        50.0,
        1.0
    );
    Force::MobilityLinearSpring spring2(
        GetWorld()->m_forces,
        pendulum2,
        1,
        50.0,
        1.0
    );

    // Add some damping to the joints.
    Force::MobilityLinearDamper damper1( GetWorld()->m_forces, pendulum1, 0, cd1 );
    Force::MobilityLinearDamper damper2( GetWorld()->m_forces, pendulum2, 0, cd2 );

    // add damping to the springs themselves.
    Force::MobilityLinearDamper damper3( GetWorld()->m_forces, pendulum1, 1, 1.0);
    Force::MobilityLinearDamper damper4( GetWorld()->m_forces, pendulum2, 1, 1.0);

    // Set up visualization.
    if(useViz)
    {
        double update_rate = py::cast<double>(cfg["viz_update_rate"]);
        GetWorld()->m_system.setUseUniformBackground(true);
        Visualizer viz(GetWorld()->m_system);
        viz.setMode(Visualizer::Mode::PassThrough);
        GetWorld()->m_system.addEventReporter(new Visualizer::Reporter(viz, update_rate));
        viz.addDecorationGenerator(new Ex2Drawer(pendulum1, pendulum2, spring1, spring2));
        //viz.setBackgroundColor(Vec3(0));
    }
 
    // Create Data Set and create the logger class for that dataset.
    DataSet * ds = new DataSet(ds_name);
    sim.getDataBase()->addDataSet(ds);

    // Add the reporter that will modify the database with information.
    GetWorld()->m_system.addEventReporter(new Ex2::Ex2DataLogger(data_rate, ds, pendulum1, pendulum2));

    // Initialize the system and state.
    State state = GetWorld()->m_system.realizeTopology();
    pendulum1.setOneQ(state, 1, 1.0);
    pendulum2.setOneQ(state, 1, 1.0);
    pendulum1.setOneU(state, 0, p1.inital_angular_rate);
    pendulum2.setOneU(state, 0, p2.inital_angular_rate);
    pendulum1.setOneU(state, 1, 5.0);
    

    // Simulate for 20 seconds.
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(integ_acc);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(state);
    ts.stepTo(simTime);

    return sim;
}