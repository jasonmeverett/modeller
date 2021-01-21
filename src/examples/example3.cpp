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
//                                          Ex3
//                                          ---
//                 Double pendulum w/ springs, dampers, ... and a spring cutter!
//     
//     ------------------------------------------------------------------------------------


/** @file example3.cpp
 * 
 * @brief This example is the same as example 2 but includes the option to instantaneously
 * disconnect the forces at an arbitrary time.
 */ 


// Include Modeller.
#include <Modeller/Modeller.hpp>
#include <string>
#include <tuple>
#include <iomanip>

#define PCD(x) py::cast<double>(x)


namespace Ex3
{

    using SpringDamper = std::tuple<Force::MobilityLinearSpring, Force::MobilityLinearDamper>;

    
    /**
     * @brief Euler 321 to Quat pulled from Wiki.
     * https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
     * 
     * @param Eul 
     * @return Quaternion_<Real> 
     */
    Quaternion_<Real> Euler321ToQuaternion(Vec3 Eul) // yaw (Z), pitch (Y), roll (X)
    {
        double roll     = Eul[0];
        double pitch    = Eul[1];
        double yaw      = Eul[2];

        // Abbreviations for the various angular functions
        double cy = cos(yaw * 0.5);
        double sy = sin(yaw * 0.5);
        double cp = cos(pitch * 0.5);
        double sp = sin(pitch * 0.5);
        double cr = cos(roll * 0.5);
        double sr = sin(roll * 0.5);
    
        Quaternion_<Real> q;
        q[0] = cr * cp * cy + sr * sp * sy;
        q[1] = sr * cp * cy - cr * sp * sy;
        q[2] = cr * sp * cy + sr * cp * sy;
        q[3] = cr * cp * sy - sr * sp * cy;
    
        return q;
    }

    
    /**
     * @brief Sheduled event handler to cut springs.
     * 
     */
    class SpringCutter : public SimTK::ScheduledEventHandler
    {
    public:
        SpringCutter(double septime, SpringDamper sd1) 
            : septime(septime), sd1(sd1) {}

        Real getNextEventTime(const State& s, bool incCurrentTime) const override {
            return this->septime;
        }

        void handleEvent(State& s, Real accuracy, bool& shouldTerminate) const override {
            std::get<0>(sd1).disable(s);
            std::get<1>(sd1).disable(s);
            std::cout << "Spring cut at " << std::setprecision(10) << s.getTime() << "\n";
            return;
        }

    protected:
        double septime;
        SpringDamper sd1;

    };



    /**
     * @brief Data logger class.
     * 
     */
    class Ex3DataLogger : public PeriodicEventReporter 
    {
    public:

        // Upon construction, clear the output.
        Ex3DataLogger(Real interval, DataSet* ds, MobilizedBody &p1, MobilizedBody &p2) : 
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
            out_row.push_back(m_p1.getBodyOriginLocation(s)[2]);
            out_row.push_back(m_p2.getBodyOriginLocation(s)[0]);
            out_row.push_back(m_p2.getBodyOriginLocation(s)[1]);
            out_row.push_back(m_p2.getBodyOriginLocation(s)[2]);
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
     * @brief Decoration generator for the third example.
     * 
     * Populates the screen with slick visuals!
     * 
     */
    class Ex3Drawer : public SimTK::DecorationGenerator
    {
    public:
        Ex3Drawer( MobilizedBody m1, MobilizedBody m2, py::dict cfg) 
            :  m1(m1),  m2(m2), cfg(cfg) {}

        /**
         * @brief Required override for inheriting from a DecorationGenerator
         * 
         * @param state 
         * @param geometry 
         */
        void generateDecorations(const State& s, 
            Array_<DecorativeGeometry>& geometry) override 
        {
            GetWorld()->m_system.realize(s, Stage::Dynamics);

            double axis_len = 1.0;

            /* Build up some axes. */
            DecorativeLine xu, yu, zu;
            xu.setPoint2(axis_len * Vec3(1,0,0));
            yu.setPoint2(axis_len * Vec3(0,1,0));
            zu.setPoint2(axis_len * Vec3(0,0,1));
            xu.setColor(Vec3(1,0,0));
            yu.setColor(Vec3(0,1,0));
            zu.setColor(Vec3(0,0,1));            
            geometry.push_back( xu );
            geometry.push_back( yu );
            geometry.push_back( zu );
            

            double l1 = m1.getOneQ(s, 0);
            double l2 = m2.getOneQ(s, 0);
            double f1 = - PCD(cfg["spring1"]["k"]) * (l1 - PCD(cfg["spring1"]["x0"]));
            double f2 = - PCD(cfg["spring2"]["k"]) * (l2 - PCD(cfg["spring2"]["x0"]));

            // Set texts
            DecorativeText text_title("Sim Time: " + std::to_string(s.getTime()) + " sec");
            text_title.setIsScreenText(true);
            text_title.setColor(Vec3(0,0,0.9));
            geometry.push_back( text_title );

            DecorativeSphere sphere(0.05);
            sphere.setColor(Vec3(0));
            sphere.setTransform(Transform(GetWorld()->m_matter.calcSystemMassCenterLocationInGround(s)));
            geometry.push_back(sphere);

            DecorativeText text_sp1(std::to_string(f1) + " N");
            text_sp1.setTransform(0.5*(m1.getBodyOriginLocation(s)));
            text_sp1.setScale(0.1);
            geometry.push_back( text_sp1 );

            DecorativeText text_sp2(std::to_string(f2) + " N");
            text_sp2.setTransform(0.5*(m1.getBodyOriginLocation(s) + m2.getBodyOriginLocation(s)));
            text_sp2.setScale(0.1);
            geometry.push_back( text_sp2 );

            if(s.getTime() >= py::cast<double>(cfg["spring1_cut_time"])){
                DecorativeText text("Spring1 Cut: " + std::to_string(py::cast<double>(cfg["spring1_cut_time"])) + " sec");
                text.setIsScreenText(true);
                text.setColor(Vec3(0.9,0,0));
                geometry.push_back( text );
            }else
            {
                DecorativeLine line1(Vec3(0), m1.getBodyOriginLocation(s));
                line1.setLineThickness(PCD(cfg["spring1"]["x0"]) / l1);
                line1.setColor( f1 > 0.0 ? Vec3(fabs(f1)/30.0,0,0) : Vec3(0,fabs(f1)/30.0,fabs(f1)/30.0) );
                geometry.push_back(line1);
            }

            if(s.getTime() >= py::cast<double>(cfg["spring2_cut_time"]))
            {
                DecorativeText text("Spring2 Cut: " + std::to_string(py::cast<double>(cfg["spring2_cut_time"])) + " sec");
                text.setIsScreenText(true);
                text.setColor(Vec3(0,0.9,0));
                geometry.push_back( text );
            }else{
                DecorativeLine line2(m1.getBodyOriginLocation(s), m2.getBodyOriginLocation(s));
                line2.setLineThickness(PCD(cfg["spring2"]["x0"]) / l2);
                line2.setColor( f2 > 0.0 ? Vec3(fabs(f2)/30.0,0,0) : Vec3(0,fabs(f2)/30.0,fabs(f2)/30.0) );
                geometry.push_back(line2);
            }
        }

    protected:
        MobilizedBody m1;
        MobilizedBody m2;
        py::dict cfg;

    };

}


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
Modeller::Core::Simulation Modeller::Examples::Run_Ex3(py::dict cfg)
{
    using namespace Ex3;

    /* Define rigid body spheres. */
    Body::Rigid rb_ball1( MassProperties( PCD(cfg["ball1"]["mass"]), Vec3(0), UnitInertia(1) ) );
    Body::Rigid rb_ball2( MassProperties( PCD(cfg["ball2"]["mass"]), Vec3(0), UnitInertia(1) ) );
    rb_ball1.addDecoration( Transform(), DecorativeSphere( PCD(cfg["ball1"]["radius"] ) ) );
    rb_ball2.addDecoration( Transform(), DecorativeSphere( PCD(cfg["ball2"]["radius"] ) ) );

    /* Massless object for joints. */
    Body::Massless massless_obj;

    MobilizedBody::Ball joint_1(
        GetWorld()->m_matter.Ground(),
        Transform(),
        massless_obj,
        Transform()
    );

    MobilizedBody::Slider ball_1(
        joint_1,
        rb_ball1
    );

    MobilizedBody::Ball joint_2(
        ball_1,
        Transform(),
        massless_obj,
        Transform()
    );

    MobilizedBody::Slider ball_2(
        joint_2,
        rb_ball2
    );

    Force::MobilityLinearSpring spring_1(
        GetWorld()->m_forces,
        ball_1,
        0,
        PCD(cfg["spring1"]["k"]),
        PCD(cfg["spring1"]["x0"])
    );

    Force::MobilityLinearSpring spring_2(
        GetWorld()->m_forces,
        ball_2,
        0, 
        PCD(cfg["spring2"]["k"]),
        PCD(cfg["spring2"]["x0"])
    );

    Force::MobilityLinearDamper damper_1(
        GetWorld()->m_forces,
        ball_1,
        0,
        PCD(cfg["spring1"]["c"])
    );

    Force::MobilityLinearDamper damper_2(
        GetWorld()->m_forces,
        ball_2,
        0,
        PCD(cfg["spring2"]["c"])
    );



    for(int ii = 0; ii < 3; ii++)
    {
        Force::MobilityLinearDamper(
            GetWorld()->m_forces,
            joint_1,
            ii,
            PCD(cfg["joint1"]["rotational_damping"])
        );
        Force::MobilityLinearDamper(
            GetWorld()->m_forces,
            joint_2,
            ii,
            PCD(cfg["joint2"]["rotational_damping"])
        );
    }
    
    /* Apply gravity */
    if(py::cast<bool>(cfg["use_grav"])) {
        Force::Gravity gravity(GetWorld()->m_forces, GetWorld()->m_matter, -YAxis, 9.8);
    }

    // /* Events to cut system */
    // GetWorld()->m_system.addEventHandler(
    //     new Ex3::SpringCutter(
    //         py::cast<double>(cfg["spring1_cut_time"]),
    //         std::make_tuple(spring1, damper1)
    //     )
    // );

    // GetWorld()->m_system.addEventHandler(
    //     new Ex3::SpringCutter(
    //         py::cast<double>(cfg["spring2_cut_time"]),
    //         std::make_tuple(spring2, damper2)
    //     )
    // );

    /* Construct our visualizer. */
    if(py::cast<bool>(cfg["use_viz"]))
    {
        GetWorld()->m_system.setUseUniformBackground(true);
        Visualizer viz(GetWorld()->m_system);
        viz.addDecorationGenerator(new Ex3Drawer(
            ball_1, 
            ball_2, 
            cfg
        ));
        GetWorld()->m_system.addEventReporter(
            new Visualizer::Reporter(
                viz, 
                py::cast<double>(cfg["viz_update_rate"])
            )
        );
    }

    // /* Construct the simulation object and add a dataset. */
    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);
    DataSet * ds = new DataSet(py::cast<std::string>(cfg["output_dataset_name"]));
    sim.getDataBase()->addDataSet(ds);
    GetWorld()->m_system.addEventReporter(new Ex3::Ex3DataLogger(
        py::cast<double>(cfg["output_data_rate"]), ds, ball_1, ball_2
    ));

    /* Initialize the system. */
    State s = GetWorld()->m_system.realizeTopology();

    /* Update the state. */
    Vec3 eul_joint_0;
    eul_joint_0[0] = (py::cast<std::vector<double>>(cfg["joint1"]["eul0"]) )[0]  ;
    eul_joint_0[1] = (py::cast<std::vector<double>>(cfg["joint1"]["eul0"]) )[1]   ;
    eul_joint_0[2] = (py::cast<std::vector<double>>(cfg["joint1"]["eul0"]) )[2]   ;

    Vec3 eul_joint_1;
    eul_joint_1[0] = (py::cast<std::vector<double>>(cfg["joint2"]["eul0"]) )[0]  ;
    eul_joint_1[1] = (py::cast<std::vector<double>>(cfg["joint2"]["eul0"]) )[1]   ;
    eul_joint_1[2] = (py::cast<std::vector<double>>(cfg["joint2"]["eul0"]) )[2]   ;

    Quaternion r1(Ex3::Euler321ToQuaternion(eul_joint_0));
    Quaternion r2(Ex3::Euler321ToQuaternion(eul_joint_1));

    joint_1.setQ( s, r1 );
    joint_2.setQ( s, r2 );

    ball_1.setQ( s, PCD(cfg["ball1"]["init_dist"]));
    ball_2.setQ( s, PCD(cfg["ball2"]["init_dist"]));

    /* Simulate. */
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(PCD(cfg["integ_acc"]));
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(s);
    ts.stepTo(PCD(cfg["sim_time"]));

    return sim;
}