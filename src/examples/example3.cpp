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
//                 Double pendulum w/ springs, dampers, ... and a stoplight!
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

    using SpringDamper = std::tuple<Force::TwoPointLinearSpring, Force::TwoPointLinearDamper>;

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
        Ex3Drawer( MobilizedBody m1, MobilizedBody m2, SpringDamper sd1, SpringDamper sd2, py::dict cfg) 
            :  m1(m1),  m2(m2), sd1(sd1), sd2(sd2), cfg(cfg) {}

        /**
         * @brief Required override for inheriting from a DecorationGenerator
         * 
         * @param state 
         * @param geometry 
         */
        void generateDecorations(const State& state, 
            Array_<DecorativeGeometry>& geometry) override 
        {
            GetWorld()->m_system.realize(state, Stage::Dynamics);

            double l1 = m1.getBodyOriginLocation(state).norm();
            double l2 = m2.findBodyOriginLocationInAnotherBody(state, m1).norm();
            double f1 = - PCD(cfg["spring1"]["k"]) * (l1 - PCD(cfg["spring1"]["x0"])) * static_cast<double>(!std::get<0>(sd1).isDisabled(state));
            double f2 = - PCD(cfg["spring2"]["k"]) * (l2 - PCD(cfg["spring2"]["x0"])) * static_cast<double>(!std::get<0>(sd2).isDisabled(state)) ;

            // Set texts
            DecorativeText text_title("Sim Time: " + std::to_string(state.getTime()) + " sec");
            text_title.setIsScreenText(true);
            text_title.setColor(Vec3(0,0,0.9));
            geometry.push_back( text_title );

            DecorativeSphere sphere(0.05);
            sphere.setColor(Vec3(0));
            sphere.setTransform(Transform(GetWorld()->m_matter.calcSystemMassCenterLocationInGround(state)));
            geometry.push_back(sphere);

            DecorativeText text_sp1(std::to_string(f1) + " N");
            text_sp1.setTransform(0.5*(m1.getBodyOriginLocation(state)));
            text_sp1.setScale(0.1);
            geometry.push_back( text_sp1 );

            DecorativeText text_sp2(std::to_string(f2) + " N");
            text_sp2.setTransform(0.5*(m1.getBodyOriginLocation(state) + m2.getBodyOriginLocation(state)));
            text_sp2.setScale(0.1);
            geometry.push_back( text_sp2 );

            if(state.getTime() >= py::cast<double>(cfg["spring1_cut_time"])){
                DecorativeText text("Spring1 Cut: " + std::to_string(py::cast<double>(cfg["spring1_cut_time"])) + " sec");
                text.setIsScreenText(true);
                text.setColor(Vec3(0.9,0,0));
                geometry.push_back( text );
            }else
            {
                DecorativeLine line1(Vec3(0), m1.getBodyOriginLocation(state));
                line1.setLineThickness(PCD(cfg["spring1"]["x0"]) / l1);
                line1.setColor( f1 > 0.0 ? Vec3(fabs(f1)/30.0,0,0) : Vec3(0,fabs(f1)/30.0,fabs(f1)/30.0) );
                geometry.push_back(line1);
            }

            if(state.getTime() >= py::cast<double>(cfg["spring2_cut_time"]))
            {
                DecorativeText text("Spring2 Cut: " + std::to_string(py::cast<double>(cfg["spring2_cut_time"])) + " sec");
                text.setIsScreenText(true);
                text.setColor(Vec3(0,0.9,0));
                geometry.push_back( text );
            }else{
                DecorativeLine line2(m1.getBodyOriginLocation(state), m2.getBodyOriginLocation(state));
                line2.setLineThickness(PCD(cfg["spring2"]["x0"]) / l2);
                line2.setColor( f2 > 0.0 ? Vec3(fabs(f2)/30.0,0,0) : Vec3(0,fabs(f2)/30.0,fabs(f2)/30.0) );
                geometry.push_back(line2);
            }
        }

    protected:
        MobilizedBody m1;
        MobilizedBody m2;
        SpringDamper sd1;
        SpringDamper sd2;
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
    Body::Rigid rb_ball1( MassProperties( py::cast<double>(cfg["ball1"]["mass"]), Vec3(0), UnitInertia(1) ) );
    Body::Rigid rb_ball2( MassProperties( py::cast<double>(cfg["ball2"]["mass"]), Vec3(0), UnitInertia(1) ) );

    /* Add sphere decorations. */
    rb_ball1.addDecoration(Transform(), DecorativeSphere(py::cast<double>(cfg["ball1"]["radius"])));
    rb_ball2.addDecoration(Transform(), DecorativeSphere(py::cast<double>(cfg["ball2"]["radius"])));

    /* Construct the mobilized bodies. */
    MobilizedBody::Translation ball_spring_1(
        GetWorld()->m_matter.Ground(),
        Transform(),
        rb_ball1,
        Transform()
    );

    MobilizedBody::Translation ball_spring_2(
        ball_spring_1,
        Transform(),
        rb_ball1,
        Transform()
    );


    /* Add in the spring forces. */
    Force::TwoPointLinearSpring spring1(
        GetWorld()->m_forces,
        GetWorld()->m_matter.Ground(),
        Vec3(0),
        ball_spring_1,
        Vec3(0),
        py::cast<double>(cfg["spring1"]["k"]),
        py::cast<double>(cfg["spring1"]["x0"])
    );

    Force::TwoPointLinearSpring spring2(
        GetWorld()->m_forces,
        ball_spring_1,
        Vec3(0),
        ball_spring_2,
        Vec3(0),
        py::cast<double>(cfg["spring2"]["k"]),
        py::cast<double>(cfg["spring2"]["x0"])
    );

    /* Add in the Damper Forces. */
    Force::TwoPointLinearDamper damper1(
        GetWorld()->m_forces,
        GetWorld()->m_matter.Ground(),
        Vec3(0),
        ball_spring_1,
        Vec3(0),
        py::cast<double>(cfg["spring1"]["c"])
    );

    Force::TwoPointLinearDamper damper2(
        GetWorld()->m_forces,
        ball_spring_1,
        Vec3(0),
        ball_spring_2,
        Vec3(0),
        py::cast<double>(cfg["spring2"]["c"])
    );

    /* Apply gravity */
    if(py::cast<bool>(cfg["use_grav"])) {
        Force::Gravity gravity(GetWorld()->m_forces, GetWorld()->m_matter, -YAxis, 9.8);
    }

    /* Events to cut system */
    GetWorld()->m_system.addEventHandler(
        new Ex3::SpringCutter(
            py::cast<double>(cfg["spring1_cut_time"]),
            std::make_tuple(spring1, damper1)
        )
    );

    GetWorld()->m_system.addEventHandler(
        new Ex3::SpringCutter(
            py::cast<double>(cfg["spring2_cut_time"]),
            std::make_tuple(spring2, damper2)
        )
    );


    /* Construct our visualizer. */
    if(py::cast<bool>(cfg["use_viz"]))
    {
        GetWorld()->m_system.setUseUniformBackground(true);
        Visualizer viz(GetWorld()->m_system);
        viz.addDecorationGenerator(new Ex3Drawer(
            ball_spring_1, 
            ball_spring_2, 
            std::make_tuple(spring1, damper1),
            std::make_tuple(spring2, damper2),
            cfg
        ));
        GetWorld()->m_system.addEventReporter(
            new Visualizer::Reporter(
                viz, 
                py::cast<double>(cfg["viz_update_rate"])
            )
        );
    }

    /* Construct the simulation object and add a dataset. */
    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);
    DataSet * ds = new DataSet(py::cast<std::string>(cfg["output_dataset_name"]));
    sim.getDataBase()->addDataSet(ds);
    GetWorld()->m_system.addEventReporter(new Ex3::Ex3DataLogger(
        py::cast<double>(cfg["output_data_rate"]), ds, ball_spring_1, ball_spring_2
    ));

    /* Initialize the system. */
    State s = GetWorld()->m_system.realizeTopology();
    
    /* Update the state. */
    std::vector<double> b1_r0 = py::cast<std::vector<double>>(cfg["ball1"]["r0"]);
    std::vector<double> b2_r0 = py::cast<std::vector<double>>(cfg["ball2"]["r0"]);
    std::vector<double> b1_v0 = py::cast<std::vector<double>>(cfg["ball1"]["v0"]);
    std::vector<double> b2_v0 = py::cast<std::vector<double>>(cfg["ball2"]["v0"]);
    ball_spring_1.setQ(s, Vec3(b1_r0[0], b1_r0[1], b1_r0[2]));
    ball_spring_2.setQ(s, Vec3(b2_r0[0], b2_r0[1], b2_r0[2]));
    ball_spring_1.setU(s, Vec3(b1_v0[0], b1_v0[1], b1_v0[2]));
    ball_spring_2.setU(s, Vec3(b2_v0[0], b2_v0[1], b2_v0[2]));

    /* Simulate. */
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(py::cast<double>(cfg["integ_acc"]));
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(s);
    ts.stepTo(py::cast<double>(cfg["sim_time"]));

    return sim;
}