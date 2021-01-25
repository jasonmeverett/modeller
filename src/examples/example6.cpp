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
//                                          Ex6
//                                          ---
//                                      Separation Example
//     
//     ------------------------------------------------------------------------------------


/** @file example6.cpp
 * 
 * @brief This example simulates the CMD block that was configured in Chapter 4.1. Note that
 * this example uses custom dynamics, so the easiest way to simulate the movement was via
 * prescribing the velocity of the mobility in question. 
 * 
 */ 


// Include Modeller.
#include <Modeller/Modeller.hpp>
#include <string>
#include <tuple>
#include <iomanip>

// Helper macro for readability
#define PCD(x) py::cast<double>(x)
#define PCB(x) py::cast<bool>(x)

namespace Ex6
{
    std::vector<MobilizedBody::Weld> base_spring_welds;
    std::vector<Force::TwoPointLinearSpring> base_to_cap_springs;
    std::vector<MobilizedBody::Weld> cap_spring_welds;
    std::vector<Constraint::Rod> init_cap_constraints;
    std::vector<Force::MobilityConstantForce> SRM_spinup_forces;
    std::vector<MobilizedBody::Weld> SRM_spinup_mounts;
    std::vector<MobilizedBody::Weld> SRM_thrust_frames;


    class Ex6DataLogger : public PeriodicEventReporter 
    {
    public:

        // Upon construction, clear the output.
        Ex6DataLogger(Real interval, DataSet* ds, MobilizedBody &st2) : 
            PeriodicEventReporter(interval), 
            ds(ds),
            st2(st2) { }

        void handleEvent(const State& s) const override {
            GetWorld()->m_system.realize(s, Stage::Dynamics);

            std::vector<double> out_row;
            out_row.push_back(s.getTime());
            out_row.push_back( 
                acos(dot(
                    st2.getBodyRotation(s) * Vec3(1,0,0),
                    Vec3(1,0,0)
                ))*180.0/PI
            );
            out_row.push_back(
                (st2.getBodyRotation(s).invert() * st2.getBodyAngularVelocity(s))[0] * 30.0 / PI
            );

            out_row.push_back(GetWorld()->m_system.calcEnergy(s));
            out_row.push_back(GetWorld()->m_system.calcKineticEnergy(s));
            out_row.push_back(GetWorld()->m_system.calcPotentialEnergy(s));

            this->ds->addDataEntry(out_row);
        }


    protected:

        // Local reference to the mobilized body.
        MobilizedBody st2;
        DataSet* ds;

    };   

    class Ex6Drawer : public SimTK::DecorationGenerator
    {
    public:
        Ex6Drawer(MobilizedBody st2, py::dict cfg) : st2(st2), cfg(cfg) {}

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

            // Set texts
            DecorativeText text_title("Sim Time: " + std::to_string(s.getTime()) + " sec");
            text_title.setIsScreenText(true);
            text_title.setColor(Vec3(0,0,0.9));
            geometry.push_back( text_title );

            DecorativeText text_angmom("BodyX to Vgo: " + std::to_string(
                acos(dot(
                    st2.getBodyRotation(s) * Vec3(1,0,0),
                    Vec3(1,0,0)
                ))*180.0/PI
            ) + " deg");
            text_angmom.setIsScreenText(true);
            text_angmom.setColor(Vec3(1,0,0));
            geometry.push_back( text_angmom );

            double roll_x = (st2.getBodyRotation(s).invert() * st2.getBodyAngularVelocity(s))[0] * 30.0 / PI;
            DecorativeText tex_angvel("Roll Rate RPM: " + std::to_string( roll_x ));
            tex_angvel.setIsScreenText(true);
            tex_angvel.setColor(Vec3(1,0,0));
            geometry.push_back( tex_angvel );

            geometry.push_back(DecorativeLine()
                .setPoint1(st2.findMassCenterLocationInGround(s) - Vec3(1,0,0)) 
                .setPoint2(st2.findMassCenterLocationInGround(s) + Vec3(1,0,0))
                .setLineThickness(3.0)
                .setColor(Vec3(1))
            );

            for(int ii = 0; ii < cap_spring_welds.size(); ii++)
            {
                auto spring = base_to_cap_springs[ii];
                if(!spring.isDisabled(s))
                {
                    DecorativeLine l_spring;
                    l_spring.setPoint1(base_spring_welds[ii].getBodyOriginLocation(s));
                    l_spring.setPoint2(cap_spring_welds[ii].getBodyOriginLocation(s));
                    l_spring.setColor(Vec3(0));
                    l_spring.setLineThickness(5.0);
                    geometry.push_back(l_spring);
                }
            }
        }

    protected:
        py::dict cfg;
        MobilizedBody st2;

    };

    class ConstraintReleaser : public ScheduledEventHandler
    {
    public:
        ConstraintReleaser(double septime, Constraint::Rod constraint)
            : septime(septime), constraint(constraint) {}

        Real getNextEventTime(const State& s, bool incCurTime) const override {
            return this->septime;
        }

        void handleEvent(State &s, Real accuracy, bool& shouldTerminate) const override {
            this->constraint.disable(s);
        }

    protected:
        double septime;
        Constraint::Rod constraint;

    };

    class ForceToggler : public ScheduledEventHandler
    {
    public:
        ForceToggler(double time, Force* force)
            : time(time), force(force) {}

        Real getNextEventTime(const State& s, bool incCurTime) const override {
            return this->time;
        }

        void handleEvent(State &s, Real accuracy, bool& shouldTerminate) const override {
            if( this->force->isDisabled(s) ) {
                this->force->enable(s);
            } else {
                this->force->disable(s);
            }
        }

    protected:
        double time;
        Force* force;

    };

    class SpringDisconnector : public TriggeredEventHandler
    {
    public:
        SpringDisconnector(Force::TwoPointLinearSpring force, MobilizedBody m1, MobilizedBody m2, double l) 
            : force(force), m1(m1), m2(m2), l(l), TriggeredEventHandler(Stage::Position) {}

        Real getValue(const State& s) const override {
            double val = m2.findBodyOriginLocationInAnotherBody(s, m1).norm() - l;
            return val;
        }

        EventTriggerInfo getTriggerInfo() {
            EventTriggerInfo info;
            info.setTriggerOnRisingSignTransition(true);
            info.setTriggerOnFallingSignTransition(false);
            info.setRequiredLocalizationTimeWindow(1e-8);
            return info;
        }  

        void handleEvent(State &s, Real accuracy, bool &shouldTerminate) const override {
            force.disable(s);
            std::cout << "Time: " << s.getTime() << " SPRING SEP\n";
            // std::cout << "Distance: " << std::setprecision(10) << 
            //     m2.findBodyOriginLocationInAnotherBody(s, m1).norm() << "\n";
            return;
        }


    protected:
        Force::TwoPointLinearSpring force;
        MobilizedBody m1;
        MobilizedBody m2;
        double l;

    };
}


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
Modeller::Core::Simulation Modeller::Examples::Run_Ex6(py::dict cfg)
{
    using namespace Ex6;

    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);

    double base_r = 0.3;
    double base_hz = 1.5;
    double base_mass = 100.0;
    double cap_hz = 0.6;
    double cap_mass = 50.0;
    
    double spring_r_fac = 0.8;
    double spring_x0 = 0.1;
    double spring_k = 80000;
    double base_cap_x0 = 0.1 - 0.0015;
    
    double const_times[] = {1.004, 1.000, 1.002, 1.005};
    double r2d = 180.0 / PI;
    double d2r = PI / 180.0;
    Vec3 spinup1_eul321_mount_to_thrust(1.0 * d2r, -2.0 * d2r, 0);
    Vec3 spinup2_eul321_mount_to_thrust(-2.0 * d2r, 1.0 * d2r, 0);
    double spinup_times[] = {2.0, 2.0};
    double thrust_durs[] = {1.0, 1.0};
    double thrust_mag = 1;

    double sim_time = 10.0;
    double integ_acc = 1e-12;
    double viz_update_rate = 0.005;
    
    Body::Massless default_bod;

    Body::Rigid rb_base(
        MassProperties(
            base_mass,
            Vec3(0),
            Inertia::cylinderAlongX(base_r, base_hz)
        )
    );
    rb_base.addDecoration(
        DecorativeCylinder(base_r, base_hz).setTransform(
            Rotation(
                -PI/2, 
                Vec3(0,0,1)
            )
        ).setOpacity(0.5)
    );

    Body::Rigid rb_cap(
        MassProperties(
            cap_mass,
            Vec3(0),
            Inertia::cylinderAlongX(base_r, cap_hz)
        )
    );
    rb_cap.addDecoration(
        DecorativeCylinder(base_r, cap_hz).setTransform(
            Rotation(
                -PI/2, 
                Vec3(0,0,1)
            )
        ).setOpacity(0.5)
    );

    MobilizedBody::Free base(
        GetWorld()->m_matter.Ground(),
        rb_base
    );

    MobilizedBody::Free cap(
        GetWorld()->m_matter.Ground(),
        rb_cap
    );

    std::vector<std::vector<int>> multTable = {
        {1, 0},
        {0, 1},
        {-1,0},
        {0,-1}
    };

    for(int ii = 0; ii < multTable.size(); ii++)
    {
        // Add the base weld joint.
        base_spring_welds.push_back(MobilizedBody::Weld(
            base,
            Transform( Vec3(
                    base_hz,
                    multTable[ii][0]*spring_r_fac*base_r,
                    multTable[ii][1]*spring_r_fac*base_r
                ) ),
            default_bod,
            Transform()
        ));

        // Add the cap weld joint.
        cap_spring_welds.push_back(MobilizedBody::Weld(
            cap,
            Transform(
                Vec3(
                    -cap_hz,
                    multTable[ii][0]*spring_r_fac*base_r,
                    multTable[ii][1]*spring_r_fac*base_r
                )
            ),
            default_bod,
            Transform()
        ));

        // Add the spring.
        base_to_cap_springs.push_back(Force::TwoPointLinearSpring(
            GetWorld()->m_forces,
            base_spring_welds[ii],
            Vec3(0),
            cap_spring_welds[ii],
            Vec3(0),
            spring_k,
            spring_x0
        ));

        init_cap_constraints.push_back(Constraint::Rod(
            base_spring_welds[ii],
            cap_spring_welds[ii],
            base_cap_x0
        ));
        
        GetWorld()->m_system.addEventHandler(new ConstraintReleaser(
            const_times[ii],
            init_cap_constraints[ii]
        ));

        GetWorld()->m_system.addEventHandler(new SpringDisconnector(
            base_to_cap_springs[ii],
            base_spring_welds[ii],
            cap_spring_welds[ii],
            spring_x0
        ));
    }

    /* -----------------------------------------------------
     * SPINUP MOUNTING FRAMES
     * -----------------------------------------------------
     */

    SRM_spinup_mounts.push_back(
        MobilizedBody::Weld(
            cap,
            Transform(
                Rotation(PI/2, Vec3(0,1,0)),
                Vec3(0,base_r,0)
            ),
            default_bod,
            Transform()
        )
    );

    SRM_spinup_mounts.push_back(
        MobilizedBody::Weld(
            cap,
            Transform(
                Rotation(PI/2, Vec3(0,1,0)) * Rotation(PI, Vec3(0,0,1)),
                Vec3(0,-base_r,0)
            ),
            default_bod,
            Transform()
        )
    );

    /* -----------------------------------------------------
     * SPINUP THRUSTING FRAMES
     * -----------------------------------------------------
     */

    SRM_thrust_frames.push_back(
        MobilizedBody::Weld(
            SRM_spinup_mounts[0],
            Transform(
                Rotation(Euler321ToQuaternion(spinup1_eul321_mount_to_thrust))
            ),
            default_bod,
            Transform()
        )
    );

    SRM_thrust_frames.push_back(
        MobilizedBody::Weld(
            SRM_spinup_mounts[1],
            Transform(
                Rotation(Euler321ToQuaternion(spinup2_eul321_mount_to_thrust))
            ),
            default_bod,
            Transform()
        )
    );

    /* -----------------------------------------------------
     * FORCES
     * -----------------------------------------------------
     */
    std::vector<ConstantForceAlongXAxis> f_spinups = {
        ConstantForceAlongXAxis(
            GetWorld()->m_forces,
            SRM_thrust_frames[0],
            thrust_mag
        ),
        ConstantForceAlongXAxis(
            GetWorld()->m_forces,
            SRM_thrust_frames[1],
            thrust_mag
        )
    };
    f_spinups[0].setDisabledByDefault(true);
    f_spinups[1].setDisabledByDefault(true);
    
    std::vector<ForceToggler> togglers_spinup = {
        ForceToggler(spinup_times[0], &f_spinups[0]),
        ForceToggler(spinup_times[1], &f_spinups[1]),
        ForceToggler(spinup_times[0] + thrust_durs[0], &f_spinups[0]),
        ForceToggler(spinup_times[1] + thrust_durs[1], &f_spinups[1]),
    };

    for(auto &toggler : togglers_spinup)
    {
        GetWorld()->m_system.addEventHandler(&toggler);
    }

    if(PCB(cfg["use_viz"]))
    {
        GetWorld()->m_system.setUseUniformBackground(true);
        Visualizer viz(GetWorld()->m_system);
        viz.addDecorationGenerator(new Ex6Drawer( cap, cfg ));
        GetWorld()->m_system.addEventReporter(  
            new Visualizer::Reporter(  viz,  viz_update_rate ) );
        viz.setBackgroundColor(Vec3(0));
    }

    DataSet * ds = new DataSet("output");
    sim.getDataBase()->addDataSet(ds);
    GetWorld()->m_system.addEventReporter(new Ex6::Ex6DataLogger( 0.001, ds, cap ));

    State s = GetWorld()->m_system.realizeTopology();
    
    cap.setQToFitTranslation(s, Vec3(base_hz + cap_hz + base_cap_x0,0,0));
    GetWorld()->m_system.project(s);

    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(integ_acc);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(s);
    ts.stepTo(sim_time);

    return sim;
}