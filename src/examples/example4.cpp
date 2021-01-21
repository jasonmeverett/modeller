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
//                                          Ex4
//                                          ---
//                              CMD Exmaple in Chapter 4.1
//     
//     ------------------------------------------------------------------------------------


/** @file example4.cpp
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


namespace Ex4
{


    /**
     * @brief Data logger class.
     * 
     */
    class Ex4DataLogger : public PeriodicEventReporter 
    {
    public:

        // Upon construction, clear the output.
        Ex4DataLogger(Real interval, DataSet* ds, MobilizedBody &p1) : 
            PeriodicEventReporter(interval), 
            ds(ds),
            m_p1(p1) { }

        void handleEvent(const State& s) const override {
            GetWorld()->m_system.realize(s, Stage::Dynamics);

            std::vector<double> out_row;
            out_row.push_back(s.getTime());
            out_row.push_back(m_p1.getBodyOriginLocation(s)[0]);
            out_row.push_back(m_p1.getBodyOriginLocation(s)[1]);
            out_row.push_back(m_p1.getBodyOriginLocation(s)[2]);
            out_row.push_back(GetWorld()->m_system.calcEnergy(s));
            out_row.push_back(GetWorld()->m_system.calcKineticEnergy(s));
            out_row.push_back(GetWorld()->m_system.calcPotentialEnergy(s));

            this->ds->addDataEntry(out_row);

            // Direct line taken from CMD
            printf( "%8.3f %8.6f\n", s.getTime(), m_p1.getOneQ(s, 0));
        }


    protected:

        // Local reference to the mobilized body.
        const SimTK::MobilizedBody &m_p1;
        DataSet* ds;

    };   


    /**
     * @brief Example 4 controller. This will calculate the "gammad" parameter
     * found in the CMD example as the prescribed velocity of the X-axis (one mobility)
     * of the slider mobilized body. Acceleration also has to be computed, which is
     * pretty straight forward as the velocity equation was already known.
     * 
     * Motion prescription has to be used here because the dynamics model used in the
     * CMD example is a custom dynamics model.
     * 
     */
    class Ex4Controller : public Motion::Custom::Implementation
    {
    public:
        Ex4Controller(MobilizedBody mobod, double gamma_cmd, double v, double k) 
            : m_mobod(mobod), gamma_cmd(gamma_cmd), v(v), k(k) {}


        Motion::Level getLevel(const State&) const override {
            return Motion::Level::Velocity;
        }

        void calcPrescribedVelocity(const State &s, int nu, Real * u) const override {
            double gamma = this->m_mobod.getOneQ(s, 0);
            u[0] = this->k * ( this->gamma_cmd - gamma ) / this->v;
        }

        void calcPrescribedVelocityDot(const State &s, int nu, Real * udot) const override {
            double gammadot = this->m_mobod.getOneU(s, 0);
            udot[0] = - 1.0 * this->k * gammadot / this->v;
        }


    protected:
        MobilizedBody m_mobod;
        double gamma_cmd;
        double v;
        double k;

    };
}


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
Modeller::Core::Simulation Modeller::Examples::Run_Ex4(py::dict cfg)
{
    using namespace Ex4;

    /* Define rigid body spheres. */
    Body::Rigid ball( MassProperties( 1.0, Vec3(0), UnitInertia(1) ) );
    
    /* Construct the mobilized bodies. */
    MobilizedBody::Slider mobod(
        GetWorld()->m_matter.Ground(),
        Transform(),
        ball,
        Transform()
    );

    Motion::Custom motion1(mobod, new Ex4Controller(mobod, 1.0, 1000.0, 1000.0));

    /* Construct the simulation object and add a dataset. */
    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);
    DataSet * ds = new DataSet("output");
    sim.getDataBase()->addDataSet(ds);
    GetWorld()->m_system.addEventReporter(new Ex4::Ex4DataLogger( 0.1, ds, mobod ));

    /* Initialize the system. */
    State s = GetWorld()->m_system.realizeTopology();

    /* Simulate. */
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(1e-8);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(s);
    ts.stepTo(2);

    return sim;
}