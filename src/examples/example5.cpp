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
//                       CMD Exmaple in Chapter 4.2 (discrete & continuous)
//     
//     ------------------------------------------------------------------------------------


/** @file example5.cpp
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


namespace Ex5
{
    double kgain = 1000;

    /**
     * @brief Data logger class.
     * 
     */
    class Ex5DataLogger : public PeriodicEventReporter 
    {
    public:

        // Upon construction, clear the output.
        Ex5DataLogger(Real interval, DataSet* ds, MobilizedBody &p1) : 
            PeriodicEventReporter(interval), 
            ds(ds),
            m_p1(p1) { }

        void handleEvent(const State& s) const override {
            GetWorld()->m_system.realize(s, Stage::Dynamics);

            std::vector<double> out_row;
            out_row.push_back(s.getTime());
            out_row.push_back(m_p1.getOneQ(s, 0));
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


    class Ex5GainSwitcher : public ScheduledEventHandler
    {
    public:
        Ex5GainSwitcher() {}

        Real getNextEventTime(const State& state, bool includeCurrentTime) const override {
            return 1.0;
        }

        void handleEvent(State &s, Real accuracy, bool &shouldTerminate) const override {
            Ex5::kgain = 2000.0;
            return;
        }
    };


    class Ex5Controller : public PeriodicEventHandler
    {
    public:
        Ex5Controller(double rate, MobilizedBody mobod) 
        : PeriodicEventHandler(rate), m_mobod(mobod), v(1000), gamma_cmd(1.0)  { }

        void handleEvent(State &s, Real accuracy, bool &shouldTerminate) const override {
            
            double gamma = this->m_mobod.getOneQ(s, 0);
            double a_cmd = Ex5::kgain * (this->gamma_cmd - gamma);

            this->m_mobod.unlock(s);
            this->m_mobod.setOneU(s,0,a_cmd/this->v);
            this->m_mobod.lock(s, Motion::Velocity);

            std::cout << " sample " << s.getTime() << " " << a_cmd << "\n";
            return;
        }

    protected:
        MobilizedBody m_mobod;
        double v;
        double gamma_cmd;

    };
}


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
Modeller::Core::Simulation Modeller::Examples::Run_Ex5(py::dict cfg)
{
    using namespace Ex5;

    /* Define rigid body spheres. */
    Body::Rigid ball( MassProperties( 1.0, Vec3(0), UnitInertia(1) ) );
    
    /* Construct the mobilized bodies. */
    MobilizedBody::Slider mobod(
        GetWorld()->m_matter.Ground(),
        Transform(),
        ball,
        Transform()
    );

    /* Construct the simulation object and add a dataset. */
    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);
    DataSet * ds = new DataSet("output");
    sim.getDataBase()->addDataSet(ds);
    GetWorld()->m_system.addEventHandler(new Ex5::Ex5GainSwitcher());
    GetWorld()->m_system.addEventHandler(new Ex5::Ex5Controller( 0.1, mobod ));
    GetWorld()->m_system.addEventReporter(new Ex5::Ex5DataLogger( 0.1, ds, mobod ));


    State s = GetWorld()->m_system.realizeTopology();
    

    /* Simulate. */
    RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    integ.setAccuracy(1e-8);
    TimeStepper ts(GetWorld()->m_system, integ);
    ts.initialize(s);
    ts.stepTo(2 + 1e-8);

    return sim;
}