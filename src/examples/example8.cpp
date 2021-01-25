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
namespace Ex8
{

    /**
     * @brief A placeholder TVC thrust class. Takes force
     * and ISP as inputs.
     */
    class LiquidTVCThruster : public Force::Custom::Implementation
    {
    public:
        LiquidTVCThruster(MobilizedBody tvcBase, double force_tgt, double isp, double kp, double kd) 
        : tvcBase(tvcBase), force_tgt(force_tgt), isp(isp), kp(kp), kd(kd) {}

        virtual void realizeTopology(State& s) const override {
            idx_mass = GetWorld()->m_matter.allocateZ(s, Vector(1, Real(0)));
            idx_force = GetWorld()->m_matter.allocateZ(s, Vector(1, Real(0)));
            idx_forcedot = GetWorld()->m_matter.allocateZ(s, Vector(1, Real(0)));
        }

        virtual void calcForce(const State&         state, 
                            Vector_<SpatialVec>& bodyForcesInG, 
                            Vector_<Vec3>&       particleForcesInG, 
                            Vector&              mobilityForces) const  {
            tvcBase.applyForceToBodyPoint(
                state,
                Vec3(0),
                Vec3(GetWorld()->m_matter.getZ(state)[idx_force],0,0),
                bodyForcesInG);
        }

        virtual Real calcPotentialEnergy(const State& state) const { return 0; }

        virtual void realizeDynamics(const State& s) const override {
            
            GetWorld()->m_matter.updZDot(s)[idx_force] = GetWorld()->m_matter.getZ(s)[idx_forcedot];
            GetWorld()->m_matter.updZDot(s)[idx_forcedot] = 
            this->kp * ( GetWorld()->m_matter.getZ(s)[idx_force] - this->force_tgt ) 
            + this->kd * ( GetWorld()->m_matter.getZDot(s)[idx_force] );


            GetWorld()->m_matter.updZDot(s)[idx_mass] = 
                GetWorld()->m_matter.getZ(s)[idx_force] / (g0 * this->isp);
        }

        Real getTotalExpelledMass(State &s) {
            return GetWorld()->m_matter.getZ(s)[idx_mass];
        }

        Real getCurrentForce(const State &s) {
            return GetWorld()->m_matter.getZ(s)[idx_force];
        }
        void setForceTarget(Real targ){ this->force_tgt = targ; }
        Real getForceTarget(){ return this->force_tgt; }

    protected:
        MobilizedBody tvcBase;
        mutable ZIndex idx_mass;
        mutable ZIndex idx_force;
        mutable ZIndex idx_forcedot;
        double force_tgt;
        double isp;
        double zprev;
        double kp;
        double kd;

    };

    class Ex8Logger : public PeriodicEventReporter 
    {
    public:

        Ex8Logger(Real interval, DataSet* ds, MobilizedBody vehicle, LiquidTVCThruster * thr) 
            : vehicle(vehicle), thr(thr), PeriodicEventReporter(interval), ds(ds) { }

        void handleEvent(const State& s) const override {
            double t = s.getTime();
            double m = vehicle.getBodyMass(s);
            double x = vehicle.getBodyOriginLocation(s)[0];
            double vx = vehicle.getBodyOriginVelocity(s)[0];
            double f = thr->getCurrentForce(s);
            double ft = thr->getForceTarget();
            std::vector<double> out = {t, x, vx, m, f, ft};
            ds->addDataEntry(out);
        }

    protected:

        DataSet* ds;
        MobilizedBody vehicle;
        LiquidTVCThruster * thr;

    };  
}



/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
Modeller::Core::Simulation Modeller::Examples::Run_Ex8(py::dict cfg)
{
    using namespace Ex8;

    double dt = PCD(cfg["dt"]);
    double mdot = - PCD(cfg["F"]) / (Constants::g0 * PCD(cfg["isp"]));
    double dm = mdot * dt;
    double t_burn = PCD(cfg["t_burn"]);

    MassProperties mp(
        PCD(cfg["m0"]),
        Vec3(0),
        UnitInertia(1)
    );

    /* Construct the shape and body parameters of the vehicle. */
    MobilizedBody::Slider vehicle(
        GetWorld()->m_matter.Ground(),
        Body::Rigid(mp)
    );

    /* Add a liquid thruster force object */
    LiquidTVCThruster *thr1 = new LiquidTVCThruster( 
        vehicle, PCD(cfg["F"]), PCD(cfg["isp"]), PCD(cfg["thr_kp"]), PCD(cfg["thr_kd"]) );
    Force::Custom(GetWorld()->m_forces, thr1);

    /* Configure the simulation and realize topology. */
    Modeller::Core::Simulation sim;
    GetWorld()->registerSimulation(&sim);
    DataSet * ds = new DataSet("output");
    sim.getDataBase()->addDataSet(ds);
    Ex8Logger * log = new Ex8Logger(Infinity, ds, vehicle, thr1);
    GetWorld()->m_system.addEventReporter(log);
    State s = GetWorld()->m_system.realizeTopology();

    /* Set up the integrator. */
    RungeKutta2Integrator integ(GetWorld()->m_system);
    integ.setMaximumStepSize(dt);
    integ.setAllowInterpolation(false);

    /* Initialize, realize, run initial state event handling. */
    integ.initialize(s);
    integ.stepTo(0.0); // Because of "startOfContinuousInterval", need to set to zero?
    s = integ.getState();
    GetWorld()->m_system.realize(s, Stage::Dynamics); // realize for my "logger"
    

    /* Time stepping parameters. */
    while(integ.getTime() < t_burn - 1e-8)
    {
        if(integ.getTime() >= 3) {
            thr1->setForceTarget(0.5 * PCD(cfg["F"]));
        }
        if(integ.getTime() >= 6) {
            thr1->setForceTarget(0.7 * PCD(cfg["F"]));
        }
        if(integ.getTime() >= 11) {
            thr1->setForceTarget(0.2 * PCD(cfg["F"]));
        }

        Integrator::SuccessfulStepStatus ss  = integ.stepBy(dt);
        State s = integ.updAdvancedState();
        GetWorld()->m_system.realize(s, Stage::Dynamics);
        
        mp.setMassProperties(
            PCD(cfg["m0"]) - thr1->getTotalExpelledMass(s),
            mp.getMassCenter(),
            mp.getInertia()
        );
        vehicle.updBody().setDefaultRigidBodyMassProperties( mp );
        GetWorld()->m_system.realizeTopology(); // re-realize topology
        GetWorld()->m_system.realize(s, Stage::Dynamics);
        log->handleEvent(s);
    }

    return sim;
}