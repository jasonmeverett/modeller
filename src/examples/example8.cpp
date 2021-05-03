//     ------------------------------------------------------------------------------------
//     
//             ███╗   ███╗ ██████╗ ██████╗ ███████╗██╗     ██╗     ███████╗██████╗ 
//             ████╗ ████║██╔═══██╗██╔══██╗██╔════╝██║     ██║     ██╔════╝██╔══██╗
//             ██╔████╔██║██║   ██║██║  ██║█████╗  ██║     ██║     █████╗  ██████╔╝
//             ██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██║     ██║     ██╔══╝  ██╔══██╗
//             ██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗███████╗███████╗███████╗██║  ██║
//             ╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝
//     
//          Example of a variable-mass system using a liquid thruster.
//          In this example, a mobilized body (1-d slider) has a thruster (custom force)
//          attached to it. This thruster will change force based on a force target, where
//          the force target is a discrete variable (but the force itself will not be discrete).
//          during a time step, auxiliary variables are used to integrate mass deltas, and then
//          mass properties are manually updated at the end of the time step. Note that standard
//          use of TimeStepper classes cannot be used because of the fact that we need to 
//          realize topology (mass property changes) each timestep. It was deemed sufficient to
//          not update mass properties during a timestep, however this means that there are
//          other mdot-related forces that do not get added to the system, which would need
//          to be added in manually for more complex systems (jet-damping?).
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

    /* Force class to handle forces on a mobod. */
    class LiquidTVCThruster : public Force::Custom::Implementation
    {
    public:

        /* Main constructor. Eventually, it would make more sense to tie a liquid TVC thruster to a 
         * tank rather than to a mobod, because the mass flow comes directly from the thruster. Essentially
         * the thruster object may be in charge of accumulating the mass-delta over an integration timestep,
         * but another omniscient part of the system should be in charge of then determining where this mass
         * should be "flushed" from. It would be really cool to eventually build up pipe-lining systems that
         * connect a thruster to a tank, and simulate flow in these pipes, but it's out of scope for now. 
         * 
         * Actually, it would make more sense to tie a liquid thruster to a tank, and then the thruster would
         * have the ability to flush the mass from the tank directly (and would also be able to query how much
         * mass is left in the tank). In reality, the mobod that the thruster is attached to should only be ONE
         * mobod axis, in order to separate out the thrusting object (essentially the nozzle) from the TVC gimbal.
         * The TVC gimbal would then, through some actuation, apply a torque from the inboard body (the vehicle)
         * to the outboard body (the nozzle).
         * 
         * A vehicle is defined structurally by a mass properties structure. Then, there is another mobilized 
         * body attached to the mass properties mobilized bod, by a fixed offset, that represents both the liquid
         * fuel tank and liquid oxidizer tank. This represents just the pure mass properties of the tank itself.
         * Then there is another mobilized body internal to the fuel tank that represents the actual internal
         * fuel or oxidizer, perhaps attached to the prop feed line location of the tank, that has variable mass
         * properties. This is up in the air depending on how slosh should be modelled internally. If tanks have
         * decreasing inertia properties, how is this handled internally to the tank? Anyways, this is something
         * that could potentially be a load-in option.
         * 
         * Attached to the vehicle structural frame, then, would be a mobilized body rotational joint that 
         * represents the gimbal. Further downstream from the gimbal would be a nozzle that contains its own
         * mass properties, and perhaps even the gimbal does as well! The gimbal mobilized body would apply 
         * torques to the mobilized body depending on some discrete parameter that represetnts the commanded
         * angle and rate of the TVC. This would be tuned then by whatever transfer function represents the
         * internal behaviour of the TVC gimballing system. When a torque is applied to this mobilized body, 
         * that torque effect would hopefully then be propagated out to the rest of the system, such that the
         * torque effects both the nozzle AND the vehicle body. This would represent the tail-wags-dog behaviour.
         * This can probably be checked by a simple example that has two mobilized bodies attached to eachother
         * with the gimbal mobilized body between them applying a torque to the body. It would be great to visualize
         * this but probably would not be easy to set up because Ubuntu just does not want to share X-server information.
         * 
         * */
        LiquidTVCThruster(MobilizedBody tvcBase, double force_tgt, double isp, double kp, double kd) : 
            tvcBase(tvcBase), 
            force_tgt(force_tgt), 
            isp(isp), 
            kp(kp), 
            kd(kd) {}

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