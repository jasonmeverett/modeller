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


/** 
 * Configure the world
 * Configure the simulation
 * Run the simulation
 */ 
DataSet Modeller::Examples::Run_Ex2(py::dict cfg)
{
    // Body::Rigid sphereBody (
    //     MassProperties(
    //         1.0,
    //         Vec3(0.0),
    //         Inertia(1.0)
    //     )
    // );

    // // Create mobod.
    // MobilizedBody::Slider sphere(
    //     GetWorld()->m_matter.Ground(),
    //     Transform(),
    //     sphereBody,
    //     Transform()
    // );

    // // Apply our new force
    // SinusoidalForce myForce(
    //     GetWorld()->m_forces,
    //     sphere,
    //     1.0,
    //     1.0,
    //     0.0,
    //     {1, 0, 0}
    // );

    // // Done with system changes. Realize topology.
    // State s = GetWorld()->m_system.realizeTopology();

    // // Configure the integrator.
    // RungeKuttaMersonIntegrator integ(GetWorld()->m_system);
    // integ.setAccuracy(1e-6);
    // TimeStepper ts(GetWorld()->m_system, integ);
    // ts.initialize(s);

    // // Run!
    // ts.stepTo(20.0);


    // Define the system.
    MultibodySystem system;
    SimbodyMatterSubsystem matter(system);
    GeneralForceSubsystem forces(system);
    Force::Gravity gravity(forces, matter, -YAxis, 9.8);

    // Describe mass and visualization properties for a generic body.
    Body::Rigid bodyInfo(MassProperties(1.0, Vec3(0), UnitInertia(1)));
    bodyInfo.addDecoration(Transform(), DecorativeSphere(0.1));

    // Create the moving (mobilized) bodies of the pendulum.
    MobilizedBody::Pin pendulum1(matter.Ground(), Transform(Vec3(0)),
            bodyInfo, Transform(Vec3(0, 1, 0)));
    MobilizedBody::Pin pendulum2(pendulum1, Transform(Vec3(0)),
            bodyInfo, Transform(Vec3(0, 1, 0)));

    // Set up visualization.
    system.setUseUniformBackground(true);
    Visualizer viz(system);
    viz.setMode(Visualizer::Mode::PassThrough);
    viz.setDesiredFrameRate(200);
    system.addEventReporter(new Visualizer::Reporter(viz, 0.1));

    // Initialize the system and state.
    State state = system.realizeTopology();
    pendulum2.setRate(state, 5.0);

    // Simulate for 20 seconds.
    RungeKuttaMersonIntegrator integ(system);
    TimeStepper ts(system, integ);
    ts.initialize(state);
    ts.stepTo(20.0);


    // Create Data Set
    DataSet * ds = new DataSet();
    return *ds;
}