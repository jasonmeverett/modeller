#include <Modeller/world.hpp>


/* Here lies the world! */
static Modeller::Core::World * WORLD = new Modeller::Core::World();

Modeller::Core::World * Modeller::Core::GetWorld() { return WORLD; } 

Modeller::Core::World::World()
: m_system(), m_matter(m_system), m_forces(m_system)
{
}