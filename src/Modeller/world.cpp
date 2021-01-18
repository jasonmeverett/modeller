#include <Modeller/world.hpp>


/* Here lies the world! */
static Modeller::Core::World * WORLD = new Modeller::Core::World();

Modeller::Core::World * Modeller::Core::GetWorld() { return WORLD; } 
void Modeller::Core::SetWorld(Modeller::Core::World * w){ WORLD = w; }

Modeller::Core::World::World()
: m_system(), m_matter(m_system), m_forces(m_system)
{
}
