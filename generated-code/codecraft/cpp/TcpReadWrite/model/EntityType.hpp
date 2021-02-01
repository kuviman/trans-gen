#ifndef __MODEL_ENTITY_TYPE_HPP__
#define __MODEL_ENTITY_TYPE_HPP__

#include "../Stream.hpp"

// Entity type
enum class EntityType {
    // Wall, can be used to prevent enemy from moving through
    WALL = 0,
    // House, used to increase population
    HOUSE = 1,
    // Base for recruiting new builder units
    BUILDER_BASE = 2,
    // Builder unit can build buildings
    BUILDER_UNIT = 3,
    // Base for recruiting new melee units
    MELEE_BASE = 4,
    // Melee unit
    MELEE_UNIT = 5,
    // Base for recruiting new ranged units
    RANGED_BASE = 6,
    // Ranged unit
    RANGED_UNIT = 7,
    // Resource can be harvested
    RESOURCE = 8,
    // Ranged attacking building
    TURRET = 9
};

// Read EntityType from input stream
EntityType readEntityType(InputStream& stream);

// Get string representation of EntityType
std::string entityTypeToString(EntityType value);

#endif