#include "EntityType.hpp"
#include <stdexcept>

EntityType readEntityType(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return EntityType::WALL;
    case 1:
        return EntityType::HOUSE;
    case 2:
        return EntityType::BUILDER_BASE;
    case 3:
        return EntityType::BUILDER_UNIT;
    case 4:
        return EntityType::MELEE_BASE;
    case 5:
        return EntityType::MELEE_UNIT;
    case 6:
        return EntityType::RANGED_BASE;
    case 7:
        return EntityType::RANGED_UNIT;
    case 8:
        return EntityType::RESOURCE;
    case 9:
        return EntityType::TURRET;
    default:
        throw std::runtime_error("Unexpected tag value");
    }
}