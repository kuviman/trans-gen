#include "EntityType.hpp"
#include <stdexcept>

// Read EntityType from input stream
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

// Get string representation of EntityType
std::string entityTypeToString(EntityType value) {
    switch (value) {
    case EntityType::WALL:
        return "WALL";
    case EntityType::HOUSE:
        return "HOUSE";
    case EntityType::BUILDER_BASE:
        return "BUILDER_BASE";
    case EntityType::BUILDER_UNIT:
        return "BUILDER_UNIT";
    case EntityType::MELEE_BASE:
        return "MELEE_BASE";
    case EntityType::MELEE_UNIT:
        return "MELEE_UNIT";
    case EntityType::RANGED_BASE:
        return "RANGED_BASE";
    case EntityType::RANGED_UNIT:
        return "RANGED_UNIT";
    case EntityType::RESOURCE:
        return "RESOURCE";
    case EntityType::TURRET:
        return "TURRET";
    default:
        throw std::runtime_error("Impossible happened");
    }
}