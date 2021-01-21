#include "RepairProperties.hpp"

RepairProperties::RepairProperties() { }

RepairProperties::RepairProperties(std::vector<EntityType> validTargets, int power) : validTargets(validTargets), power(power) { }

RepairProperties RepairProperties::readFrom(InputStream& stream) {
    std::vector<EntityType> validTargets;
    validTargets = std::vector<EntityType>(stream.readInt());
    for (size_t validTargetsIndex = 0; validTargetsIndex < validTargets.size(); validTargetsIndex++) {
        switch (stream.readInt()) {
        case 0:
            validTargets[validTargetsIndex] = EntityType::WALL;
            break;
        case 1:
            validTargets[validTargetsIndex] = EntityType::HOUSE;
            break;
        case 2:
            validTargets[validTargetsIndex] = EntityType::BUILDER_BASE;
            break;
        case 3:
            validTargets[validTargetsIndex] = EntityType::BUILDER_UNIT;
            break;
        case 4:
            validTargets[validTargetsIndex] = EntityType::MELEE_BASE;
            break;
        case 5:
            validTargets[validTargetsIndex] = EntityType::MELEE_UNIT;
            break;
        case 6:
            validTargets[validTargetsIndex] = EntityType::RANGED_BASE;
            break;
        case 7:
            validTargets[validTargetsIndex] = EntityType::RANGED_UNIT;
            break;
        case 8:
            validTargets[validTargetsIndex] = EntityType::RESOURCE;
            break;
        case 9:
            validTargets[validTargetsIndex] = EntityType::TURRET;
            break;
        default:
            throw std::runtime_error("Unexpected tag value");
        }
    }
    int power;
    power = stream.readInt();
    return RepairProperties(validTargets, power);
}

void RepairProperties::writeTo(OutputStream& stream) const {
    stream.write((int)(validTargets.size()));
    for (const EntityType& validTargetsElement : validTargets) {
        stream.write((int)(validTargetsElement));
    }
    stream.write(power);
}