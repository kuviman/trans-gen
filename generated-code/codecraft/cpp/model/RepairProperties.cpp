#include "RepairProperties.hpp"

RepairProperties::RepairProperties() { }

RepairProperties::RepairProperties(std::vector<EntityType> validTargets, int power) : validTargets(validTargets), power(power) { }

RepairProperties RepairProperties::readFrom(InputStream& stream) {
    std::vector<EntityType> validTargets;
    validTargets = std::vector<EntityType>(stream.readInt());
    for (size_t validTargetsIndex = 0; validTargetsIndex < validTargets.size(); validTargetsIndex++) {
        validTargets[validTargetsIndex] = readEntityType(stream);
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