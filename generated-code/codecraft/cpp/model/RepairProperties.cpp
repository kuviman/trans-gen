#include "RepairProperties.hpp"

RepairProperties::RepairProperties() { }

RepairProperties::RepairProperties(std::vector<EntityType> validTargets, int power) : validTargets(validTargets), power(power) { }

// Read RepairProperties from input stream
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

// Write RepairProperties to output stream
void RepairProperties::writeTo(OutputStream& stream) const {
    stream.write((int)(validTargets.size()));
    for (const EntityType& validTargetsElement : validTargets) {
        stream.write((int)(validTargetsElement));
    }
    stream.write(power);
}

// Get string representation of RepairProperties
std::string RepairProperties::toString() const {
    std::stringstream ss;
    ss << "RepairProperties { ";
    ss << "validTargets: ";
    ss << "[ ";
    for (size_t validTargetsIndex = 0; validTargetsIndex < validTargets.size(); validTargetsIndex++) {
        const EntityType& validTargetsElement = validTargets[validTargetsIndex];
        if (validTargetsIndex != 0) {
            ss << ", ";
        }
        ss << entityTypeToString(validTargetsElement);
    }
    ss << " ]";
    ss << ", ";
    ss << "power: ";
    ss << power;
    ss << " }";
    return ss.str();
}