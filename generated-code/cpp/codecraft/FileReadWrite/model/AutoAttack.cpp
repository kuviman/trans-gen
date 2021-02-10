#include "AutoAttack.hpp"

namespace model {

AutoAttack::AutoAttack(int pathfindRange, std::vector<model::EntityType> validTargets) : pathfindRange(pathfindRange), validTargets(validTargets) { }

// Read AutoAttack from input stream
AutoAttack AutoAttack::readFrom(InputStream& stream) {
    int pathfindRange = stream.readInt();
    std::vector<model::EntityType> validTargets = std::vector<model::EntityType>();
    size_t validTargetsSize = stream.readInt();
    validTargets.reserve(validTargetsSize);
    for (size_t validTargetsIndex = 0; validTargetsIndex < validTargetsSize; validTargetsIndex++) {
        model::EntityType validTargetsElement = readEntityType(stream);
        validTargets.emplace_back(validTargetsElement);
    }
    return AutoAttack(pathfindRange, validTargets);
}

// Write AutoAttack to output stream
void AutoAttack::writeTo(OutputStream& stream) const {
    stream.write(pathfindRange);
    stream.write((int)(validTargets.size()));
    for (const model::EntityType& validTargetsElement : validTargets) {
        stream.write((int)(validTargetsElement));
    }
}

// Get string representation of AutoAttack
std::string AutoAttack::toString() const {
    std::stringstream ss;
    ss << "AutoAttack { ";
    ss << "pathfindRange: ";
    ss << pathfindRange;
    ss << ", ";
    ss << "validTargets: ";
    ss << "[ ";
    for (size_t validTargetsIndex = 0; validTargetsIndex < validTargets.size(); validTargetsIndex++) {
        const model::EntityType& validTargetsElement = validTargets[validTargetsIndex];
        if (validTargetsIndex != 0) {
            ss << ", ";
        }
        ss << entityTypeToString(validTargetsElement);
    }
    ss << " ]";
    ss << " }";
    return ss.str();
}

}