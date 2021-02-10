#include "BuildAction.hpp"

namespace model {

BuildAction::BuildAction(model::EntityType entityType, Vec2Int position) : entityType(entityType), position(position) { }

// Read BuildAction from input stream
BuildAction BuildAction::readFrom(InputStream& stream) {
    model::EntityType entityType = readEntityType(stream);
    Vec2Int position = Vec2Int::readFrom(stream);
    return BuildAction(entityType, position);
}

// Write BuildAction to output stream
void BuildAction::writeTo(OutputStream& stream) const {
    stream.write((int)(entityType));
    position.writeTo(stream);
}

// Get string representation of BuildAction
std::string BuildAction::toString() const {
    std::stringstream ss;
    ss << "BuildAction { ";
    ss << "entityType: ";
    ss << entityTypeToString(entityType);
    ss << ", ";
    ss << "position: ";
    ss << position.toString();
    ss << " }";
    return ss.str();
}

bool BuildAction::operator ==(const BuildAction& other) const {
    return entityType == other.entityType && position == other.position;
}

}

size_t std::hash<model::BuildAction>::operator ()(const model::BuildAction& value) const {
    size_t result = 0;
    result ^= std::hash<model::EntityType>{}(value.entityType) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<Vec2Int>{}(value.position) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}