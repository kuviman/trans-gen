#include "MoveAction.hpp"

namespace model {

MoveAction::MoveAction(Vec2Int target, bool findClosestPosition, bool breakThrough) : target(target), findClosestPosition(findClosestPosition), breakThrough(breakThrough) { }

// Read MoveAction from input stream
MoveAction MoveAction::readFrom(InputStream& stream) {
    Vec2Int target = Vec2Int::readFrom(stream);
    bool findClosestPosition = stream.readBool();
    bool breakThrough = stream.readBool();
    return MoveAction(target, findClosestPosition, breakThrough);
}

// Write MoveAction to output stream
void MoveAction::writeTo(OutputStream& stream) const {
    target.writeTo(stream);
    stream.write(findClosestPosition);
    stream.write(breakThrough);
}

// Get string representation of MoveAction
std::string MoveAction::toString() const {
    std::stringstream ss;
    ss << "MoveAction { ";
    ss << "target: ";
    ss << target.toString();
    ss << ", ";
    ss << "findClosestPosition: ";
    ss << findClosestPosition;
    ss << ", ";
    ss << "breakThrough: ";
    ss << breakThrough;
    ss << " }";
    return ss.str();
}

bool MoveAction::operator ==(const MoveAction& other) const {
    return target == other.target && findClosestPosition == other.findClosestPosition && breakThrough == other.breakThrough;
}

}

size_t std::hash<model::MoveAction>::operator ()(const model::MoveAction& value) const {
    size_t result = 0;
    result ^= std::hash<Vec2Int>{}(value.target) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<bool>{}(value.findClosestPosition) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<bool>{}(value.breakThrough) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}