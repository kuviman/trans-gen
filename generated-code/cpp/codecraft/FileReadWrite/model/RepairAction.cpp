#include "RepairAction.hpp"

namespace model {

RepairAction::RepairAction(int target) : target(target) { }

// Read RepairAction from input stream
RepairAction RepairAction::readFrom(InputStream& stream) {
    int target = stream.readInt();
    return RepairAction(target);
}

// Write RepairAction to output stream
void RepairAction::writeTo(OutputStream& stream) const {
    stream.write(target);
}

// Get string representation of RepairAction
std::string RepairAction::toString() const {
    std::stringstream ss;
    ss << "RepairAction { ";
    ss << "target: ";
    ss << target;
    ss << " }";
    return ss.str();
}

bool RepairAction::operator ==(const RepairAction& other) const {
    return target == other.target;
}

}

size_t std::hash<model::RepairAction>::operator ()(const model::RepairAction& value) const {
    size_t result = 0;
    result ^= std::hash<int>{}(value.target) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}