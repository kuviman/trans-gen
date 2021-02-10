#include "PrimitiveType.hpp"
#include <stdexcept>

namespace model::debug_interface {

// Read PrimitiveType from input stream
PrimitiveType readPrimitiveType(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return PrimitiveType::LINES;
    case 1:
        return PrimitiveType::TRIANGLES;
    default:
        throw std::runtime_error("Unexpected tag value");
    }
}

// Get string representation of PrimitiveType
std::string primitiveTypeToString(PrimitiveType value) {
    switch (value) {
    case PrimitiveType::LINES:
        return "LINES";
    case PrimitiveType::TRIANGLES:
        return "TRIANGLES";
    default:
        throw std::runtime_error("Impossible happened");
    }
}

}