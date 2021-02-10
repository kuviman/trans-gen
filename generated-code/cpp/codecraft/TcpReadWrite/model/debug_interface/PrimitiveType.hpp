#ifndef __MODEL_PRIMITIVE_TYPE_HPP__
#define __MODEL_PRIMITIVE_TYPE_HPP__

#include "Stream.hpp"

namespace model::debug_interface {

// Primitive type for debug rendering
enum class PrimitiveType {
    // Lines, number of vertices should be divisible by 2
    LINES = 0,
    // Triangles, number of vertices should be divisible by 3
    TRIANGLES = 1
};

// Read PrimitiveType from input stream
PrimitiveType readPrimitiveType(InputStream& stream);

// Get string representation of PrimitiveType
std::string primitiveTypeToString(PrimitiveType value);

}

#endif