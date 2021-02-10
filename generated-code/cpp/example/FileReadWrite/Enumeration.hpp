#ifndef __MODEL_ENUMERATION_HPP__
#define __MODEL_ENUMERATION_HPP__

#include "Stream.hpp"

// Example enumeration
enum class Enumeration {
    // First option
    VALUE_ONE = 0,
    // Second option
    VALUE_TWO = 1
};

// Read Enumeration from input stream
Enumeration readEnumeration(InputStream& stream);

// Get string representation of Enumeration
std::string enumerationToString(Enumeration value);

#endif