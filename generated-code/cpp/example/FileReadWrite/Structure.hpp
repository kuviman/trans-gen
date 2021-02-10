#ifndef __MODEL_STRUCTURE_HPP__
#define __MODEL_STRUCTURE_HPP__

#include "Stream.hpp"
#include <sstream>
#include <string>

// Example structure
class Structure {
public:
    // Text
    std::string text;
    // 32-bit float
    float floatNumber;
    // 64-bit float
    double doubleNumber;

    Structure(std::string text, float floatNumber, double doubleNumber);

    // Read Structure from input stream
    static Structure readFrom(InputStream& stream);

    // Write Structure to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Structure
    std::string toString() const;
};

#endif