#ifndef __MODEL_COLOR_HPP__
#define __MODEL_COLOR_HPP__

#include "Stream.hpp"
#include <sstream>
#include <string>

// RGBA Color
class Color {
public:
    // Red component
    float r;
    // Green component
    float g;
    // Blue component
    float b;
    // Alpha (opacity) component
    float a;

    Color(float r, float g, float b, float a);

    // Read Color from input stream
    static Color readFrom(InputStream& stream);

    // Write Color to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Color
    std::string toString() const;
};

#endif