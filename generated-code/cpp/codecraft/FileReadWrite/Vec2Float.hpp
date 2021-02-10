#ifndef __MODEL_VEC2_FLOAT_HPP__
#define __MODEL_VEC2_FLOAT_HPP__

#include "Stream.hpp"
#include <sstream>
#include <string>

// 2 dimensional vector.
class Vec2Float {
public:
    // `x` coordinate of the vector
    float x;
    // `y` coordinate of the vector
    float y;

    Vec2Float(float x, float y);

    // Read Vec2Float from input stream
    static Vec2Float readFrom(InputStream& stream);

    // Write Vec2Float to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Vec2Float
    std::string toString() const;
};

#endif