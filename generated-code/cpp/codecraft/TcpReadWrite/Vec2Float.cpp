#include "Vec2Float.hpp"

Vec2Float::Vec2Float(float x, float y) : x(x), y(y) { }

// Read Vec2Float from input stream
Vec2Float Vec2Float::readFrom(InputStream& stream) {
    float x = stream.readFloat();
    float y = stream.readFloat();
    return Vec2Float(x, y);
}

// Write Vec2Float to output stream
void Vec2Float::writeTo(OutputStream& stream) const {
    stream.write(x);
    stream.write(y);
}

// Get string representation of Vec2Float
std::string Vec2Float::toString() const {
    std::stringstream ss;
    ss << "Vec2Float { ";
    ss << "x: ";
    ss << x;
    ss << ", ";
    ss << "y: ";
    ss << y;
    ss << " }";
    return ss.str();
}