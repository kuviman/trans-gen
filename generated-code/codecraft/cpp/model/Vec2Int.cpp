#include "Vec2Int.hpp"

Vec2Int::Vec2Int() { }

Vec2Int::Vec2Int(int x, int y) : x(x), y(y) { }

Vec2Int Vec2Int::readFrom(InputStream& stream) {
    int x;
    x = stream.readInt();
    int y;
    y = stream.readInt();
    return Vec2Int(x, y);
}

void Vec2Int::writeTo(OutputStream& stream) const {
    stream.write(x);
    stream.write(y);
}

std::string Vec2Int::toString() const {
    std::stringstream ss;
    ss << "Vec2Int { ";
    ss << "x: ";
    ss << x;
    ss << ", ";
    ss << "y: ";
    ss << y;
    ss << " }";
    return ss.str();
}

bool Vec2Int::operator ==(const Vec2Int& other) const {
    return x == other.x && y == other.y;
}

size_t std::hash<Vec2Int>::operator ()(const Vec2Int& value) const {
    size_t result = 0;
    result ^= std::hash<int>{}(value.x) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<int>{}(value.y) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}