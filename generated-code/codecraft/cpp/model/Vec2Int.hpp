#ifndef __MODEL_VEC2_INT_HPP__
#define __MODEL_VEC2_INT_HPP__

#include "../Stream.hpp"
#include <sstream>
#include <string>

// 2 dimensional vector.
class Vec2Int {
public:
    // `x` coordinate of the vector
    int x;
    // `y` coordinate of the vector
    int y;

    Vec2Int();

    Vec2Int(int x, int y);

    // Read Vec2Int from input stream
    static Vec2Int readFrom(InputStream& stream);

    // Write Vec2Int to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Vec2Int
    std::string toString() const;

    bool operator ==(const Vec2Int& other) const;
};

namespace std {
    template<>
    struct hash<Vec2Int> {
        size_t operator ()(const Vec2Int& value) const;
    };
}

#endif