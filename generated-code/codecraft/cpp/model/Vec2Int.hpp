#ifndef __MODEL_VEC2_INT_HPP__
#define __MODEL_VEC2_INT_HPP__

#include "../Stream.hpp"
#include <sstream>
#include <string>

class Vec2Int {
public:
    int x;
    int y;

    Vec2Int();

    Vec2Int(int x, int y);

    static Vec2Int readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;

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