#ifndef __MODEL_MOVE_ACTION_HPP__
#define __MODEL_MOVE_ACTION_HPP__

#include "Stream.hpp"
#include "Vec2Int.hpp"
#include <sstream>
#include <stdexcept>
#include <string>

namespace model {

// Move action
class MoveAction {
public:
    // Target position
    Vec2Int target;
    // Whether to try find closest position, if path to target is not found
    bool findClosestPosition;
    // Whether to destroy other entities on the way
    bool breakThrough;

    MoveAction(Vec2Int target, bool findClosestPosition, bool breakThrough);

    // Read MoveAction from input stream
    static MoveAction readFrom(InputStream& stream);

    // Write MoveAction to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of MoveAction
    std::string toString() const;

    bool operator ==(const MoveAction& other) const;
};

}

namespace std {
    template<>
    struct hash<model::MoveAction> {
        size_t operator ()(const model::MoveAction& value) const;
    };
}

#endif