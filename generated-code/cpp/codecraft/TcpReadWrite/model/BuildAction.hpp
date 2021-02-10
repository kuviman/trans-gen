#ifndef __MODEL_BUILD_ACTION_HPP__
#define __MODEL_BUILD_ACTION_HPP__

#include "Stream.hpp"
#include "Vec2Int.hpp"
#include "model/EntityType.hpp"
#include <sstream>
#include <stdexcept>
#include <string>

namespace model {

// Build action
class BuildAction {
public:
    // Type of an entity to build
    model::EntityType entityType;
    // Desired position of new entity
    Vec2Int position;

    BuildAction(model::EntityType entityType, Vec2Int position);

    // Read BuildAction from input stream
    static BuildAction readFrom(InputStream& stream);

    // Write BuildAction to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of BuildAction
    std::string toString() const;

    bool operator ==(const BuildAction& other) const;
};

}

namespace std {
    template<>
    struct hash<model::BuildAction> {
        size_t operator ()(const model::BuildAction& value) const;
    };
}

#endif