#ifndef __MODEL_REPAIR_ACTION_HPP__
#define __MODEL_REPAIR_ACTION_HPP__

#include "Stream.hpp"
#include <sstream>
#include <string>

namespace model {

// Repair action
class RepairAction {
public:
    // Target entity's ID
    int target;

    RepairAction(int target);

    // Read RepairAction from input stream
    static RepairAction readFrom(InputStream& stream);

    // Write RepairAction to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of RepairAction
    std::string toString() const;

    bool operator ==(const RepairAction& other) const;
};

}

namespace std {
    template<>
    struct hash<model::RepairAction> {
        size_t operator ()(const model::RepairAction& value) const;
    };
}

#endif