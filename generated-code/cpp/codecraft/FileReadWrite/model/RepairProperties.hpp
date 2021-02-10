#ifndef __MODEL_REPAIR_PROPERTIES_HPP__
#define __MODEL_REPAIR_PROPERTIES_HPP__

#include "Stream.hpp"
#include "model/EntityType.hpp"
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace model {

// Entity's repair properties
class RepairProperties {
public:
    // Valid target entity types
    std::vector<model::EntityType> validTargets;
    // Health restored in one tick
    int power;

    RepairProperties(std::vector<model::EntityType> validTargets, int power);

    // Read RepairProperties from input stream
    static RepairProperties readFrom(InputStream& stream);

    // Write RepairProperties to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of RepairProperties
    std::string toString() const;
};

}

#endif