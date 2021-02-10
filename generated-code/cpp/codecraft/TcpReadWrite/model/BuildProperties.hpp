#ifndef __MODEL_BUILD_PROPERTIES_HPP__
#define __MODEL_BUILD_PROPERTIES_HPP__

#include "Stream.hpp"
#include "model/EntityType.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace model {

// Entity's build properties
class BuildProperties {
public:
    // Valid new entity types
    std::vector<model::EntityType> options;
    // Initial health of new entity. If absent, it will have full health
    std::optional<int> initHealth;

    BuildProperties(std::vector<model::EntityType> options, std::optional<int> initHealth);

    // Read BuildProperties from input stream
    static BuildProperties readFrom(InputStream& stream);

    // Write BuildProperties to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of BuildProperties
    std::string toString() const;
};

}

#endif