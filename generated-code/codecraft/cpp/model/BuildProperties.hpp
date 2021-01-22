#ifndef __MODEL_BUILD_PROPERTIES_HPP__
#define __MODEL_BUILD_PROPERTIES_HPP__

#include "../Stream.hpp"
#include "EntityType.hpp"
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

class BuildProperties {
public:
    std::vector<EntityType> options;
    std::shared_ptr<int> initHealth;

    BuildProperties();

    BuildProperties(std::vector<EntityType> options, std::shared_ptr<int> initHealth);

    static BuildProperties readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;

    std::string toString() const;
};

#endif