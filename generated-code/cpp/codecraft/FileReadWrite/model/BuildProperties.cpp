#include "BuildProperties.hpp"

namespace model {

BuildProperties::BuildProperties(std::vector<model::EntityType> options, std::optional<int> initHealth) : options(options), initHealth(initHealth) { }

// Read BuildProperties from input stream
BuildProperties BuildProperties::readFrom(InputStream& stream) {
    std::vector<model::EntityType> options = std::vector<model::EntityType>();
    size_t optionsSize = stream.readInt();
    options.reserve(optionsSize);
    for (size_t optionsIndex = 0; optionsIndex < optionsSize; optionsIndex++) {
        model::EntityType optionsElement = readEntityType(stream);
        options.emplace_back(optionsElement);
    }
    std::optional<int> initHealth = std::optional<int>();
    if (stream.readBool()) {
        initHealth = stream.readInt();
    }
    return BuildProperties(options, initHealth);
}

// Write BuildProperties to output stream
void BuildProperties::writeTo(OutputStream& stream) const {
    stream.write((int)(options.size()));
    for (const model::EntityType& optionsElement : options) {
        stream.write((int)(optionsElement));
    }
    if (initHealth) {
        stream.write(true);
        const int& initHealthValue = *initHealth;
        stream.write(initHealthValue);
    } else {
        stream.write(false);
    }
}

// Get string representation of BuildProperties
std::string BuildProperties::toString() const {
    std::stringstream ss;
    ss << "BuildProperties { ";
    ss << "options: ";
    ss << "[ ";
    for (size_t optionsIndex = 0; optionsIndex < options.size(); optionsIndex++) {
        const model::EntityType& optionsElement = options[optionsIndex];
        if (optionsIndex != 0) {
            ss << ", ";
        }
        ss << entityTypeToString(optionsElement);
    }
    ss << " ]";
    ss << ", ";
    ss << "initHealth: ";
    if (initHealth) {
        const int& initHealthValue = *initHealth;
        ss << initHealthValue;
    } else {
        ss << "none";
    }
    ss << " }";
    return ss.str();
}

}