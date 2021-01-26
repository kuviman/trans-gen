#include "BuildProperties.hpp"

BuildProperties::BuildProperties() { }

BuildProperties::BuildProperties(std::vector<EntityType> options, std::shared_ptr<int> initHealth) : options(options), initHealth(initHealth) { }

// Read BuildProperties from input stream
BuildProperties BuildProperties::readFrom(InputStream& stream) {
    std::vector<EntityType> options;
    options = std::vector<EntityType>(stream.readInt());
    for (size_t optionsIndex = 0; optionsIndex < options.size(); optionsIndex++) {
        options[optionsIndex] = readEntityType(stream);
    }
    std::shared_ptr<int> initHealth;
    if (stream.readBool()) {
        initHealth = std::shared_ptr<int>(new int());
        *initHealth = stream.readInt();
    } else {
        initHealth = std::shared_ptr<int>();
    }
    return BuildProperties(options, initHealth);
}

// Write BuildProperties to output stream
void BuildProperties::writeTo(OutputStream& stream) const {
    stream.write((int)(options.size()));
    for (const EntityType& optionsElement : options) {
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
        const EntityType& optionsElement = options[optionsIndex];
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