#include "BuildProperties.hpp"

BuildProperties::BuildProperties() { }

BuildProperties::BuildProperties(std::vector<EntityType> options, std::shared_ptr<int> initHealth) : options(options), initHealth(initHealth) { }

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