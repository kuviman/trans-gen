#include "BuildProperties.hpp"

BuildProperties::BuildProperties() { }

BuildProperties::BuildProperties(std::vector<EntityType> options, std::shared_ptr<int> initHealth) : options(options), initHealth(initHealth) { }

BuildProperties BuildProperties::readFrom(InputStream& stream) {
    std::vector<EntityType> options;
    options = std::vector<EntityType>(stream.readInt());
    for (size_t optionsIndex = 0; optionsIndex < options.size(); optionsIndex++) {
        switch (stream.readInt()) {
        case 0:
            options[optionsIndex] = EntityType::WALL;
            break;
        case 1:
            options[optionsIndex] = EntityType::HOUSE;
            break;
        case 2:
            options[optionsIndex] = EntityType::BUILDER_BASE;
            break;
        case 3:
            options[optionsIndex] = EntityType::BUILDER_UNIT;
            break;
        case 4:
            options[optionsIndex] = EntityType::MELEE_BASE;
            break;
        case 5:
            options[optionsIndex] = EntityType::MELEE_UNIT;
            break;
        case 6:
            options[optionsIndex] = EntityType::RANGED_BASE;
            break;
        case 7:
            options[optionsIndex] = EntityType::RANGED_UNIT;
            break;
        case 8:
            options[optionsIndex] = EntityType::RESOURCE;
            break;
        case 9:
            options[optionsIndex] = EntityType::TURRET;
            break;
        default:
            throw std::runtime_error("Unexpected tag value");
        }
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