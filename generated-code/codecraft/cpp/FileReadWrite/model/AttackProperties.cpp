#include "AttackProperties.hpp"

AttackProperties::AttackProperties(int attackRange, int damage, bool collectResource) : attackRange(attackRange), damage(damage), collectResource(collectResource) { }

// Read AttackProperties from input stream
AttackProperties AttackProperties::readFrom(InputStream& stream) {
    int attackRange = stream.readInt();
    int damage = stream.readInt();
    bool collectResource = stream.readBool();
    return AttackProperties(attackRange, damage, collectResource);
}

// Write AttackProperties to output stream
void AttackProperties::writeTo(OutputStream& stream) const {
    stream.write(attackRange);
    stream.write(damage);
    stream.write(collectResource);
}

// Get string representation of AttackProperties
std::string AttackProperties::toString() const {
    std::stringstream ss;
    ss << "AttackProperties { ";
    ss << "attackRange: ";
    ss << attackRange;
    ss << ", ";
    ss << "damage: ";
    ss << damage;
    ss << ", ";
    ss << "collectResource: ";
    ss << collectResource;
    ss << " }";
    return ss.str();
}

bool AttackProperties::operator ==(const AttackProperties& other) const {
    return attackRange == other.attackRange && damage == other.damage && collectResource == other.collectResource;
}

size_t std::hash<AttackProperties>::operator ()(const AttackProperties& value) const {
    size_t result = 0;
    result ^= std::hash<int>{}(value.attackRange) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<int>{}(value.damage) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<bool>{}(value.collectResource) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}