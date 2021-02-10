#ifndef __MODEL_ATTACK_PROPERTIES_HPP__
#define __MODEL_ATTACK_PROPERTIES_HPP__

#include "Stream.hpp"
#include <sstream>
#include <string>

namespace model {

// Entity's attack properties
class AttackProperties {
public:
    // Maximum attack range
    int attackRange;
    // Damage dealt in one tick
    int damage;
    // If true, dealing damage will collect resource from target
    bool collectResource;

    AttackProperties(int attackRange, int damage, bool collectResource);

    // Read AttackProperties from input stream
    static AttackProperties readFrom(InputStream& stream);

    // Write AttackProperties to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of AttackProperties
    std::string toString() const;

    bool operator ==(const AttackProperties& other) const;
};

}

namespace std {
    template<>
    struct hash<model::AttackProperties> {
        size_t operator ()(const model::AttackProperties& value) const;
    };
}

#endif