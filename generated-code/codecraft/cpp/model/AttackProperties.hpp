#ifndef __MODEL_ATTACK_PROPERTIES_HPP__
#define __MODEL_ATTACK_PROPERTIES_HPP__

#include "../Stream.hpp"
#include <string>

class AttackProperties {
public:
    int attackRange;
    int damage;
    bool collectResource;

    AttackProperties();

    AttackProperties(int attackRange, int damage, bool collectResource);

    static AttackProperties readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;

    bool operator ==(const AttackProperties& other) const;
};

namespace std {
    template<>
    struct hash<AttackProperties> {
        size_t operator ()(const AttackProperties& value) const;
    };
}

#endif