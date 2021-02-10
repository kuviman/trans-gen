#ifndef __MODEL_AUTO_ATTACK_HPP__
#define __MODEL_AUTO_ATTACK_HPP__

#include "Stream.hpp"
#include "model/EntityType.hpp"
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace model {

// Auto attack options
class AutoAttack {
public:
    // Maximum distance to pathfind
    int pathfindRange;
    // List of target entity types to try to attack. If empty, all types but resource are considered
    std::vector<model::EntityType> validTargets;

    AutoAttack(int pathfindRange, std::vector<model::EntityType> validTargets);

    // Read AutoAttack from input stream
    static AutoAttack readFrom(InputStream& stream);

    // Write AutoAttack to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of AutoAttack
    std::string toString() const;
};

}

#endif