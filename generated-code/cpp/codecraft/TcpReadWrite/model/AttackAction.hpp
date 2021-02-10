#ifndef __MODEL_ATTACK_ACTION_HPP__
#define __MODEL_ATTACK_ACTION_HPP__

#include "Stream.hpp"
#include "model/AutoAttack.hpp"
#include "model/EntityType.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace model {

// Attack action
class AttackAction {
public:
    // If specified, target entity's ID
    std::optional<int> target;
    // If specified, configures auto attacking
    std::optional<model::AutoAttack> autoAttack;

    AttackAction(std::optional<int> target, std::optional<model::AutoAttack> autoAttack);

    // Read AttackAction from input stream
    static AttackAction readFrom(InputStream& stream);

    // Write AttackAction to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of AttackAction
    std::string toString() const;
};

}

#endif