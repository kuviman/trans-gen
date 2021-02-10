#ifndef __MODEL_ACTION_HPP__
#define __MODEL_ACTION_HPP__

#include "Stream.hpp"
#include "Vec2Int.hpp"
#include "model/AttackAction.hpp"
#include "model/AutoAttack.hpp"
#include "model/BuildAction.hpp"
#include "model/EntityAction.hpp"
#include "model/EntityType.hpp"
#include "model/MoveAction.hpp"
#include "model/RepairAction.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace model {

// Player's action
class Action {
public:
    // New actions for entities. If entity does not get new action, if will continue to perform previously set one
    std::unordered_map<int, model::EntityAction> entityActions;

    Action(std::unordered_map<int, model::EntityAction> entityActions);

    // Read Action from input stream
    static Action readFrom(InputStream& stream);

    // Write Action to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Action
    std::string toString() const;
};

}

#endif