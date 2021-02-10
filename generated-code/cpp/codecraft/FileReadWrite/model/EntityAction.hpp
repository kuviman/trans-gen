#ifndef __MODEL_ENTITY_ACTION_HPP__
#define __MODEL_ENTITY_ACTION_HPP__

#include "Stream.hpp"
#include "Vec2Int.hpp"
#include "model/AttackAction.hpp"
#include "model/AutoAttack.hpp"
#include "model/BuildAction.hpp"
#include "model/EntityType.hpp"
#include "model/MoveAction.hpp"
#include "model/RepairAction.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace model {

// Entity's action
class EntityAction {
public:
    // Move action
    std::optional<model::MoveAction> moveAction;
    // Build action
    std::optional<model::BuildAction> buildAction;
    // Attack action
    std::optional<model::AttackAction> attackAction;
    // Repair action
    std::optional<model::RepairAction> repairAction;

    EntityAction(std::optional<model::MoveAction> moveAction, std::optional<model::BuildAction> buildAction, std::optional<model::AttackAction> attackAction, std::optional<model::RepairAction> repairAction);

    // Read EntityAction from input stream
    static EntityAction readFrom(InputStream& stream);

    // Write EntityAction to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of EntityAction
    std::string toString() const;
};

}

#endif