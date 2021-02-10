#include "EntityAction.hpp"

namespace model {

EntityAction::EntityAction(std::optional<model::MoveAction> moveAction, std::optional<model::BuildAction> buildAction, std::optional<model::AttackAction> attackAction, std::optional<model::RepairAction> repairAction) : moveAction(moveAction), buildAction(buildAction), attackAction(attackAction), repairAction(repairAction) { }

// Read EntityAction from input stream
EntityAction EntityAction::readFrom(InputStream& stream) {
    std::optional<model::MoveAction> moveAction = std::optional<model::MoveAction>();
    if (stream.readBool()) {
        moveAction = model::MoveAction::readFrom(stream);
    }
    std::optional<model::BuildAction> buildAction = std::optional<model::BuildAction>();
    if (stream.readBool()) {
        buildAction = model::BuildAction::readFrom(stream);
    }
    std::optional<model::AttackAction> attackAction = std::optional<model::AttackAction>();
    if (stream.readBool()) {
        attackAction = model::AttackAction::readFrom(stream);
    }
    std::optional<model::RepairAction> repairAction = std::optional<model::RepairAction>();
    if (stream.readBool()) {
        repairAction = model::RepairAction::readFrom(stream);
    }
    return EntityAction(moveAction, buildAction, attackAction, repairAction);
}

// Write EntityAction to output stream
void EntityAction::writeTo(OutputStream& stream) const {
    if (moveAction) {
        stream.write(true);
        const model::MoveAction& moveActionValue = *moveAction;
        moveActionValue.writeTo(stream);
    } else {
        stream.write(false);
    }
    if (buildAction) {
        stream.write(true);
        const model::BuildAction& buildActionValue = *buildAction;
        buildActionValue.writeTo(stream);
    } else {
        stream.write(false);
    }
    if (attackAction) {
        stream.write(true);
        const model::AttackAction& attackActionValue = *attackAction;
        attackActionValue.writeTo(stream);
    } else {
        stream.write(false);
    }
    if (repairAction) {
        stream.write(true);
        const model::RepairAction& repairActionValue = *repairAction;
        repairActionValue.writeTo(stream);
    } else {
        stream.write(false);
    }
}

// Get string representation of EntityAction
std::string EntityAction::toString() const {
    std::stringstream ss;
    ss << "EntityAction { ";
    ss << "moveAction: ";
    if (moveAction) {
        const model::MoveAction& moveActionValue = *moveAction;
        ss << moveActionValue.toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "buildAction: ";
    if (buildAction) {
        const model::BuildAction& buildActionValue = *buildAction;
        ss << buildActionValue.toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "attackAction: ";
    if (attackAction) {
        const model::AttackAction& attackActionValue = *attackAction;
        ss << attackActionValue.toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "repairAction: ";
    if (repairAction) {
        const model::RepairAction& repairActionValue = *repairAction;
        ss << repairActionValue.toString();
    } else {
        ss << "none";
    }
    ss << " }";
    return ss.str();
}

}