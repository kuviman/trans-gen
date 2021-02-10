#include "Action.hpp"

namespace model {

Action::Action(std::unordered_map<int, model::EntityAction> entityActions) : entityActions(entityActions) { }

// Read Action from input stream
Action Action::readFrom(InputStream& stream) {
    size_t entityActionsSize = stream.readInt();
    std::unordered_map<int, model::EntityAction> entityActions = std::unordered_map<int, model::EntityAction>();
    entityActions.reserve(entityActionsSize);
    for (size_t entityActionsIndex = 0; entityActionsIndex < entityActionsSize; entityActionsIndex++) {
        int entityActionsKey = stream.readInt();
        model::EntityAction entityActionsValue = model::EntityAction::readFrom(stream);
        entityActions.emplace(std::make_pair(entityActionsKey, entityActionsValue));
    }
    return Action(entityActions);
}

// Write Action to output stream
void Action::writeTo(OutputStream& stream) const {
    stream.write((int)(entityActions.size()));
    for (const auto& entityActionsEntry : entityActions) {
        const int& entityActionsKey = entityActionsEntry.first;
        const model::EntityAction& entityActionsValue = entityActionsEntry.second;
        stream.write(entityActionsKey);
        entityActionsValue.writeTo(stream);
    }
}

// Get string representation of Action
std::string Action::toString() const {
    std::stringstream ss;
    ss << "Action { ";
    ss << "entityActions: ";
    ss << "{ ";
    size_t entityActionsIndex = 0;
    for (const auto& entityActionsEntry : entityActions) {
        if (entityActionsIndex != 0) {
            ss << ", ";
        }
        const int& entityActionsKey = entityActionsEntry.first;
        const model::EntityAction& entityActionsValue = entityActionsEntry.second;
        ss << entityActionsKey;
        ss << ": ";
        ss << entityActionsValue.toString();
        entityActionsIndex++;
    }
    ss << " }";
    ss << " }";
    return ss.str();
}

}