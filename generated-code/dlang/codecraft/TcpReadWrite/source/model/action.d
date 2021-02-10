module model.action;

import stream;
import std.conv;
import std.typecons : Nullable;
import model.entity_action;

/// Player's action
struct Action {
    /// New actions for entities. If entity does not get new action, if will continue to perform previously set one
    model.EntityAction[int] entityActions;

    this(model.EntityAction[int] entityActions) {
        this.entityActions = entityActions;
    }

    /// Read Action from reader
    static Action readFrom(Stream reader) {
        model.EntityAction[int] entityActions;
        int entityActionsSize = reader.readInt();
        entityActions.clear();
        for (int entityActionsIndex = 0; entityActionsIndex < entityActionsSize; entityActionsIndex++) {
            int entityActionsKey;
            model.EntityAction entityActionsValue;
            entityActionsKey = reader.readInt();
            entityActionsValue = model.EntityAction.readFrom(reader);
            entityActions[entityActionsKey] = entityActionsValue;
        }
        return Action(entityActions);
    }

    /// Write Action to writer
    void writeTo(Stream writer) const {
        writer.write(cast(int)(entityActions.length));
        foreach (entityActionsKey, entityActionsValue; entityActions) {
            writer.write(entityActionsKey);
            entityActionsValue.writeTo(writer);
        }
    }
}