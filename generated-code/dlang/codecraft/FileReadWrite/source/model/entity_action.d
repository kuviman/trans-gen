module model.entity_action;

import stream;
import std.conv;
import std.typecons : Nullable;
import model.attack_action;
import model.build_action;
import model.move_action;
import model.repair_action;

/// Entity's action
struct EntityAction {
    /// Move action
    Nullable!(model.MoveAction) moveAction;
    /// Build action
    Nullable!(model.BuildAction) buildAction;
    /// Attack action
    Nullable!(model.AttackAction) attackAction;
    /// Repair action
    Nullable!(model.RepairAction) repairAction;

    this(Nullable!(model.MoveAction) moveAction, Nullable!(model.BuildAction) buildAction, Nullable!(model.AttackAction) attackAction, Nullable!(model.RepairAction) repairAction) {
        this.moveAction = moveAction;
        this.buildAction = buildAction;
        this.attackAction = attackAction;
        this.repairAction = repairAction;
    }

    /// Read EntityAction from reader
    static EntityAction readFrom(Stream reader) {
        Nullable!(model.MoveAction) moveAction;
        if (reader.readBool()) {
            moveAction = model.MoveAction.readFrom(reader);
        } else {
            moveAction.nullify();
        }
        Nullable!(model.BuildAction) buildAction;
        if (reader.readBool()) {
            buildAction = model.BuildAction.readFrom(reader);
        } else {
            buildAction.nullify();
        }
        Nullable!(model.AttackAction) attackAction;
        if (reader.readBool()) {
            attackAction = model.AttackAction.readFrom(reader);
        } else {
            attackAction.nullify();
        }
        Nullable!(model.RepairAction) repairAction;
        if (reader.readBool()) {
            repairAction = model.RepairAction.readFrom(reader);
        } else {
            repairAction.nullify();
        }
        return EntityAction(moveAction, buildAction, attackAction, repairAction);
    }

    /// Write EntityAction to writer
    void writeTo(Stream writer) const {
        if (moveAction.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            moveAction.get.writeTo(writer);
        }
        if (buildAction.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            buildAction.get.writeTo(writer);
        }
        if (attackAction.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            attackAction.get.writeTo(writer);
        }
        if (repairAction.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            repairAction.get.writeTo(writer);
        }
    }
}