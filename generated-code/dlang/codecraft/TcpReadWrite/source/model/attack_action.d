module model.attack_action;

import stream;
import std.conv;
import std.typecons : Nullable;
import model.auto_attack;

/// Attack action
struct AttackAction {
    /// If specified, target entity's ID
    Nullable!(int) target;
    /// If specified, configures auto attacking
    Nullable!(model.AutoAttack) autoAttack;

    this(Nullable!(int) target, Nullable!(model.AutoAttack) autoAttack) {
        this.target = target;
        this.autoAttack = autoAttack;
    }

    /// Read AttackAction from reader
    static AttackAction readFrom(Stream reader) {
        Nullable!(int) target;
        if (reader.readBool()) {
            target = reader.readInt();
        } else {
            target.nullify();
        }
        Nullable!(model.AutoAttack) autoAttack;
        if (reader.readBool()) {
            autoAttack = model.AutoAttack.readFrom(reader);
        } else {
            autoAttack.nullify();
        }
        return AttackAction(target, autoAttack);
    }

    /// Write AttackAction to writer
    void writeTo(Stream writer) const {
        if (target.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(target.get);
        }
        if (autoAttack.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            autoAttack.get.writeTo(writer);
        }
    }
}