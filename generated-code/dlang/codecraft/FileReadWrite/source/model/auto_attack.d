module model.auto_attack;

import stream;
import std.conv;
import std.typecons : Nullable;
import model.entity_type;

/// Auto attack options
struct AutoAttack {
    /// Maximum distance to pathfind
    int pathfindRange;
    /// List of target entity types to try to attack. If empty, all types but resource are considered
    model.EntityType[] validTargets;

    this(int pathfindRange, model.EntityType[] validTargets) {
        this.pathfindRange = pathfindRange;
        this.validTargets = validTargets;
    }

    /// Read AutoAttack from reader
    static AutoAttack readFrom(Stream reader) {
        int pathfindRange;
        pathfindRange = reader.readInt();
        model.EntityType[] validTargets;
        validTargets = new model.EntityType[reader.readInt()];
        for (int validTargetsIndex = 0; validTargetsIndex < validTargets.length; validTargetsIndex++) {
            model.EntityType validTargetsKey;
            validTargetsKey = readEntityType(reader);
            validTargets[validTargetsIndex] = validTargetsKey;
        }
        return AutoAttack(pathfindRange, validTargets);
    }

    /// Write AutoAttack to writer
    void writeTo(Stream writer) const {
        writer.write(pathfindRange);
        writer.write(cast(int)(validTargets.length));
        foreach (validTargetsElement; validTargets) {
            writer.write(cast(int)(validTargetsElement));
        }
    }
}