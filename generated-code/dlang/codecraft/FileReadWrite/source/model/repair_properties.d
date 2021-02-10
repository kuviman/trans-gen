module model.repair_properties;

import stream;
import std.conv;
import std.typecons : Nullable;
import model.entity_type;

/// Entity's repair properties
struct RepairProperties {
    /// Valid target entity types
    model.EntityType[] validTargets;
    /// Health restored in one tick
    int power;

    this(model.EntityType[] validTargets, int power) {
        this.validTargets = validTargets;
        this.power = power;
    }

    /// Read RepairProperties from reader
    static RepairProperties readFrom(Stream reader) {
        model.EntityType[] validTargets;
        validTargets = new model.EntityType[reader.readInt()];
        for (int validTargetsIndex = 0; validTargetsIndex < validTargets.length; validTargetsIndex++) {
            model.EntityType validTargetsKey;
            validTargetsKey = readEntityType(reader);
            validTargets[validTargetsIndex] = validTargetsKey;
        }
        int power;
        power = reader.readInt();
        return RepairProperties(validTargets, power);
    }

    /// Write RepairProperties to writer
    void writeTo(Stream writer) const {
        writer.write(cast(int)(validTargets.length));
        foreach (validTargetsElement; validTargets) {
            writer.write(cast(int)(validTargetsElement));
        }
        writer.write(power);
    }
}