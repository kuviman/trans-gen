import model;
import stream;
import std.conv;
import std.typecons : Nullable;

/// Entity's repair properties
struct RepairProperties {
    /// Valid target entity types
    EntityType[] validTargets;
    /// Health restored in one tick
    int power;

    this(EntityType[] validTargets, int power) {
        this.validTargets = validTargets;
        this.power = power;
    }

    /// Read RepairProperties from reader
    static RepairProperties readFrom(Stream reader) {
        EntityType[] validTargets;
        validTargets = new EntityType[reader.readInt()];
        for (int validTargetsIndex = 0; validTargetsIndex < validTargets.length; validTargetsIndex++) {
            EntityType validTargetsKey;
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