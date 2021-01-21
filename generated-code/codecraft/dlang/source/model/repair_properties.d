import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct RepairProperties {
    EntityType[] validTargets;
    int power;

    this(EntityType[] validTargets, int power) {
        this.validTargets = validTargets;
        this.power = power;
    }

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

    void writeTo(Stream writer) const {
        writer.write(cast(int)(validTargets.length));
        foreach (validTargetsElement; validTargets) {
            writer.write(cast(int)(validTargetsElement));
        }
        writer.write(power);
    }
}