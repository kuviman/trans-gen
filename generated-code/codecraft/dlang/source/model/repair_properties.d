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
            switch (reader.readInt()) {
                case 0:
                    validTargetsKey = EntityType.Wall;
                    break;
                case 1:
                    validTargetsKey = EntityType.House;
                    break;
                case 2:
                    validTargetsKey = EntityType.BuilderBase;
                    break;
                case 3:
                    validTargetsKey = EntityType.BuilderUnit;
                    break;
                case 4:
                    validTargetsKey = EntityType.MeleeBase;
                    break;
                case 5:
                    validTargetsKey = EntityType.MeleeUnit;
                    break;
                case 6:
                    validTargetsKey = EntityType.RangedBase;
                    break;
                case 7:
                    validTargetsKey = EntityType.RangedUnit;
                    break;
                case 8:
                    validTargetsKey = EntityType.Resource;
                    break;
                case 9:
                    validTargetsKey = EntityType.Turret;
                    break;
                default:
                    throw new Exception("Unexpected tag value");
            }
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