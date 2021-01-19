import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct BuildProperties {
    EntityType[] options;
    Nullable!(int) initHealth;

    this(EntityType[] options, Nullable!(int) initHealth) {
        this.options = options;
        this.initHealth = initHealth;
    }

    static BuildProperties readFrom(Stream reader) {
        EntityType[] options;
        options = new EntityType[reader.readInt()];
        for (int optionsIndex = 0; optionsIndex < options.length; optionsIndex++) {
            EntityType optionsKey;
            switch (reader.readInt()) {
                case 0:
                    optionsKey = EntityType.Wall;
                    break;
                case 1:
                    optionsKey = EntityType.House;
                    break;
                case 2:
                    optionsKey = EntityType.BuilderBase;
                    break;
                case 3:
                    optionsKey = EntityType.BuilderUnit;
                    break;
                case 4:
                    optionsKey = EntityType.MeleeBase;
                    break;
                case 5:
                    optionsKey = EntityType.MeleeUnit;
                    break;
                case 6:
                    optionsKey = EntityType.RangedBase;
                    break;
                case 7:
                    optionsKey = EntityType.RangedUnit;
                    break;
                case 8:
                    optionsKey = EntityType.Resource;
                    break;
                case 9:
                    optionsKey = EntityType.Turret;
                    break;
                default:
                    throw new Exception("Unexpected tag value");
            }
            options[optionsIndex] = optionsKey;
        }
        Nullable!(int) initHealth;
        if (reader.readBool()) {
            initHealth = reader.readInt();
        } else {
            initHealth.nullify();
        }
        return BuildProperties(options, initHealth);
    }

    void writeTo(Stream writer) const {
        writer.write(cast(int)(options.length));
        foreach (optionsElement; options) {
            writer.write(cast(int)(optionsElement));
        }
        if (initHealth.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(initHealth.get);
        }
    }
}