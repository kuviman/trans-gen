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
            optionsKey = readEntityType(reader);
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