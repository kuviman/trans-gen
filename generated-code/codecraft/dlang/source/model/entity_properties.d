import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct EntityProperties {
    int size;
    int buildScore;
    int destroyScore;
    bool canMove;
    int populationProvide;
    int populationUse;
    int maxHealth;
    int initialCost;
    int sightRange;
    int resourcePerHealth;
    Nullable!(BuildProperties) build;
    Nullable!(AttackProperties) attack;
    Nullable!(RepairProperties) repair;

    this(int size, int buildScore, int destroyScore, bool canMove, int populationProvide, int populationUse, int maxHealth, int initialCost, int sightRange, int resourcePerHealth, Nullable!(BuildProperties) build, Nullable!(AttackProperties) attack, Nullable!(RepairProperties) repair) {
        this.size = size;
        this.buildScore = buildScore;
        this.destroyScore = destroyScore;
        this.canMove = canMove;
        this.populationProvide = populationProvide;
        this.populationUse = populationUse;
        this.maxHealth = maxHealth;
        this.initialCost = initialCost;
        this.sightRange = sightRange;
        this.resourcePerHealth = resourcePerHealth;
        this.build = build;
        this.attack = attack;
        this.repair = repair;
    }

    static EntityProperties readFrom(Stream reader) {
        int size;
        size = reader.readInt();
        int buildScore;
        buildScore = reader.readInt();
        int destroyScore;
        destroyScore = reader.readInt();
        bool canMove;
        canMove = reader.readBool();
        int populationProvide;
        populationProvide = reader.readInt();
        int populationUse;
        populationUse = reader.readInt();
        int maxHealth;
        maxHealth = reader.readInt();
        int initialCost;
        initialCost = reader.readInt();
        int sightRange;
        sightRange = reader.readInt();
        int resourcePerHealth;
        resourcePerHealth = reader.readInt();
        Nullable!(BuildProperties) build;
        if (reader.readBool()) {
            build = BuildProperties.readFrom(reader);
        } else {
            build.nullify();
        }
        Nullable!(AttackProperties) attack;
        if (reader.readBool()) {
            attack = AttackProperties.readFrom(reader);
        } else {
            attack.nullify();
        }
        Nullable!(RepairProperties) repair;
        if (reader.readBool()) {
            repair = RepairProperties.readFrom(reader);
        } else {
            repair.nullify();
        }
        return EntityProperties(size, buildScore, destroyScore, canMove, populationProvide, populationUse, maxHealth, initialCost, sightRange, resourcePerHealth, build, attack, repair);
    }

    void writeTo(Stream writer) const {
        writer.write(size);
        writer.write(buildScore);
        writer.write(destroyScore);
        writer.write(canMove);
        writer.write(populationProvide);
        writer.write(populationUse);
        writer.write(maxHealth);
        writer.write(initialCost);
        writer.write(sightRange);
        writer.write(resourcePerHealth);
        if (build.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            build.get.writeTo(writer);
        }
        if (attack.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            attack.get.writeTo(writer);
        }
        if (repair.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            repair.get.writeTo(writer);
        }
    }
}