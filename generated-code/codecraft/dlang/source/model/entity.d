import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Entity {
    int id;
    Nullable!(int) playerId;
    EntityType entityType;
    Vec2Int position;
    int health;
    bool active;

    this(int id, Nullable!(int) playerId, EntityType entityType, Vec2Int position, int health, bool active) {
        this.id = id;
        this.playerId = playerId;
        this.entityType = entityType;
        this.position = position;
        this.health = health;
        this.active = active;
    }

    static Entity readFrom(Stream reader) {
        int id;
        id = reader.readInt();
        Nullable!(int) playerId;
        if (reader.readBool()) {
            playerId = reader.readInt();
        } else {
            playerId.nullify();
        }
        EntityType entityType;
        switch (reader.readInt()) {
            case 0:
                entityType = EntityType.Wall;
                break;
            case 1:
                entityType = EntityType.House;
                break;
            case 2:
                entityType = EntityType.BuilderBase;
                break;
            case 3:
                entityType = EntityType.BuilderUnit;
                break;
            case 4:
                entityType = EntityType.MeleeBase;
                break;
            case 5:
                entityType = EntityType.MeleeUnit;
                break;
            case 6:
                entityType = EntityType.RangedBase;
                break;
            case 7:
                entityType = EntityType.RangedUnit;
                break;
            case 8:
                entityType = EntityType.Resource;
                break;
            case 9:
                entityType = EntityType.Turret;
                break;
            default:
                throw new Exception("Unexpected tag value");
        }
        Vec2Int position;
        position = Vec2Int.readFrom(reader);
        int health;
        health = reader.readInt();
        bool active;
        active = reader.readBool();
        return Entity(id, playerId, entityType, position, health, active);
    }

    void writeTo(Stream writer) const {
        writer.write(id);
        if (playerId.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            writer.write(playerId.get);
        }
        writer.write(cast(int)(entityType));
        position.writeTo(writer);
        writer.write(health);
        writer.write(active);
    }
}