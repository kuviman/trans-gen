import model;
import stream;
import std.conv;
import std.typecons : Nullable;

/// Game entity
struct Entity {
    /// Entity's ID. Unique for each entity
    int id;
    /// Entity's owner player ID, if owned by a player
    Nullable!(int) playerId;
    /// Entity's type
    EntityType entityType;
    /// Entity's position (corner with minimal coordinates)
    Vec2Int position;
    /// Current health
    int health;
    /// If entity is active, it can perform actions
    bool active;

    this(int id, Nullable!(int) playerId, EntityType entityType, Vec2Int position, int health, bool active) {
        this.id = id;
        this.playerId = playerId;
        this.entityType = entityType;
        this.position = position;
        this.health = health;
        this.active = active;
    }

    /// Read Entity from input stream
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
        entityType = readEntityType(reader);
        Vec2Int position;
        position = Vec2Int.readFrom(reader);
        int health;
        health = reader.readInt();
        bool active;
        active = reader.readBool();
        return Entity(id, playerId, entityType, position, health, active);
    }

    /// Write Entity to output stream
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