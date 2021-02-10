module model.build_action;

import stream;
import std.conv;
import std.typecons : Nullable;
import model.entity_type;
import vec2_int;

/// Build action
struct BuildAction {
    /// Type of an entity to build
    model.EntityType entityType;
    /// Desired position of new entity
    Vec2Int position;

    this(model.EntityType entityType, Vec2Int position) {
        this.entityType = entityType;
        this.position = position;
    }

    /// Read BuildAction from reader
    static BuildAction readFrom(Stream reader) {
        model.EntityType entityType;
        entityType = readEntityType(reader);
        Vec2Int position;
        position = Vec2Int.readFrom(reader);
        return BuildAction(entityType, position);
    }

    /// Write BuildAction to writer
    void writeTo(Stream writer) const {
        writer.write(cast(int)(entityType));
        position.writeTo(writer);
    }
}