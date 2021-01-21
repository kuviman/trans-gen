#include "Entity.hpp"

Entity::Entity() { }

Entity::Entity(int id, std::shared_ptr<int> playerId, EntityType entityType, Vec2Int position, int health, bool active) : id(id), playerId(playerId), entityType(entityType), position(position), health(health), active(active) { }

Entity Entity::readFrom(InputStream& stream) {
    int id;
    id = stream.readInt();
    std::shared_ptr<int> playerId;
    if (stream.readBool()) {
        playerId = std::shared_ptr<int>(new int());
        *playerId = stream.readInt();
    } else {
        playerId = std::shared_ptr<int>();
    }
    EntityType entityType;
    entityType = readEntityType(stream);
    Vec2Int position;
    position = Vec2Int::readFrom(stream);
    int health;
    health = stream.readInt();
    bool active;
    active = stream.readBool();
    return Entity(id, playerId, entityType, position, health, active);
}

void Entity::writeTo(OutputStream& stream) const {
    stream.write(id);
    if (playerId) {
        stream.write(true);
        const int& playerIdValue = *playerId;
        stream.write(playerIdValue);
    } else {
        stream.write(false);
    }
    stream.write((int)(entityType));
    position.writeTo(stream);
    stream.write(health);
    stream.write(active);
}