#include "Entity.hpp"

Entity::Entity() { }

Entity::Entity(int id, std::optional<int> playerId, EntityType entityType, Vec2Int position, int health, bool active) : id(id), playerId(playerId), entityType(entityType), position(position), health(health), active(active) { }

// Read Entity from input stream
Entity Entity::readFrom(InputStream& stream) {
    int id;
    id = stream.readInt();
    std::optional<int> playerId;
    if (stream.readBool()) {
        int playerIdValue;
        playerIdValue = stream.readInt();
        playerId = playerIdValue;
    } else {
        playerId = std::optional<int>();
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

// Write Entity to output stream
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

// Get string representation of Entity
std::string Entity::toString() const {
    std::stringstream ss;
    ss << "Entity { ";
    ss << "id: ";
    ss << id;
    ss << ", ";
    ss << "playerId: ";
    if (playerId) {
        const int& playerIdValue = *playerId;
        ss << playerIdValue;
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "entityType: ";
    ss << entityTypeToString(entityType);
    ss << ", ";
    ss << "position: ";
    ss << position.toString();
    ss << ", ";
    ss << "health: ";
    ss << health;
    ss << ", ";
    ss << "active: ";
    ss << active;
    ss << " }";
    return ss.str();
}