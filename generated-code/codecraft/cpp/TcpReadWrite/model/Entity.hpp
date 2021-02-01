#ifndef __MODEL_ENTITY_HPP__
#define __MODEL_ENTITY_HPP__

#include "../Stream.hpp"
#include "EntityType.hpp"
#include "Vec2Int.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>

// Game entity
class Entity {
public:
    // Entity's ID. Unique for each entity
    int id;
    // Entity's owner player ID, if owned by a player
    std::optional<int> playerId;
    // Entity's type
    EntityType entityType;
    // Entity's position (corner with minimal coordinates)
    Vec2Int position;
    // Current health
    int health;
    // If entity is active, it can perform actions
    bool active;

    Entity(int id, std::optional<int> playerId, EntityType entityType, Vec2Int position, int health, bool active);

    // Read Entity from input stream
    static Entity readFrom(InputStream& stream);

    // Write Entity to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Entity
    std::string toString() const;
};

#endif